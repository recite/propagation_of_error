"""Fix the treated set: the whole frame by default, or a stratified sample.

Usage: python3 -m scripts.retraction_watch.03_sample [--size N] [--pilot 200]
The pilot flag marks the first articles of a seeded random order; pilot fetches
are cached and reused, so the pilot costs nothing extra.
Output: data/retraction_watch/sample.csv, sampling.json
"""

from __future__ import annotations

import argparse
import collections
import json
import random

from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

DATA = ROOT / "data/retraction_watch"
SEED = 20261028
FLOOR = 20
JOURNAL_YEAR_CAP = 10
SAMPLE_FIELDS = [
    "article_id",
    "stratum",
    "stratum_size",
    "stratum_sample_size",
    "inclusion_probability",
    "sample_order",
    "pilot",
]


def stratified_sample(frame, size, seed, floor=FLOOR, cap=JOURNAL_YEAR_CAP):
    """Proportional allocation across retraction-year x field cells with a floor.

    Within a cell, draws are in seeded random order but skip an article once its
    journal-year already holds `cap` sampled articles, so paper-mill journals cannot
    dominate the sample or exhaust their own comparison pool.
    """
    rng = random.Random(seed)
    cells = collections.defaultdict(list)
    for row in sorted(frame, key=lambda r: r["article_id"]):
        cells[(row["retraction_year"], row["field"])].append(row)
    total = len(frame)
    allocation = {}
    for key, rows in cells.items():
        allocation[key] = min(len(rows), max(floor, round(size * len(rows) / total)))
    journal_year = collections.Counter()
    sample = []
    for key in sorted(cells):
        rows = cells[key][:]
        rng.shuffle(rows)
        taken = 0
        for row in rows:
            if taken >= allocation[key]:
                break
            jy = (row["journal"], row["publication_year"])
            if journal_year[jy] >= cap:
                continue
            journal_year[jy] += 1
            taken += 1
            sample.append(
                dict(
                    article_id=row["article_id"],
                    stratum=f"{key[0]}/{key[1]}",
                    stratum_size=len(cells[key]),
                    stratum_sample_size=allocation[key],
                    inclusion_probability=allocation[key] / len(cells[key]),
                )
            )
    rng.shuffle(sample)
    for order, row in enumerate(sample, 1):
        row["sample_order"] = order
    return sample


def census(frame, seed):
    rng = random.Random(seed)
    rows = sorted(frame, key=lambda r: r["article_id"])
    rng.shuffle(rows)
    return [
        dict(
            article_id=r["article_id"],
            stratum=f"{r['retraction_year']}/{r['field']}",
            stratum_size="",
            stratum_sample_size="",
            inclusion_probability=1.0,
            sample_order=order,
        )
        for order, r in enumerate(rows, 1)
    ]


def main(size, pilot):
    with Run("retraction_watch", "03_sample", __file__, offline=True) as run:
        run.require("02_frame")
        frame = read_csv(DATA / "frame.csv")
        if size is None or size >= len(frame):
            sample = census(frame, SEED)
            size = len(frame)
        else:
            sample = stratified_sample(frame, size, SEED)
        for row in sample:
            row["pilot"] = int(row["sample_order"] <= pilot)
        unique(sample, ["article_id"])
        run.check("sample_size", abs(len(sample) - size) <= 0.1 * size, len(sample))
        run.check(
            "every_cell_represented",
            {r["stratum"] for r in sample}
            == {f"{r['retraction_year']}/{r['field']}" for r in frame},
            "strata",
        )
        write_csv(DATA / "sample.csv", sample, SAMPLE_FIELDS)

        run.output(DATA / "sample.csv")
        summary = dict(
            seed=SEED,
            requested=size,
            drawn=len(sample),
            pilot=pilot,
            floor=FLOOR,
            journal_year_cap=JOURNAL_YEAR_CAP,
            by_field=dict(collections.Counter(r["stratum"].split("/")[1] for r in sample)),
        )
        write_json(DATA / "sampling.json", summary)

        run.output(DATA / "sampling.json")
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--size", type=int, default=None)
    parser.add_argument("--pilot", type=int, default=200)
    args = parser.parse_args()
    main(args.size, args.pilot)
