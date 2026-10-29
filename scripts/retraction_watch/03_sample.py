"""Draw the stratified treated sample from the frame with a recorded seed.

Usage: python3 -m scripts.retraction_watch.03_sample [--size 4000] [--pilot 200]
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
SAMPLE_FIELDS = ["article_id", "stratum", "stratum_size", "stratum_sample_size",
                 "inclusion_probability", "sample_order", "pilot"]


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
            sample.append(dict(
                article_id=row["article_id"], stratum=f"{key[0]}/{key[1]}",
                stratum_size=len(cells[key]), stratum_sample_size=allocation[key],
                inclusion_probability=allocation[key] / len(cells[key]),
            ))
    rng.shuffle(sample)
    for order, row in enumerate(sample, 1):
        row["sample_order"] = order
    return sample


def main(size, pilot):
    with Run("retraction_watch", "03_sample", __file__, offline=True) as run:
        run.require("02_frame")
        frame = read_csv(DATA / "frame.csv")
        sample = stratified_sample(frame, size, SEED)
        for row in sample:
            row["pilot"] = int(row["sample_order"] <= pilot)
        unique(sample, ["article_id"])
        run.check("sample_size", abs(len(sample) - size) <= 0.05 * size, len(sample))
        run.check("every_cell_represented",
                  {r["stratum"] for r in sample} == {f"{r['retraction_year']}/{r['field']}" for r in frame},
                  "strata")
        write_csv(run.output(DATA / "sample.csv"), sample, SAMPLE_FIELDS)
        summary = dict(seed=SEED, requested=size, drawn=len(sample), pilot=pilot, floor=FLOOR,
                       journal_year_cap=JOURNAL_YEAR_CAP,
                       by_field=dict(collections.Counter(r["stratum"].split("/")[1] for r in sample)))
        write_json(run.output(DATA / "sampling.json"), summary)
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--size", type=int, default=4000)
    parser.add_argument("--pilot", type=int, default=200)
    args = parser.parse_args()
    main(args.size, args.pilot)
