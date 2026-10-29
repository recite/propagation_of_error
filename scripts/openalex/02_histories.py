"""Annual citation counts for every resolved target, zero-padded through 2025.

Usage: python3 -m scripts.openalex.02_histories [--offline] [--only wos|frame] [--pilot] [--workers 4]
Output: data/openalex/histories.csv.gz, history_totals.csv

The outcome is the OpenAlex count of citing articles and reviews by publication year
(one grouped call per target). Controls are measured the same way, so the two arms
never differ in measurement.
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import csv
import gzip
import json

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

DATA = ROOT / "data/openalex"
LAST_YEAR = 2025
CUTOFF = f"{LAST_YEAR}-12-31"
HISTORY_FIELDS = ["target_id", "year", "citations"]
TOTAL_FIELDS = ["target_id", "work_id", "first_year", "api_count", "sum_counts",
                "before_publication", "after_cutoff", "complete"]


def pad(counts, first_year, last_year=LAST_YEAR):
    """Complete series from the publication year; earlier or later years are reported, not kept."""
    before = sum(n for y, n in counts.items() if y < first_year)
    after = sum(n for y, n in counts.items() if y > last_year)
    series = [(year, counts.get(year, 0)) for year in range(first_year, last_year + 1)]
    return series, before, after


def acquire(run, target):
    counts, api_count = openalex.group_by_year(run, target["work_id"], cutoff=CUTOFF)
    first_year = int(target["openalex_year"])
    series, before, after = pad(counts, first_year)
    total = dict(target_id=target["target_id"], work_id=target["work_id"], first_year=first_year,
                 api_count=api_count, sum_counts=sum(counts.values()), before_publication=before,
                 after_cutoff=after, complete=int(sum(counts.values()) == api_count))
    return [dict(target_id=target["target_id"], year=y, citations=n) for y, n in series], total


def write_gzip_csv(path, rows, fields):
    with gzip.open(path, "wt", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def read_gzip_csv(path):
    with gzip.open(path, "rt", encoding="utf-8", newline="") as stream:
        return list(csv.DictReader(stream))


def main(offline, only, pilot, workers):
    with Run("openalex", "02_histories", __file__, offline) as run:
        run.require("01_targets")
        targets = [t for t in read_csv(DATA / "targets.csv") if t["resolution"] == "resolved"]
        if only == "wos":
            targets = [t for t in targets if t["wos_index"]]
        elif only == "frame":
            targets = [t for t in targets if t["article_id"] and not t["frame_exclusion"]]
        else:
            targets = [t for t in targets if t["wos_index"] or (t["article_id"] and not t["frame_exclusion"])]
        if pilot:
            targets = [t for t in targets if t["pilot"] == "1"]
        histories, totals, failures = [], [], []
        with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
            futures = {pool.submit(acquire, run, t): t for t in targets}
            for done, future in enumerate(concurrent.futures.as_completed(futures), 1):
                target = futures[future]
                try:
                    rows, total = future.result()
                except FileNotFoundError as error:
                    failures.append(dict(target_id=target["target_id"], error=str(error)))
                    continue
                histories.extend(rows)
                totals.append(total)
                if done % 500 == 0:
                    print(f"{done}/{len(targets)} histories", flush=True)
        totals.sort(key=lambda r: r["target_id"])
        histories.sort(key=lambda r: (r["target_id"], r["year"]))
        unique(histories, ["target_id", "year"])
        run.check("all_requested_histories_retrieved", not failures, failures[:5])
        run.check("grouped_counts_match_api_totals", all(t["complete"] for t in totals),
                  sum(1 for t in totals if not t["complete"]))
        write_gzip_csv(DATA / "histories.csv.gz", histories, HISTORY_FIELDS)
        write_csv(DATA / "history_totals.csv", totals, TOTAL_FIELDS)
        run.output(DATA / "histories.csv.gz")
        run.output(DATA / "history_totals.csv")
        summary = dict(targets=len(targets), only=only or "all", pilot=pilot,
                       article_years=len(histories), citations=sum(t["sum_counts"] for t in totals),
                       before_publication=sum(t["before_publication"] for t in totals),
                       after_cutoff=sum(t["after_cutoff"] for t in totals))
        write_json(DATA / "histories_summary.json", summary)
        run.output(DATA / "histories_summary.json")
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--only", choices=["wos", "frame"])
    parser.add_argument("--pilot", action="store_true")
    parser.add_argument("--workers", type=int, default=4)
    args = parser.parse_args()
    main(args.offline, args.only, args.pilot, args.workers)
