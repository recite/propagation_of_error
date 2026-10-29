"""Annual citation counts for usable comparison candidates, measured like treated articles.

Usage: python3 -m scripts.openalex.04_candidates [--offline] [--pilot] [--workers 4]
Output: data/openalex/candidate_histories.csv.gz, candidate_totals.csv
"""

from __future__ import annotations

import argparse
import concurrent.futures
import importlib
import json

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

histories = importlib.import_module("scripts.openalex.02_histories")
DATA = ROOT / "data/openalex"


def main(offline, pilot, workers):
    with Run("openalex", "04_candidates", __file__, offline) as run:
        run.require("03_pools")
        candidates = [c for c in read_csv(DATA / "candidates.csv") if not c["exclusion"]]
        if pilot:
            targets = read_csv(DATA / "targets.csv")
            pilot_keys = {
                (t["journal_id"], t["openalex_year"])
                for t in targets
                if t["pilot"] == "1" and t["resolution"] == "resolved" and not t["frame_exclusion"]
            }
            pools = {
                p["pool_id"]
                for p in read_csv(DATA / "pools.csv")
                if (p["journal_id"], p["year"]) in pilot_keys
            }
            candidates = [c for c in candidates if c["pool_id"] in pools]
        seen, work = set(), []
        for c in candidates:
            if c["candidate_id"] in seen:
                continue
            seen.add(c["candidate_id"])
            work.append(
                dict(
                    target_id=c["candidate_id"],
                    work_id=c["work_id"],
                    openalex_year=c["publication_year"],
                )
            )
        rows, totals, failures = [], [], []
        with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {executor.submit(histories.acquire, run, t): t for t in work}
            for done, future in enumerate(concurrent.futures.as_completed(futures), 1):
                try:
                    series, total = future.result()
                except (FileNotFoundError, openalex.BudgetExhausted) as error:
                    failures.append(dict(target_id=futures[future]["target_id"], error=str(error)))
                    continue
                rows.extend(series)
                totals.append(total)
                if done % 2000 == 0:
                    print(f"{done}/{len(work)} candidate histories", flush=True)
        rows.sort(key=lambda r: (r["target_id"], r["year"]))
        totals.sort(key=lambda r: r["target_id"])
        unique(rows, ["target_id", "year"])
        run.check("all_candidate_histories_retrieved", not failures, failures[:5])
        run.check(
            "grouped_counts_match_api_totals",
            all(t["complete"] for t in totals),
            sum(1 for t in totals if not t["complete"]),
        )
        histories.write_gzip_csv(
            DATA / "candidate_histories.csv.gz", rows, histories.HISTORY_FIELDS
        )
        write_csv(DATA / "candidate_totals.csv", totals, histories.TOTAL_FIELDS)
        run.output(DATA / "candidate_histories.csv.gz")
        run.output(DATA / "candidate_totals.csv")
        summary = dict(
            candidates=len(work),
            pilot=pilot,
            article_years=len(rows),
            citations=sum(t["sum_counts"] for t in totals),
        )
        write_json(DATA / "candidates_summary.json", summary)
        run.output(DATA / "candidates_summary.json")
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--pilot", action="store_true")
    parser.add_argument("--workers", type=int, default=4)
    args = parser.parse_args()
    main(args.offline, args.pilot, args.workers)
