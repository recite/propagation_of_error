"""Journal-year comparison pools for every eligible treated article.

Usage: python3 -m scripts.openalex.03_pools [--offline] [--pilot] [--workers 4]
Output: data/openalex/pools.csv, candidates.csv

A pool is the set of OpenAlex journal articles in the same source and publication
year as a treated article. Each pool is represented by a seeded random sample of up to
POOL_SAMPLE works drawn by the API, so no post-event information (such as citation
counts) can influence which candidates are seen.
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import hashlib
import importlib
import json

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

get = importlib.import_module("scripts.retraction_watch.01_get")
DATA = ROOT / "data/openalex"
SEED = 20261028
POOL_SAMPLE = 20
SELECT = "id,doi,title,publication_year,publication_date,type,is_retracted,primary_location"
POOL_FIELDS = [
    "pool_id",
    "journal_id",
    "journal_name",
    "year",
    "pool_count",
    "retrieved",
    "complete_pool",
    "treated_in_pool",
    "usable_candidates",
]
CANDIDATE_FIELDS = [
    "candidate_id",
    "pool_id",
    "work_id",
    "doi",
    "title",
    "publication_year",
    "publication_date",
    "type",
    "is_retracted",
    "exclusion",
]


def pool_id(journal_id, year):
    return "pool_" + hashlib.sha256(f"{journal_id}|{year}".encode()).hexdigest()[:12]


def candidate_id(work_id):
    return "ctl_" + hashlib.sha256(work_id.encode()).hexdigest()[:14]


def acquire(run, journal_id, year):
    """Pool size from a one-record call, then a seeded random sample of the pool.

    OpenAlex orders unsampled lists by citations, so a plain first page would select
    comparison articles by their (post-event) citation counts. The sample call reports
    the sample size as its count, hence the separate count call.
    """
    base = dict(
        filter=f"primary_location.source.id:{openalex.work_key(journal_id)},"
        f"publication_year:{year},type:article",
        select=SELECT,
    )
    counted = run.fetch(
        openalex.api_url("works", per_page=1, select="id", filter=base["filter"]),
        "pool_counts",
    )
    count = int(counted["meta"]["count"])
    sampled = run.fetch(
        openalex.api_url("works", sample=POOL_SAMPLE, seed=SEED, per_page=POOL_SAMPLE, **base),
        "pools",
    )
    works = sampled["results"]
    if count <= POOL_SAMPLE and len(works) != count:
        raise ValueError(f"Sample of a small pool is incomplete: {journal_id} {year}")
    return count, works, count <= POOL_SAMPLE


def classify(work, screened_dois, treated_ids):
    if work["id"] in treated_ids:
        return "treated_article"
    if work.get("is_retracted"):
        return "openalex_is_retracted"
    doi = openalex.normalize_doi(work.get("doi") or "")
    if doi and doi in screened_dois:
        return "retraction_watch_record"
    if work.get("type") != "article":
        return "not_article"
    return ""


def main(offline, pilot, workers):
    with Run("openalex", "03_pools", __file__, offline) as run:
        run.require("01_targets")
        run.require("01_get", study="retraction_watch")
        run.input(get.CACHE)
        screened = {openalex.normalize_doi(r["OriginalPaperDOI"]) for r in get.read_snapshot()}
        screened.discard("")
        targets = [
            t
            for t in read_csv(DATA / "targets.csv")
            if t["article_id"] and t["resolution"] == "resolved" and not t["frame_exclusion"]
        ]
        treated_ids = {t["work_id"] for t in targets}
        if pilot:
            targets = [t for t in targets if t["pilot"] == "1"]
        keys = collections.defaultdict(int)
        names = {}
        for t in targets:
            keys[(t["journal_id"], int(t["openalex_year"]))] += 1
            names[t["journal_id"]] = t["journal_name"]
        pools, candidates, failures = [], [], []
        with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {executor.submit(acquire, run, j, y): (j, y) for j, y in sorted(keys)}
            for done, future in enumerate(concurrent.futures.as_completed(futures), 1):
                journal_id, year = futures[future]
                try:
                    count, works, complete = future.result()
                except FileNotFoundError as error:
                    failures.append(dict(journal_id=journal_id, year=year, error=str(error)))
                    continue
                pid = pool_id(journal_id, year)
                usable = 0
                for work in works:
                    exclusion = classify(work, screened, treated_ids)
                    usable += not exclusion
                    candidates.append(
                        dict(
                            candidate_id=candidate_id(work["id"]),
                            pool_id=pid,
                            work_id=work["id"],
                            doi=openalex.normalize_doi(work.get("doi") or ""),
                            title=work.get("title") or "",
                            publication_year=work.get("publication_year") or "",
                            publication_date=work.get("publication_date") or "",
                            type=work.get("type") or "",
                            is_retracted=int(bool(work.get("is_retracted"))),
                            exclusion=exclusion,
                        )
                    )
                pools.append(
                    dict(
                        pool_id=pid,
                        journal_id=journal_id,
                        journal_name=names[journal_id],
                        year=year,
                        pool_count=count,
                        retrieved=len(works),
                        complete_pool=int(complete),
                        treated_in_pool=keys[(journal_id, year)],
                        usable_candidates=usable,
                    )
                )
                if done % 500 == 0:
                    print(f"{done}/{len(keys)} pools", flush=True)
        pools.sort(key=lambda r: r["pool_id"])
        candidates.sort(key=lambda r: (r["pool_id"], r["candidate_id"]))
        unique(pools, ["pool_id"])
        unique(candidates, ["pool_id", "candidate_id"])
        run.check("all_pools_retrieved", not failures, failures[:5])
        run.check(
            "pools_cover_treated",
            len(pools) == len(keys),
            dict(pools=len(pools), keys=len(keys)),
        )
        write_csv(DATA / "pools.csv", pools, POOL_FIELDS)
        write_csv(DATA / "candidates.csv", candidates, CANDIDATE_FIELDS)
        run.output(DATA / "pools.csv")
        run.output(DATA / "candidates.csv")
        summary = dict(
            pools=len(pools),
            pilot=pilot,
            candidates=len(candidates),
            usable_candidates=sum(p["usable_candidates"] for p in pools),
            exclusions=dict(
                collections.Counter(c["exclusion"] for c in candidates if c["exclusion"])
            ),
            thin_pools=sum(1 for p in pools if p["usable_candidates"] < 6),
            complete_pools=sum(p["complete_pool"] for p in pools),
            median_pool_count=(
                sorted(p["pool_count"] for p in pools)[len(pools) // 2] if pools else None
            ),
        )
        write_json(DATA / "pools_summary.json", summary)
        run.output(DATA / "pools_summary.json")
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--pilot", action="store_true")
    parser.add_argument("--workers", type=int, default=4)
    args = parser.parse_args()
    main(args.offline, args.pilot, args.workers)
