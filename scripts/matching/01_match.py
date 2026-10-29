"""Match each retracted article to same-journal-year comparison articles using pre-retraction
citations only.

Usage: python3 -m scripts.matching.01_match [--controls 5] [--caliper 1.0]
Output: data/matching/matches.csv, match_candidates.csv, match_exclusions.csv, match_balance.csv

Adapted from recite/sig_fault scripts/i4r_match.py at commit
1efbfd2172879c93b9c8a5c31f601d1e53f976a2: the text-distance term is dropped, and the only
route into the matcher is `pre_event_histories`, which truncates every history at the year
before retraction, so a test can perturb post-event outcomes and assert identical matches.
"""

from __future__ import annotations

import argparse
import collections
import importlib
import json

import numpy as np

from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

histories = importlib.import_module("scripts.openalex.02_histories")
DATA = ROOT / "data/matching"
OPENALEX = ROOT / "data/openalex"
MATCH_FIELDS = ["article_id", "control_id", "rank", "distance", "weight"]
CANDIDATE_FIELDS = ["article_id", "candidate_id", "distance", "eligible", "reason"]
BALANCE_FIELDS = [
    "article_id",
    "control_id",
    "cumulative_smd",
    "last_year_smd",
    "treated_cumulative",
    "control_cumulative",
    "treated_last_year",
    "control_last_year",
]


def history_index(rows):
    index = collections.defaultdict(dict)
    for r in rows:
        index[r["target_id"]][int(r["year"])] = int(r["citations"])
    return index


def pre_event_histories(index, retraction_year):
    """Citation years strictly before the retraction year; nothing later is visible."""
    return {
        tid: {y: n for y, n in series.items() if y < retraction_year}
        for tid, series in index.items()
    }


def pre_inputs(series, retraction_year):
    """Two matching inputs: log1p cumulative citations through r-1 and log1p citations in r-1."""
    if not series or (retraction_year - 1) not in series:
        return None
    return np.log1p([sum(series.values()), series[retraction_year - 1]])


def match(treated, pools, candidates, treated_index, candidate_index, k=5, caliper=1.0):
    pool_candidates = collections.defaultdict(list)
    for c in candidates:
        if not c["exclusion"]:
            pool_candidates[c["pool_id"]].append(c)
    pool_of = {(p["journal_id"], p["year"]): p["pool_id"] for p in pools}
    matches, scored, exclusions, balance = [], [], [], []
    for t in treated:
        r = int(t["retraction_year"])
        pid = pool_of.get((t["journal_id"], t["openalex_year"]))
        record = dict(article_id=t["article_id"])
        if pid is None:
            exclusions.append(dict(record, reason="pool_not_retrieved"))
            continue
        visible_t = pre_event_histories(
            {t["article_id"]: treated_index.get(t["article_id"], {})}, r
        )
        tv = pre_inputs(visible_t[t["article_id"]], r)
        if tv is None:
            exclusions.append(dict(record, reason="incomplete_pre_citations"))
            continue
        pool = []
        for c in pool_candidates.get(pid, []):
            visible_c = pre_event_histories(
                {c["candidate_id"]: candidate_index.get(c["candidate_id"], {})}, r
            )
            cv = pre_inputs(visible_c[c["candidate_id"]], r)
            if cv is None:
                scored.append(
                    dict(
                        record,
                        candidate_id=c["candidate_id"],
                        distance="",
                        eligible="no",
                        reason="incomplete_pre_citations",
                    )
                )
                continue
            pool.append((c, cv))
        if not pool:
            exclusions.append(dict(record, reason="no_usable_candidates"))
            continue
        raw = np.vstack([tv] + [cv for _, cv in pool])
        sd = raw.std(axis=0, ddof=1)
        delta = np.divide(raw[1:] - raw[0], sd, out=np.zeros_like(raw[1:]), where=sd > 0)
        distance = np.sqrt((delta**2).sum(axis=1))
        accepted = []
        for j, (c, cv) in enumerate(pool):
            valid = bool((np.abs(delta[j]) <= caliper + 1e-12).all())
            scored.append(
                dict(
                    record,
                    candidate_id=c["candidate_id"],
                    distance=float(distance[j]),
                    eligible="yes" if valid else "no",
                    reason="" if valid else "citation_caliper",
                )
            )
            if valid:
                accepted.append((float(distance[j]), c["candidate_id"], j))
        best = sorted(accepted)[:k]
        if not best:
            exclusions.append(dict(record, reason="no_match_within_caliper"))
            continue
        for rank, (dist, cid, j) in enumerate(best, 1):
            matches.append(
                dict(
                    record,
                    control_id=cid,
                    rank=rank,
                    distance=dist,
                    weight=1 / len(best),
                )
            )
            balance.append(
                dict(
                    record,
                    control_id=cid,
                    cumulative_smd=float(delta[j, 0]),
                    last_year_smd=float(delta[j, 1]),
                    treated_cumulative=float(np.expm1(raw[0, 0])),
                    control_cumulative=float(np.expm1(raw[j + 1, 0])),
                    treated_last_year=float(np.expm1(raw[0, 1])),
                    control_last_year=float(np.expm1(raw[j + 1, 1])),
                )
            )
    return matches, scored, exclusions, balance


def main(k, caliper):
    with Run("matching", "01_match", __file__, offline=True) as run:
        run.require("02_histories", study="openalex")
        run.require("03_pools", study="openalex")
        run.require("04_candidates", study="openalex")
        run.require("02_frame", study="retraction_watch")
        frame = {r["article_id"]: r for r in read_csv(ROOT / "data/retraction_watch/frame.csv")}
        targets = read_csv(OPENALEX / "targets.csv")
        treated = []
        for t in targets:
            if t["article_id"] and t["resolution"] == "resolved" and not t["frame_exclusion"]:
                treated.append(dict(t, retraction_year=frame[t["article_id"]]["retraction_year"]))
        pools = read_csv(OPENALEX / "pools.csv")
        candidates = read_csv(OPENALEX / "candidates.csv")
        treated_index = history_index(histories.read_gzip_csv(OPENALEX / "histories.csv.gz"))
        candidate_index = history_index(
            histories.read_gzip_csv(OPENALEX / "candidate_histories.csv.gz")
        )
        matches, scored, exclusions, balance = match(
            treated, pools, candidates, treated_index, candidate_index, k, caliper
        )
        unique(matches, ["article_id", "control_id"])
        matched = {m["article_id"] for m in matches}
        run.check(
            "every_treated_matched_or_excluded",
            len(matched) + len(exclusions) == len(treated),
            dict(treated=len(treated), matched=len(matched), excluded=len(exclusions)),
        )
        write_csv(DATA / "matches.csv", matches, MATCH_FIELDS)
        write_csv(DATA / "match_candidates.csv", scored, CANDIDATE_FIELDS)
        write_csv(DATA / "match_exclusions.csv", exclusions, ["article_id", "reason"])
        write_csv(DATA / "match_balance.csv", balance, BALANCE_FIELDS)
        summary = dict(
            controls_per_treated=k,
            caliper=caliper,
            treated=len(treated),
            matched=len(matched),
            controls=len({m["control_id"] for m in matches}),
            exclusions=dict(collections.Counter(e["reason"] for e in exclusions)),
            matches_per_treated=dict(
                collections.Counter(collections.Counter(m["article_id"] for m in matches).values())
            ),
            mean_abs_cumulative_smd=(
                float(np.mean([abs(b["cumulative_smd"]) for b in balance])) if balance else None
            ),
            mean_abs_last_year_smd=(
                float(np.mean([abs(b["last_year_smd"]) for b in balance])) if balance else None
            ),
        )
        write_json(DATA / "match_summary.json", summary)
        for name in [
            "matches.csv",
            "match_candidates.csv",
            "match_exclusions.csv",
            "match_balance.csv",
            "match_summary.json",
        ]:
            run.output(DATA / name)
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--controls", type=int, default=5)
    parser.add_argument("--caliper", type=float, default=1.0)
    args = parser.parse_args()
    main(args.controls, args.caliper)
