"""Stacked event-time panel: each retracted article with its matched comparison articles.

Usage: python3 -m scripts.matching.02_panel
Output: data/matching/panel.csv.gz, panel_exclusions.csv, sample_flow.csv, stacks.csv
"""

from __future__ import annotations

import collections
import importlib
import json

from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv

histories = importlib.import_module("scripts.openalex.02_histories")
matching = importlib.import_module("scripts.matching.01_match")
DATA = ROOT / "data/matching"
OPENALEX = ROOT / "data/openalex"
EVENT_TIMES = list(range(-3, 6))
LAST_YEAR = histories.LAST_YEAR
PANEL_FIELDS = [
    "stack_id",
    "article_id",
    "work_id",
    "treated",
    "match_rank",
    "weight",
    "year",
    "event_time",
    "citations",
    "status",
    "retraction_year",
    "publication_year",
    "journal_id",
    "reason_category",
    "field",
    "prior_concern",
    "lag_years",
    "pre_tercile",
    "sdid_eligible",
]
STACK_FIELDS = [
    "stack_id",
    "retraction_year",
    "publication_year",
    "journal_id",
    "controls",
    "reason_category",
    "field",
    "prior_concern",
    "lag_years",
    "pre_cumulative",
    "pre_tercile",
    "complete_pre_years",
    "complete_post_years",
    "sdid_eligible",
]


def tercile(values):
    ordered = sorted(values)
    cuts = [ordered[len(ordered) // 3], ordered[2 * len(ordered) // 3]]
    return lambda v: 1 if v < cuts[0] else 2 if v < cuts[1] else 3


def build(matches, treated, treated_index, candidate_index, candidates):
    grouped = collections.defaultdict(list)
    for m in matches:
        grouped[m["article_id"]].append(m)
    candidate_year = {c["candidate_id"]: int(c["publication_year"]) for c in candidates}
    candidate_work = {c["candidate_id"]: c["work_id"] for c in candidates}
    pre_cum = {}
    for t in treated:
        r = int(t["retraction_year"])
        visible = matching.pre_event_histories({t["article_id"]: treated_index[t["article_id"]]}, r)
        pre_cum[t["article_id"]] = sum(visible[t["article_id"]].values())
    tercile_of = tercile([pre_cum[a] for a in grouped])
    rows, stacks, omissions = [], [], []
    for t in treated:
        aid = t["article_id"]
        if aid not in grouped:
            continue
        r, pub = int(t["retraction_year"]), int(t["openalex_year"])
        members = [(aid, t["work_id"], 1, 0, 1.0, pub, treated_index[aid])] + [
            (
                m["control_id"],
                candidate_work[m["control_id"]],
                0,
                int(m["rank"]),
                float(m["weight"]),
                candidate_year[m["control_id"]],
                candidate_index[m["control_id"]],
            )
            for m in grouped[aid]
        ]
        pre_ok = min(r - 1 - pub, 3)
        post_ok = min(LAST_YEAR - r, 5)
        if post_ok < 1:
            omissions.append(dict(stack_id=aid, reason="no_complete_post_year"))
            continue
        sdid = int(all(r - 3 >= year for _, _, _, _, _, year, _ in members) and post_ok >= 3)
        meta = dict(
            retraction_year=r,
            publication_year=pub,
            journal_id=t["journal_id"],
            reason_category=t["reason_category"],
            field=t["field"],
            prior_concern=t["prior_concern"],
            lag_years=t["lag_years"],
            pre_tercile=tercile_of(pre_cum[aid]),
            sdid_eligible=sdid,
        )
        stacks.append(
            dict(
                stack_id=aid,
                controls=len(grouped[aid]),
                pre_cumulative=pre_cum[aid],
                complete_pre_years=pre_ok,
                complete_post_years=post_ok,
                **meta,
            )
        )
        for member_id, work_id, flag, rank, weight, year0, series in members:
            for et in EVENT_TIMES:
                year = r + et
                if year < year0 or year > LAST_YEAR:
                    continue
                rows.append(
                    dict(
                        stack_id=aid,
                        article_id=member_id,
                        work_id=work_id,
                        treated=flag,
                        match_rank=rank,
                        weight=weight,
                        year=year,
                        event_time=et,
                        citations=series[year],
                        status="complete",
                        **meta,
                    )
                )
    return rows, stacks, omissions


def main():
    with Run("matching", "02_panel", __file__, offline=True) as run:
        run.require("01_match")
        frame = {r["article_id"]: r for r in read_csv(ROOT / "data/retraction_watch/frame.csv")}
        targets = read_csv(OPENALEX / "targets.csv")
        treated = [
            dict(
                t,
                **{
                    k: frame[t["article_id"]][k]
                    for k in [
                        "retraction_year",
                        "reason_category",
                        "field",
                        "prior_concern",
                        "lag_years",
                    ]
                },
            )
            for t in targets
            if t["article_id"] and t["resolution"] == "resolved" and not t["frame_exclusion"]
        ]
        matches = read_csv(DATA / "matches.csv")
        candidates = read_csv(OPENALEX / "candidates.csv")
        treated_index = matching.history_index(
            histories.read_gzip_csv(OPENALEX / "histories.csv.gz")
        )
        candidate_index = matching.history_index(
            histories.read_gzip_csv(OPENALEX / "candidate_histories.csv.gz")
        )
        rows, stacks, omissions = build(
            matches, treated, treated_index, candidate_index, candidates
        )
        unique(rows, ["stack_id", "article_id", "year"])
        run.check(
            "stacks_match_matched_treated",
            len(stacks) + len(omissions) == len({m["article_id"] for m in matches}),
            len(stacks),
        )
        run.check("no_citations_missing", all(r["citations"] != "" for r in rows), len(rows))
        histories.write_gzip_csv(DATA / "panel.csv.gz", rows, PANEL_FIELDS)
        write_csv(DATA / "stacks.csv", stacks, STACK_FIELDS)
        write_csv(DATA / "panel_exclusions.csv", omissions, ["stack_id", "reason"])
        snapshot = json.loads((ROOT / "data/retraction_watch/snapshot.json").read_text())
        resolution = json.loads((OPENALEX / "target_resolution.json").read_text())
        match_summary = json.loads((DATA / "match_summary.json").read_text())
        flow = [
            dict(
                step="Retraction Watch retractions",
                articles=snapshot["rows_by_nature"]["Retraction"],
            ),
            dict(
                step=(
                    "Frame: research articles, published 2000-2019, retracted 2005-2022, "
                    "two or more years after publication, with a DOI"
                ),
                articles=len(frame),
            ),
            dict(
                step="Resolved to an OpenAlex journal article in the same year",
                articles=resolution["frame_eligible"],
            ),
            dict(
                step="Matched to at least one comparison article",
                articles=match_summary["matched"],
            ),
            dict(
                step="With at least one complete post-retraction year",
                articles=len(stacks),
            ),
            dict(
                step="With three complete pre- and post-retraction years (synthetic DiD)",
                articles=sum(s["sdid_eligible"] for s in stacks),
            ),
        ]
        write_csv(DATA / "sample_flow.csv", flow, ["step", "articles"])
        for name in [
            "panel.csv.gz",
            "stacks.csv",
            "panel_exclusions.csv",
            "sample_flow.csv",
        ]:
            run.output(DATA / name)
        summary = dict(
            stacks=len(stacks),
            rows=len(rows),
            omissions=len(omissions),
            sdid_eligible=sum(s["sdid_eligible"] for s in stacks),
            flow=flow,
        )
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    main()
