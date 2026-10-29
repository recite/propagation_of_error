"""Pair the 2016 Web of Science counts with OpenAlex counts for the same articles and years.

Usage: python3 -m scripts.wos.02_compare
Output: data/validation/paired_panel.csv, date_agreement.csv, comparison_summary.json
"""

from __future__ import annotations

import collections
import importlib
import json

from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

histories = importlib.import_module("scripts.openalex.02_histories")
DATA = ROOT / "data/validation"
LAST_WOS_YEAR = 2015
PANEL_FIELDS = ["wos_index", "target_id", "year", "wos", "openalex", "in_frame", "notice_year"]


def paired_panel(articles, counts, targets, oa):
    by_target = {t["wos_index"]: t for t in targets if t["wos_index"] and t["resolution"] == "resolved"}
    wos = {(r["index"], int(r["year"])): int(r["citations"]) for r in counts if r["complete"] == "1"}
    rows = []
    for article in articles:
        target = by_target.get(article["index"])
        if not target or target["target_id"] not in oa:
            continue
        start = max(int(article["publication_year"]), int(target["openalex_year"]))
        for year in range(start, LAST_WOS_YEAR + 1):
            if (article["index"], year) not in wos or year not in oa[target["target_id"]]:
                continue
            rows.append(dict(wos_index=article["index"], target_id=target["target_id"], year=year,
                             wos=wos[(article["index"], year)], openalex=oa[target["target_id"]][year],
                             in_frame=int(bool(target["article_id"])), notice_year=article["notice_year"]))
    return rows


def summarise(rows):
    by_year = collections.defaultdict(lambda: dict(wos=0, openalex=0, n=0))
    for r in rows:
        by_year[r["year"]]["wos"] += r["wos"]
        by_year[r["year"]]["openalex"] += r["openalex"]
        by_year[r["year"]]["n"] += 1
    totals = dict(wos=sum(r["wos"] for r in rows), openalex=sum(r["openalex"] for r in rows))
    both_zero = sum(1 for r in rows if r["wos"] == 0 and r["openalex"] == 0)
    exact = sum(1 for r in rows if r["wos"] == r["openalex"])
    within_one = sum(1 for r in rows if abs(r["wos"] - r["openalex"]) <= 1)
    return dict(
        article_years=len(rows), articles=len({r["wos_index"] for r in rows}), totals=totals,
        ratio_openalex_to_wos=round(totals["openalex"] / totals["wos"], 4) if totals["wos"] else None,
        share_exact=round(exact / len(rows), 4), share_within_one=round(within_one / len(rows), 4),
        share_both_zero=round(both_zero / len(rows), 4),
        by_year={y: dict(v, ratio=round(v["openalex"] / v["wos"], 3) if v["wos"] else None)
                 for y, v in sorted(by_year.items())},
    )


def main():
    with Run("validation", "02_compare", __file__, offline=True) as run:
        run.require("01_import", study="wos_2016")
        run.require("02_histories", study="openalex")
        run.require("02_frame", study="retraction_watch")
        articles = read_csv(ROOT / "data/wos_2016/tidy/articles.csv")
        counts = read_csv(ROOT / "data/wos_2016/tidy/citing_counts.csv")
        targets = read_csv(ROOT / "data/openalex/targets.csv")
        oa = collections.defaultdict(dict)
        for r in histories.read_gzip_csv(ROOT / "data/openalex/histories.csv.gz"):
            oa[r["target_id"]][int(r["year"])] = int(r["citations"])
        rows = paired_panel(articles, counts, targets, oa)
        unique(rows, ["wos_index", "year"])
        frame = {r["doi"]: r for r in read_csv(ROOT / "data/retraction_watch/frame.csv")}
        dates = []
        for t in targets:
            if t["wos_index"] and t["article_id"] and t["doi"] in frame:
                article = next(a for a in articles if a["index"] == t["wos_index"])
                dates.append(dict(wos_index=t["wos_index"], article_id=t["article_id"],
                                  wos_notice_year=int(article["notice_year"]),
                                  rw_retraction_year=int(frame[t["doi"]]["retraction_year"]),
                                  wos_publication_year=int(article["publication_year"]),
                                  rw_publication_year=int(frame[t["doi"]]["publication_year"])))
        summary = summarise(rows)
        summary["date_agreement"] = dict(
            overlap=len(dates),
            notice_year_equal=sum(1 for d in dates if d["wos_notice_year"] == d["rw_retraction_year"]),
            notice_year_within_one=sum(1 for d in dates if abs(d["wos_notice_year"] - d["rw_retraction_year"]) <= 1),
            publication_year_equal=sum(1 for d in dates if d["wos_publication_year"] == d["rw_publication_year"]),
        )
        run.check("paired_panel_nonempty", len(rows) > 5000, len(rows))
        write_csv(DATA / "paired_panel.csv", rows, PANEL_FIELDS)
        write_csv(DATA / "date_agreement.csv", dates, list(dates[0]) if dates else ["wos_index"])
        write_json(DATA / "comparison_summary.json", summary)
        for name in ["paired_panel.csv", "date_agreement.csv", "comparison_summary.json"]:
            run.output(DATA / name)
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    main()
