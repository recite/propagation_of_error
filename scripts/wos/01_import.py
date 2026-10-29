"""Freeze the 2016 Web of Science files and derive tidy, zero-padded tables.

Usage: python3 -m scripts.wos.01_import
Output: data/wos_2016/manifest.json and data/wos_2016/tidy/{articles,citing_counts,
        acknowledgment,reasons}.csv plus legacy_check.json reproducing the 2021 draft's
        headline counts from the same files.
"""

from __future__ import annotations

import collections
import csv
import io
import json
import zipfile

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, unique, write_csv, write_json

DATA = ROOT / "data/wos_2016"
TIDY = DATA / "tidy"
LAST_COMPLETE_YEAR = 2015
FILES = {
    "retraction_notices.csv": "utf-8",
    "retracted_articles.csv": "utf-8",
    "retracted_article_citations.zip": None,
    "retraction_notice_citations.csv": "utf-8",
    "retraction_reasons.csv": "latin-1",
    "acknowledgment_pre.csv": "utf-8",
    "acknowledgment_post.csv": "utf-8",
    "date_checks.csv": "utf-8",
    "wos_codebook.csv": "utf-8",
    "journal_impact_factor.csv": "utf-8",
    "legacy_panel.csv": "utf-8",
}
ARTICLE_FIELDS = [
    "index", "doi", "accession", "title", "journal", "publication_year", "notice_year",
    "notice_doi", "notice_accession", "notice_article_year", "wos_categories", "research_areas",
]
COUNT_FIELDS = ["index", "year", "citations", "citing_records", "complete"]
ACK_FIELDS = ["sample", "index", "years_from_notice", "acknowledged", "retraction_year_citation"]


def read_rows(name):
    csv.field_size_limit(1 << 30)
    path = DATA / name
    if name.endswith(".zip"):
        with zipfile.ZipFile(path) as archive:
            raw = archive.read(archive.namelist()[0])
        return list(csv.DictReader(io.StringIO(raw.decode("latin-1"))))
    with path.open(encoding=FILES[name], newline="") as stream:
        return list(csv.DictReader(stream))


def tidy_articles(notices, articles):
    """One row per retraction notice joined to its matched retracted article."""
    matched = {r["index"]: r for r in articles if r["final_filter"] == "1"}
    rows = []
    for notice in notices:
        article = matched[notice["index"]]
        rows.append(dict(
            index=notice["index"], doi=openalex.normalize_doi(article["DI"]),
            accession=article["UT"], title=article["TI"], journal=article["SO"],
            publication_year=int(article["PY"]), notice_year=int(notice["PY"]),
            notice_doi=openalex.normalize_doi(notice["DI"]), notice_accession=notice["UT"],
            notice_article_year=notice["year"], wos_categories=article["WC"],
            research_areas=article["SC"],
        ))
    return rows


def tidy_counts(articles, citing):
    """Article-year panel from publication year through 2016, zeros included.

    `citations` counts distinct citing accession numbers; `citing_records` counts raw
    export rows, which the 2021 draft used. Citations dated before the cited article's
    publication year are export errors and are excluded from both.
    """
    published = {r["index"]: r["publication_year"] for r in articles}
    distinct, records = collections.Counter(), collections.Counter()
    seen = set()
    dropped = collections.Counter()
    for row in citing:
        index, year = row["index"], int(row["PY"])
        if index not in published:
            dropped["unmatched_index"] += 1
            continue
        if year < published[index]:
            dropped["before_publication"] += 1
            continue
        if year > 2016:
            dropped["after_export_year"] += 1
            continue
        records[(index, year)] += 1
        if (index, row["UT"]) in seen:
            dropped["duplicate_accession"] += 1
            continue
        seen.add((index, row["UT"]))
        distinct[(index, year)] += 1
    rows = []
    for index, start in sorted(published.items(), key=lambda x: int(x[0])):
        for year in range(start, 2017):
            rows.append(dict(index=index, year=year, citations=distinct[(index, year)],
                             citing_records=records[(index, year)],
                             complete=int(year <= LAST_COMPLETE_YEAR)))
    return rows, dropped


def tidy_acknowledgment(pre, post):
    rows = []
    for sample, source in [("pre", pre), ("post", post)]:
        for row in source:
            code = {"1": 0, "0": 1, "NA": ""}[row["approving"]]
            rows.append(dict(sample=sample, index=row["index"], years_from_notice=int(row["diff"]),
                             acknowledged=code, retraction_year_citation=int(int(row["diff"]) == 0)))
    return rows


def legacy_check(counts, articles):
    """Reproduce the 2021 draft's counts from raw records, then the corrected ones."""
    notice = {r["index"]: r["notice_year"] for r in articles}
    pre = post = same = 0
    pre_d = post_d = same_d = 0
    for row in counts:
        diff = row["year"] - notice[row["index"]]
        bucket = "pre" if diff < 0 else "post" if diff > 0 else "same"
        if bucket == "pre":
            pre += row["citing_records"]
            pre_d += row["citations"]
        elif bucket == "post":
            post += row["citing_records"]
            post_d += row["citations"]
        else:
            same += row["citing_records"]
            same_d += row["citations"]
    total = pre + post + same
    return dict(
        draft=dict(total=total, before_notice_year=pre, notice_year=same, after_notice_year=post,
                   share_after=round(post / total, 4)),
        distinct=dict(total=pre_d + post_d + same_d, before_notice_year=pre_d, notice_year=same_d,
                      after_notice_year=post_d, share_after=round(post_d / (pre_d + post_d + same_d), 4)),
        zero_article_years=sum(1 for r in counts if r["citations"] == 0 and r["complete"]),
        article_years=sum(1 for r in counts if r["complete"]),
        articles=len(articles),
        never_cited=sum(1 for index in notice if not any(
            r["citations"] for r in counts if r["index"] == index)),
    )


def main():
    with Run("wos_2016", "01_import", __file__, offline=True) as run:
        manifest = {}
        for name, encoding in FILES.items():
            path = run.input(DATA / name)
            manifest[name] = dict(sha256=openalex.sha256(path.read_bytes()), bytes=path.stat().st_size,
                                  encoding=encoding or "latin-1 (zip member)")
        notices = read_rows("retraction_notices.csv")
        articles_raw = read_rows("retracted_articles.csv")
        citing = read_rows("retracted_article_citations.zip")
        reasons = read_rows("retraction_reasons.csv")
        pre = read_rows("acknowledgment_pre.csv")
        post = read_rows("acknowledgment_post.csv")
        notice_citations = read_rows("retraction_notice_citations.csv")
        for name, rows, expected in [("notices", notices, 3029), ("articles", articles_raw, 3052),
                                     ("citing", citing, 73565), ("reasons", reasons, 95),
                                     ("acknowledgment_pre", pre, 100), ("acknowledgment_post", post, 275),
                                     ("notice_citations", notice_citations, 1897)]:
            run.check(f"rows_{name}", len(rows) == expected, len(rows))
        unique(notices, ["index"])
        unique(articles_raw, ["index"])
        run.check("every_notice_has_one_matched_article",
                  {r["index"] for r in notices} == {r["index"] for r in articles_raw if r["final_filter"] == "1"},
                  "final_filter == 1")
        articles = tidy_articles(notices, articles_raw)
        counts, dropped = tidy_counts(articles, citing)
        acknowledgment = tidy_acknowledgment(pre, post)
        reason_rows = [dict(index=r["index"], reason=r["reason"], reason_label=r["reason_label"],
                            notes=r["notes"]) for r in reasons]
        check = legacy_check(counts, articles)
        check["dropped_citing_records"] = dict(dropped)
        run.check("draft_total_citations", check["draft"]["total"] == 73564, check["draft"]["total"])
        run.check("draft_post_citations", check["draft"]["after_notice_year"] == 22932,
                  check["draft"]["after_notice_year"])
        run.check("draft_pre_citations", check["draft"]["before_notice_year"] == 39792,
                  check["draft"]["before_notice_year"])
        run.check("panel_is_rectangular",
                  len(counts) == sum(2017 - r["publication_year"] for r in articles), len(counts))
        TIDY.mkdir(parents=True, exist_ok=True)
        write_csv(TIDY / "articles.csv", articles, ARTICLE_FIELDS)
        write_csv(TIDY / "citing_counts.csv", counts, COUNT_FIELDS)
        write_csv(TIDY / "acknowledgment.csv", acknowledgment, ACK_FIELDS)
        write_csv(TIDY / "reasons.csv", reason_rows, ["index", "reason", "reason_label", "notes"])
        write_json(TIDY / "legacy_check.json", check)
        write_json(DATA / "manifest.json", dict(
            files=manifest, retrieved="August 2016 Web of Science exports; see data/wos_2016/README.md",
            dropped_duplicate_of_reasons_file="data/06_are_retracted_articles_retracted/retracted_article_sample.csv (byte-identical to retraction_reasons.csv; removed)",
        ))
        for name in ["articles.csv", "citing_counts.csv", "acknowledgment.csv", "reasons.csv", "legacy_check.json"]:
            run.output(TIDY / name)
        run.output(DATA / "manifest.json")
        run.record["metrics"] = check
        print(json.dumps(check, indent=1))


if __name__ == "__main__":
    main()
