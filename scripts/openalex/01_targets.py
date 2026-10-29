"""Resolve retracted-article DOIs (frame and 2016 WoS sample) to OpenAlex works.

Usage: python3 -m scripts.openalex.01_targets [--offline]
Output: data/openalex/targets.csv, target_resolution.json
"""

from __future__ import annotations

import argparse
import collections
import json
import urllib.error
import urllib.parse

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

DATA = ROOT / "data/openalex"
SELECT = (
    "id,doi,title,publication_year,publication_date,type,is_retracted,"
    "primary_location,cited_by_count"
)
TARGET_FIELDS = [
    "target_id",
    "doi",
    "article_id",
    "wos_index",
    "pilot",
    "work_id",
    "openalex_title",
    "openalex_year",
    "openalex_date",
    "openalex_type",
    "journal_id",
    "journal_name",
    "source_type",
    "is_retracted",
    "cited_by_count",
    "resolution",
    "frame_exclusion",
]


def resolve(run, dois):
    """Map each DOI to its OpenAlex works; DOIs that break the filter syntax go one by one."""
    safe = sorted(d for d in dois if not any(c in d for c in ",|"))
    awkward = sorted(set(dois) - set(safe))
    found = openalex.batched_doi_lookup(run, safe, SELECT)
    for doi in awkward:
        url = (
            "https://api.openalex.org/works/https://doi.org/"
            + urllib.parse.quote(doi, safe="/")
            + "?"
            + urllib.parse.urlencode(dict(select=SELECT, mailto=openalex.MAILTO))
        )
        try:
            work = run.fetch(url, "doi_singles")
        except urllib.error.HTTPError as error:
            if error.code == 404:
                continue
            raise
        found.setdefault(doi, []).append(work)
    return found


def describe(doi, works, rw_year):
    if not works:
        return dict(resolution="unresolved", frame_exclusion="openalex_unresolved")
    if len(works) > 1:
        return dict(resolution="multiple", frame_exclusion="openalex_multiple_works")
    work = works[0]
    location = work.get("primary_location") or {}
    source = location.get("source") or {}
    record = dict(
        resolution="resolved",
        work_id=work["id"],
        openalex_title=work.get("title") or "",
        openalex_year=work.get("publication_year") or "",
        openalex_date=work.get("publication_date") or "",
        openalex_type=work.get("type") or "",
        journal_id=source.get("id") or "",
        journal_name=source.get("display_name") or "",
        source_type=source.get("type") or "",
        is_retracted=int(bool(work.get("is_retracted"))),
        cited_by_count=work.get("cited_by_count", ""),
        frame_exclusion="",
    )
    if rw_year is None:
        return record
    if record["openalex_type"] != "article":
        record["frame_exclusion"] = "openalex_not_article"
    elif record["source_type"] != "journal" or not record["journal_id"]:
        record["frame_exclusion"] = "openalex_not_journal"
    elif not record["openalex_year"] or abs(int(record["openalex_year"]) - rw_year) > 1:
        record["frame_exclusion"] = "year_disagreement"
    return record


def main(offline):
    with Run("openalex", "01_targets", __file__, offline) as run:
        run.require("03_sample", study="retraction_watch")
        run.require("01_import", study="wos_2016")
        frame = read_csv(ROOT / "data/retraction_watch/frame.csv")
        pilot = {
            r["article_id"]
            for r in read_csv(ROOT / "data/retraction_watch/sample.csv")
            if r["pilot"] == "1"
        }
        wos = read_csv(ROOT / "data/wos_2016/tidy/articles.csv")
        by_doi = collections.defaultdict(dict)
        for r in frame:
            by_doi[r["doi"]]["article_id"] = r["article_id"]
            by_doi[r["doi"]]["rw_year"] = int(r["publication_year"])
        wos_dupes = collections.Counter(r["doi"] for r in wos if r["doi"])
        for r in wos:
            if r["doi"] and wos_dupes[r["doi"]] == 1:
                by_doi[r["doi"]]["wos_index"] = r["index"]
        found = resolve(run, sorted(by_doi))
        targets = []
        for doi, meta in sorted(by_doi.items()):
            record = dict.fromkeys(TARGET_FIELDS, "")
            record.update(describe(doi, found.get(doi, []), meta.get("rw_year")))
            record.update(
                doi=doi,
                article_id=meta.get("article_id", ""),
                wos_index=meta.get("wos_index", ""),
                pilot=int(meta.get("article_id", "") in pilot),
            )
            record["target_id"] = meta.get("article_id") or "wos_" + meta["wos_index"]
            targets.append(record)
        unique(targets, ["target_id"])
        unique(targets, ["doi"])
        in_frame = [t for t in targets if t["article_id"]]
        eligible = [
            t for t in in_frame if t["resolution"] == "resolved" and not t["frame_exclusion"]
        ]
        summary = dict(
            dois=len(targets),
            frame=len(in_frame),
            wos=sum(1 for t in targets if t["wos_index"]),
            wos_duplicate_dois_skipped=sum(1 for d, n in wos_dupes.items() if n > 1),
            overlap=sum(1 for t in targets if t["article_id"] and t["wos_index"]),
            resolution=dict(collections.Counter(t["resolution"] for t in targets)),
            frame_exclusions=dict(
                collections.Counter(t["frame_exclusion"] for t in in_frame if t["frame_exclusion"])
            ),
            frame_eligible=len(eligible),
            frame_flagged_retracted=sum(1 for t in eligible if t["is_retracted"] == 1),
            wos_resolved=sum(
                1 for t in targets if t["wos_index"] and t["resolution"] == "resolved"
            ),
        )
        run.check(
            "most_frame_dois_resolve",
            summary["frame_eligible"] >= 0.8 * len(in_frame),
            summary,
        )
        run.check(
            "retraction_flag_mostly_agrees",
            summary["frame_flagged_retracted"] >= 0.8 * summary["frame_eligible"],
            summary,
        )
        write_csv(DATA / "targets.csv", targets, TARGET_FIELDS)
        write_json(DATA / "target_resolution.json", summary)
        run.output(DATA / "targets.csv")
        run.output(DATA / "target_resolution.json")
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    main(parser.parse_args().offline)
