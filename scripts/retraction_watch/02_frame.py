"""Build the sampling frame of retracted research articles with reason codes.

Usage: python3 -m scripts.retraction_watch.02_frame
Output: data/retraction_watch/frame.csv, exclusions.csv, frame_summary.json
"""

from __future__ import annotations

import collections
import datetime as dt
import hashlib
import importlib
import json
import re

from scripts import openalex
from scripts.research_pipeline import ROOT, Run, read_csv, unique, write_csv, write_json

get = importlib.import_module("scripts.retraction_watch.01_get")
DATA = ROOT / "data/retraction_watch"
PUBLICATION_YEARS = (2000, 2019)
RETRACTION_YEARS = (2005, 2022)
MINIMUM_LAG = 2
ARTICLE_TYPES = {"Research Article", "Clinical Study"}
FIELD_GROUPS = {"BLS": "bls", "HSC": "hsc", "PHY": "phy", "B/T": "bt_env", "ENV": "bt_env",
                "SOC": "soc_hum", "HUM": "soc_hum"}
FIELD_ORDER = ["BLS", "HSC", "PHY", "B/T", "ENV", "SOC", "HUM"]
FRAME_FIELDS = [
    "article_id", "record_id", "doi", "title", "journal", "publisher", "publication_date",
    "publication_year", "retraction_date", "retraction_year", "lag_years", "notice_doi",
    "article_type", "reason_raw", "reason_category", "field", "subjects_raw", "prior_concern",
]


def article_id(doi):
    return "rw_" + hashlib.sha256(doi.encode()).hexdigest()[:14]


def parse_date(value):
    value = (value or "").strip()
    if not value:
        return None
    try:
        return dt.datetime.strptime(value.split(" ")[0], "%m/%d/%Y").date()
    except ValueError:
        return None


def split_list(value):
    return [x.strip().lstrip("+").strip() for x in (value or "").split(";") if x.strip()]


def reason_category(reasons, mapping):
    """Hierarchical: misconduct > duplication > error > other; unmapped labels fail."""
    categories = set()
    for reason in reasons:
        if reason not in mapping:
            raise ValueError("Unmapped Retraction Watch reason: " + reason)
        categories.add(mapping[reason])
    for level in ["misconduct", "duplication", "error"]:
        if level in categories:
            return level
    return "other"


def field_group(subjects):
    """Modal broad-field prefix across subject labels, ties broken by FIELD_ORDER."""
    prefixes = [m.group(1) for s in subjects for m in [re.match(r"\((B/T|BLS|HSC|PHY|SOC|ENV|HUM)\)", s)] if m]
    if not prefixes:
        return ""
    counts = collections.Counter(prefixes)
    best = max(counts.values())
    for prefix in FIELD_ORDER:
        if counts.get(prefix) == best:
            return FIELD_GROUPS[prefix]
    raise AssertionError


def build_frame(rows, mapping):
    """Apply the frame rules in order; every dropped record keeps its first reason."""
    frame, exclusions = [], []
    concerns = collections.defaultdict(list)
    for row in rows:
        if row["RetractionNature"] == "Expression of concern":
            doi = openalex.normalize_doi(row["OriginalPaperDOI"])
            date = parse_date(row["RetractionDate"])
            if doi and date:
                concerns[doi].append(date)
    retractions = [r for r in rows if r["RetractionNature"] == "Retraction"]
    for row in rows:
        if row["RetractionNature"] != "Retraction":
            exclusions.append(dict(record_id=row["Record ID"], doi="", reason="not_retraction"))
    by_doi = collections.defaultdict(list)
    for row in retractions:
        doi = openalex.normalize_doi(row["OriginalPaperDOI"])
        if not doi:
            exclusions.append(dict(record_id=row["Record ID"], doi="", reason="no_original_doi"))
            continue
        by_doi[doi].append(row)
    for doi, group in sorted(by_doi.items()):
        dated = [(parse_date(r["RetractionDate"]), r) for r in group]
        if any(d is None for d, _ in dated):
            for _, r in dated:
                exclusions.append(dict(record_id=r["Record ID"], doi=doi, reason="unparseable_date"))
            continue
        dated.sort(key=lambda x: (x[0], x[1]["Record ID"]))
        first_date, first = dated[0]
        for _, later in dated[1:]:
            exclusions.append(dict(record_id=later["Record ID"], doi=doi, reason="duplicate_retraction"))
        published = parse_date(first["OriginalPaperDate"])
        record = dict(record_id=first["Record ID"], doi=doi)
        if published is None:
            exclusions.append(dict(record, reason="unparseable_date"))
            continue
        if not (PUBLICATION_YEARS[0] <= published.year <= PUBLICATION_YEARS[1]
                and RETRACTION_YEARS[0] <= first_date.year <= RETRACTION_YEARS[1]):
            exclusions.append(dict(record, reason="outside_window"))
            continue
        if first_date.year - published.year < MINIMUM_LAG:
            exclusions.append(dict(record, reason="retracted_too_soon"))
            continue
        types = split_list(first["ArticleType"])
        if not ARTICLE_TYPES & set(types):
            exclusions.append(dict(record, reason="article_type"))
            continue
        reasons = split_list(first["Reason"])
        subjects = split_list(first["Subject"])
        frame.append(dict(
            article_id=article_id(doi), record_id=first["Record ID"], doi=doi,
            title=first["Title"].strip(), journal=first["Journal"].strip(),
            publisher=first["Publisher"].strip(), publication_date=published.isoformat(),
            publication_year=published.year, retraction_date=first_date.isoformat(),
            retraction_year=first_date.year, lag_years=first_date.year - published.year,
            notice_doi=openalex.normalize_doi(first["RetractionDOI"]),
            article_type=";".join(types), reason_raw=";".join(reasons),
            reason_category=reason_category(reasons, mapping), field=field_group(subjects),
            subjects_raw=";".join(subjects),
            prior_concern=int(any(d < first_date for d in concerns.get(doi, []))),
        ))
    return frame, exclusions


def main():
    with Run("retraction_watch", "02_frame", __file__, offline=True) as run:
        run.require("01_get")
        snapshot = json.loads((DATA / "snapshot.json").read_text())
        run.input(get.CACHE)
        run.check("snapshot_hash", openalex.sha256(get.CACHE.read_bytes()) == snapshot["sha256"],
                  snapshot["sha256"])
        mapping_rows = read_csv(run.input(DATA / "reason_categories.csv"))
        mapping = {r["reason"]: r["category"] for r in mapping_rows}
        rows = get.read_snapshot()
        frame, exclusions = build_frame(rows, mapping)
        unique(frame, ["doi"])
        unique(frame, ["article_id"])
        run.check("frame_plus_exclusions_cover_snapshot",
                  len(frame) + len(exclusions) == len(rows), dict(frame=len(frame), excluded=len(exclusions)))
        run.check("frame_size", 10000 <= len(frame) <= 25000, len(frame))
        run.check("fields_assigned", all(r["field"] for r in frame),
                  sum(1 for r in frame if not r["field"]))
        write_csv(run.output(DATA / "frame.csv"), frame, FRAME_FIELDS)
        write_csv(run.output(DATA / "exclusions.csv"), exclusions, ["record_id", "doi", "reason"])
        summary = dict(
            frame=len(frame),
            exclusions=dict(collections.Counter(r["reason"] for r in exclusions)),
            by_retraction_year=dict(sorted(collections.Counter(r["retraction_year"] for r in frame).items())),
            by_field=dict(collections.Counter(r["field"] for r in frame)),
            by_reason_category=dict(collections.Counter(r["reason_category"] for r in frame)),
            by_lag=dict(sorted(collections.Counter(min(r["lag_years"], 10) for r in frame).items())),
            rules=dict(publication_years=PUBLICATION_YEARS, retraction_years=RETRACTION_YEARS,
                       minimum_lag=MINIMUM_LAG, article_types=sorted(ARTICLE_TYPES)),
        )
        write_json(run.output(DATA / "frame_summary.json"), summary)
        run.record["metrics"] = summary
        print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    main()
