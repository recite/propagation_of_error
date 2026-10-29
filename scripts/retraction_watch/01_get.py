"""Freeze the Retraction Watch snapshot published by Crossref and record its hash.

Usage: python3 -m scripts.retraction_watch.01_get [--offline]
Output: data/retraction_watch/snapshot.json
"""

from __future__ import annotations

import argparse
import collections
import csv
import json

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, write_json

URL = "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv"
CACHE = ROOT / "private-data/retraction_watch/retraction_watch.csv"
SNAPSHOT = ROOT / "data/retraction_watch/snapshot.json"
FIELDS = [
    "Record ID", "Title", "Subject", "Institution", "Journal", "Publisher", "Country",
    "Author", "URLS", "ArticleType", "RetractionDate", "RetractionDOI",
    "RetractionPubMedID", "OriginalPaperDate", "OriginalPaperDOI",
    "OriginalPaperPubMedID", "RetractionNature", "Reason", "Paywalled", "Notes",
]


def read_snapshot():
    """Rows of the frozen snapshot; the caller checks the hash against snapshot.json."""
    csv.field_size_limit(1 << 30)
    with CACHE.open(encoding="utf-8", errors="replace", newline="") as stream:
        reader = csv.DictReader(stream)
        # The published CSV ends every line with a trailing comma, hence one empty name.
        if [f for f in reader.fieldnames if f] != FIELDS:
            raise ValueError("Unexpected Retraction Watch columns: " + str(reader.fieldnames))
        return list(reader)


def main(offline):
    with Run("retraction_watch", "01_get", __file__, offline) as run:
        if not CACHE.exists() and offline:
            raise FileNotFoundError("Snapshot is not cached; run without --offline")
        openalex.request(URL, CACHE, json_response=False)
        source = json.loads(CACHE.with_suffix(".csv.source.json").read_text())
        rows = read_snapshot()
        natures = collections.Counter(r["RetractionNature"] for r in rows)
        run.check("snapshot_rows", len(rows) > 50000, len(rows))
        run.check("retraction_nature_values", set(natures) <= {
            "Retraction", "Correction", "Expression of concern", "Reinstatement"}, dict(natures))
        write_json(SNAPSHOT, dict(
            url=URL, retrieved_at=source["retrieved_at"], sha256=source["sha256"],
            bytes=source["bytes"], rows=len(rows), rows_by_nature=dict(natures),
            cache_path=str(CACHE.relative_to(ROOT)),
        ))
        run.output(SNAPSHOT)
        print(f"Snapshot: {len(rows)} rows, {natures['Retraction']} retractions")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    main(parser.parse_args().offline)
