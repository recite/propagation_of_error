"""OpenAlex journal articles per publication year, the denominator for retraction rates.

Usage: python3 -m scripts.openalex.05_world_counts [--offline]
Output: data/openalex/world_counts.csv
"""

from __future__ import annotations

import argparse

from scripts import openalex_client as openalex
from scripts.research_pipeline import ROOT, Run, write_csv

DATA = ROOT / "data/openalex"


def main(offline):
    with Run("openalex", "05_world_counts", __file__, offline) as run:
        payload = run.fetch(openalex.api_url("works", filter="type:article,primary_location.source.type:journal",
                                             group_by="publication_year"), "world_counts")
        rows = sorted((dict(year=int(g["key"]), articles=int(g["count"]))
                       for g in payload["group_by"] if g["key"].isdigit()), key=lambda r: r["year"])
        run.check("years_cover_frame", {r["year"] for r in rows} >= set(range(2000, 2026)), len(rows))
        write_csv(DATA / "world_counts.csv", rows, ["year", "articles"])
        run.output(DATA / "world_counts.csv")
        print(f"{len(rows)} years")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    main(parser.parse_args().offline)
