"""Run the synthetic DiD script under a receipt.

Usage: RSCRIPT=Rscript python3 -m scripts.analysis.02_sdid
"""

from __future__ import annotations

import os
import subprocess

from scripts.research_pipeline import ROOT, Run, read_csv

DATA = ROOT / "data/analysis"


def main():
    with Run("analysis", "02_sdid", __file__, offline=True) as run:
        run.require("02_panel", study="matching")
        for module in [
            "R/synthetic_did.R",
            "R/sdid_cohorts.R",
            "scripts/analysis/sdid.R",
        ]:
            run.input(ROOT / module)
        subprocess.run(
            [os.environ.get("RSCRIPT", "Rscript"), "scripts/analysis/sdid.R"],
            cwd=ROOT,
            check=True,
        )
        cohorts = read_csv(DATA / "sdid_cohorts.csv")
        run.check("cohorts_estimated", len(cohorts) > 0, len(cohorts))
        for name in [
            "sdid_cohorts.csv",
            "sdid_pooled.csv",
            "sdid_paths.csv",
            "sdid_weights.csv",
            "sdid_draws.csv",
        ]:
            run.output(DATA / name)
        run.record["metrics"] = dict(cohorts=len(cohorts))


if __name__ == "__main__":
    main()
