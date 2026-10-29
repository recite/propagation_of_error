"""Run the R estimation script under a receipt that freezes its inputs and outputs.

Usage: RSCRIPT=Rscript python3 -m scripts.analysis.01_estimate
"""

from __future__ import annotations

import json
import os
import subprocess

from scripts.research_pipeline import ROOT, Run

DATA = ROOT / "data/analysis"
OUTPUTS = [
    "estimates.csv",
    "event_study.csv",
    "event_study_ols.csv",
    "pretrends.csv",
    "honest_status.json",
    "heterogeneity.csv",
    "leave_one_out.csv",
    "journal_clustered.csv",
    "legacy_estimates.csv",
    "citation_paths.csv",
    "retraction_rates.csv",
    "retractions_by_year.csv",
    "time_to_retraction.csv",
    "acknowledgment.csv",
    "reasons.csv",
    "status.json",
    "R-session.txt",
]


def main():
    with Run("analysis", "01_estimate", __file__, offline=True) as run:
        run.require("02_panel", study="matching")
        run.require("05_world_counts", study="openalex")
        run.require("01_import", study="wos_2016")
        for module in sorted((ROOT / "R").glob("*.R")) + [ROOT / "scripts/analysis/estimate.R"]:
            run.input(module)
        subprocess.run(
            [os.environ.get("RSCRIPT", "Rscript"), "scripts/analysis/estimate.R"],
            cwd=ROOT,
            check=True,
        )
        status = json.loads((DATA / "status.json").read_text())
        run.check("model_converged", status["converged"], status)
        run.check("stacks_match_panel", status["stacks"] > 0, status["stacks"])
        for name in OUTPUTS:
            if (DATA / name).exists():
                run.output(DATA / name)
        if status["honest_available"]:
            run.output(DATA / "honest_did.csv")
            run.output(DATA / "honest_original.csv")
        run.record["metrics"] = status


if __name__ == "__main__":
    main()
