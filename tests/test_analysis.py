import unittest

from scripts.research_pipeline import ROOT, verify_receipt

STAGES = ["01_estimate", "02_sdid"]


class AnalysisReceiptTests(unittest.TestCase):
    def test_reported_results_match_current_code_and_inputs(self):
        for stage in STAGES:
            with self.subTest(stage=stage):
                verify_receipt(ROOT / "data/analysis/receipts" / (stage + ".json"))
