import importlib
import unittest

from scripts.research_pipeline import ROOT, read_csv, verify_receipt

histories = importlib.import_module("scripts.openalex.02_histories")


class PaddingTests(unittest.TestCase):
    def test_series_is_complete_from_publication_year(self):
        series, before, after = histories.pad({2009: 2, 2011: 4, 2026: 1}, 2010, last_year=2012)
        self.assertEqual(series, [(2010, 0), (2011, 4), (2012, 0)])
        self.assertEqual((before, after), (2, 1))


class HistoryArtifactTests(unittest.TestCase):
    def setUp(self):
        if not (ROOT / "data/openalex/receipts/02_histories.json").exists():
            self.skipTest("histories not fetched")

    def test_receipt_verifies(self):
        verify_receipt(ROOT / "data/openalex/receipts/02_histories.json")

    def test_every_article_has_every_year_through_the_cutoff(self):
        totals = {t["target_id"]: t for t in read_csv(ROOT / "data/openalex/history_totals.csv")}
        years = {}
        for r in histories.read_gzip_csv(ROOT / "data/openalex/histories.csv.gz"):
            years.setdefault(r["target_id"], []).append(int(r["year"]))
            self.assertNotEqual(r["citations"], "")
        for tid, observed in years.items():
            first = int(totals[tid]["first_year"])
            self.assertEqual(observed, list(range(first, histories.LAST_YEAR + 1)), tid)
        self.assertTrue(all(t["complete"] == "1" for t in totals.values()))

    def test_resolution_and_retraction_flags(self):
        targets = read_csv(ROOT / "data/openalex/targets.csv")
        eligible = [
            t
            for t in targets
            if t["article_id"] and t["resolution"] == "resolved" and not t["frame_exclusion"]
        ]
        self.assertGreater(len(eligible), 8000)
        self.assertGreater(sum(t["is_retracted"] == "1" for t in eligible) / len(eligible), 0.9)
