import email.message
import importlib
import io
import json
import pathlib
import tempfile
import unittest
import urllib.error
from unittest import mock

from scripts import openalex_client as openalex
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


class RateLimitTests(unittest.TestCase):
    def _http_error(self, retry_after):
        return (
            urllib.error.HTTPError(
                "https://api.openalex.org/works",
                429,
                "Too Many Requests",
                email.message.Message(),
                io.BytesIO(b""),
            )
            if retry_after is None
            else urllib.error.HTTPError(
                "https://api.openalex.org/works",
                429,
                "Too Many Requests",
                {"Retry-After": str(retry_after)},
                io.BytesIO(b""),
            )
        )

    def _run(self, responses, tmp):
        calls = []

        def fake_urlopen(req, timeout=None):
            calls.append(req.full_url)
            outcome = responses.pop(0)
            if isinstance(outcome, Exception):
                raise outcome
            return mock.MagicMock(
                __enter__=lambda s: mock.Mock(read=lambda: outcome), __exit__=lambda *a: False
            )

        with mock.patch.object(openalex, "CACHE", tmp), mock.patch.object(
            openalex.urllib.request, "urlopen", fake_urlopen
        ), mock.patch.object(openalex.time, "sleep"):
            return openalex.request("https://api.openalex.org/works?x=1", tmp / "r.json"), calls

    def test_short_retry_after_is_waited_out_and_retried(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            result, calls = self._run([self._http_error(1), b'{"ok": 1}'], tmp)
            self.assertEqual(result, {"ok": 1})
            self.assertEqual(len(calls), 2)
            self.assertFalse((tmp / "api.openalex.org_retry_after.json").exists())

    def test_long_retry_after_blocks_and_raises(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            with self.assertRaises(openalex.BudgetExhausted):
                self._run([self._http_error(3600)], tmp)
            block = json.loads((tmp / "api.openalex.org_retry_after.json").read_text())
            self.assertEqual(block["code"], 429)
            self.assertGreater(block["seconds"], openalex.RATE_LIMIT_WAIT)
