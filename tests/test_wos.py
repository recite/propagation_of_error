import importlib
import json
import unittest

from scripts.research_pipeline import ROOT, read_csv, verify_receipt

wos = importlib.import_module("scripts.wos.01_import")


class LegacyImportTests(unittest.TestCase):
    def test_receipt_is_complete_and_current(self):
        verify_receipt(ROOT / "data/wos_2016/receipts/01_import.json")

    def test_draft_counts_are_reproduced_from_raw_records(self):
        check = json.loads((wos.TIDY / "legacy_check.json").read_text())
        self.assertEqual(check["draft"]["total"], 73564)
        self.assertEqual(check["draft"]["before_notice_year"], 39792)
        self.assertEqual(check["draft"]["after_notice_year"], 22932)
        self.assertEqual(check["articles"], 3029)

    def test_panel_is_zero_padded_and_rectangular(self):
        articles = read_csv(wos.TIDY / "articles.csv")
        counts = read_csv(wos.TIDY / "citing_counts.csv")
        expected = sum(2017 - int(r["publication_year"]) for r in articles)
        self.assertEqual(len(counts), expected)
        self.assertGreater(sum(1 for r in counts if r["citations"] == "0"), 9000)
        self.assertTrue(all(r["complete"] == ("1" if int(r["year"]) <= 2015 else "0") for r in counts))

    def test_tidy_counts_exclude_pre_publication_and_duplicate_records(self):
        articles = [dict(index="1", publication_year=2010)]
        citing = [
            dict(index="1", PY="2009", UT="a"),
            dict(index="1", PY="2011", UT="b"),
            dict(index="1", PY="2011", UT="b"),
            dict(index="1", PY="2017", UT="c"),
            dict(index="9", PY="2011", UT="d"),
        ]
        rows, dropped = wos.tidy_counts(articles, citing)
        self.assertEqual([r["year"] for r in rows], list(range(2010, 2017)))
        by_year = {r["year"]: r for r in rows}
        self.assertEqual(by_year[2011]["citations"], 1)
        self.assertEqual(by_year[2011]["citing_records"], 2)
        self.assertEqual(by_year[2010]["citations"], 0)
        self.assertEqual(dict(dropped), dict(before_publication=1, duplicate_accession=1,
                                             after_export_year=1, unmatched_index=1))

    def test_acknowledgment_codes_follow_the_original_protocol(self):
        ack = read_csv(wos.TIDY / "acknowledgment.csv")
        post = [r for r in ack if r["sample"] == "post"]
        pre = [r for r in ack if r["sample"] == "pre"]
        self.assertEqual((len(post), sum(r["acknowledged"] == "0" for r in post),
                          sum(r["acknowledged"] == "1" for r in post)), (275, 234, 22))
        self.assertEqual((len(pre), sum(r["acknowledged"] == "0" for r in pre),
                          sum(r["acknowledged"] == "1" for r in pre)), (100, 85, 2))
        self.assertEqual(sum(r["retraction_year_citation"] == "1" for r in pre), 42)

    def test_manifest_freezes_every_legacy_file(self):
        manifest = json.loads((wos.DATA / "manifest.json").read_text())
        self.assertEqual(set(manifest["files"]), set(wos.FILES))
        self.assertEqual(manifest["files"]["retraction_reasons.csv"]["encoding"], "latin-1")
