import importlib
import json
import unittest

from scripts.research_pipeline import ROOT, read_csv, verify_receipt

frame = importlib.import_module("scripts.retraction_watch.02_frame")
sample = importlib.import_module("scripts.retraction_watch.03_sample")

MAPPING = {"Paper Mill": "misconduct", "Plagiarism of Text": "duplication",
           "Error in Data": "error", "Investigation by Journal/Publisher": "other"}


def row(**values):
    base = {"Record ID": "1", "Title": "t", "Subject": "(BLS) Biology;(HSC) Medicine", "Journal": "J",
            "Publisher": "P", "ArticleType": "Research Article;", "RetractionDate": "6/1/2015 0:00",
            "RetractionDOI": "10.1/notice", "OriginalPaperDate": "1/1/2010 0:00",
            "OriginalPaperDOI": "10.1/a", "RetractionNature": "Retraction",
            "Reason": "+Error in Data;+Paper Mill;"}
    base.update(values)
    return base


class FrameRuleTests(unittest.TestCase):
    def reason_of(self, rows):
        _, exclusions = frame.build_frame(rows, MAPPING)
        return [e["reason"] for e in exclusions]

    def test_each_exclusion_rule_fires_with_its_reason(self):
        cases = [
            (row(RetractionNature="Correction"), "not_retraction"),
            (row(OriginalPaperDOI="unavailable"), "no_original_doi"),
            (row(RetractionDate="unknown"), "unparseable_date"),
            (row(OriginalPaperDate="1/1/1999 0:00"), "outside_window"),
            (row(RetractionDate="6/1/2023 0:00"), "outside_window"),
            (row(OriginalPaperDate="1/1/2014 0:00"), "retracted_too_soon"),
            (row(ArticleType="Conference Abstract/Paper;"), "article_type"),
        ]
        for record, expected in cases:
            with self.subTest(expected):
                self.assertEqual(self.reason_of([record]), [expected])

    def test_one_record_per_doi_keeps_the_earliest_retraction(self):
        kept, exclusions = frame.build_frame(
            [row(**{"Record ID": "2", "RetractionDate": "6/1/2016 0:00"}), row()], MAPPING)
        self.assertEqual([r["record_id"] for r in kept], ["1"])
        self.assertEqual([(e["record_id"], e["reason"]) for e in exclusions], [("2", "duplicate_retraction")])
        self.assertEqual(kept[0]["retraction_year"], 2015)
        self.assertEqual(kept[0]["lag_years"], 5)

    def test_reason_hierarchy_and_unmapped_labels(self):
        self.assertEqual(frame.reason_category(["Error in Data", "Paper Mill"], MAPPING), "misconduct")
        self.assertEqual(frame.reason_category(["Error in Data", "Plagiarism of Text"], MAPPING), "duplication")
        self.assertEqual(frame.reason_category(["Investigation by Journal/Publisher"], MAPPING), "other")
        with self.assertRaises(ValueError):
            frame.reason_category(["Novel label"], MAPPING)

    def test_prior_expression_of_concern_is_flagged(self):
        concern = row(**{"Record ID": "3", "RetractionNature": "Expression of concern",
                         "RetractionDate": "1/1/2014 0:00"})
        kept, _ = frame.build_frame([row(), concern], MAPPING)
        self.assertEqual(kept[0]["prior_concern"], 1)

    def test_field_is_the_modal_prefix(self):
        self.assertEqual(frame.field_group(["(BLS) a", "(HSC) b", "(HSC) c"]), "hsc")
        self.assertEqual(frame.field_group(["(B/T) a", "(ENV) b"]), "bt_env")
        self.assertEqual(frame.field_group([]), "")


class FrameArtifactTests(unittest.TestCase):
    def test_receipts_verify(self):
        verify_receipt(ROOT / "data/retraction_watch/receipts/03_sample.json")

    def test_every_snapshot_reason_is_mapped(self):
        mapping = {r["reason"] for r in read_csv(ROOT / "data/retraction_watch/reason_categories.csv")}
        observed = {x for r in read_csv(ROOT / "data/retraction_watch/frame.csv")
                    for x in r["reason_raw"].split(";") if x}
        self.assertTrue(observed <= mapping)

    def test_frame_and_exclusions_cover_the_snapshot(self):
        snapshot = json.loads((ROOT / "data/retraction_watch/snapshot.json").read_text())
        kept = read_csv(ROOT / "data/retraction_watch/frame.csv")
        dropped = read_csv(ROOT / "data/retraction_watch/exclusions.csv")
        self.assertEqual(len(kept) + len(dropped), snapshot["rows"])
        self.assertEqual(len({r["doi"] for r in kept}), len(kept))

    def test_treated_set_is_the_whole_frame_with_a_fixed_order(self):
        rows = read_csv(ROOT / "data/retraction_watch/sample.csv")
        kept = read_csv(ROOT / "data/retraction_watch/frame.csv")
        self.assertEqual({r["article_id"] for r in rows}, {r["article_id"] for r in kept})
        self.assertEqual(sorted(int(r["sample_order"]) for r in rows), list(range(1, len(rows) + 1)))
        self.assertEqual(sum(r["pilot"] == "1" for r in rows), 200)

    def test_stratified_sampler_respects_floor_and_cap(self):
        kept = read_csv(ROOT / "data/retraction_watch/frame.csv")
        drawn = sample.stratified_sample(kept, 1000, 1, floor=5, cap=3)
        import collections
        per = collections.Counter((r["journal"], r["publication_year"]) for r in kept
                                  if r["article_id"] in {d["article_id"] for d in drawn})
        self.assertLessEqual(max(per.values()), 3)
        self.assertEqual(len({d["article_id"] for d in drawn}), len(drawn))
