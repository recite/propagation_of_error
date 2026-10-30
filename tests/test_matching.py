import collections
import importlib
import json
import unittest

from scripts.research_pipeline import ROOT, read_csv, verify_receipt

matching = importlib.import_module("scripts.matching.01_match")
histories = importlib.import_module("scripts.openalex.02_histories")


def fixture():
    treated = [
        dict(
            article_id="t1",
            journal_id="J",
            openalex_year="2010",
            retraction_year="2014",
        )
    ]
    pools = [dict(pool_id="p", journal_id="J", year="2010")]
    candidates = [dict(candidate_id=f"c{i}", pool_id="p", exclusion="") for i in range(1, 8)]
    candidates.append(dict(candidate_id="c8", pool_id="p", exclusion="openalex_is_retracted"))
    series = {"t1": {2010: 5, 2011: 10, 2012: 12, 2013: 11, 2014: 9, 2015: 4}}
    levels = {"c1": 11, "c2": 10, "c3": 13, "c4": 9, "c5": 12, "c6": 40, "c7": 1}
    for cid, level in levels.items():
        series[cid] = {
            2010: level // 2,
            2011: level,
            2012: level,
            2013: level,
            2014: level,
            2015: level,
        }
    series["c8"] = dict(series["c1"])
    return treated, pools, candidates, series


class MatchTests(unittest.TestCase):
    def test_post_event_outcomes_cannot_change_matches(self):
        treated, pools, candidates, series = fixture()
        before = matching.match(treated, pools, candidates, series, series, k=5)
        perturbed = {
            k: {y: (n * 10 if y >= 2014 else n) for y, n in v.items()} for k, v in series.items()
        }
        after = matching.match(treated, pools, candidates, perturbed, perturbed, k=5)
        self.assertEqual(before[0], after[0])
        self.assertEqual(before[1], after[1])

    def test_caliper_excludes_distant_candidates_and_ranks_the_rest(self):
        treated, pools, candidates, series = fixture()
        matches, scored, exclusions, balance = matching.match(
            treated, pools, candidates, series, series, k=5
        )
        chosen = [m["control_id"] for m in matches]
        self.assertNotIn("c6", chosen)
        self.assertNotIn("c7", chosen)
        self.assertNotIn("c8", chosen)
        self.assertEqual([m["rank"] for m in matches], list(range(1, len(chosen) + 1)))
        self.assertAlmostEqual(sum(m["weight"] for m in matches), 1.0)
        self.assertEqual(exclusions, [])
        reasons = {s["candidate_id"]: s["reason"] for s in scored}
        self.assertEqual(reasons["c6"], "citation_caliper")

    def test_treated_without_candidates_is_reported(self):
        treated, pools, candidates, series = fixture()
        matches, _, exclusions, _ = matching.match(treated, pools, [], series, series)
        self.assertEqual(matches, [])
        self.assertEqual(exclusions, [dict(article_id="t1", reason="no_usable_candidates")])

    def test_pre_event_histories_hide_the_retraction_year_onward(self):
        visible = matching.pre_event_histories({"a": {2012: 1, 2013: 2, 2014: 3}}, 2014)
        self.assertEqual(visible, {"a": {2012: 1, 2013: 2}})


class MatchArtifactTests(unittest.TestCase):
    def setUp(self):
        if not (ROOT / "data/matching/receipts/02_panel.json").exists():
            self.skipTest("panel not built")

    def test_receipts_verify(self):
        verify_receipt(ROOT / "data/matching/receipts/02_panel.json")

    def test_every_treated_is_matched_or_excluded(self):
        summary = json.loads((ROOT / "data/matching/match_summary.json").read_text())
        exclusions = read_csv(ROOT / "data/matching/match_exclusions.csv")
        self.assertEqual(summary["matched"] + len(exclusions), summary["treated"])

    def test_panel_rows_are_complete_event_windows(self):
        rows = histories.read_gzip_csv(ROOT / "data/matching/panel.csv.gz")
        self.assertTrue(all(r["citations"] != "" for r in rows))
        by_member = {}
        for r in rows:
            by_member.setdefault((r["stack_id"], r["article_id"]), []).append(int(r["event_time"]))
        for key, times in by_member.items():
            self.assertEqual(times, list(range(min(times), max(times) + 1)), key)
        self.assertTrue(
            all(int(r["year"]) >= int(r["publication_year"]) for r in rows if r["treated"] == "1")
        )


class JournalYearPanelTests(unittest.TestCase):
    def setUp(self):
        path = ROOT / "data/matching/panel_journal_year.csv.gz"
        if not path.exists():
            self.skipTest("journal-year panel not built")
        self.rows = histories.read_gzip_csv(path)

    def test_comparison_weights_sum_to_one_within_each_stack(self):
        weights = collections.defaultdict(float)
        for r in self.rows:
            if r["treated"] == "0" and r["event_time"] == "-1":
                weights[r["stack_id"]] += float(r["weight"])
        self.assertTrue(weights)
        self.assertTrue(all(abs(w - 1) < 1e-9 for w in weights.values()))

    def test_comparison_articles_are_the_usable_pool_sample(self):
        candidates = read_csv(ROOT / "data/openalex/candidates.csv")
        pools = {
            (p["journal_id"], p["year"]): p["pool_id"]
            for p in read_csv(ROOT / "data/openalex/pools.csv")
        }
        usable = collections.defaultdict(set)
        for c in candidates:
            if not c["exclusion"]:
                usable[c["pool_id"]].add(c["candidate_id"])
        members = collections.defaultdict(set)
        meta = {}
        for r in self.rows:
            if r["treated"] == "0":
                members[r["stack_id"]].add(r["article_id"])
            meta[r["stack_id"]] = (r["journal_id"], r["publication_year"])
        for stack, ids in list(members.items())[:200]:
            self.assertEqual(ids, usable[pools[meta[stack]]])
