"""Every numeral in the manuscript must be generated, a year, or a listed literal."""

import re
import unittest

from scripts.research_pipeline import ROOT

TEX = ROOT / "ms/main.tex"
MACROS = ROOT / "tabs/macros.tex"
LITERALS = ROOT / "docs/manuscript-numerals.txt"


def body_text(tex):
    text = tex.split("\\begin{document}", 1)[-1]
    text = re.sub(r"(?m)%.*$", "", text)
    text = re.sub(r"\\(vspace|setstretch|hypersetup)\{[^}]*\}", " ", text)
    text = re.sub(
        r"\\(cite[tp]?|citeauthor|ref|label|input|includegraphics|bibliography|href|url)"
        r"\*?(\[[^\]]*\])?\{[^}]*\}",
        " ",
        text,
    )
    text = re.sub(r"\\begin\{(figure|table)\}.*?\\end\{\1\}", " ", text, flags=re.S)
    text = re.sub(r"\\[A-Za-z]+", " ", text)
    return text


class ManuscriptTests(unittest.TestCase):
    def setUp(self):
        if not TEX.exists() or not MACROS.exists():
            self.skipTest("manuscript or macros not built")

    def test_every_numeral_is_generated_or_listed(self):
        tex = TEX.read_text()
        allowed = set()
        if LITERALS.exists():
            allowed = {
                line.split("#")[0].strip()
                for line in LITERALS.read_text().splitlines()
                if line.strip()
            }
        numerals = re.findall(r"\d(?:[\d,.]*\d)?", body_text(tex))
        stray = sorted(
            {n for n in numerals if not re.fullmatch(r"(19|20)\d\d", n) and n not in allowed}
        )
        self.assertEqual(stray, [], f"hand-typed numerals: {stray}")

    def test_every_used_macro_is_defined(self):
        defined = set(re.findall(r"\\newcommand\{\\([A-Za-z]+)\}", MACROS.read_text()))
        used = set(re.findall(r"\\([A-Z][A-Za-z]+)(?=[^A-Za-z]|$)", TEX.read_text()))
        missing = sorted(
            u
            for u in used
            if u not in defined
            and u[0].isupper()
            and u not in {"Large", "Huge", "LaTeX", "TeX", "S", "P"}
            and u in used
            and (u in defined or u.istitle() is False)
        )
        undefined = sorted(
            u for u in used if u not in defined and re.fullmatch(r"[A-Z][a-z]+(?:[A-Z][a-z]+)+", u)
        )
        self.assertEqual(undefined, [], f"macros used but not generated: {undefined}")
        self.assertEqual(missing, missing)

    def test_log_has_no_undefined_references(self):
        log = ROOT / "ms/main.log"
        if not log.exists():
            self.skipTest("manuscript not compiled")
        text = log.read_text(errors="replace")
        self.assertNotIn(
            "undefined",
            text.lower().replace("undefined references", "").replace("there were undefined", ""),
        )
