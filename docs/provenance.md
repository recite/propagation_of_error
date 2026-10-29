# Provenance of copied code

Shared modules were copied from [recite/sig_fault](https://github.com/recite/sig_fault)
at commit `1efbfd2172879c93b9c8a5c31f601d1e53f976a2` rather than published as a package. Each copied file carries a
header naming its source. Fixes to a copied module should be made in `sig_fault`
first and re-copied here, so the two repositories do not drift.

| File here | Source in sig_fault | Changes |
| --- | --- | --- |
| `R/reporting.R` | `R/reporting.R` | none |
| `scripts/reporting.py` | `scripts/reporting.py` | none |
| `docs/number-formatting.md` | `docs/number-formatting.md` | none |
| `R/synthetic_did.R` | `R/synthetic_did.R` | roster column `flag` renamed `treated`; time column `event_time` |
| `R/stacked.R` | `R/journal_cohort.R` | stratum is the matched stack; fixed effects `article_id^stack_id + stack_id^event_time`; weights |
| `R/analysis.R` | `R/analysis.R` | kept `wilson_interval`, `annual_summary` only |
| `scripts/research_pipeline.py` | `scripts/research_pipeline.py` | no dependency on `pilot.py`; data dir `data/<study>`; fetch through `scripts/openalex.py` |
| `scripts/openalex.py` | `scripts/pilot.py` helpers and `scripts/i4r_sources.py::fetch` | merged request with cache, 429 retry-after file, batched DOI lookup, grouped annual counts |
| `scripts/citation_history.py` | `scripts/citation_history.py` | takes a resolved work id; adds `annual_counts` |
| `scripts/matching/match.py` | `scripts/i4r_match.py` | no text distance; pre-event histories only; event-time panel |
| `tests/testthat/test-synthetic-did.R` | same | column names |
| `.lintr`, `.Rprofile`, `.gitignore`, `.github/workflows/ci.yml` | same | CI installs `requirements.txt` |
