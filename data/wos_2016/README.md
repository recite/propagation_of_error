# Web of Science exports, August 2016

Files from the 2021 draft, renamed from their original `new_*` names; see `manifest.json`
for hashes and the import stage for row counts.

| File | What it is |
| --- | --- |
| `retraction_notices.csv` | 3,029 WoS records of retraction notices with an `index`; `year` is the original article's year parsed from the notice |
| `retracted_articles.csv` | 3,052 WoS records of the matched retracted articles; `final_filter == 1` marks the 3,029 accepted matches |
| `retracted_article_citations.zip` | 73,565 WoS records of articles citing the retracted articles (latin-1) |
| `retraction_notice_citations.csv` | 1,897 WoS records of articles citing the notices |
| `retraction_reasons.csv` | Hand coding of reasons for 95 sampled notices (latin-1). The draft's "100 articles checked for retraction" file was a byte-identical copy and was removed |
| `acknowledgment_pre.csv`, `acknowledgment_post.csv` | Hand coding of 100 pre- and 275 post-retraction citing articles |
| `date_checks.csv` | 300 citing records with manually checked online publication dates |
| `wos_codebook.csv` | WoS field labels |
| `journal_impact_factor.csv` | Journal impact factors as used in the draft (title-keyed; 625 duplicated titles) |
| `legacy_panel.csv` | The draft's article-year panel, kept to document the dropped-zero defect |
| `reference/` | WoS category lists, journal crosswalk, and publications per year by field |

Tidy derivatives are written to `tidy/` by `make wos`.
