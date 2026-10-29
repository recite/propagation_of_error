# Data and measurement

## Retraction Watch

`data/retraction_watch/snapshot.json` records the URL, retrieval time, size and SHA-256 of
the Retraction Watch CSV published by Crossref; the file itself is cached outside the
repository. The frame rules are in `docs/design.md` and `scripts/retraction_watch/02_frame.py`,
and every excluded record is listed with a reason in `exclusions.csv`.

Reason labels are grouped literally by `reason_categories.csv`: a retraction whose labels
include any fabrication, falsification, manipulation, paper-mill, peer-review compromise or
misconduct finding is `misconduct`; otherwise any plagiarism or duplication label gives
`duplication`; otherwise any error, unreliability or data-concern label gives `error`; the
rest (investigations, notices with little information, authorship or ethics disputes) are
`other`. A label is a notice's stated reason, not an adjudication of what happened.

## OpenAlex

Articles are resolved by DOI. An article enters the analysis only when OpenAlex returns
one work of type `article` in a journal source with a publication year within one year of
Retraction Watch's. The outcome is the number of citing works of type article or review by
their publication year, from the article's publication year through 2025, from one grouped
query per article. Comparison articles are measured identically. A citing work dated before
the cited article's publication year is excluded and counted in `history_totals.csv`.

OpenAlex orders unsampled lists by citations, so comparison pools are drawn with the API's
seeded random sample (`sample=20&seed=20261028`) after a separate count query. Candidates
flagged as retracted by OpenAlex, present in the Retraction Watch snapshot under any nature,
or themselves treated are excluded before matching.

## Web of Science, 2016

`data/wos_2016/` holds the August 2016 exports behind the earlier draft (see its README).
`scripts/wos/01_import.py` freezes their hashes, tidies them and builds a zero-padded
article-year panel. The earlier draft's analysis dropped every article-year with no
citation; the tidy panel keeps them (9,872 of 22,926 complete article-years are zero).

The hand-coded samples in `acknowledgment_{pre,post}.csv` record, for citing articles
sampled in 2016, whether the citing passage noted a concern with the cited article. The
original column was called `approving` and coded 1 when no concern was recorded; here
`acknowledged` is 1 when a concern was recorded. Citations from the retraction year itself
are separated from citations before it.
