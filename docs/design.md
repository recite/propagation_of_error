# Pre-analysis plan

Written and committed before any estimate on the matched panel was computed. The
Retraction Watch frame, the OpenAlex resolution, and the WoS–OpenAlex measurement
comparison were inspected first; no post-retraction outcome had been tabulated.

## Question and estimand

How do citations to an article change after it is retracted, relative to what the same
article would have received without the retraction?

The primary estimand is the proportional change in annual citations to a retracted
article in the first three complete years after the retraction year, relative to its own
pre-retraction level, net of the citation path of matched articles from the same journal
and publication year. The unit is the article × calendar year. The event year is the
Retraction Watch retraction date's year; year 0 is partially exposed and is reported in
the dynamic specification but excluded from the pooled post period.

Secondary estimands: the additive change in citations per year on the same design; the
same proportional change by event year (−3 to +5); and, from the 2016 hand coding, the
share of citing passages that recorded a concern with the cited article.

## Population and frame

Retraction Watch snapshot from Crossref (SHA-256 in `data/retraction_watch/snapshot.json`).
Rules, applied in order, each with a logged exclusion reason:

1. `RetractionNature == "Retraction"`.
2. Original DOI present and parseable.
3. Retraction and original dates parseable; one record per DOI, keeping the earliest
   retraction; an earlier expression of concern is flagged as `prior_concern`.
4. Original published 2000–2019; retracted 2005–2022.
5. Retraction year at least two years after the publication year, so one complete
   pre-retraction year exists for matching.
6. Article type includes "Research Article" or "Clinical Study".
7. Resolves to one OpenAlex work of type `article` in a journal source with a
   publication year within one year of Retraction Watch's.

This gives 9,555 articles, of which 8,963 resolve. The whole frame is the treated set;
there is no sampling. Reason categories group Retraction Watch's labels literally
(`data/retraction_watch/reason_categories.csv`): misconduct > duplication > error > other,
assigned hierarchically. Fields are the modal broad subject prefix collapsed to five groups.

## Outcome

Annual count of citing works of type article or review, by the citing work's publication
year, from OpenAlex, through 2025. Treated and comparison articles are measured by the same
grouped query. Zero years are kept from the publication year onward. A citing work dated
before the cited article's publication year is excluded and counted.

## Comparison articles and matching

For each treated article: OpenAlex journal articles in the same source and publication
year (the full pool when it has at most 200 works, otherwise a seeded random sample of 20).
Excluded: works OpenAlex flags as retracted, any DOI in the Retraction Watch snapshot (any
nature), other treated articles, non-articles.

Matching uses only citation years strictly before the retraction year, through a single
function (`pre_event_histories`) that a test perturbs to show post-event outcomes cannot
change matches. Inputs: log(1 + cumulative citations through r−1) and log(1 + citations in
r−1), standardized within the pool; caliper of one standard deviation on each; up to five
nearest neighbours with equal weight. Treated articles without an in-caliper candidate are
reported as unmatched, never silently dropped.

## Estimators

All models cluster by article, use fixest's nonnested small-sample correction, and t(G−1)
intervals, as in the companion repository.

1. Static: Poisson quasi-maximum likelihood,
   `citations ~ treated_post | article_id^stack_id + stack_id^event_time`, weights 1 for
   treated and 1/k for controls, pre years −3..−1 (as available), post years +1..+3.
   Reported as 100·(exp(b)−1).
2. Dynamic: the same with event-time interactions, reference −1, event years −3..+5.
3. Additive: OLS on the same design.
4. Synthetic difference-in-differences by retraction-year cohort, restricted to stacks with
   three complete pre and post years, donors are the cohort's matched comparison articles;
   whole-article bootstrap; pooled by inverse variance and by equal treated weight.
5. Legacy comparison: the 2021 draft's control-free within-article model on the new treated
   articles and on the corrected zero-padded WoS 2016 panel.
6. Heterogeneity, pre-specified: reason category, field, pre-retraction citation tercile,
   years from publication to retraction (2, 3, 4–5, 6+), prior expression of concern.
7. Diagnostics: joint Wald test on the −3 and −2 coefficients; Rambachan–Roth relative
   magnitude bounds if HonestDiD installs; leave-one-cohort-out and leave-one-field-out;
   journal-year groups with more than ten retractions dropped; journal-clustered errors.

## Interpretation rules

A null is reported as the largest effect the interval still permits. The causal reading
requires that matched comparison articles follow the citation path the retracted article
would have followed; the pre-period coefficients and the synthetic DiD pre-fit are the
evidence offered on that, not proof. Continued citation and a penalty can coexist.

## Out of scope

A new citation-context (acknowledgment) sample; citations to the retraction notices;
author- or institution-level outcomes.

## Changes after this plan

None yet. Any change is recorded here with its date and reason.

- 2026-10-29: the pilot panel (200 articles drawn by the seeded order, used to size the
  comparison pools and to measure caliper hit rates) was also used to run the estimation,
  synthetic DiD, figure and table code end to end before the full citation pull. No rule
  above was changed as a result; pilot estimates are not reported.
- 2026-10-29: OpenAlex lists are ordered by citations when unsampled, so comparison pools
  are drawn with the API's seeded random sample after a separate count query (20 per
  pool). This is a measurement detail of the pre-specified pool rule, not a change to it.
- 2026-10-29: the full citation pull runs on prepaid OpenAlex credit rather than the free
  daily budget, with eight parallel workers. A 429 whose Retry-After is under a minute is
  now treated as a per-second rate limit and retried in place; longer waits still stop the
  run until the stated time. No sampling or matching rule changed.
