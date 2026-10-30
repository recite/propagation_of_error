# Propagation of Error: Citations to Retracted Research

Ken Cor and Gaurav Sood

[Read the paper](ms/main.pdf) · [Analysis results](tabs/results.json) · [Data and measurement](docs/data.md)

A retraction is the strongest public signal that an article should not be relied on. We ask how citations to retracted articles change after the retraction, relative to what the same articles would have received otherwise, and whether citing passages record the retraction. The companion paper on methodological critiques that do not lead to retraction is [recite/sig_fault](https://github.com/recite/sig_fault).

## What we learn

**Retracted articles keep being cited, but at a markedly lower rate than matched articles from the same journal and year.** In the three complete years after retraction, citations to retracted articles were **{{MainPercent}}%** (95% interval {{MainInterval}}) relative to the path of matched comparison articles. The mean number of citations per year to a retracted article was {{RetractedMeanBefore}} in the year before retraction and {{RetractedMeanAfter}} three years after; for matched articles the corresponding means were {{ComparisonMeanBefore}} and {{ComparisonMeanAfter}}. Three years after retraction, {{RetractedZeroAfter}}% of retracted articles received no citations at all, against {{ComparisonZeroAfter}}% of matched articles.

Among 2016 hand-coded citing passages, {{AckAfterUnack}} of {{AckAfterCoded}} citations made after the retraction year recorded no concern with the cited article ({{AckAfterPercent}}%, 95% interval {{AckAfterInterval}}).

## Data and research design

The treated articles are every Retraction Watch retraction of a research article published 2000–2019 and retracted 2005–2022 at least two years after publication with an original DOI ({{Frame}} articles), of which {{Resolved}} resolve to an OpenAlex journal article and {{Matched}} find at least one comparison article. Citation histories through 2025 come from OpenAlex for treated and comparison articles alike. Comparison articles are drawn from the same journal and publication year and matched on pre-retraction citations only, up to five per retracted article ({{Controls}} distinct comparison articles).

The main estimate is a Poisson regression with a fixed effect for each article within its matched set and for each matched set and event year, clustered by article: it compares each retracted article's proportional change in citations with that of its own comparison articles. Event-year estimates, an additive version, a synthetic difference-in-differences by retraction-year cohort, heterogeneity, pre-trend checks and leave-one-out exercises are in the paper. The [pre-analysis plan](docs/design.md) was committed before estimation.

A 2016 Web of Science snapshot of citations to the same articles serves as measurement validation: on {{WosArticleYears}} article-years for the same articles, OpenAlex counts are {{WosOpenAlexRatio}} times the WoS counts, agree exactly for {{WosExactShare}}% and within one citation for {{WosWithinOneShare}}%.

## Reproduce

```sh
make restore
python3 -m pip install -r requirements.txt
make check
```

`make check` rebuilds every table, figure, this README and the PDF from the committed analysis outputs and runs the tests, which verify the receipts of every committed stage against the current code and inputs. `make rebuild` regenerates the matching, estimation, synthetic DiD and validation outputs from the cached responses. Network acquisition is separate (`make rw-fetch`, `make targets-fetch`, `make openalex-fetch`) and records a receipt with the URL, retrieval time and hash of every response.

| Path | Contents |
| --- | --- |
| `data/retraction_watch/` | Frozen snapshot record, frame, exclusions, reason categories |
| `data/openalex/` | Resolved targets, annual citation histories, journal-year pools, candidates |
| `data/matching/` | Matches, balance, exclusions, stacked panel, sample flow |
| `data/validation/` | Paired WoS–OpenAlex panel and date agreement |
| `data/wos_2016/` | The 2016 Web of Science files and their tidy derivatives |
| `data/analysis/` | Every estimate as CSV |
| `R/`, `scripts/` | Estimation modules and numbered acquisition stages |
| `docs/` | Pre-analysis plan, data notes, provenance of copied code |
