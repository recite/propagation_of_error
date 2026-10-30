# Retraction timing among retracted articles only: articles not yet retracted serve as the
# comparison for those already retracted, holding fixed each article, its age and the calendar
# year (the design of Lu et al. 2013). Needs no comparison articles, so it runs on any source
# that records citations to the retracted articles alone. Sun and Abraham's interaction
# estimator keeps already-treated cohorts out of the comparison; relative years beyond
# `window` are binned. Every article is eventually retracted, so with article, age and year
# effects a linear trend in event time is not identified (Borusyak, Jaravel and Spiess); the
# binned far pre-period is set to zero along with the year before retraction to pin it down.
timing_event_study <- function(panel, label, window = 5L) {
  required <- c("article_id", "year", "citations", "retraction_year", "publication_year")
  stopifnot(all(required %in% names(panel)))
  stopifnot(!anyNA(panel$citations), all(panel$citations >= 0))
  panel$age <- panel$year - panel$publication_year
  relative <- pmin(pmax(panel$year - panel$retraction_year, -(window + 1L)), window + 1L)
  panel$binned_year <- panel$retraction_year + relative
  fit <- fixest::fepois(
    citations ~ sunab(retraction_year, binned_year, ref.p = c(-1, -(window + 1L))) |
      article_id + year + age,
    panel,
    vcov = ~article_id, nthreads = 1
  )
  terms <- tidy_terms(fit, "^binned_year::-?[0-9]+$")
  terms$event_time <- as.integer(sub("^binned_year::", "", terms$term))
  terms <- terms[abs(terms$event_time) <= window, ]
  used <- panel[fixest::obs(fit), ]
  df <- length(unique(used$article_id)) - 1L
  half <- qt(.975, df) * terms$se
  # Cohort-share-weighted average of the cohort-by-year coefficients in years one to three,
  # aggregated by fixest with its standard error.
  pooled <- stats::aggregate(fit, c(post = "binned_year::[123]:cohort"))
  average <- unname(pooled[1, "Estimate"])
  average_se <- unname(pooled[1, "Std. Error"])
  rbind(
    data.frame(
      source = label, event_time = c(terms$event_time, -1L), estimate = c(terms$estimate, 0),
      se = c(terms$se, 0), lower = c(terms$estimate - half, 0),
      upper = c(terms$estimate + half, 0), articles = length(unique(used$article_id)),
      observations = nrow(used)
    ),
    data.frame(
      source = label, event_time = NA_integer_, estimate = average, se = average_se,
      lower = average - qt(.975, df) * average_se, upper = average + qt(.975, df) * average_se,
      articles = length(unique(used$article_id)), observations = nrow(used)
    )
  ) |>
    transform(
      percent = 100 * expm1(estimate), percent_lower = 100 * expm1(lower),
      percent_upper = 100 * expm1(upper)
    )
}
