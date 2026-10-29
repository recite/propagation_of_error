# Synthetic difference-in-differences by retraction-year cohort. Within a cohort every unit
# shares the same calendar years at each event time, so the cohort matrix is a block-adoption
# panel: donors are the matched comparison articles of that cohort's treated articles.

sdid_cohort_roster <- function(panel, stacks, cohort) {
  ids <- stacks$stack_id[stacks$retraction_year == cohort & stacks$sdid_eligible == 1L]
  x <- panel[panel$stack_id %in% ids, ]
  units <- unique(x[c("article_id", "treated", "publication_year")])
  # A comparison article matched to two treated articles of one cohort appears once.
  units <- units[!duplicated(units$article_id), ]
  units$first_complete_event_time <- units$publication_year - cohort
  rows <- x[!duplicated(x[c("article_id", "event_time")]), ]
  list(roster = units, panel = rows, stacks = ids)
}

sdid_by_cohort <- function(panel, stacks, pre = -3:-1, post = 1:3, draws = 499L, seed = 20261028L) {
  cohorts <- sort(unique(stacks$retraction_year[stacks$sdid_eligible == 1L]))
  estimates <- paths <- weights <- boots <- list()
  for (cohort in cohorts) {
    parts <- sdid_cohort_roster(panel, stacks, cohort)
    if (sum(parts$roster$treated == 1L) < 2L || sum(parts$roster$treated == 0L) < 2L) next
    setup <- sdid_matrix(parts$panel, parts$roster, pre, post)
    fitted <- sdid_fit(setup)
    boot <- sdid_bootstrap(fitted$fit, draws, seed + cohort)
    did <- sdid_did(setup)
    estimates[[length(estimates) + 1L]] <- data.frame(
      cohort = cohort, treated = nrow(setup$y) - setup$n0, comparison = setup$n0,
      estimate = as.numeric(fitted$fit), se = boot$se,
      lower = as.numeric(fitted$fit) - qnorm(.975) * boot$se,
      upper = as.numeric(fitted$fit) + qnorm(.975) * boot$se,
      did_estimate = did$estimate, did_se = did$se,
      treated_post_mean = mean(fitted$paths$treated_mean[-seq_len(setup$t0)]),
      synthetic_post_mean = mean(fitted$paths$synthetic_adjusted[-seq_len(setup$t0)]),
      bootstrap_draws = draws, rejected_draws = boot$rejected, fitted$diagnostics
    )
    paths[[length(paths) + 1L]] <- cbind(cohort = cohort, fitted$paths)
    omega <- attr(fitted$fit, "weights")$omega
    weights[[length(weights) + 1L]] <- data.frame(
      cohort = cohort, article_id = setup$roster$article_id[seq_len(setup$n0)], omega = omega
    )
    boots[[length(boots) + 1L]] <- cbind(cohort = cohort, boot$draws)
  }
  estimates <- do.call(rbind, estimates)
  w <- 1 / estimates$se^2
  pooled <- data.frame(
    pooling = c("inverse_variance", "equal_treated_weight"),
    estimate = c(sum(w * estimates$estimate) / sum(w),
      sum(estimates$treated * estimates$estimate) / sum(estimates$treated)),
    se = c(sqrt(1 / sum(w)),
      sqrt(sum((estimates$treated / sum(estimates$treated))^2 * estimates$se^2))),
    cohorts = nrow(estimates), treated = sum(estimates$treated), comparison = sum(estimates$comparison)
  )
  pooled$lower <- pooled$estimate - qnorm(.975) * pooled$se
  pooled$upper <- pooled$estimate + qnorm(.975) * pooled$se
  list(
    cohorts = estimates, pooled = pooled, paths = do.call(rbind, paths),
    weights = do.call(rbind, weights), draws = do.call(rbind, boots)
  )
}
