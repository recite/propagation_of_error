# Synthetic difference-in-differences by retraction-year cohort. Within a cohort every unit
# shares the same calendar years at each event time, so the cohort matrix is a block-adoption
# panel: donors are the matched comparison articles of that cohort's treated articles.

sdid_cohort_roster <- function(panel, stacks, cohort) {
  ids <- stacks$stack_id[stacks$retraction_year == cohort & stacks$sdid_eligible == 1L]
  x <- panel[panel$stack_id %in% ids, ]
  units <- unique(x[c("article_id", "treated", "publication_year")])
  # A comparison article matched to two treated articles of one cohort appears once.
  units <- units[!duplicated(units$article_id), ]
  units$first_complete_event_time <- units$publication_year - cohort + 1L
  rows <- x[!duplicated(x[c("article_id", "event_time")]), ]
  list(roster = units, panel = rows, stacks = ids)
}

sdid_cohort_setups <- function(panel, stacks, pre, post) {
  cohorts <- sort(unique(stacks$retraction_year[stacks$sdid_eligible == 1L]))
  setups <- lapply(cohorts, function(cohort) {
    parts <- sdid_cohort_roster(panel, stacks, cohort)
    if (sum(parts$roster$treated == 1L) < 2L || sum(parts$roster$treated == 0L) < 2L) {
      return(NULL)
    }
    setup <- sdid_matrix(parts$panel, parts$roster, pre, post)
    setup$cohort <- cohort
    setup
  })
  Filter(Negate(is.null), setups)
}

# One bootstrap draw for every cohort at once. A comparison article matched in several
# retraction years appears in several cohort matrices, so cohort estimates are not
# independent; drawing article ids once for the whole study keeps that dependence. Each
# cohort's weights are refitted from a warm start, as in the authors' Algorithm 2.
sdid_joint_draw <- function(setups, fits, ids) {
  counts <- tabulate(match(sample(ids, replace = TRUE), ids), nbins = length(ids))
  names(counts) <- ids
  vapply(seq_along(setups), function(i) {
    s <- setups[[i]]
    index <- rep(seq_len(nrow(s$y)), times = counts[rownames(s$y)])
    n0 <- sum(index <= s$n0)
    if (n0 == 0L || n0 == length(index)) {
      return(NA_real_)
    }
    weights <- attr(fits[[i]], "weights")
    omega <- weights$omega[index[index <= s$n0]]
    weights$omega <- if (sum(omega) == 0) rep(1 / n0, n0) else omega / sum(omega)
    as.numeric(synthdid::synthdid_estimate(
      s$y[index, , drop = FALSE], n0, s$t0,
      weights = weights, update.omega = TRUE, update.lambda = TRUE
    ))
  }, numeric(1))
}

# A draw in which some cohort loses all its treated or all its comparison articles is
# rejected and replaced, so every retained draw estimates every cohort.
sdid_joint_bootstrap <- function(setups, fits, draws, seed, cores) {
  ids <- unique(unlist(lapply(setups, function(s) rownames(s$y))))
  # Parallel streams need L'Ecuyer-CMRG; the caller's generator is restored afterwards.
  previous <- RNGkind("L'Ecuyer-CMRG")
  on.exit(RNGkind(previous[1], previous[2], previous[3]), add = TRUE)
  set.seed(seed)
  kept <- matrix(numeric(0), 0L, length(setups))
  attempts <- 0L
  while (nrow(kept) < draws) {
    stopifnot(attempts <= 10L * draws)
    new <- do.call(rbind, parallel::mclapply(
      seq_len(draws - nrow(kept)), function(d) sdid_joint_draw(setups, fits, ids),
      mc.cores = cores, mc.set.seed = TRUE
    ))
    attempts <- attempts + nrow(new)
    kept <- rbind(kept, new[stats::complete.cases(new), , drop = FALSE])
  }
  list(draws = kept, rejected = attempts - nrow(kept))
}

sdid_by_cohort <- function(panel, stacks, pre = -3:-1, post = 1:3, draws = 499L,
                           seed = 20261028L, cores = max(1L, parallel::detectCores() - 1L)) {
  setups <- sdid_cohort_setups(panel, stacks, pre, post)
  fitted <- lapply(setups, sdid_fit)
  fits <- lapply(fitted, `[[`, "fit")
  started <- Sys.time()
  boot <- sdid_joint_bootstrap(setups, fits, draws, seed, cores)
  message(sprintf(
    "sdid joint bootstrap: %d cohorts, %d draws, %d rejected, %.0f s", length(setups), draws,
    boot$rejected, as.numeric(difftime(Sys.time(), started, units = "secs"))
  ))
  # Bootstrap standard errors use the same small-sample scaling as the authors' package.
  scale <- sqrt((draws - 1) / draws)
  estimates <- do.call(rbind, lapply(seq_along(setups), function(i) {
    s <- setups[[i]]
    f <- fitted[[i]]
    did <- sdid_did(s)
    se <- scale * stats::sd(boot$draws[, i])
    data.frame(
      cohort = s$cohort, treated = nrow(s$y) - s$n0, comparison = s$n0,
      estimate = as.numeric(f$fit), se = se,
      lower = as.numeric(f$fit) - qnorm(.975) * se,
      upper = as.numeric(f$fit) + qnorm(.975) * se,
      did_estimate = did$estimate, did_se = did$se,
      treated_post_mean = mean(f$paths$treated_mean[-seq_len(s$t0)]),
      synthetic_post_mean = mean(f$paths$synthetic_adjusted[-seq_len(s$t0)]),
      bootstrap_draws = draws, rejected_draws = boot$rejected, f$diagnostics
    )
  }))
  donors <- unique(unlist(lapply(setups, function(s) rownames(s$y)[seq_len(s$n0)])))
  # Pooling weights are fixed at their point values; the pooled interval comes from the same
  # joint draws, so it carries the covariance between cohorts that share comparison articles.
  pooling <- list(
    inverse_variance = 1 / estimates$se^2,
    equal_treated_weight = estimates$treated
  )
  pooled <- do.call(rbind, lapply(names(pooling), function(name) {
    w <- pooling[[name]] / sum(pooling[[name]])
    se <- scale * stats::sd(drop(boot$draws %*% w))
    data.frame(
      pooling = name, estimate = sum(w * estimates$estimate), se = se,
      cohorts = nrow(estimates), treated = sum(estimates$treated), comparison = length(donors)
    )
  }))
  pooled$lower <- pooled$estimate - qnorm(.975) * pooled$se
  pooled$upper <- pooled$estimate + qnorm(.975) * pooled$se
  cohort_ids <- vapply(setups, function(s) as.numeric(s$cohort), numeric(1))
  list(
    cohorts = estimates, pooled = pooled,
    paths = do.call(rbind, lapply(seq_along(setups), function(i) {
      cbind(cohort = setups[[i]]$cohort, fitted[[i]]$paths)
    })),
    weights = do.call(rbind, lapply(seq_along(setups), function(i) {
      s <- setups[[i]]
      data.frame(
        cohort = s$cohort, article_id = s$roster$article_id[seq_len(s$n0)],
        omega = attr(fits[[i]], "weights")$omega
      )
    })),
    draws = data.frame(
      draw = rep(seq_len(nrow(boot$draws)), times = length(setups)),
      cohort = rep(cohort_ids, each = nrow(boot$draws)),
      estimate = as.vector(boot$draws)
    )
  )
}
