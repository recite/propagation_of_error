# Adapted from recite/sig_fault R/synthetic_did.R at commit 1efbfd2172879c93b9c8a5c31f601d1e53f976a2
sdid_matrix <- function(panel, roster, pre, post) {
  stopifnot(!anyDuplicated(roster$article_id), all(roster$treated %in% 0:1))
  stopifnot(length(pre) >= 2L, length(post) >= 1L, max(pre) < min(post))
  stopifnot(!anyDuplicated(c(pre, post)))
  roster <- roster[order(roster$treated, roster$article_id), ]
  years <- c(pre, post)
  x <- panel[panel$article_id %in% roster$article_id & panel$event_time %in% years, ]
  stopifnot(!anyDuplicated(x[c("article_id", "event_time")]), all(x$status == "complete"))
  stopifnot(all(is.finite(x$citations)), all(x$citations >= 0))
  # Every unit must be published before its first pre period so counts are complete years.
  stopifnot(all(roster$first_complete_event_time <= min(pre)))
  y <- matrix(NA_real_, nrow(roster), length(years), dimnames = list(roster$article_id, years))
  y[cbind(match(x$article_id, roster$article_id), match(x$event_time, years))] <- x$citations
  stopifnot(!anyNA(y))
  n0 <- sum(roster$treated == 0L)
  stopifnot(n0 >= 2L, nrow(y) - n0 >= 2L)
  list(y = y, n0 = n0, t0 = length(pre), roster = roster, years = years)
}

sdid_fit <- function(setup) {
  fit <- synthdid::synthdid_estimate(setup$y, setup$n0, setup$t0,
    update.omega = TRUE, update.lambda = TRUE
  )
  weights <- attr(fit, "weights")
  stopifnot(all(is.finite(fit)), all(weights$omega >= 0), all(weights$lambda >= 0))
  stopifnot(abs(sum(weights$omega) - 1) < 1e-8, abs(sum(weights$lambda) - 1) < 1e-8)
  treated <- colMeans(setup$y[-seq_len(setup$n0), , drop = FALSE])
  control <- drop(weights$omega %*% setup$y[seq_len(setup$n0), , drop = FALSE])
  gap <- treated - control
  offset <- sum(weights$lambda * gap[seq_len(setup$t0)])
  direct <- mean(gap[-seq_len(setup$t0)]) - offset
  stopifnot(abs(direct - as.numeric(fit)) < 1e-8)
  list(
    fit = fit,
    paths = data.frame(
      event_time = setup$years, treated_mean = treated, weighted_comparison = control,
      synthetic_adjusted = control + offset, gap = gap - offset,
      treated_median = apply(setup$y[-seq_len(setup$n0), , drop = FALSE], 2, median),
      comparison_mean = colMeans(setup$y[seq_len(setup$n0), , drop = FALSE]),
      comparison_median = apply(setup$y[seq_len(setup$n0), , drop = FALSE], 2, median)
    ),
    diagnostics = data.frame(
      donor_effective_n = 1 / sum(weights$omega^2), donor_max = max(weights$omega),
      pre_effective_n = 1 / sum(weights$lambda^2), pre_max = max(weights$lambda),
      pre_rmse = sqrt(mean((gap[seq_len(setup$t0)] - offset)^2)), identity_error = abs(direct - fit)
    )
  )
}

# Algorithm 2 from the pinned authors' implementation, retaining resampling receipts.
sdid_bootstrap <- function(fit, repetitions, seed) {
  setup <- attr(fit, "setup")
  options <- attr(fit, "opts")
  weights <- attr(fit, "weights")
  stopifnot(isTRUE(options$update.omega), isTRUE(options$update.lambda), repetitions >= 2L)
  set.seed(seed)
  draws <- indices <- vector("list", repetitions)
  accepted <- attempts <- rejected <- 0L
  while (accepted < repetitions) {
    attempts <- attempts + 1L
    stopifnot(attempts <= 10L * repetitions)
    index <- sort(sample(seq_len(nrow(setup$Y)), replace = TRUE))
    n0 <- sum(index <= setup$N0)
    if (n0 == 0L || n0 == length(index)) {
      rejected <- rejected + 1L
      next
    }
    initial <- weights
    omega <- weights$omega[index[index <= setup$N0]]
    initial$omega <- if (sum(omega) == 0) rep(1 / n0, n0) else omega / sum(omega)
    estimate <- do.call(synthdid::synthdid_estimate, c(list(
      Y = setup$Y[index, , drop = FALSE], N0 = n0, T0 = setup$T0,
      X = setup$X[index, , , drop = FALSE], weights = initial
    ), options))
    stopifnot(is.finite(estimate))
    accepted <- accepted + 1L
    draws[[accepted]] <- data.frame(
      draw = accepted, attempt = attempts, estimate = as.numeric(estimate)
    )
    indices[[accepted]] <- data.frame(draw = accepted, indices = paste(index, collapse = ";"))
  }
  draws <- do.call(rbind, draws)
  se <- sqrt((repetitions - 1) / repetitions) * sd(draws$estimate)
  list(draws = draws, indices = do.call(rbind, indices), se = se, rejected = rejected)
}

sdid_did <- function(setup) {
  changes <- rowMeans(setup$y[, -seq_len(setup$t0), drop = FALSE]) -
    rowMeans(setup$y[, seq_len(setup$t0), drop = FALSE])
  comparison <- changes[seq_len(setup$n0)]
  treated <- changes[-seq_len(setup$n0)]
  fit <- t.test(treated, comparison)
  data.frame(
    estimate = mean(treated) - mean(comparison), se = unname(fit$stderr),
    lower = fit$conf.int[1], upper = fit$conf.int[2], df = unname(fit$parameter)
  )
}
