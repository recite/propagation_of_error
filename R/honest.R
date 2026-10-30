# Rambachan and Roth (2023) sensitivity: how large a violation of parallel pre-trends, relative
# to the observed pre-period coefficients, would be needed to overturn the post-period effect.
# The event-study coefficients are on the log scale from the Poisson model. HonestDiD's
# restrictions run over consecutive periods, so every post year from 0 is supplied and the
# target puts zero weight on the partly exposed retraction year: the bound is for the average
# log change in years `target`, not for the pooled main coefficient.

honest_did <- function(event_study, pre_times = -3:-2, post_times = 0:3, target = 1:3,
                       mbar = seq(0, 2, .5)) {
  stopifnot(all(diff(c(pre_times, -1L, post_times)) == 1L), all(target %in% post_times))
  if (!requireNamespace("HonestDiD", quietly = TRUE)) {
    return(list(available = FALSE, results = NULL))
  }
  fit <- event_study$model
  pre_names <- paste0("event_time::", pre_times, ":treated")
  post_names <- paste0("event_time::", post_times, ":treated")
  names <- c(pre_names, post_names)
  stopifnot(all(names %in% names(coef(fit))))
  beta <- unname(coef(fit)[names])
  sigma <- unname(vcov(fit)[names, names])
  l_vec <- ifelse(post_times %in% target, 1 / length(target), 0)
  # HonestDiD's default search grid is +/- 20 standard errors around zero, not around the
  # estimate; with a precise, large effect that grid contains no accepted value and every
  # set comes back empty. Centre the grid on the estimate and let it reach the widest
  # relative-magnitude bound plus a margin.
  post <- -seq_along(pre_times)
  theta <- sum(l_vec * beta[post])
  sd_theta <- sqrt(as.numeric(t(l_vec) %*% sigma[post, post] %*% l_vec))
  largest_pre_change <- max(abs(diff(c(0, beta[seq_along(pre_times)]))))
  reach <- largest_pre_change * max(mbar) * length(post_times) + 20 * sd_theta + abs(theta)
  relative <- HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = beta, sigma = sigma, numPrePeriods = length(pre_times),
    numPostPeriods = length(post_times), l_vec = l_vec, Mbarvec = mbar,
    grid.lb = theta - reach, grid.ub = theta + reach, gridPoints = 4000
  )
  stopifnot(all(is.finite(relative$lb)), all(is.finite(relative$ub)))
  original <- HonestDiD::constructOriginalCS(
    betahat = beta, sigma = sigma, numPrePeriods = length(pre_times),
    numPostPeriods = length(post_times), l_vec = l_vec
  )
  list(
    available = TRUE, results = as.data.frame(relative), original = as.data.frame(original),
    target = theta
  )
}
