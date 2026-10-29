# Rambachan and Roth (2023) sensitivity: how large a violation of parallel pre-trends, relative
# to the observed pre-period coefficients, would be needed to overturn the post-period effect.
# The event-study coefficients are on the log scale from the Poisson model; the method only
# needs asymptotic normality of the coefficient vector.

honest_did <- function(event_study, pre_times = -3:-2, post_times = 1:3, mbar = seq(0, 2, .5)) {
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
  l_vec <- rep(1 / length(post_times), length(post_times))
  relative <- HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = beta, sigma = sigma, numPrePeriods = length(pre_times),
    numPostPeriods = length(post_times), l_vec = l_vec, Mbarvec = mbar
  )
  original <- HonestDiD::constructOriginalCS(
    betahat = beta, sigma = sigma, numPrePeriods = length(pre_times),
    numPostPeriods = length(post_times), l_vec = l_vec
  )
  list(available = TRUE, results = as.data.frame(relative), original = as.data.frame(original))
}
