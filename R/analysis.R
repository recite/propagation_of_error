# Adapted from recite/sig_fault R/analysis.R at commit 1efbfd2172879c93b9c8a5c31f601d1e53f976a2 (helpers only).

wilson_interval <- function(successes, n, level = .95) {
  z <- qnorm(1 - (1 - level) / 2)
  p <- successes / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(lower = center - half, upper = center + half)
}

annual_summary <- function(panel, group = "treated", time = "event_time") {
  do.call(rbind, lapply(split(panel, list(panel[[group]], panel[[time]]), drop = TRUE), function(x) {
    out <- data.frame(
      n = nrow(x), total = sum(x$citations), mean = mean(x$citations),
      median = median(x$citations), zero = sum(x$citations == 0L), maximum = max(x$citations)
    )
    out[[group]] <- x[[group]][1]
    out[[time]] <- x[[time]][1]
    out
  }))
}
