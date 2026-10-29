# The 2021 draft's design: retracted articles only, a post-retraction indicator and a linear
# trend in years from the notice, article fixed effects, clustered by article. It is kept so
# the new estimates can be compared with the control-free design on the same outcome.

legacy_within_model <- function(panel, label, time = "event_time", post_from = 1L) {
  x <- panel[panel$treated == 1L, ]
  x$post <- as.integer(x[[time]] >= post_from)
  x$trend <- x[[time]]
  stopifnot(!anyDuplicated(x[c("article_id", "trend")]))
  fit <- fixest::feols(citations ~ post + trend | article_id, x,
    vcov = ~article_id, ssc = stack_correction(), nthreads = 1
  )
  g <- length(unique(x$article_id))
  b <- unname(coef(fit)["post"])
  se <- sqrt(vcov(fit)["post", "post"])
  df <- g - 1L
  pois <- fixest::fepois(citations ~ post + trend | article_id, x,
    vcov = ~article_id, ssc = stack_correction(), nthreads = 1, glm.tol = 1e-10, fixef.tol = 1e-10
  )
  bp <- unname(coef(pois)["post"])
  sep <- sqrt(vcov(pois)["post", "post"])
  data.frame(
    specification = label, articles = g, observations = nrow(x),
    estimate = b, se = se, df = df, lower = b - qt(.975, df) * se, upper = b + qt(.975, df) * se,
    poisson_estimate = bp, poisson_se = sep,
    percent = 100 * expm1(bp),
    percent_lower = 100 * expm1(bp - qt(.975, df) * sep),
    percent_upper = 100 * expm1(bp + qt(.975, df) * sep),
    mean_pre = mean(x$citations[x$post == 0L]), mean_post = mean(x$citations[x$post == 1L])
  )
}
