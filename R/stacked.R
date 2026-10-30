# Adapted from recite/sig_fault R/journal_cohort.R at commit
# 1efbfd2172879c93b9c8a5c31f601d1e53f976a2.
# A stack is one retracted article with its matched same-journal-year comparison articles.
# Fixed effects: one per article within its stack, and one per stack and event time, so each
# stack supplies its own citation path and the contrast is treated minus matched controls.

stack_correction <- function() {
  fixest::ssc(
    K.adj = TRUE, K.fixef = "nonnested", K.exact = FALSE,
    G.adj = TRUE, G.df = "min", t.df = "min"
  )
}

check_stacked_panel <- function(panel) {
  required <- c("stack_id", "article_id", "treated", "weight", "event_time", "citations")
  stopifnot(all(required %in% names(panel)), !anyNA(panel[required]))
  stopifnot(!anyDuplicated(panel[c("stack_id", "article_id", "event_time")]))
  stopifnot(all(panel$treated %in% 0:1), all(is.finite(panel$citations)), all(panel$citations >= 0))
  stopifnot(all(panel$weight > 0))
  invisible(panel)
}

stack_window <- function(panel, pre = -3:-1, post = 1:3) {
  stopifnot(length(intersect(pre, post)) == 0L, all(pre < 0), all(post > 0))
  x <- panel[panel$event_time %in% c(pre, post), ]
  x$treated_post <- x$treated * as.integer(x$event_time %in% post)
  # A stack identifies the contrast only if its treated article has both periods.
  has <- aggregate(cbind(pre = event_time < 0, post = event_time > 0) ~ stack_id,
    x[x$treated == 1L, ], function(v) any(v)
  )
  keep <- has$stack_id[has$pre & has$post]
  x[x$stack_id %in% keep, ]
}

fit_stacked <- function(formula, x, poisson) {
  options <- list(
    fml = formula, data = x, weights = ~weight, nthreads = 1,
    vcov = fixest::vcov_cluster(cluster = ~article_id, vcov_fix = FALSE), ssc = stack_correction()
  )
  if (poisson) options <- c(options, list(glm.tol = 1e-10, fixef.tol = 1e-10))
  fit <- do.call(if (poisson) fixest::fepois else fixest::feols, options)
  if (poisson) stopifnot(isTRUE(fit$convStatus))
  fit
}

# Coefficients and their clustered standard errors as a tidy frame (broom's tidy.fixest reads
# the vcov the model was fitted with), restricted to terms matching `pattern`.
tidy_terms <- function(fit, pattern) {
  out <- broom::tidy(fit)
  out <- out[grepl(pattern, out$term), ]
  data.frame(
    term = as.character(out$term), estimate = as.vector(out$estimate),
    se = as.vector(out$std.error)
  )
}

summarise_coefficient <- function(fit, x, name, poisson, label) {
  used <- x[fixest::obs(fit), ]
  g <- length(unique(used$article_id))
  df <- g - 1L
  term <- tidy_terms(fit, paste0("^", name, "$"))
  stopifnot(nrow(term) == 1L)
  b <- term$estimate
  se <- term$se
  lower <- b - qt(.975, df) * se
  upper <- b + qt(.975, df) * se
  data.frame(
    specification = label, model = if (poisson) "PPML" else "OLS",
    stacks = length(unique(used$stack_id)),
    treated = length(unique(used$article_id[used$treated == 1L])),
    comparison = length(unique(used$article_id[used$treated == 0L])),
    observations = nrow(used), clusters = g, dropped_observations = nrow(x) - nrow(used),
    estimate = b, se = se, df = df, lower = lower, upper = upper,
    percent = if (poisson) 100 * expm1(b) else NA_real_,
    percent_lower = if (poisson) 100 * expm1(lower) else NA_real_,
    percent_upper = if (poisson) 100 * expm1(upper) else NA_real_,
    vcov_parameters = attr(vcov(fit, attr = TRUE), "df.K"),
    method = "Article-clustered; nonnested FE adjustment; t(G-1)"
  )
}

stack_model <- function(panel, label = "Main", pre = -3:-1, post = 1:3, poisson = TRUE) {
  check_stacked_panel(panel)
  x <- stack_window(panel, pre, post)
  fit <- fit_stacked(
    citations ~ treated_post | article_id^stack_id + stack_id^event_time, x, poisson
  )
  stopifnot("treated_post" %in% names(coef(fit)))
  list(
    estimate = summarise_coefficient(fit, x, "treated_post", poisson, label),
    model = fit, panel = x
  )
}

stack_fit <- function(...) stack_model(...)$estimate

stack_event_study <- function(panel, label = "Main", times = -3:5, poisson = TRUE) {
  check_stacked_panel(panel)
  x <- panel[panel$event_time %in% times, ]
  fit <- fit_stacked(
    citations ~ i(event_time, treated, ref = -1) | article_id^stack_id + stack_id^event_time,
    x, poisson
  )
  used <- x[fixest::obs(fit), ]
  df <- length(unique(used$article_id)) - 1L
  terms <- tidy_terms(fit, "^event_time::")
  b <- terms$estimate
  se <- terms$se
  out <- data.frame(
    specification = label, model = if (poisson) "PPML" else "OLS",
    event_time = as.integer(sub("event_time::(-?[0-9]+):treated", "\\1", terms$term)),
    estimate = b, se = se, df = df,
    lower = b - qt(.975, df) * se, upper = b + qt(.975, df) * se,
    stacks = length(unique(used$stack_id)), clusters = df + 1L
  )
  out <- rbind(out, data.frame(
    specification = label, model = out$model[1], event_time = -1L, estimate = 0, se = 0,
    df = df, lower = 0, upper = 0, stacks = out$stacks[1], clusters = out$clusters[1]
  ))
  out <- out[order(out$event_time), ]
  if (poisson) {
    out$percent <- 100 * expm1(out$estimate)
    out$percent_lower <- 100 * expm1(out$lower)
    out$percent_upper <- 100 * expm1(out$upper)
  }
  list(estimates = out, model = fit, panel = used)
}

pretrend_test <- function(event_study) {
  fit <- event_study$model
  names <- grep("^event_time::-[23]:treated$", names(coef(fit)), value = TRUE)
  stopifnot(length(names) >= 1L)
  b <- coef(fit)[names]
  v <- vcov(fit)[names, names, drop = FALSE]
  stat <- as.numeric(t(b) %*% solve(v) %*% b)
  data.frame(
    coefficients = paste(names, collapse = ";"), wald = stat, df = length(names),
    p_value = pchisq(stat, length(names), lower.tail = FALSE)
  )
}

# Chi-square Wald test that a set of coefficients are all equal: contrasts of each
# coefficient against the first. fixest::wald() tests that they are all zero, which is a
# different hypothesis and is rejected whenever the common effect is nonzero.
wald_equal <- function(b, v) {
  k <- length(b)
  stopifnot(k >= 2L, all(dim(v) == k))
  contrasts <- cbind(-1, diag(k - 1L))
  d <- contrasts %*% b
  stat <- as.numeric(t(d) %*% solve(contrasts %*% v %*% t(contrasts)) %*% d)
  data.frame(wald = stat, df = k - 1L, p_value = pchisq(stat, k - 1L, lower.tail = FALSE))
}

stack_heterogeneity <- function(panel, by, pre = -3:-1, post = 1:3, poisson = TRUE) {
  check_stacked_panel(panel)
  stopifnot(by %in% names(panel), !anyNA(panel[[by]]))
  x <- stack_window(panel, pre, post)
  x$group <- as.character(x[[by]])
  fit <- fit_stacked(
    citations ~ i(group, treated_post) | article_id^stack_id + stack_id^event_time, x, poisson
  )
  used <- x[fixest::obs(fit), ]
  df <- length(unique(used$article_id)) - 1L
  terms <- tidy_terms(fit, "^group::")
  names <- terms$term
  b <- terms$estimate
  se <- terms$se
  equal <- wald_equal(b, vcov(fit)[names, names, drop = FALSE])
  data.frame(
    variable = by, group = sub("^group::(.*):treated_post$", "\\1", names),
    stacks = as.vector(
      table(unique(used[c("stack_id", "group")])$group)[
        sub("^group::(.*):treated_post$", "\\1", names)
      ]
    ),
    estimate = unname(b), se = unname(se), df = df,
    lower = unname(b - qt(.975, df) * se), upper = unname(b + qt(.975, df) * se),
    percent = if (poisson) 100 * expm1(unname(b)) else NA_real_,
    percent_lower = if (poisson) 100 * expm1(unname(b - qt(.975, df) * se)) else NA_real_,
    percent_upper = if (poisson) 100 * expm1(unname(b + qt(.975, df) * se)) else NA_real_,
    joint_wald_p = equal$p_value
  )
}

weighted_median_count <- function(value, weight) {
  stopifnot(length(value) == length(weight), length(value) > 0L)
  stopifnot(!anyNA(value), !anyNA(weight), all(weight > 0), all(is.finite(weight)))
  order <- order(value)
  value[order][which(cumsum(weight[order]) >= sum(weight) / 2)[1]]
}

stack_paths <- function(panel) {
  check_stacked_panel(panel)
  do.call(rbind, lapply(
    split(panel, list(panel$treated, panel$event_time), drop = TRUE),
    function(g) {
      data.frame(
        treated = g$treated[1], event_time = g$event_time[1], articles = nrow(g),
        stacks = length(unique(g$stack_id)), sum_weights = sum(g$weight),
        mean = weighted.mean(g$citations, g$weight),
        median = weighted_median_count(g$citations, g$weight),
        zero_share = weighted.mean(g$citations == 0, g$weight)
      )
    }
  ))
}
