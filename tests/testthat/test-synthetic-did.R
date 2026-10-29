# Adapted from recite/sig_fault tests/testthat/test-synthetic-did.R at commit 1efbfd2172879c93b9c8a5c31f601d1e53f976a2
sdid_fixture <- function() {
  set.seed(51)
  y <- matrix(rpois(24 * 7, 12), 24, 7)
  y[17:24, 5:7] <- y[17:24, 5:7] + 6
  list(y = y, n0 = 16L, t0 = 4L, years = -4:2)
}

test_that("synthetic DiD uses held-out treated outcomes only in the effect", {
  setup <- sdid_fixture()
  fitted <- sdid_fit(setup)
  changed <- setup
  changed$y[17:24, 5:7] <- changed$y[17:24, 5:7] + 100
  second <- sdid_fit(changed)
  expect_equal(as.numeric(second$fit - fitted$fit), 100, tolerance = 1e-8)
  expect_equal(attr(second$fit, "weights"), attr(fitted$fit, "weights"))
  expect_lt(fitted$diagnostics$identity_error, 1e-8)
  expect_equal(mean(tail(fitted$paths$gap, 3)), as.numeric(fitted$fit))
})

test_that("recorded bootstrap reproduces the official refitted-weight variance", {
  fit <- sdid_fit(sdid_fixture())$fit
  draws <- sdid_bootstrap(fit, 25L, 67L)
  set.seed(67)
  official <- sqrt(as.numeric(vcov(fit, method = "bootstrap", replications = 25L)))
  expect_equal(draws$se, official, tolerance = 1e-10)
  expect_equal(nrow(draws$draws), 25L)
})

test_that("equal-article DiD agrees with an explicit balanced fixed-effects regression", {
  setup <- sdid_fixture()
  panel <- expand.grid(article = seq_len(24), year = seq_len(7))
  panel$outcome <- as.vector(setup$y)
  panel$exposure <- as.integer(panel$article > 16 & panel$year > 4)
  fit <- lm(outcome ~ exposure + factor(article) + factor(year), panel)
  expect_equal(sdid_did(setup)$estimate, unname(coef(fit)["exposure"]), tolerance = 1e-8)
})

test_that("matrix construction rejects missing event times and late publications", {
  panel <- expand.grid(article_id = letters[1:4], event_time = -3:1, stringsAsFactors = FALSE)
  panel$citations <- 2
  panel$status <- "complete"
  roster <- data.frame(article_id = letters[1:4], treated = c(0, 0, 1, 1), first_complete_event_time = -4L)
  setup <- sdid_matrix(panel, roster, -3:-1, 1L)
  expect_equal(dim(setup$y), c(4L, 4L))
  expect_error(sdid_matrix(panel[-1, ], roster, -3:-1, 1L))
  roster$first_complete_event_time[1] <- -2L
  expect_error(sdid_matrix(panel, roster, -3:-1, 1L))
})

test_that("cohort synthetic DiD recovers a planted additive change", {
  set.seed(7)
  rows <- list()
  for (s in 1:6) {
    for (m in 0:4) {
      for (t in -3:3) {
        mu <- 10 + s + 2 * m + (t + 3) + if (m == 0L && t > 0) -3 else 0
        rows[[length(rows) + 1L]] <- data.frame(
          stack_id = paste0("s", s), article_id = paste0("s", s, "_", m), treated = as.integer(m == 0L),
          weight = 1, event_time = t, year = 2012L + t, citations = mu, status = "complete",
          publication_year = 2005L, retraction_year = 2012L
        )
      }
    }
  }
  panel <- do.call(rbind, rows)
  stacks <- data.frame(stack_id = paste0("s", 1:6), retraction_year = 2012L, sdid_eligible = 1L)
  out <- sdid_by_cohort(panel, stacks, draws = 10L)
  expect_equal(out$cohorts$estimate, -3, tolerance = 1e-6)
  expect_equal(out$pooled$estimate, c(-3, -3), tolerance = 1e-6)
})
