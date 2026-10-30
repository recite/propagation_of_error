test_that("stacked Poisson recovers a planted proportional post-retraction change", {
  x <- stack_fixture(effect = .7)
  fit <- stack_fit(x, post = 1:3)
  expect_equal(fit$estimate, log(.7), tolerance = 1e-6)
  expect_equal(fit$stacks, 12L)
  expect_equal(fit$treated, 12L)
  scaled <- transform(x, citations = citations * 10)
  expect_equal(stack_fit(scaled, post = 1:3)$estimate, fit$estimate, tolerance = 1e-6)
})

test_that("stacked coefficient equals an explicit dummy regression", {
  x <- stack_window(stack_fixture(effect = .7), -3:-1, 1:3)
  # Quasi-likelihood: stats::glm warns about non-integer outcomes; the estimate is unaffected.
  explicit <- suppressWarnings(glm(
    citations ~ treated_post + factor(paste(article_id, stack_id)) +
      factor(paste(stack_id, event_time)),
    data = x, family = poisson(), weights = weight
  ))
  fit <- stack_fit(x, post = 1:3)
  expect_equal(fit$estimate, unname(coef(explicit)["treated_post"]), tolerance = 1e-6)
})

test_that("event-study coefficients are zero before and planted after", {
  x <- stack_fixture(effect = .7)
  es <- stack_event_study(x)$estimates
  expect_equal(es$estimate[es$event_time %in% -3:-2], c(0, 0), tolerance = 1e-6)
  expect_equal(es$estimate[es$event_time == 2], log(.7), tolerance = 1e-6)
  expect_equal(es$estimate[es$event_time == -1], 0)
})

test_that("heterogeneity groups recover their own planted effects", {
  a <- stack_fixture(effect = .5, stacks = 8L)
  b <- stack_fixture(effect = .9, stacks = 8L)
  b$stack_id <- paste0("b", b$stack_id)
  b$article_id <- paste0("b", b$article_id)
  a$group_var <- "misconduct"
  b$group_var <- "error"
  h <- stack_heterogeneity(rbind(a, b), "group_var")
  expect_equal(h$estimate[h$group == "misconduct"], log(.5), tolerance = 1e-6)
  expect_equal(h$estimate[h$group == "error"], log(.9), tolerance = 1e-6)
})

test_that("the joint test compares groups with each other, not with zero", {
  set.seed(7)
  a <- stack_fixture(effect = .6, stacks = 40L)
  b <- stack_fixture(effect = .6, stacks = 40L)
  b$stack_id <- paste0("b", b$stack_id)
  b$article_id <- paste0("b", b$article_id)
  a$group_var <- "x"
  b$group_var <- "y"
  both <- rbind(a, b)
  both$citations <- rpois(nrow(both), both$citations)
  same <- stack_heterogeneity(both, "group_var")
  expect_gt(same$joint_wald_p[1], .05)
  expect_equal(same$joint_wald_p[1], same$joint_wald_p[2])
  c <- stack_fixture(effect = .2, stacks = 40L)
  c$stack_id <- paste0("c", c$stack_id)
  c$article_id <- paste0("c", c$article_id)
  c$group_var <- "y"
  c$citations <- rpois(nrow(c), c$citations)
  different <- stack_heterogeneity(rbind(a, c), "group_var")
  expect_lt(different$joint_wald_p[1], .001)
  expect_equal(unname(wald_equal(c(1, 1, 1), diag(3))$wald), 0)
})

test_that("stacks whose treated article lacks a period are excluded, duplicates are rejected", {
  x <- stack_fixture()
  y <- x[!(x$stack_id == "s1" & x$treated == 1L & x$event_time > 0), ]
  expect_equal(stack_fit(y)$stacks, 11L)
  expect_error(check_stacked_panel(rbind(x, x[1, ])))
})

test_that("weighted paths use the matching weights", {
  p <- stack_paths(stack_fixture())
  expect_setequal(p$event_time, -3:3)
  expect_equal(weighted_median_count(c(1, 2, 100), c(1, 1, 3)), 100)
})
