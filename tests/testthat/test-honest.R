noisy_fixture <- function(effect = .3, stacks = 60L, controls = 4L, seed = 1L) {
  set.seed(seed)
  x <- stack_fixture(effect = effect, stacks = stacks, controls = controls)
  x$citations <- rpois(nrow(x), x$citations)
  x
}

test_that("relative-magnitude sets are finite and contain the original set at Mbar = 0", {
  skip_if_not_installed("HonestDiD")
  x <- noisy_fixture()
  es <- stack_event_study(x)
  h <- honest_did(es, mbar = c(0, 1))
  expect_true(h$available)
  expect_true(all(is.finite(h$results$lb)))
  expect_true(all(is.finite(h$results$ub)))
  expect_lt(h$results$ub[h$results$Mbar == 0], 0)
  expect_gte(h$results$ub[h$results$Mbar == 1], h$results$ub[h$results$Mbar == 0])
  expect_lte(h$results$lb[h$results$Mbar == 1], h$results$lb[h$results$Mbar == 0])
})

test_that("a large precise effect far from zero still yields a finite set", {
  skip_if_not_installed("HonestDiD")
  x <- noisy_fixture(effect = .05, stacks = 150L)
  h <- honest_did(stack_event_study(x), mbar = c(0, 2))
  expect_true(all(is.finite(c(h$results$lb, h$results$ub))))
  expect_lt(h$results$ub[h$results$Mbar == 0], -1)
})

test_that("periods with a gap are refused, since HonestDiD assumes consecutive periods", {
  skip_if_not_installed("HonestDiD")
  es <- stack_event_study(noisy_fixture())
  expect_error(honest_did(es, post_times = 1:3))
})
