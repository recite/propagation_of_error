timing_fixture <- function(effect = .5, articles = 1500L, seed = 3L) {
  set.seed(seed)
  rows <- lapply(seq_len(articles), function(a) {
    pub <- sample(2000:2008, 1)
    retraction <- pub + sample(2:8, 1)
    years <- pub:2018
    age <- years - pub
    mu <- exp(1 + .3 * log1p(a %% 7) + .4 * age - .05 * age^2 + .02 * (years - 2000))
    mu <- mu * ifelse(years > retraction, effect, 1)
    data.frame(
      article_id = paste0("a", a), year = years, citations = rpois(length(years), mu),
      retraction_year = retraction, publication_year = pub
    )
  })
  do.call(rbind, rows)
}

test_that("the timing design recovers a planted post-retraction change and flat pre-trends", {
  out <- timing_event_study(timing_fixture(effect = .5), "fixture")
  average <- out[is.na(out$event_time), ]
  expect_equal(average$estimate, log(.5), tolerance = .08)
  pre <- out[out$event_time %in% -4:-2, ]
  expect_true(all(abs(pre$estimate) < .1))
  expect_equal(out$estimate[out$event_time %in% -1], 0)
})

test_that("no change is estimated as no change", {
  out <- timing_event_study(timing_fixture(effect = 1), "fixture")
  expect_lt(abs(out$estimate[is.na(out$event_time)]), .08)
})
