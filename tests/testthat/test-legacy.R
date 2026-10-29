test_that("the control-free legacy model reproduces the draft's sign on the corrected WoS panel", {
  counts <- read.csv(file.path(root, "data/wos_2016/tidy/citing_counts.csv"))
  articles <- read.csv(file.path(root, "data/wos_2016/tidy/articles.csv"))
  x <- counts[counts$complete == 1L, ]
  x$event_time <- x$year - articles$notice_year[match(x$index, articles$index)]
  x$article_id <- as.character(x$index)
  x$treated <- 1L
  fit <- legacy_within_model(x, "WoS 2016, zero-padded")
  expect_equal(fit$articles, 3029L)
  expect_lt(fit$estimate, 0)
  expect_lt(fit$upper, 0)
})
