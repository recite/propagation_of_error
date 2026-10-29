test_that("historical coding counts and Wilson intervals are reproduced", {
  coding <- read.csv(file.path(root, "data/wos_2016/tidy/acknowledgment.csv"))
  s <- acknowledgment_summary(coding)
  after <- s[s$period == "after", ]
  expect_equal(c(after$sampled, after$coded, after$unacknowledged, after$acknowledged), c(275, 256, 234, 22))
  before <- s[s$period == "before", ]
  notice <- s[s$period == "notice_year", ]
  expect_equal(before$sampled + notice$sampled, 100)
  expect_equal(notice$sampled, 42)
  expect_equal(unname(wilson_interval(234, 256)), c(0.8757, 0.9426), tolerance = 1e-3)
})
