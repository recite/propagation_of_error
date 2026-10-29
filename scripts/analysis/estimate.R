# Estimate every pre-specified quantity on the matched panel and write tidy CSVs.
# Invoked by scripts/analysis/01_estimate.py, which records the receipt.
for (module in c("reporting.R", "analysis.R", "stacked.R", "legacy.R", "describe.R",
                 "acknowledgment.R", "honest.R")) {
  source(file.path("R", module))
}
out <- "data/analysis"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
panel <- read.csv(gzfile("data/matching/panel.csv.gz"), stringsAsFactors = FALSE)
stacks <- read.csv("data/matching/stacks.csv", stringsAsFactors = FALSE)
frame <- read.csv("data/retraction_watch/frame.csv", stringsAsFactors = FALSE)
world <- read.csv("data/openalex/world_counts.csv")
coding <- read.csv("data/wos_2016/tidy/acknowledgment.csv")
wos_counts <- read.csv("data/wos_2016/tidy/citing_counts.csv")
wos_articles <- read.csv("data/wos_2016/tidy/articles.csv")
write_out <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE)

main <- stack_model(panel, "Main: matched stacks, years +1 to +3")
estimates <- rbind(
  main$estimate,
  stack_fit(panel, "Years +1 to +5", post = 1:5),
  stack_fit(panel, "Year +1 only", post = 1L),
  stack_fit(panel, "Pre-period -1 only", pre = -1L),
  stack_fit(panel, "Additive (OLS)", poisson = FALSE),
  {
    nearest <- panel[panel$match_rank <= 1L, ]
    nearest$weight <- 1
    stack_fit(nearest, "Nearest match only")
  }
)
write_out(estimates, "estimates.csv")

dynamic <- stack_event_study(panel, "Main")
write_out(dynamic$estimates, "event_study.csv")
write_out(stack_event_study(panel, "Additive (OLS)", poisson = FALSE)$estimates, "event_study_ols.csv")
write_out(pretrend_test(dynamic), "pretrends.csv")
honest <- honest_did(dynamic)
jsonlite::write_json(list(available = honest$available), file.path(out, "honest_status.json"), auto_unbox = TRUE)
if (honest$available) {
  write_out(honest$results, "honest_did.csv")
  write_out(honest$original, "honest_original.csv")
}

panel$lag_bin <- cut(panel$lag_years, c(1, 2, 3, 5, Inf), labels = c("2", "3", "4-5", "6+"))
panel$lag_bin <- as.character(panel$lag_bin)
heterogeneity <- do.call(rbind, lapply(
  c("reason_category", "field", "pre_tercile", "lag_bin", "prior_concern"),
  function(by) stack_heterogeneity(panel, by)
))
write_out(heterogeneity, "heterogeneity.csv")

leave_out <- list()
for (cohort in sort(unique(panel$retraction_year))) {
  leave_out[[length(leave_out) + 1L]] <- stack_fit(panel[panel$retraction_year != cohort, ],
    paste("Drop retraction year", cohort))
}
for (field in sort(unique(panel$field))) {
  leave_out[[length(leave_out) + 1L]] <- stack_fit(panel[panel$field != field, ], paste("Drop field", field))
}
journal_year <- paste(stacks$journal_id, stacks$publication_year)
mass <- names(which(table(journal_year) > 10L))
leave_out[[length(leave_out) + 1L]] <- stack_fit(
  panel[!paste(panel$journal_id, panel$publication_year) %in% mass, ],
  "Drop journal-years with more than ten retractions"
)
leave_out[[length(leave_out) + 1L]] <- stack_fit(panel[panel$prior_concern == 0L, ],
  "Drop articles with a prior expression of concern")
top <- stacks$stack_id[stacks$pre_cumulative >= quantile(stacks$pre_cumulative, .99)]
leave_out[[length(leave_out) + 1L]] <- stack_fit(panel[!panel$stack_id %in% top, ],
  "Drop top 1% by pre-retraction citations")
leave_out <- do.call(rbind, leave_out)
write_out(leave_out, "leave_one_out.csv")

journal_clustered <- {
  x <- stack_window(panel, -3:-1, 1:3)
  fit <- fixest::fepois(citations ~ treated_post | article_id^stack_id + stack_id^event_time, x,
    weights = ~weight, vcov = ~journal_id, ssc = stack_correction(), nthreads = 1
  )
  b <- unname(coef(fit)["treated_post"])
  se <- sqrt(vcov(fit)["treated_post", "treated_post"])
  g <- length(unique(x$journal_id[fixest::obs(fit)]))
  data.frame(specification = "Journal-clustered", estimate = b, se = se, clusters = g,
    lower = b - qt(.975, g - 1) * se, upper = b + qt(.975, g - 1) * se,
    percent = 100 * expm1(b), percent_lower = 100 * expm1(b - qt(.975, g - 1) * se),
    percent_upper = 100 * expm1(b + qt(.975, g - 1) * se))
}
write_out(journal_clustered, "journal_clustered.csv")

legacy <- rbind(
  legacy_within_model(panel, "Retracted articles only, OpenAlex, no comparison"),
  {
    x <- wos_counts[wos_counts$complete == 1L, ]
    x$event_time <- x$year - wos_articles$notice_year[match(x$index, wos_articles$index)]
    x$article_id <- as.character(x$index)
    x$treated <- 1L
    legacy_within_model(x, "2016 WoS sample, zero-padded, no comparison")
  }
)
write_out(legacy, "legacy_estimates.csv")

write_out(stack_paths(panel), "citation_paths.csv")
rates <- retraction_rates(frame, world)
write_out(rates$by_publication_year, "retraction_rates.csv")
write_out(rates$by_retraction_year, "retractions_by_year.csv")
write_out(rbind(time_to_retraction(frame), time_to_retraction(frame, "field"),
  time_to_retraction(frame, "reason_category")), "time_to_retraction.csv")
write_out(acknowledgment_summary(coding), "acknowledgment.csv")
write_out(as.data.frame(table(reason = frame$reason_category)), "reasons.csv")

status <- list(
  stacks = length(unique(panel$stack_id)), treated = main$estimate$treated,
  comparison = main$estimate$comparison, observations = main$estimate$observations,
  converged = isTRUE(main$model$convStatus), honest_available = honest$available
)
jsonlite::write_json(status, file.path(out, "status.json"), auto_unbox = TRUE, pretty = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "R-session.txt"))
cat("Main estimate:", main$estimate$percent, "% [", main$estimate$percent_lower, ",",
  main$estimate$percent_upper, "]\n")
