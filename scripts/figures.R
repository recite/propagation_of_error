source("R/plots.R")
a <- function(name) read.csv(file.path("data/analysis", name), stringsAsFactors = FALSE)
dir.create("figs", showWarnings = FALSE)
save_figure(plot_citation_paths(a("citation_paths.csv")), "citation_paths")
save_figure(plot_event_study(a("event_study.csv")), "event_study")
heterogeneity_labels <- c(
  reason_category = "Reason for retraction",
  field = "Field",
  pre_tercile = "Pre-retraction citations (tercile)",
  lag_bin = "Years to retraction",
  prior_concern = "Prior expression of concern"
)
save_figure(plot_heterogeneity(a("heterogeneity.csv"), heterogeneity_labels), "heterogeneity")
save_figure(plot_retraction_rates(a("retraction_rates.csv")), "retraction_rates")
frame <- read.csv("data/retraction_watch/frame.csv")
save_figure(plot_time_to_retraction(frame), "time_to_retraction")
if (file.exists("data/analysis/sdid_paths.csv")) {
  save_figure(plot_sdid_paths(a("sdid_paths.csv"), a("sdid_cohorts.csv")), "sdid_paths")
}
cat("Figures written\n")
