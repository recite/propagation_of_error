source("R/plots.R")
a <- function(name) read.csv(file.path("data/analysis", name), stringsAsFactors = FALSE)
dir.create("figs", showWarnings = FALSE)
save_figure(plot_citation_paths(a("citation_paths.csv")), "citation_paths")
save_figure(plot_event_study(a("event_study.csv")), "event_study")
save_figure(plot_heterogeneity(a("heterogeneity.csv"), c(
  reason_category = "Reason for retraction", field = "Field", pre_tercile = "Pre-retraction citations (tercile)",
  lag_bin = "Years to retraction", prior_concern = "Prior expression of concern"
)), "heterogeneity")
save_figure(plot_retraction_rates(a("retraction_rates.csv")), "retraction_rates")
save_figure(plot_time_to_retraction(read.csv("data/retraction_watch/frame.csv")), "time_to_retraction")
if (file.exists("data/analysis/sdid_paths.csv")) {
  save_figure(plot_sdid_paths(a("sdid_paths.csv"), a("sdid_cohorts.csv")), "sdid_paths")
}
cat("Figures written\n")
