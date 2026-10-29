# Synthetic difference-in-differences by retraction-year cohort.
for (module in c("synthetic_did.R", "sdid_cohorts.R")) source(file.path("R", module))
out <- "data/analysis"
draws <- as.integer(Sys.getenv("SDID_DRAWS", "499"))
panel <- read.csv(gzfile("data/matching/panel.csv.gz"), stringsAsFactors = FALSE)
stacks <- read.csv("data/matching/stacks.csv", stringsAsFactors = FALSE)
result <- sdid_by_cohort(panel, stacks, draws = draws)
write.csv(result$cohorts, file.path(out, "sdid_cohorts.csv"), row.names = FALSE)
write.csv(result$pooled, file.path(out, "sdid_pooled.csv"), row.names = FALSE)
write.csv(result$paths, file.path(out, "sdid_paths.csv"), row.names = FALSE)
write.csv(result$weights, file.path(out, "sdid_weights.csv"), row.names = FALSE)
write.csv(result$draws, file.path(out, "sdid_draws.csv"), row.names = FALSE)
print(result$pooled)
