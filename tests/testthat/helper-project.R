root <- normalizePath(file.path("..", ".."))
for (module in c("reporting.R", "analysis.R", "stacked.R", "synthetic_did.R", "sdid_cohorts.R",
                 "legacy.R", "describe.R", "acknowledgment.R", "honest.R")) {
  source(file.path(root, "R", module))
}
read_gzip_csv <- function(path) read.csv(gzfile(path), stringsAsFactors = FALSE)
