root <- normalizePath(file.path("..", ".."))
for (module in c("reporting.R", "analysis.R", "stacked.R", "synthetic_did.R", "sdid_cohorts.R",
                 "describe.R", "acknowledgment.R", "honest.R")) {
  source(file.path(root, "R", module))
}
read_gzip_csv <- function(path) read.csv(gzfile(path), stringsAsFactors = FALSE)

stack_fixture <- function(effect = .7, stacks = 12L, controls = 4L) {
  rows <- list()
  for (s in seq_len(stacks)) {
    for (member in 0:controls) {
      base <- 8 + 2 * s + 3 * member
      for (t in -3:3) {
        growth <- 1 + .15 * (t + 3) * (1 + .1 * s)
        mu <- base * growth
        if (member == 0L && t > 0) mu <- mu * effect
        rows[[length(rows) + 1L]] <- data.frame(
          stack_id = paste0("s", s), article_id = paste0("s", s, "_", member),
          treated = as.integer(member == 0L), weight = if (member == 0L) 1 else 1 / controls,
          event_time = t, year = 2010L + t, citations = mu, status = "complete"
        )
      }
    }
  }
  do.call(rbind, rows)
}
