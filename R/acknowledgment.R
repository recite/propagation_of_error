# Historical 2016 coding of whether a citing passage acknowledged the retraction.
# `acknowledged` is 1 when a concern was recorded, 0 when none was, NA when the citing
# article could not be located. The original column was called `approving`; the protocol
# coded any citation without a recorded concern as approving, which is not endorsement.

acknowledgment_summary <- function(coding) {
  stopifnot(all(coding$sample %in% c("pre", "post")))
  coding$period <- ifelse(coding$sample == "post", "after",
    ifelse(coding$retraction_year_citation == 1L, "notice_year", "before")
  )
  do.call(rbind, lapply(split(coding, coding$period), function(x) {
    coded <- x[!is.na(x$acknowledged), ]
    n <- nrow(coded)
    unacknowledged <- sum(coded$acknowledged == 0L)
    interval <- wilson_interval(unacknowledged, n)
    data.frame(
      period = x$period[1], sampled = nrow(x), not_located = sum(is.na(x$acknowledged)), coded = n,
      acknowledged = n - unacknowledged, unacknowledged = unacknowledged,
      unacknowledged_percent = 100 * unacknowledged / n,
      lower = 100 * interval[["lower"]], upper = 100 * interval[["upper"]]
    )
  }))
}
