retraction_rates <- function(frame, world) {
  by_publication <- as.data.frame(table(frame$publication_year), stringsAsFactors = FALSE)
  names(by_publication) <- c("year", "retracted_articles")
  by_publication$year <- as.integer(by_publication$year)
  by_publication$articles <- world$articles[match(by_publication$year, world$year)]
  stopifnot(!anyNA(by_publication$articles))
  by_publication$per_ten_thousand <-
    1e4 * by_publication$retracted_articles / by_publication$articles
  by_retraction <- as.data.frame(table(frame$retraction_year), stringsAsFactors = FALSE)
  names(by_retraction) <- c("year", "retractions")
  by_retraction$year <- as.integer(by_retraction$year)
  list(by_publication_year = by_publication, by_retraction_year = by_retraction)
}

time_to_retraction <- function(frame, by = NULL) {
  groups <- if (is.null(by)) list(all = frame) else split(frame, frame[[by]])
  do.call(rbind, lapply(names(groups), function(name) {
    lag <- groups[[name]]$lag_years
    data.frame(
      group = name, articles = length(lag), mean = mean(lag), median = median(lag),
      q25 = unname(quantile(lag, .25)), q75 = unname(quantile(lag, .75)),
      share_four_or_more = mean(lag >= 4)
    )
  }))
}
