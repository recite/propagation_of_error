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

# Citations that arrive after the retraction year, for every resolved retracted article with a
# complete history: the descriptive fact the estimates are set against. `last_year` is the
# last complete calendar year of the citation data.
post_retraction_citations <- function(histories, frame, last_year = 2025L) {
  r <- frame$retraction_year[match(histories$article_id, frame$article_id)]
  stopifnot(!anyNA(r), !anyNA(histories$citations))
  histories$event_time <- histories$year - r
  per_article <- do.call(rbind, lapply(split(histories, histories$article_id), function(h) {
    data.frame(
      article_id = h$article_id[1],
      before = sum(h$citations[h$event_time < 0]),
      retraction_year = sum(h$citations[h$event_time == 0]),
      after = sum(h$citations[h$event_time > 0]),
      post_years = sum(h$event_time > 0)
    )
  }))
  total <- per_article$before + per_article$retraction_year + per_article$after
  with_post_year <- per_article[per_article$post_years >= 1L, ]
  summary <- data.frame(
    articles = nrow(per_article),
    citations = sum(total),
    after = sum(per_article$after),
    after_share = sum(per_article$after) / sum(total),
    articles_with_post_year = nrow(with_post_year),
    articles_cited_after = sum(with_post_year$after > 0),
    articles_cited_after_share = mean(with_post_year$after > 0),
    median_after = median(with_post_year$after),
    mean_after = mean(with_post_year$after)
  )
  after <- histories[histories$event_time >= 1L & histories$year <= last_year, ]
  by_year <- do.call(rbind, lapply(split(after, after$event_time), function(h) {
    data.frame(
      event_time = h$event_time[1], articles = nrow(h), citations = sum(h$citations),
      mean = mean(h$citations), median = median(h$citations),
      share_cited = mean(h$citations > 0), share_five_or_more = mean(h$citations >= 5)
    )
  }))
  list(summary = summary, by_year = by_year, per_article = per_article)
}
