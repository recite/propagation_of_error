# One visual grammar for every exhibit: neutral ink for comparison articles, one accent for
# retracted articles, intervals as thin ranges, direct labels instead of legends where possible.

plot_colours <- c(retracted = "#B2452C", comparison = "#5B5B5B", synthetic = "#2F6B9A")
figure_sizes <- list(
  citation_paths = c(6.5, 3.4), event_study = c(6.5, 3.4), heterogeneity = c(6.5, 5),
  retraction_rates = c(6.5, 3), time_to_retraction = c(6.5, 3), sdid_paths = c(6.5, 3.4)
)

theme_paper <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E6E6E6", linewidth = .3),
      strip.text = ggplot2::element_text(hjust = 0, face = "bold"),
      legend.position = "none",
      plot.title.position = "plot",
      axis.title = ggplot2::element_text(size = base_size - 1)
    )
}

save_figure <- function(plot, name) {
  size <- figure_sizes[[name]]
  for (ext in c("pdf", "png")) {
    ggplot2::ggsave(
      file.path("figs", paste0(name, ".", ext)), plot,
      width = size[1], height = size[2], dpi = 200,
      device = if (ext == "pdf") grDevices::cairo_pdf else ext
    )
  }
  invisible(plot)
}

label_group <- function(treated) ifelse(treated == 1, "Retracted", "Comparison")

plot_citation_paths <- function(paths) {
  long <- rbind(
    data.frame(
      paths[c("treated", "event_time")],
      statistic = "Mean citations per year", value = paths$mean
    ),
    data.frame(
      paths[c("treated", "event_time")],
      statistic = "Median citations per year", value = paths$median
    )
  )
  long$group <- label_group(long$treated)
  ends <- long[long$event_time == max(long$event_time), ]
  ggplot2::ggplot(long, ggplot2::aes(event_time, value, colour = group)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#BBBBBB", linetype = "dashed", linewidth = .3) +
    ggplot2::geom_line(linewidth = .7) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::geom_text(
      data = ends, ggplot2::aes(label = group), hjust = -.1, size = 3, show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~statistic, scales = "free_y") +
    ggplot2::scale_colour_manual(
      values = c(Retracted = plot_colours[["retracted"]], Comparison = plot_colours[["comparison"]])
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(-3, 5), expand = ggplot2::expansion(mult = c(.05, .4))
    ) +
    ggplot2::labs(x = "Years from retraction (0 = retraction year)", y = NULL) +
    theme_paper()
}

plot_event_study <- function(es) {
  es$kind <- ifelse(es$event_time == -1, "reference", "estimate")
  es$design <- factor(
    ifelse(es$specification == "Main", "Matched comparison", "Journal-year, unmatched"),
    levels = c("Matched comparison", "Journal-year, unmatched")
  )
  dodge <- ggplot2::position_dodge(width = .5)
  ggplot2::ggplot(es, ggplot2::aes(event_time, percent, colour = design, group = design)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#BBBBBB", linewidth = .3) +
    ggplot2::geom_vline(xintercept = -.5, colour = "#BBBBBB", linetype = "dashed", linewidth = .3) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = percent_lower, ymax = percent_upper), linewidth = .5, position = dodge
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = kind), size = 2.2, position = dodge) +
    ggplot2::scale_shape_manual(values = c(estimate = 16, reference = 1), guide = "none") +
    ggplot2::scale_colour_manual(
      values = c(
        `Matched comparison` = plot_colours[["retracted"]],
        `Journal-year, unmatched` = plot_colours[["comparison"]]
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = seq(-3, 5)) +
    ggplot2::labs(
      x = "Years from retraction (0 = retraction year)",
      y = "Change in citations relative to comparison articles (%)"
    ) +
    theme_paper() +
    ggplot2::theme(legend.position = "top")
}

plot_heterogeneity <- function(h, labels) {
  h$variable <- labels[h$variable]
  h <- h[order(h$variable, h$estimate), ]
  h$row <- factor(paste(h$variable, h$group), levels = unique(paste(h$variable, h$group)))
  ggplot2::ggplot(h, ggplot2::aes(percent, row)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#BBBBBB", linewidth = .3) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = percent_lower, xmax = percent_upper),
      colour = plot_colours[["retracted"]], linewidth = .5
    ) +
    ggplot2::geom_point(colour = plot_colours[["retracted"]], size = 2) +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.* ", "", x)) +
    ggplot2::facet_grid(variable ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(
      x = "Change in citations relative to matched articles, years +1 to +3 (%)", y = NULL
    ) +
    theme_paper() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1)
    )
}

plot_retraction_rates <- function(rates) {
  ggplot2::ggplot(rates, ggplot2::aes(year, per_ten_thousand)) +
    ggplot2::geom_line(colour = plot_colours[["comparison"]], linewidth = .6) +
    ggplot2::geom_point(colour = plot_colours[["comparison"]], size = 1.6) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2020, 5)) +
    ggplot2::labs(
      x = "Publication year", y = "Retracted research articles\nper 10,000 journal articles"
    ) +
    theme_paper()
}

plot_time_to_retraction <- function(frame) {
  lag <- pmin(frame$lag_years, 12L)
  counts <- as.data.frame(table(lag))
  counts$lag <- as.integer(as.character(counts$lag))
  ggplot2::ggplot(counts, ggplot2::aes(lag, Freq)) +
    ggplot2::geom_col(fill = plot_colours[["comparison"]], width = .8) +
    ggplot2::scale_x_continuous(breaks = 2:12, labels = c(2:11, "12+")) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = "Years from publication to retraction", y = "Retracted articles") +
    theme_paper()
}

plot_sdid_paths <- function(paths, cohorts) {
  weights <- cohorts$treated[match(paths$cohort, cohorts$cohort)]
  pooled <- do.call(rbind, lapply(split(seq_len(nrow(paths)), paths$event_time), function(i) {
    data.frame(
      event_time = paths$event_time[i[1]],
      Retracted = weighted.mean(paths$treated_mean[i], weights[i]),
      `Synthetic comparison` = weighted.mean(paths$synthetic_adjusted[i], weights[i]),
      check.names = FALSE
    )
  }))
  long <- rbind(
    data.frame(event_time = pooled$event_time, series = "Retracted", value = pooled$Retracted),
    data.frame(
      event_time = pooled$event_time, series = "Synthetic comparison",
      value = pooled$`Synthetic comparison`
    )
  )
  ends <- long[long$event_time == max(long$event_time), ]
  ggplot2::ggplot(long, ggplot2::aes(event_time, value, colour = series)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#BBBBBB", linetype = "dashed", linewidth = .3) +
    ggplot2::geom_line(linewidth = .7) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = series), hjust = -.1, size = 3) +
    ggplot2::scale_colour_manual(
      values = c(
        Retracted = plot_colours[["retracted"]],
        `Synthetic comparison` = plot_colours[["synthetic"]]
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = -3:3, expand = ggplot2::expansion(mult = c(.05, .6))
    ) +
    ggplot2::labs(
      x = "Years from retraction (0 = retraction year)", y = "Mean citations per year"
    ) +
    theme_paper()
}
