# Tables and macros for the manuscript and README. Every number in ms/main.tex comes from
# tabs/macros.tex; every table body is a kableExtra fragment built from the tidy CSVs that
# scripts/analysis/estimate.R and scripts/analysis/sdid.R write.
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(kableExtra)
})
dir.create("tabs", showWarnings = FALSE)

read_analysis <- function(name) {
  readr::read_csv(file.path("data/analysis", name), show_col_types = FALSE)
}
read_json <- function(path) jsonlite::read_json(path)

num <- function(x, digits = 1L) {
  if_else(is.finite(x), scales::comma(x, accuracy = 10^-digits), "--")
}
count <- function(x) scales::comma(x, accuracy = 1)
pct <- function(x) num(x, 1L)
ci <- function(lower, upper, digits = 1L, unit = "") {
  paste0("[", num(lower, digits), ", ", num(upper, digits), "]", unit)
}
pct_ci <- function(lower, upper) ci(lower, upper, 1L, "\\%")
# Table cells print the value alone; prose macros expand inside $p \MacroName$ and so carry
# their own relation sign.
p_cell <- function(p) if_else(p < .001, "$<$0.001", num(p, 3L))
p_macro <- function(p) if_else(p < .001, "< 0.001", paste("=", num(p, 3L)))
tex <- function(x) {
  x |>
    stringr::str_replace_all(stringr::fixed("&"), "\\&") |>
    stringr::str_replace_all(stringr::fixed("%"), "\\%") |>
    stringr::str_replace_all(stringr::fixed("_"), "\\_")
}

# Cells already carry their LaTeX (\%, $<$); kableExtra must not escape them again.
save_table <- function(x, path, align = NULL, groups = NULL, header_above = NULL) {
  align <- align %||% paste(c("l", rep("r", ncol(x) - 1L)), collapse = "")
  table <- kbl(x, format = "latex", booktabs = TRUE, escape = FALSE, align = align, linesep = "")
  if (!is.null(header_above)) table <- add_header_above(table, header_above, escape = FALSE)
  if (!is.null(groups)) {
    table <- pack_rows(table, index = groups, escape = FALSE, bold = FALSE, italic = TRUE)
  }
  save_kable(table, path)
  invisible(x)
}

# Sample flow ---------------------------------------------------------------------------------
readr::read_csv("data/matching/sample_flow.csv", show_col_types = FALSE) |>
  transmute(Step = tex(step), Articles = count(articles)) |>
  save_table("tabs/sample_flow.tex")

# Main estimates ------------------------------------------------------------------------------
estimates <- read_analysis("estimates.csv")
estimates |>
  transmute(
    Specification = tex(specification),
    Model = model,
    `Retracted / comparison` = paste(count(treated), count(comparison), sep = " / "),
    Estimate = if_else(model == "PPML", paste0(pct(percent), "\\%"), num(estimate, 2L)),
    `95\\% interval` = if_else(
      model == "PPML", pct_ci(percent_lower, percent_upper), ci(lower, upper, 2L)
    )
  ) |>
  save_table("tabs/estimates.tex", align = "llrrr")

# Event study: matched and unmatched designs side by side --------------------------------------
event_study <- read_analysis("event_study.csv")
event_cells <- function(x) {
  x |>
    transmute(
      event_time,
      change = if_else(event_time == -1, "reference", pct(percent)),
      interval = if_else(event_time == -1, "--", pct_ci(percent_lower, percent_upper))
    )
}
es_main <- filter(event_study, specification == "Main")
es_journal_year <- filter(event_study, specification == "Journal-year, unmatched")
stopifnot(identical(es_main$event_time, es_journal_year$event_time))
inner_join(
  event_cells(es_main), event_cells(es_journal_year),
  by = "event_time", suffix = c("_matched", "_journal_year")
) |>
  rename(
    `Years from retraction` = event_time,
    `Change (\\%)` = change_matched, `95\\% interval` = interval_matched,
    `Change (\\%) ` = change_journal_year, `95\\% interval ` = interval_journal_year
  ) |>
  save_table(
    "tabs/event_study.tex",
    align = "rrrrr",
    header_above = c(" " = 1, "Matched comparison" = 2, "Journal-year, unmatched" = 2)
  )

# Heterogeneity: one block per pre-specified split ---------------------------------------------
split_labels <- c(
  reason_category = "Reason for retraction",
  field = "Field",
  pre_tercile = "Pre-retraction citations (tercile)",
  lag_bin = "Years to retraction",
  prior_concern = "Prior expression of concern"
)
group_labels <- tribble(
  ~variable, ~group, ~label,
  "reason_category", "misconduct", "Misconduct",
  "reason_category", "duplication", "Plagiarism or duplication",
  "reason_category", "error", "Error",
  "reason_category", "other", "Other or unstated",
  "field", "bls", "Life sciences",
  "field", "hsc", "Health sciences",
  "field", "phy", "Physical sciences",
  "field", "bt_env", "Technology and environment",
  "field", "soc_hum", "Social sciences and humanities",
  "pre_tercile", "1", "Lowest",
  "pre_tercile", "2", "Middle",
  "pre_tercile", "3", "Highest",
  "lag_bin", "2", "2",
  "lag_bin", "3", "3",
  "lag_bin", "4-5", "4--5",
  "lag_bin", "6+", "6 or more",
  "prior_concern", "0", "No",
  "prior_concern", "1", "Yes"
)
heterogeneity <- read_analysis("heterogeneity.csv") |>
  mutate(group = as.character(group), variable = factor(variable, names(split_labels))) |>
  inner_join(group_labels, by = c("variable", "group")) |>
  arrange(variable)
stopifnot(nrow(heterogeneity) == nrow(group_labels))
joint_tests <- heterogeneity |>
  group_by(variable) |>
  summarise(p = first(joint_wald_p), n = n(), .groups = "drop")
block_sizes <- set_names(
  joint_tests$n,
  paste0(
    split_labels[as.character(joint_tests$variable)],
    " (joint test $p$ ", p_cell(joint_tests$p), ")"
  )
)
heterogeneity |>
  transmute(
    Group = label, `Retracted articles` = count(stacks), `Change (\\%)` = pct(percent),
    `95\\% interval` = pct_ci(percent_lower, percent_upper)
  ) |>
  save_table("tabs/heterogeneity.tex", groups = block_sizes)

# Robustness ----------------------------------------------------------------------------------
leave_one_out <- read_analysis("leave_one_out.csv")
journal_clustered <- read_analysis("journal_clustered.csv")
bind_rows(
  leave_one_out |> select(specification, stacks, percent, percent_lower, percent_upper),
  journal_clustered |>
    transmute(
      specification = "Journal-clustered standard errors", stacks = estimates$stacks[1],
      percent, percent_lower, percent_upper
    )
) |>
  transmute(
    Specification = tex(specification), `Retracted articles` = count(stacks),
    `Change (\\%)` = pct(percent), `95\\% interval` = pct_ci(percent_lower, percent_upper)
  ) |>
  save_table("tabs/robustness.tex")

# The earlier draft's control-free design ------------------------------------------------------
legacy <- read_analysis("legacy_estimates.csv")
legacy |>
  transmute(
    Sample = tex(specification), Articles = count(articles),
    `Additive change` = num(estimate, 2L), `95\\% interval` = ci(lower, upper, 2L),
    `Proportional change (\\%)` = paste(pct(percent), pct_ci(percent_lower, percent_upper))
  ) |>
  save_table("tabs/legacy_comparison.tex")

# Acknowledgment coding (2016) ------------------------------------------------------------------
period_labels <- c(
  before = "Before the retraction year", notice_year = "Retraction year",
  after = "After the retraction year"
)
acknowledgment <- read_analysis("acknowledgment.csv") |>
  mutate(period = factor(period, names(period_labels))) |>
  arrange(period)
acknowledgment |>
  transmute(
    `Citing year` = period_labels[as.character(period)], Sampled = count(sampled),
    `Not located` = count(not_located), Coded = count(coded),
    `No concern recorded` = count(unacknowledged), `Share (\\%)` = pct(unacknowledged_percent),
    `95\\% interval` = ci(lower, upper)
  ) |>
  save_table("tabs/acknowledgment.tex")

# Web of Science versus OpenAlex ----------------------------------------------------------------
comparison <- read_json("data/validation/comparison_summary.json")
imap(comparison$by_year, \(v, year) {
  tibble(year = as.integer(year), n = v$n, wos = v$wos, openalex = v$openalex)
}) |>
  list_rbind() |>
  # Early years hold a handful of article-years each; pool them into one row.
  mutate(label = if_else(year < 2000L, "Before 2000", as.character(year))) |>
  group_by(label) |>
  summarise(year = min(year), across(c(n, wos, openalex), sum), .groups = "drop") |>
  arrange(year) |>
  transmute(
    Year = label, `Article-years` = count(n), `Web of Science` = count(wos),
    OpenAlex = count(openalex), Ratio = if_else(wos > 0, num(openalex / wos, 2L), "--")
  ) |>
  save_table("tabs/source_comparison.tex")

# Citations after retraction, year by year ------------------------------------------------------
post <- read_analysis("post_retraction.csv")
post_by_year <- read_analysis("post_retraction_by_year.csv")
post_by_year |>
  filter(event_time <= 8L) |>
  transmute(
    `Years after retraction` = event_time, `Articles observed` = count(articles),
    Citations = count(citations), Mean = num(mean), Median = num(median, 0L),
    `Cited at all (\\%)` = pct(100 * share_cited),
    `Five or more (\\%)` = pct(100 * share_five_or_more)
  ) |>
  save_table("tabs/post_retraction.tex", align = "rrrrrrr")

# Synthetic difference-in-differences by cohort -----------------------------------------------
sdid_available <- file.exists("data/analysis/sdid_pooled.csv")
if (sdid_available) {
  sdid_pooled <- read_analysis("sdid_pooled.csv")
  read_analysis("sdid_cohorts.csv") |>
    transmute(
      `Retraction year` = cohort,
      `Retracted / donors` = paste(count(treated), count(comparison), sep = " / "),
      `Synthetic DiD` = num(estimate, 2L), `95\\% interval` = ci(lower, upper, 2L),
      DiD = num(did_estimate, 2L)
    ) |>
    save_table("tabs/sdid.tex")
}

# Macros ----------------------------------------------------------------------------------------
paths <- read_analysis("citation_paths.csv") |> filter(comparison == "matched")
reasons <- read_analysis("reasons.csv")
pretrends <- read_analysis("pretrends.csv")
honest <- read_json("data/analysis/honest_status.json")
status <- read_json("data/analysis/status.json")
resolution <- read_json("data/openalex/target_resolution.json")
match_summary <- read_json("data/matching/match_summary.json")
frame_summary <- read_json("data/retraction_watch/frame_summary.json")
snapshot <- read_json("data/retraction_watch/snapshot.json")
lag_all <- read_analysis("time_to_retraction.csv") |> filter(group == "all")

spec <- function(name) filter(estimates, specification == name)
main <- estimates[1, ]
five <- spec("Years +1 to +5")
additive <- filter(estimates, model == "OLS")[1, ]
journal_year <- spec("All sampled journal-year articles, unmatched")
path_at <- function(arm, time, column) {
  paths[[column]][paths$treated == arm & paths$event_time == time]
}
event_at <- function(time, column = "percent") pct(es_main[[column]][es_main$event_time == time])
journal_year_at <- function(time) pct(es_journal_year$percent[es_journal_year$event_time == time])
reason_share <- function(name) pct(100 * reasons$Freq[reasons$reason == name] / sum(reasons$Freq))
het_at <- function(variable, group) {
  pct(heterogeneity$percent[heterogeneity$variable == variable & heterogeneity$group == group])
}
het_p <- function(variable) p_macro(joint_tests$p[joint_tests$variable == variable])
field_range <- range(heterogeneity$percent[heterogeneity$variable == "field"])
ack <- function(period, column) acknowledgment[[column]][acknowledgment$period == period]
post_at <- function(time, column) post_by_year[[column]][post_by_year$event_time == time]

macros <- c(
  Retractions = count(snapshot$rows_by_nature$Retraction),
  Frame = count(frame_summary$frame),
  Resolved = count(resolution$frame_eligible),
  Matched = count(match_summary$matched),
  Unmatched = count(match_summary$exclusions$no_match_within_caliper %||% 0L),
  Stacks = count(status$stacks),
  Comparison = count(status$comparison),
  Controls = count(match_summary$controls),
  MainPercent = pct(main$percent),
  MainLower = pct(main$percent_lower),
  MainUpper = pct(main$percent_upper),
  MainReduction = pct(-main$percent),
  FivePercent = pct(five$percent),
  FiveLower = pct(five$percent_lower),
  FiveUpper = pct(five$percent_upper),
  JournalYearPercent = pct(journal_year$percent),
  JournalYearLower = pct(journal_year$percent_lower),
  JournalYearUpper = pct(journal_year$percent_upper),
  JournalYearComparison = count(journal_year$comparison),
  JournalYearMinusThree = journal_year_at(-3),
  JournalYearMinusTwo = journal_year_at(-2),
  AdditiveEstimate = num(additive$estimate, 2L),
  AdditiveLower = num(additive$lower, 2L),
  AdditiveUpper = num(additive$upper, 2L),
  PretrendP = p_macro(pretrends$p_value),
  EventMinusThree = event_at(-3),
  EventMinusTwo = event_at(-2),
  EventZero = event_at(0),
  EventOne = event_at(1),
  EventThree = event_at(3),
  EventFive = event_at(5),
  EventFiveLower = event_at(5, "percent_lower"),
  EventFiveUpper = event_at(5, "percent_upper"),
  RetractedMeanBefore = num(path_at(1, -1, "mean")),
  RetractedMeanAfter = num(path_at(1, 3, "mean")),
  ComparisonMeanBefore = num(path_at(0, -1, "mean")),
  ComparisonMeanAfter = num(path_at(0, 3, "mean")),
  RetractedMedianBefore = num(path_at(1, -1, "median"), 0L),
  RetractedMedianAfter = num(path_at(1, 3, "median"), 0L),
  RetractedZeroAfter = pct(100 * path_at(1, 3, "zero_share")),
  ComparisonZeroAfter = pct(100 * path_at(0, 3, "zero_share")),
  LegacyOpenAlexPercent = pct(legacy$percent[1]),
  LegacyWosPercent = pct(legacy$percent[2]),
  LegacyWosAdditive = num(legacy$estimate[2], 2L),
  LegacyWosArticles = count(legacy$articles[2]),
  AckAfterPercent = pct(ack("after", "unacknowledged_percent")),
  AckAfterLower = pct(ack("after", "lower")),
  AckAfterUpper = pct(ack("after", "upper")),
  AckAfterCoded = count(ack("after", "coded")),
  AckAfterUnack = count(ack("after", "unacknowledged")),
  AckBeforeCoded = count(ack("before", "coded")),
  AckBeforeUnack = count(ack("before", "unacknowledged")),
  AckBeforePercent = pct(ack("before", "unacknowledged_percent")),
  PostArticles = count(post$articles),
  PostCitations = count(post$citations),
  PostAfter = count(post$after),
  PostAfterShare = pct(100 * post$after_share),
  PostCitedShare = pct(100 * post$articles_cited_after_share),
  PostMedianAfter = num(post$median_after, 0L),
  PostMeanAfter = num(post$mean_after),
  PostCitedAtFive = pct(100 * post_at(5, "share_cited")),
  PostMeanAtFive = num(post_at(5, "mean")),
  PostArticlesAtFive = count(post_at(5, "articles")),
  PostCitedAtEight = pct(100 * post_at(8, "share_cited")),
  MeanLag = num(lag_all$mean),
  MedianLag = num(lag_all$median, 0L),
  LagFourPlusPercent = pct(100 * lag_all$share_four_or_more),
  HetMisconduct = het_at("reason_category", "misconduct"),
  HetDuplication = het_at("reason_category", "duplication"),
  HetError = het_at("reason_category", "error"),
  HetOther = het_at("reason_category", "other"),
  HetReasonP = het_p("reason_category"),
  HetFieldP = het_p("field"),
  HetFieldMin = pct(field_range[1]),
  HetFieldMax = pct(field_range[2]),
  HetTercileP = het_p("pre_tercile"),
  HetLagTwo = het_at("lag_bin", "2"),
  HetLagSixPlus = het_at("lag_bin", "6+"),
  HetLagP = het_p("lag_bin"),
  HetPriorYes = het_at("prior_concern", "1"),
  HetPriorNo = het_at("prior_concern", "0"),
  HetPriorP = het_p("prior_concern"),
  LooMin = pct(min(leave_one_out$percent)),
  LooMax = pct(max(leave_one_out$percent)),
  JournalClusteredLower = pct(journal_clustered$percent_lower),
  JournalClusteredUpper = pct(journal_clustered$percent_upper),
  MisconductShare = reason_share("misconduct"),
  DuplicationShare = reason_share("duplication"),
  ErrorShare = reason_share("error"),
  WosOpenAlexRatio = num(comparison$ratio_openalex_to_wos, 2L),
  WosExactShare = pct(100 * comparison$share_exact),
  WosWithinOneShare = pct(100 * comparison$share_within_one),
  WosArticleYears = count(comparison$article_years),
  WosOverlap = count(comparison$date_agreement$overlap),
  WosNoticeYearAgreement = pct(
    100 * comparison$date_agreement$notice_year_equal / comparison$date_agreement$overlap
  ),
  HonestAvailable = if (isTRUE(honest$available)) "1" else "0"
)
if (sdid_available) {
  macros <- c(
    macros,
    SdidEstimate = num(sdid_pooled$estimate[1], 2L),
    SdidLower = num(sdid_pooled$lower[1], 2L),
    SdidUpper = num(sdid_pooled$upper[1], 2L),
    SdidCohorts = count(sdid_pooled$cohorts[1]),
    SdidTreated = count(sdid_pooled$treated[1])
  )
}
if (isTRUE(honest$available)) {
  honest_did <- read_analysis("honest_did.csv")
  holds <- filter(honest_did, lb > 0 | ub < 0)
  widest <- slice_max(honest_did, Mbar, n = 1)
  pre_coefficients <- es_main$estimate[es_main$event_time %in% -3:-2]
  macros <- c(
    macros,
    HonestBreakdown = if (nrow(holds)) num(max(holds$Mbar), 1L) else "none",
    HonestMaxMbar = num(widest$Mbar, 1L),
    HonestLowerAtMax = pct(100 * expm1(widest$lb)),
    HonestUpperAtMax = pct(100 * expm1(widest$ub)),
    HonestMaxPreChange = pct(100 * expm1(max(abs(diff(c(pre_coefficients, 0))))))
  )
}
stopifnot(all(grepl("^[A-Za-z]+$", names(macros))), !anyDuplicated(names(macros)), !anyNA(macros))
writeLines(
  c(
    "% Generated by scripts/tables.R; do not edit.",
    glue::glue("\\newcommand{{\\{names(macros)}}}{{{macros}}}")
  ),
  "tabs/macros.tex"
)
jsonlite::write_json(as.list(macros), "tabs/results.json", auto_unbox = TRUE, pretty = TRUE)

readme <- reduce(
  names(macros),
  \(text, name) gsub(paste0("{{", name, "}}"), macros[[name]], text, fixed = TRUE),
  .init = readLines("docs/README.in.md")
)
stopifnot(!any(grepl("\\{\\{[A-Za-z]+\\}\\}", readme)))
writeLines(readme, "README.md")
cat("Tables, macros and README written\n")
