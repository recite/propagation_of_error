# Tables and macros for the manuscript and README. Every number in ms/main.tex comes from
# tabs/macros.tex; every table body is a generated fragment.
source("R/reporting.R")
a <- function(name) read.csv(file.path("data/analysis", name), stringsAsFactors = FALSE)
dir.create("tabs", showWarnings = FALSE)
fmt <- function(x, digits = 1L) ifelse(is.finite(x), formatC(x, format = "f", digits = digits, big.mark = ","), "--")
pct <- function(x) fmt(x, 1L)
interval <- function(lower, upper, digits = 1L) paste0("[", fmt(lower, digits), ", ", fmt(upper, digits), "]")
escape <- function(x) gsub("%", "\\\\%", gsub("&", "\\\\&", x))

write_table <- function(header, rows, alignment, path) {
  writeLines(c(
    paste0("\\begin{tabular}{", alignment, "}"), "\\toprule",
    paste0(paste(header, collapse = " & "), " \\\\"), "\\midrule",
    apply(rows, 1, function(row) paste0(paste(row, collapse = " & "), " \\\\")),
    "\\bottomrule", "\\end{tabular}"
  ), path)
}

flow <- read.csv("data/matching/sample_flow.csv", stringsAsFactors = FALSE)
write_table(c("Step", "Articles"), cbind(escape(flow$step), format_count(flow$articles)), "p{11cm}r", "tabs/sample_flow.tex")

e <- a("estimates.csv")
write_table(
  c("Specification", "Model", "Retracted / comparison", "Estimate", "95\\% interval"),
  cbind(escape(e$specification), e$model,
    paste(format_count(e$treated), format_count(e$comparison), sep = " / "),
    ifelse(e$model == "PPML", paste0(pct(e$percent), "\\%"), fmt(e$estimate, 2L)),
    ifelse(e$model == "PPML", paste0(interval(e$percent_lower, e$percent_upper), "\\%"),
      interval(e$lower, e$upper, 2L))),
  "p{5.2cm}lrrr", "tabs/estimates.tex"
)

es <- a("event_study.csv")
write_table(
  c("Years from retraction", "Change (\\%)", "95\\% interval"),
  cbind(es$event_time, ifelse(es$event_time == -1, "reference", pct(es$percent)),
    ifelse(es$event_time == -1, "--", paste0(interval(es$percent_lower, es$percent_upper), "\\%"))),
  "rrr", "tabs/event_study.tex"
)

h <- a("heterogeneity.csv")
labels <- c(reason_category = "Reason for retraction", field = "Field",
  pre_tercile = "Pre-retraction citations (tercile)", lag_bin = "Years to retraction",
  prior_concern = "Prior expression of concern")
group_labels <- c(misconduct = "Misconduct", duplication = "Plagiarism or duplication", error = "Error",
  other = "Other or unstated", bls = "Life sciences", hsc = "Health sciences", phy = "Physical sciences",
  bt_env = "Technology and environment", soc_hum = "Social sciences and humanities",
  `1` = "Lowest", `2` = "Middle", `3` = "Highest", `0` = "No", `2` = "2", `3` = "3", `4-5` = "4--5", `6+` = "6 or more")
h$group_label <- ifelse(h$variable == "pre_tercile", c(`1` = "Lowest", `2` = "Middle", `3` = "Highest")[h$group],
  ifelse(h$variable == "prior_concern", c(`0` = "No", `1` = "Yes")[h$group],
    ifelse(h$variable == "lag_bin", gsub("-", "--", h$group), group_labels[h$group])))
write_table(
  c("Group", "Retracted articles", "Change (\\%)", "95\\% interval", "Joint test $p$"),
  cbind(paste0(ifelse(!duplicated(h$variable), paste0("\\textit{", labels[h$variable], "}: "), ""), h$group_label),
    format_count(h$stacks), pct(h$percent), paste0(interval(h$percent_lower, h$percent_upper), "\\%"),
    ifelse(!duplicated(h$variable), fmt(h$joint_wald_p, 3L), "")),
  "p{6cm}rrrr", "tabs/heterogeneity.tex"
)

lo <- a("leave_one_out.csv")
jc <- a("journal_clustered.csv")
robust <- rbind(
  cbind(escape(lo$specification), format_count(lo$stacks), pct(lo$percent), paste0(interval(lo$percent_lower, lo$percent_upper), "\\%")),
  cbind("Journal-clustered standard errors", format_count(e$stacks[1]), pct(jc$percent), paste0(interval(jc$percent_lower, jc$percent_upper), "\\%"))
)
write_table(c("Specification", "Retracted articles", "Change (\\%)", "95\\% interval"), robust, "p{7cm}rrr", "tabs/robustness.tex")

legacy <- a("legacy_estimates.csv")
write_table(
  c("Sample", "Articles", "Additive change", "95\\% interval", "Proportional change (\\%)"),
  cbind(escape(legacy$specification), format_count(legacy$articles), fmt(legacy$estimate, 2L),
    interval(legacy$lower, legacy$upper, 2L), paste0(pct(legacy$percent), " ", interval(legacy$percent_lower, legacy$percent_upper), "\\%")),
  "p{6cm}rrrr", "tabs/legacy_comparison.tex"
)

ack <- a("acknowledgment.csv")
ack <- ack[match(c("before", "notice_year", "after"), ack$period), ]
write_table(
  c("Citing year", "Sampled", "Not located", "Coded", "No concern recorded", "Share (\\%)", "95\\% interval"),
  cbind(c("Before the retraction year", "Retraction year", "After the retraction year"),
    format_count(ack$sampled), format_count(ack$not_located), format_count(ack$coded),
    format_count(ack$unacknowledged), pct(ack$unacknowledged_percent), interval(ack$lower, ack$upper)),
  "p{4.2cm}rrrrrr", "tabs/acknowledgment.tex"
)

cmp <- jsonlite::read_json("data/validation/comparison_summary.json")
by_year <- cmp$by_year
write_table(
  c("Year", "Article-years", "Web of Science", "OpenAlex", "Ratio"),
  do.call(rbind, lapply(names(by_year), function(y) c(y, format_count(by_year[[y]]$n),
    format_count(by_year[[y]]$wos), format_count(by_year[[y]]$openalex), fmt(by_year[[y]]$ratio, 2L)))),
  "lrrrr", "tabs/source_comparison.tex"
)

ttr <- a("time_to_retraction.csv")
sdid <- if (file.exists("data/analysis/sdid_pooled.csv")) a("sdid_pooled.csv") else NULL
if (!is.null(sdid)) {
  co <- a("sdid_cohorts.csv")
  write_table(
    c("Retraction year", "Retracted / donors", "Synthetic DiD", "95\\% interval", "DiD"),
    cbind(co$cohort, paste(format_count(co$treated), format_count(co$comparison), sep = " / "),
      fmt(co$estimate, 2L), interval(co$lower, co$upper, 2L), fmt(co$did_estimate, 2L)),
    "lrrrr", "tabs/sdid.tex"
  )
}

main <- e[1, ]
paths <- a("citation_paths.csv")
rates <- a("retraction_rates.csv")
reasons <- a("reasons.csv")
pre <- a("pretrends.csv")
honest <- jsonlite::read_json("data/analysis/honest_status.json")
status <- jsonlite::read_json("data/analysis/status.json")
resolution <- jsonlite::read_json("data/openalex/target_resolution.json")
match_summary <- jsonlite::read_json("data/matching/match_summary.json")
frame_summary <- jsonlite::read_json("data/retraction_watch/frame_summary.json")
at <- function(treated, time, column) paths[[column]][paths$treated == treated & paths$event_time == time]
macros <- c(
  Retractions = format_count(jsonlite::read_json("data/retraction_watch/snapshot.json")$rows_by_nature$Retraction),
  Frame = format_count(frame_summary$frame),
  Resolved = format_count(resolution$frame_eligible),
  Matched = format_count(match_summary$matched),
  Unmatched = format_count(if (is.null(match_summary$exclusions$no_match_within_caliper)) 0L else match_summary$exclusions$no_match_within_caliper),
  Stacks = format_count(status$stacks),
  Comparison = format_count(status$comparison),
  Controls = format_count(match_summary$controls),
  MainPercent = pct(main$percent), MainLower = pct(main$percent_lower), MainUpper = pct(main$percent_upper),
  MainReduction = pct(-main$percent),
  FivePercent = pct(e$percent[e$specification == "Years +1 to +5"]),
  FiveLower = pct(e$percent_lower[e$specification == "Years +1 to +5"]),
  FiveUpper = pct(e$percent_upper[e$specification == "Years +1 to +5"]),
  AdditiveEstimate = fmt(e$estimate[e$model == "OLS"][1], 2L),
  AdditiveLower = fmt(e$lower[e$model == "OLS"][1], 2L), AdditiveUpper = fmt(e$upper[e$model == "OLS"][1], 2L),
  PretrendP = fmt(pre$p_value, 2L),
  EventMinusThree = pct(es$percent[es$event_time == -3]), EventMinusTwo = pct(es$percent[es$event_time == -2]),
  EventZero = pct(es$percent[es$event_time == 0]), EventOne = pct(es$percent[es$event_time == 1]),
  EventThree = pct(es$percent[es$event_time == 3]), EventFive = pct(es$percent[es$event_time == 5]),
  EventFiveLower = pct(es$percent_lower[es$event_time == 5]), EventFiveUpper = pct(es$percent_upper[es$event_time == 5]),
  RetractedMeanBefore = fmt(at(1, -1, "mean")), RetractedMeanAfter = fmt(at(1, 3, "mean")),
  ComparisonMeanBefore = fmt(at(0, -1, "mean")), ComparisonMeanAfter = fmt(at(0, 3, "mean")),
  RetractedMedianBefore = fmt(at(1, -1, "median"), 0L), RetractedMedianAfter = fmt(at(1, 3, "median"), 0L),
  RetractedZeroAfter = pct(100 * at(1, 3, "zero_share")), ComparisonZeroAfter = pct(100 * at(0, 3, "zero_share")),
  LegacyOpenAlexPercent = pct(legacy$percent[1]), LegacyWosPercent = pct(legacy$percent[2]),
  LegacyWosAdditive = fmt(legacy$estimate[2], 2L),
  LegacyWosArticles = format_count(legacy$articles[2]),
  AckAfterPercent = pct(ack$unacknowledged_percent[3]), AckAfterLower = pct(ack$lower[3]), AckAfterUpper = pct(ack$upper[3]),
  AckAfterCoded = format_count(ack$coded[3]), AckAfterUnack = format_count(ack$unacknowledged[3]),
  AckBeforeCoded = format_count(ack$coded[1]), AckBeforeUnack = format_count(ack$unacknowledged[1]),
  AckBeforePercent = pct(ack$unacknowledged_percent[1]),
  MeanLag = fmt(ttr$mean[ttr$group == "all"]), MedianLag = fmt(ttr$median[ttr$group == "all"], 0L),
  LagFourPlusPercent = pct(100 * ttr$share_four_or_more[ttr$group == "all"]),
  MisconductShare = pct(100 * reasons$Freq[reasons$reason == "misconduct"] / sum(reasons$Freq)),
  DuplicationShare = pct(100 * reasons$Freq[reasons$reason == "duplication"] / sum(reasons$Freq)),
  ErrorShare = pct(100 * reasons$Freq[reasons$reason == "error"] / sum(reasons$Freq)),
  WosOpenAlexRatio = fmt(cmp$ratio_openalex_to_wos, 2L), WosExactShare = pct(100 * cmp$share_exact),
  WosWithinOneShare = pct(100 * cmp$share_within_one), WosArticleYears = format_count(cmp$article_years),
  WosOverlap = format_count(cmp$date_agreement$overlap),
  WosNoticeYearAgreement = pct(100 * cmp$date_agreement$notice_year_equal / cmp$date_agreement$overlap),
  HonestAvailable = if (isTRUE(honest$available)) "1" else "0"
)
if (!is.null(sdid)) {
  macros <- c(macros,
    SdidEstimate = fmt(sdid$estimate[1], 2L), SdidLower = fmt(sdid$lower[1], 2L), SdidUpper = fmt(sdid$upper[1], 2L),
    SdidCohorts = format_count(sdid$cohorts[1]), SdidTreated = format_count(sdid$treated[1])
  )
}
if (isTRUE(honest$available)) {
  hd <- a("honest_did.csv")
  breakdown <- hd$Mbar[which(hd$lb > 0 | hd$ub < 0)]
  macros <- c(macros, HonestBreakdown = if (length(breakdown)) fmt(max(breakdown), 1L) else "none")
}
stopifnot(all(grepl("^[A-Za-z]+$", names(macros))), !anyDuplicated(names(macros)), !anyNA(macros))
writeLines(c("% Generated by scripts/tables.R; do not edit.",
  paste0("\\newcommand{\\", names(macros), "}{", macros, "}")), "tabs/macros.tex")
jsonlite::write_json(as.list(macros), "tabs/results.json", auto_unbox = TRUE, pretty = TRUE)

readme <- readLines("docs/README.in.md")
for (name in names(macros)) readme <- gsub(paste0("{{", name, "}}"), macros[[name]], readme, fixed = TRUE)
missing <- unique(regmatches(readme, gregexpr("\\{\\{[A-Za-z]+\\}\\}", readme))[[1]])
stopifnot(length(unlist(regmatches(readme, gregexpr("\\{\\{[A-Za-z]+\\}\\}", readme)))) == 0L)
writeLines(readme, "README.md")
cat("Tables, macros and README written\n")
