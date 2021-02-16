"
  1. Are the dates trustworthy?
  2. Get data ready
  3. n_cites pre and post
  4. Skewness in cites
  5. Correlation between pre- and post- citations
  6. Impact of Retraction on Citations --- graph and lme4

"

# Global options
options(StringsAsFactors = FALSE)

# Set dir 
setwd(githubdir)
setwd("propagation_of_error/")

# Load lib.
library(tidyverse)
library(equatiomatic)

# 1. Are dates trustworthy?
# ---------------------------
bad_dates <- read_csv("data/10_bad_dates/check_for_bad_dates.csv")
names(bad_dates) <- tolower(names(bad_dates))

# RE: retracted article
# PY: citation to retracted
# NO: no_py

# Average Diff.
with(bad_dates, mean(re_py - re_py_good, na.rm = T))
with(bad_dates, mean(no_py - no_py_good, na.rm = T))
with(bad_dates, mean(py - py_good, na.rm = T))

# Given on avg. diff. are +ve, it seems like it is because of online pub.

# Create pre-post based on good data
bad_dates$pre_post_good <- (bad_dates$py_good - bad_dates$no_py_good)  > 0

# Create pre-post based on orig. data
bad_dates$pre_post <- (bad_dates$py - bad_dates$no_py)  > 0

# See how often pre is coded as post and vice-versa
table(bad_dates$pre_post_good, bad_dates$pre_post)

# 1. Get Data Ready
# ---------------------------

# Read in data 
retracted  <- read_csv("data/07_citations_to_retracted_articles/new_retracted_article_citations.zip")
notices    <- read.csv("data/03_retraction_notices/new_retraction_notices.csv")
codebook   <- read.csv("data/citation_codebook.csv")

# n_retracted that were ever cited
length(unique(retracted$index))
# [1] 2520

# The total number of articles that are eventually retracted 
ret_art  <- read.csv("data/05_retracted_articles/new_retracted_articles.csv")
nrow(ret_art)
# 3052

# Left_join notices to retracted so we have all the retracted articles
# Includes 192 that weren't cited
re_no <- left_join(notices, retracted, by = "index",  suffix = c("_no", "_re"))

# Rename cols.
new_names_re <- codebook$r_label[match(names(re_no), paste0(codebook$label, "_re"))]
new_names_no <- codebook$r_label[match(names(re_no), paste0(codebook$label, "_no"))]
names(re_no) <- ifelse(!is.na(new_names_re), paste0(new_names_re, "_re"), names(re_no))
names(re_no) <- ifelse(!is.na(new_names_no), paste0(new_names_no, "_no"), names(re_no))

# Let's subset on the columns we will be using
re_no <- re_no[, c("index", "pub_year_re", "pub_year_no")]

# summary(re_no$pub_year_re)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1974    2006    2010    2009    2013    2017     518

# Get index and pub_date of all the retracted articles
ret_art <- subset(ret_art, select = c("index", "PY"))
names(ret_art) <- c("index", "ret_art_pub_year")

# Add the year the retracted article was published to the data frame
# this allows us to add 0s for all years where there were no citations
# that are after the publication year and before 2016 
re_no <- re_no %>%
  left_join(ret_art)

# Sanity checks
sum(re_no$ret_art_pub_year > re_no$pub_year_re, na.rm = T)
sum(re_no$ret_art_pub_year > re_no$pub_year_no)
re_no <- subset(re_no, is.na(pub_year_re) | (!is.na(pub_year_re) & (ret_art_pub_year <= pub_year_re)))

# Total cites by year
cites_by_article_year <- re_no %>%
  group_by(index, pub_year_re) %>%
  summarise(n_cites = n(),
            pub_year_no = unique(pub_year_no),
            ret_art_pub_year = unique(ret_art_pub_year))

# Get complete set of years for each index after the retracted article is published
all_years <- data.frame(index = NA, pub_year_re = NA, notice_year = NA)

j <- 0

for (i in unique(re_no$index)) {

   pub_year    <- unique(re_no[re_no$index == i, "ret_art_pub_year"])
   n_entries   <- 2016 - pub_year
   all_years[(j + 1):(j + n_entries + 1), ] <- cbind(i, pub_year:(pub_year + n_entries), unique(re_no[re_no$index == i, "pub_year_no"]))
   j <- j + n_entries + 1
}

all_years$key <- paste0(all_years$index, ";", all_years$pub_year_re)
cites_by_article_year$key <- paste0(cites_by_article_year$index, ";", cites_by_article_year$pub_year_re)

# Left join incomplete cites to all years
cites_by_article_all_years <- all_years %>%
    left_join(cites_by_article_year,
              by = c("index" = "index",
                     "pub_year_re" = "pub_year_re"))

# Time from publication of retraction notice 
cites_by_article_all_years$diff <- as.numeric(cites_by_article_all_years$pub_year_re) - as.numeric(cites_by_article_all_years$notice_year)

# Pad 0s
cites_by_article_all_years$n_cites <- ifelse(is.na(cites_by_article_all_years$n_cites), 0, cites_by_article_all_years$n_cites)

# Subset to all the years for which we have complete data
cites_by_article_all_years_2016 <- subset(cites_by_article_all_years, pub_year_re < 2016 & ret_art_pub_year < 2016 & notice_year < 2016)

## Write this out
write.csv(cites_by_article_all_years_2016, file = "data/cites_by_article_all_years_2016.csv")

# 2. Out of the box numbers: Total cites before/after notification
# ------------------------------------------
sum(cites_by_article_all_years_2016[cites_by_article_all_years_2016$diff < 0, "n_cites"])
# [1] 35684
sum(cites_by_article_all_years_2016[cites_by_article_all_years_2016$diff > 0, "n_cites"])
# [1] 20636

sum(cites_by_article_all_years[cites_by_article_all_years$diff < 0, "n_cites"])
# [1] 39792

sum(cites_by_article_all_years[cites_by_article_all_years$diff > 0, "n_cites"])
# [1] 22932

# Min.  
sum(cites_by_article_all_years[, "n_cites"])
# [1] 73564

# Number of cites year after retraction notice published
22932/73564

# 3. Skewness in cites 
# ---------------------------
e1071::skewness(cites_by_article_all_years_2016$n_cites)

#  5.657

# 4. Correlation b/w n_pre_cites and n_pst_cites (perhaps do n_pre_cites/year)
# -----------------------------------------------

# Take out diff = 0
cites_by_article_all_years_2016_wo_diff0 <- subset(cites_by_article_all_years_2016, diff != 0)
cites_by_article_all_years_2016_wo_diff0$transition <- cites_by_article_all_years_2016_wo_diff0$diff > 0

pre_pst <- cites_by_article_all_years_2016_wo_diff0 %>%
  group_by(index) %>%
  summarize(pre_cites = sum(!transition), pst_cites = sum(transition))

cor(pre_pst$pre_cites, pre_pst$pst_cites)
# [1]  -0.1169728

with(pre_pst, scatter.smooth(log(pre_cites + 1), log(pst_cites + 1)))
cor(log(pre_pst$pre_cites + 1), log(pre_pst$pst_cites + 1))

# [1]  -0.165279

# 5. Impact of publication
# ----------------------------------------------------

# Plot n_cites_by_year by article growth curve

# Median cites per year 
median_cites_by_year <- cites_by_article_all_years_2016 %>%
   group_by(diff) %>%
   summarize(median_cites = median(n_cites))

# Mean of the median
mean(median_cites_by_year[median_cites_by_year$diff > 0, ]$median_cites, na.rm = T)
#  1.565217
mean(median_cites_by_year[median_cites_by_year$diff <= 0, ]$median_cites, na.rm = T)
#  2.928571
# 
median_cites_by_year[median_cites_by_year$diff == 0, ]$median_cites
median_cites_by_year[median_cites_by_year$diff == 0, ]$median_cites


# Total cites per article per year 
# Smoothed growth curves + medians per year 

library(ggplot2)
library(grid)

# Base
theme_base <- theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "#f7f7f7", linetype = "solid", size = .5),
      panel.border       = element_blank(),
      legend.position    = "bottom",
      legend.background  = element_rect(color = "#ffffff"),
      legend.key         = element_rect(color = "#ffffff", fill = "#ffffff"),
      legend.key.size    = unit(.1,"cm"),
      title              = element_text(size = 8, colour = "#333333"),
      axis.title         = element_text(size = 8, colour = "#333333"),
      axis.text          = element_text(size = 8, colour = "#333333"),
      axis.ticks.y       = element_blank(),
      axis.ticks.x       = element_blank(),
      strip.text.x       = element_text(size = 9),
      legend.text        = element_text(size = 8),
      plot.margin        = unit(c(.5, .5, .5, .5), "cm"))

ggplot(subset(cites_by_article_all_years_2016, diff > -11 & diff < 11), aes(y = n_cites, x = diff, group = index)) + 
      geom_point(alpha = .01, pch = 20, size = 1, col="#aaaaaa") + 
      scale_y_continuous("Citations per Year", breaks = seq(0, 150, 5), limits = c(-.5, 130), expand = c(0, 0)) + 
      scale_x_continuous("Years from Publication of Retraction",
                        breaks = seq(min(cites_by_article_all_years_2016$diff) - 1, max(cites_by_article_all_years_2016$diff), 1)) + 
      geom_line(size = .3, alpha = .3, col = "#dddddd") + 
      #geom_smooth(method = "loess", span = 1, linetype = "solid", size = .3, alpha = .1, col = "#ededed", se = F) + 
      stat_summary(aes(group = 1),  geom = "point", fun.y = median,  pch = 16, size = 1.5, col = "#777777") + 
      stat_summary(aes(group = 1),  geom = "line",  fun.y = median,  size = .8, col="#777777") + 
      geom_vline(xintercept = 0, col = "red", linetype = "dotted") + 
      theme_minimal() + 
      theme_base

ggsave("figs/retracted_growth_curve.pdf")

# Formal
#--------------

## Formal 1. Sandwich Estimator + f.e. for article and without it
## 

# Loading the required libraries
library(plm)
library(lmtest)
library(multiwayvcov)
library(sandwich)

# Subset on articles with at least 1 year of full post retraction notice data
cites_by_article_all_years_1 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 1)
cites_by_article_all_years_1$transition <- I(cites_by_article_all_years_1$diff > 0)

change_1_lm_fe   <- lm(n_cites ~ transition*diff + as.factor(index), data = cites_by_article_all_years_1)
change_1_lm_fe_c <- coeftest(change_1_lm_fe, vcov = vcovCL, cluster = ~ index)

change_1_lm_no_fe   <- lm(n_cites ~ transition*diff, data = cites_by_article_all_years_1)
change_1_lm_no_fe_c <- coeftest(change_1_lm_no_fe, vcov = vcovCL, cluster = ~ index)

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_2 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 2)
cites_by_article_all_years_2$transition <- I(cites_by_article_all_years_2$diff > 1)

change_2_lm_fe   <- lm(n_cites ~ transition*diff + as.factor(index), data = cites_by_article_all_years_2)
change_2_lm_fe_c <- coeftest(change_2_lm_fe, vcov = vcovCL, cluster = ~ index)

change_2_lm_no_fe   <- lm(n_cites ~ transition*diff, data = cites_by_article_all_years_2)
change_2_lm_no_fe_c <- coeftest(change_2_lm_no_fe, vcov = vcovCL, cluster = ~ index)

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_3 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 3)
cites_by_article_all_years_3$transition <- I(cites_by_article_all_years_3$diff > 2)

change_3_lm_fe   <- lm(n_cites ~ transition*diff + as.factor(index), data = cites_by_article_all_years_3)
change_3_lm_fe_c <- coeftest(change_3_lm_fe, vcov = vcovCL, cluster = ~ index)

change_3_lm_no_fe   <- lm(n_cites ~ transition*diff, data = cites_by_article_all_years_3)
change_3_lm_no_fe_c <- coeftest(change_3_lm_no_fe, vcov = vcovCL, cluster = ~ index)

# Table 
library(stargazer)
stargazer(change_1_lm_fe_c, change_1_lm_no_fe_c, change_2_lm_fe_c, change_2_lm_no_fe_c, change_3_lm_fe_c, change_3_lm_no_fe_c,
      title = "Impact of Publication of Retraction Notice on the Number of Times Retracted Articles Are Cited per Year With Article Fixed Effects and Clustered Standard Errors by Article",
      align = TRUE,
      digits = 1,
      font.size = "small",
      float.env = "sidewaystable",
      dep.var.labels = c("Citations Per Year"), 
      column.separate = c(2, 2, 2), 
      column.labels = c("1 Year Later", "2 Years Later", "3 Years Later"),
      covariate.labels = c("(1, 2, 3) Years After Notice", "Years to Notice", "(1, 2, 3) Years After Notice*Years to Notice"),
      no.space = TRUE, 
      omit = "as.factor", 
      #ci = T,
      ci.level = .95,
      omit.stat = c("LL", "ser", "f"),
      label = "tab:lm_fe_clustered",
      notes = c("Models (1), (3), and (5) have fixed effects for each article, which are suppressed.",
                "And Models (2), (4), and (6) do not have fixed effects."),
      notes.align = "l",
      out = "tabs/retracted_tab_lm_fe_clustered.tex")


## Formal 2. Fit with hierarchical model with the f.e. and without it
## Main Text Specification w/ fe

library(lme4)

# Subset on articles with at least 1 year of full post retraction notice data
cites_by_article_all_years_1 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 1)
cites_by_article_all_years_1$transition <- I(cites_by_article_all_years_1$diff > 0)

change_1_lmer_fe      <- lmer(n_cites ~ transition*diff + as.factor(index) + (1 |index), data = cites_by_article_all_years_1)
change_1_lmer_no_fe   <- lmer(n_cites ~ transition*diff + (1 |index), data = cites_by_article_all_years_1)

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_2 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 2)
cites_by_article_all_years_2$transition <- I(cites_by_article_all_years_2$diff > 1)

change_2_lmer_fe      <- lmer(n_cites ~ transition*diff + as.factor(index) + (1 |index), data = cites_by_article_all_years_2)
change_2_lmer_no_fe   <- lmer(n_cites ~ transition*diff + (1 |index), data = cites_by_article_all_years_2)

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_3 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 3)
cites_by_article_all_years_3$transition <- I(cites_by_article_all_years_3$diff > 2)

change_3_lmer_fe      <- lmer(n_cites ~ transition*diff + as.factor(index) + (1 |index), data = cites_by_article_all_years_3)
change_3_lmer_no_fe   <- lmer(n_cites ~ transition*diff + (1 |index), data = cites_by_article_all_years_3)

# Table 
stargazer(change_1_lmer_fe, change_2_lmer_fe, change_3_lmer_fe, 
      title = "Impact of Publication of Retraction Notice on the Number of Times Retracted Articles Are Cited per Year",
      align = TRUE,
      digits = 1,
      dep.var.labels = c("Citations Per Year"), 
      column.labels = c("1 Year Later", "2 Years Later", "3 Years Later"),
      covariate.labels = c("(1, 2, 3) Years After Notice", "Years to Notice", "(1, 2, 3) Years After Notice*Years to Notice"),
      no.space = TRUE, 
      omit = "as.factor", 
      omit.stat = c("LL", "ser", "f"),
      label = "tab:tab2",
      out = "tabs/retracted_tab.tex")

## Formal 3. Changing Estimand: Quantiles Using Quantile Regression
##

library(quantreg)
taus <- c(.100, .250, .500, .750, .900)
fit1 <- with(cites_by_article_all_years_1, rq(n_cites ~ I(diff > 0)*diff, tau = taus))
fit2 <- with(cites_by_article_all_years_2, rq(n_cites ~ I(diff > 1)*diff, tau = taus))
fit3 <- with(cites_by_article_all_years_3, rq(n_cites ~ I(diff > 2)*diff, tau = taus))
round(coef(fit1), 3)


## Formal 4. Robustness with non-linear time trend pre and post 
##

cites_by_article_all_years_1 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 1)
cites_by_article_all_years_1$transition <- I(cites_by_article_all_years_1$diff > 0)

change_r         <- with(cites_by_article_all_years_1, lmer(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + as.factor(index) + (1 |index)))

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_2 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 2)
cites_by_article_all_years_2$transition <- I(cites_by_article_all_years_2$diff > 1)

change_2_r       <- with(cites_by_article_all_years_2, lmer(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + as.factor(index) + (1 |index)))

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_3 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 3)

cites_by_article_all_years_3$transition <- I(cites_by_article_all_years_3$diff > 2)
change_3_r       <- with(cites_by_article_all_years_3, lmer(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + as.factor(index) + (1 |index)))

stargazer(change_r, change_2_r, change_3_r, 
      title = "Impact of Publication of Retraction Notice on the Number of Times Retracted Articles Are Cited per Year With Non-Linear Time Trends",
      align = TRUE,
      digits = 1,
      dep.var.labels = c("Citations Per Year"), 
      column.labels = c("1 Year Later", "2 Years Later", "3 Years Later"),
      covariate.labels = c("(1, 2, 3) Years After Notice",
                           "Years to Notice",
                           "(1, 2, 3) Years After Notice*Years to Notice",
                           "Years to Notice Squared",
                           "Years to Notice Cubed"),
      no.space = TRUE, 
      omit = "as.factor", 
      omit.stat = c("LL", "ser", "f"),
      label = "tab:non_linear",
      out = "tabs/retracted_tab_non_linear_time_trends.tex")

## Formal 5. Robustness with non-linear time trend pre and post and Poisson

## Notes: 
## With glmer, it gives convergence warning 
## rstanarm also but it is amazingly slow

cites_by_article_all_years_1 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 1)
cites_by_article_all_years_1$transition <- I(cites_by_article_all_years_1$diff > 0)
change_p         <- with(cites_by_article_all_years_1, glmer(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + (1 |index), family = poisson(link = "log"), nAGQ = 100))
 
# Doing the sandwich estimator
change_p <- glm(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + as.factor(index), data = cites_by_article_all_years_1, family = poisson)
change_p <- coeftest(change_p, vcov = vcovCL, cluster = ~ index)

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_2 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 2)
cites_by_article_all_years_2$transition <- I(cites_by_article_all_years_2$diff > 1)

change_2_p <- glm(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + as.factor(index), data = cites_by_article_all_years_2, family = poisson)
change_2_p <- coeftest(change_2_p, vcov = vcovCL, cluster = ~ index)

# Subset on articles with at least 2 years of full post retraction notice data
cites_by_article_all_years_3 <- subset(cites_by_article_all_years_2016, 2016 - notice_year >= 3)
cites_by_article_all_years_3$transition <- I(cites_by_article_all_years_3$diff > 2)

change_3_p <- glm(n_cites ~ transition*diff + I(diff^2) + I(diff^3) + as.factor(index), data = cites_by_article_all_years_3, family = poisson)
change_3_p <- coeftest(change_3_p, vcov = vcovCL, cluster = ~ index)

stargazer(change_p, change_2_p, change_3_p, 
      title = "Impact of Publication of Retraction Notice on the Number of Times Retracted Articles Are Cited per Year With Non-Linear Time Trends And Modeled as a Poisson",
      align = TRUE,
      digits = 1,
      dep.var.labels = c("Citations Per Year"), 
      column.labels = c("1 Year Later", "2 Years Later", "3 Years Later"),
      covariate.labels = c("(1, 2, 3) Years After Notice",
                           "Years to Notice",
                           "(1, 2, 3) Years After Notice*Years to Notice",
                           "Years to Notice Squared",
                           "Years to Notice Cubed"),
      no.space = TRUE, 
      omit = "as.factor", 
      omit.stat = c("LL", "ser", "f"),
      label = "tab:poisson",
      out = "tabs/retracted_tab_non_linear_time_trends_poisson.tex")
