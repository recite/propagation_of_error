"

Includes:

1. Reads in Nieuwenhuis citation data from excel files in data/nieuwenhuis/ 
   a. citations_to_articles_with_sig_fault.xlxs
   b. citations_to_articles_wo_sig_fault.xlxs

2. Fixes column names by reading in codebook from: 
   data/citation_codebook.csv

3. Merges it with the original Nieuwenhuis data: 
   data/nieuwenhuis/from_nieuwenhuis/nieuwenhuis_with_id.csv

4. Outputs a sample of citations post publication of Nieuwenhuis to:
      data/nieuwenhuis/post_nw_pub_citation_100.csv      

5. Analyses: 
   a. How many citations before and after Nieuwenhuis
   b. Skewness in cites
   c. Change in cites among articles with errors, serious errors controlling for time trend
   d. Diff. in difference. And 2 and 3 year out estimates.

6. What % of Citations post Nieuwenhuis are approving?
"

# Global options
options(StringsAsFactors = FALSE)

# Set dir 
setwd(githubdir)
setwd("propagation_of_error/")

# Load libs 
library(readxl)
library(dplyr)
library(stargazer)
library(ggplot2)
library(grid)
library(lme4)
library(equatiomatic)

# 1. Read in the data
# --------------------------------

# sig_fault 
sig_fault_path   <- "data/01_nieuwenhuis/citations_to_articles_with_sig_fault.xlsx"
sig_fault        <- lapply(excel_sheets(sig_fault_path), read_excel, path = sig_fault_path,  col_types = rep("text", 55))
sig_fault_id     <- mapply(`[<-`, sig_fault, 'id', value = excel_sheets(sig_fault_path), SIMPLIFY = FALSE) # Add id (sheet name = article id)
# lapply(sig_fault, names) --- names not consistent
sig_fault_df     <- plyr::ldply(sig_fault_id, data.frame)
sig_fault_df$sig_fault <- 1
# Deal with PT 
sig_fault_df$PT  <- ifelse(is.na(sig_fault_df$PT), sig_fault_df[,1], sig_fault_df$PT)
sig_fault_df     <- subset(sig_fault_df, select=-1) 

# no sig_fault
no_sig_fault_path <- "data/01_nieuwenhuis/citations_to_articles_wo_sig_fault.xlsx"
no_sig_fault      <- lapply(excel_sheets(no_sig_fault_path), read_excel, path = no_sig_fault_path,  col_types = rep("text", 55))
no_sig_fault_id   <- mapply(`[<-`, no_sig_fault, 'id', value = excel_sheets(no_sig_fault_path), SIMPLIFY = FALSE) # Add id (sheet name = article id)
# lapply(sig_fault, names) --- names not consistent
no_sig_fault_df   <- plyr::ldply(no_sig_fault_id, data.frame)
no_sig_fault_df$sig_fault <- 0

# Combine the two 
nieu_cite <- rbind(sig_fault_df, no_sig_fault_df)

# 2. col_names
# --------------------------

codebook             <- read.csv("data/citation_codebook.csv")
cite_new_names       <- codebook$r_label[match(names(nieu_cite), codebook$label)]
names(nieu_cite)     <- ifelse(!is.na(cite_new_names), cite_new_names, names(nieu_cite))

# Recode
nieu_cite$diff_time <- as.numeric(nieu_cite$pub_year) - 2011

# Since the earliest published piece Nieuwenhuis analyze is from 2009 (even accounting for some early pub., need to nuke the rest. 
# Looks reasonable. False positives not a huge issue it seems.)
nieu_cite <- subset(nieu_cite, pub_year > 2008) 

# 3. Merge with Nieuwenhuis dat 
# ---------------------------------
nieu           <- read.csv("data/01_nieuwenhuis/from_nieuwenhuis/nieuwenhuis_with_id.csv")
names(nieu)    <- paste0("orig_article_", tolower(names(nieu)))
nw_nieu_cite   <- merge(nieu_cite, nieu, by.x = "id", by.y = "orig_article_article_id", all.x = T, all.y = F)

# How many with and without sig_fault
with(nw_nieu_cite,table(orig_article_main_q, sig_fault ))

# 4. Output sample of 100 post publication, and only publications with errors
# ----------------------------------
# Set seed and get sample 
nw_niew_cite_post_pub <- subset(nw_nieu_cite, pub_year > 2011 & sig_fault == 1)
set.seed(31415)
nw_100 <- nw_niew_cite_post_pub[sample(1:nrow(nw_niew_cite_post_pub), 100), ]
write.csv(nw_100, file = "data/01_nieuwenhuis/post_nw_pub_citation_100.csv", row.names = F)

# 5. Analyses 
# --------------------

# Subset on journals in which Nieuwenhuis etc. think there is an error 
# Remove 2016 as we have incomplete data for the year 
nw_nieu_sig_fault <- subset(nw_nieu_cite, sig_fault == 1 & pub_year < 2016)

# Group by diff. year
cites_by_year <- 
   nw_nieu_sig_fault %>% 
   group_by(diff_time) %>% 
   summarise(n_cites = n()) 

# How many citations before and after Nieuwenhuis
colSums(cites_by_year[4:7, ])
colSums(cites_by_year)

# Ans: 6604 out of 8871 or 74.4%

# Skewness in cites 
cites_by_article <- 
   nw_nieu_sig_fault[ , c("id", "diff_time")] %>% 
   group_by(id) %>% 
   summarise(n_cites = n())

e1071::skewness(cites_by_article$n_cites)

# Subset on potentially serious sig. fault and group by diff_year
serious_cites_by_year <- 
 nw_nieu_sig_fault[grep("potentially serious", nw_nieu_sig_fault$orig_article_seriousness_of_mistake), ] %>% 
   group_by(diff_time) %>% 
   summarise(n_cites = n()) 

# Group by journal, year
cites_by_journal_year <- 
   nw_nieu_sig_fault[ , c("orig_article_journal", "diff_time")] %>% 
   group_by(orig_article_journal, diff_time) %>% 
   summarise(n_cites = n())  

# Group by article, year
cites_by_article_year <- 
   nw_nieu_sig_fault[ , c("id", "diff_time")] %>% 
   group_by(id, diff_time) %>% 
   summarise(n_cites = n())  

# Median cites per year 
median_cites_by_year <- 
   cites_by_article_year %>%
   group_by(diff_time) %>%
   summarize(median_cites = median(n_cites))

# Limit to articles with potentially serious sig. fault 
serious_cites_by_article_year <- 
   nw_nieu_sig_fault[grep("potentially serious", nw_nieu_sig_fault$orig_article_seriousness_of_mistake), c("id", "diff_time")] %>% 
   group_by(id, diff_time) %>% 
   summarise(n_cites = n())   

# OLS with time trends and article f.e.
change         <- with(cites_by_article_year, lmer(n_cites ~ I(diff_time > 0) + diff_time + id + (1| id)))
serious_change <- with(serious_cites_by_article_year, lmer(n_cites ~ I(diff_time > 0) + diff_time + id + (1| id)))

cites_by_article_year$treat <- I(cites_by_article_year$diff_time > 0)
change_r <- with(cites_by_article_year, lmer(n_cites ~ treat + diff_time + (1| id)))

extract_eq(change_r)

# Table
stargazer(change, serious_change, 
      title = "Change in the Number of Citations to Articles Containing the Error Per Year Before and After Publication of Nieuwenhuis", align = TRUE,
      dep.var.labels = c("Citations Per Year"), 
      column.labels = c("All Articles with Mistakes", "Articles with Potentially Serious Errors"),
      covariate.labels = c("Transition Date", "Time"),
      no.space = TRUE,
      digits = 1,
      omit = "id", 
      omit.stat = c("LL", "ser", "f"),
      label = "tab:si_tab1",
      out = "tabs/nw_tab.tex")

# Plot
# Base Theme 
theme_base <- theme_minimal() +
      theme(panel.grid.major  = element_line(color = "#e7e7e7",  linetype = "dotted", size = .25),
      panel.grid.minor  =  element_blank(),
      legend.position   = "none",
      axis.title   = element_text(size = 10, color = "#555555"),
      axis.text    = element_text(size = 8, color = "#555555"),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(vjust = -1),
      axis.title.y = element_text(vjust = 1),
      axis.ticks.x = element_line(color = "#e7e7e7",  linetype = "dotted", size = .2),
      plot.margin = unit(c(0, 1, .5, .5), "cm"))

# Total cites per year  
# ------------------------------
ggplot(cites_by_year, aes(y = n_cites, x = diff_time)) + 
      geom_point(alpha = .7, pch = 16, size = 3, col = "#777777") + 
      scale_y_continuous("Total Citations per Year", breaks = seq(0, 2000, 200)) + 
      scale_x_continuous("Years from Publication of Nieuwenhuis", breaks = seq(min(cites_by_year$diff_time) - 1, max(cites_by_year$diff_time), 1)) + 
      #geom_smooth(method = "loess", size = .2, col = "#2b8cbe", se = F) + 
      geom_vline(xintercept = 0, col = "red", linetype = "dotted") + 
      theme_base
ggsave("figs/nw_fig.pdf")

# Total cites per article per year 
# Smoothed growth curves + means per year +  smoothed 
# ---------------------------------------------------
ggplot(cites_by_article_year, aes(y = n_cites, x = diff_time, group = id)) + 
      geom_point(alpha = .3, pch = 20, size = 1, col = "#aaaaaa") + 
      scale_y_continuous("Citations per Year", breaks = seq(0, 200, 10), expand=c(0,0)) + 
      scale_x_continuous("Years from Publication of Nieuwenhuis", breaks = seq(min(cites_by_year$diff_time) - 1, max(cites_by_year$diff_time), 1)) + 
      geom_smooth(method = "loess", span = 1, linetype = "solid", size = .4, alpha = .1, col = "#eeeeee", se = F) + 
      stat_smooth(method = "loess", aes(group = 1), size = .8, col="#555555", se = F) +
      stat_summary(aes(group = 1),  geom = "point", fun.y = mean,  pch = 16, size = 2, col = "#555555") + 
      geom_vline(xintercept = 0, col = "red", linetype = "dotted") + 
      theme_base 
ggsave("figs/nw_growth_curve.pdf")

# Total cites per article per year 
# Smoothed growth curves + medians per year, which are connected (not smoothed)
# -----------------------------------------------------------------------------
ggplot(cites_by_article_year, aes(y = n_cites, x = diff_time, group = id)) + 
      geom_point(alpha = .3, pch = 20, size = 1, col="#aaaaaa") + 
      scale_y_continuous("Citations per Year", breaks = seq(0, 200, 10), expand = c(0, 0)) + 
      scale_x_continuous("Years from Publication of Nieuwenhuis",
                         breaks = seq(min(cites_by_year$diff_time) - 1, max(cites_by_year$diff_time), 1)) + 
      geom_smooth(method = "loess", span = 1, linetype = "solid", size = .4, alpha = .1, col = "#eeeeee", se = F) + 
      stat_summary(aes(group = 1),  geom = "point", fun.y = median,  pch = 16, size = 2, col = "#555555") +
      stat_summary(aes(group = 1),  geom = "line",  fun.y = median,  size=.8, col="#555555") +
      geom_vline(xintercept = 0, col = "red", linetype = "dotted") + 
      theme_base 
ggsave("figs/nw_median_growth_curve.pdf")

# Total cites per article per year for articles that have 'potentially serious' error
# Smoothed growth curves + means per year +  smoothed 
# --------------------------------------------------------------
ggplot(serious_cites_by_article_year, aes(y = n_cites, x = diff_time, group = id)) + 
      geom_point(alpha = .3, pch = 20, size = 1, col = "#aaaaaa") + 
      scale_y_continuous("Citations per Year", breaks = seq(0, 200, 10), expand = c(0,0)) + 
      scale_x_continuous("Years from Publication of Nieuwenhuis", breaks= seq(min(cites_by_year$diff_time) - 1, max(cites_by_year$diff_time), 1)) +
      geom_smooth(method = "loess", span = 1, linetype ="solid", size = .4, alpha = .1, col="#eeeeee", se = F) + 
      stat_smooth(method = "loess", aes(group = 1), size = .8, col = "#555555", se = F) +
      stat_summary(aes(group = 1),  geom = "point", fun.y = mean,  pch = 16, size = 2, col = "#555555") + 
      geom_vline(xintercept = 0, col = "red", linetype = "dotted") +
      theme_base
ggsave("figs/serious_nw_growth_curve.pdf")

# Difference in Differences
# --------------------------------

nw_nieu_all <- subset(nw_nieu_cite, pub_year < 2016)

# Cites by year
cites_by_article_year <- nw_nieu_all[, c("id", "diff_time", "sig_fault")] %>% 
   group_by(id, diff_time) %>% 
   summarise(n_cites = n(), sig_fault = unique(sig_fault))

# Limit to articles with potentially serious sig. fault 
nw_nieu_all$serious_error <- I(grepl("potentially serious", nw_nieu_all$orig_article_seriousness_of_mistake)*1)

serious_cites_by_article_year <- 
   nw_nieu_all[, c("id", "diff_time", "serious_error")] %>% 
   group_by(id, diff_time) %>% 
   summarise(n_cites = n(), sig_fault = unique(serious_error))   

# 1, 2, and 3 year out estimates
# Models (keep var name of sig_fault same for stargazer)
cites_by_article_year$nw_years <- I(cites_by_article_year$diff_time > 0)
serious_cites_by_article_year$nw_years <- I(serious_cites_by_article_year$diff_time > 0)

did_fit          <- with(cites_by_article_year, lmer(n_cites ~ nw_years + sig_fault + nw_years*sig_fault + (1|id)))
serious_did_fit  <- with(serious_cites_by_article_year, lmer(n_cites ~ nw_years + sig_fault + nw_years*sig_fault + (1|id)))

# Plausibly switch to sandwich estimator
library(plm)
library(lmtest)
library(multiwayvcov)

plm_did_fit <- plm(n_cites ~ 
               nw_years + 
               sig_fault + 
               nw_years*sig_fault, 
               model = "pooling",
               data = cites_by_article_year,
               index = c("id"))

# Bottom line assessment for interaction term = 2.4

# Another possibility (but seems off in how clustering is handled)
library(miceadds)
mice_did_fit <- summary(with(cites_by_article_year, lm.cluster(data = cites_by_article_year, formula = n_cites ~ nw_years + sig_fault + nw_years*sig_fault + as.factor(id), cluster = "id")))

# Bottom line assessment for interaction term ~ 3

# 2 year out
cites_by_article_year$nw_years <- I(cites_by_article_year$diff_time > 1)
serious_cites_by_article_year$nw_years <- I(serious_cites_by_article_year$diff_time > 1)

did_fit_2          <- with(cites_by_article_year, lmer(n_cites ~ nw_years + sig_fault + nw_years*sig_fault + (1|id)))
serious_did_fit_2  <- with(serious_cites_by_article_year, lmer(n_cites ~ nw_years + sig_fault + nw_years*sig_fault + (1|id)))

# 3 year out estimates
cites_by_article_year$nw_years <- I(cites_by_article_year$diff_time > 2)
serious_cites_by_article_year$nw_years <- I(serious_cites_by_article_year$diff_time > 2)

did_fit_3          <- with(cites_by_article_year, lmer(n_cites ~ nw_years + sig_fault + nw_years*sig_fault + (1|id)))
serious_did_fit_3  <- with(serious_cites_by_article_year, lmer(n_cites ~ nw_years + sig_fault + nw_years*sig_fault + (1|id)))

stargazer(did_fit, serious_did_fit, did_fit_2, serious_did_fit_2, did_fit_3, serious_did_fit_3,
      title = "Difference-in-Difference Analysis of the Impact of Publication of Nieuwenhuis on the Number of Times per Year Articles Containing the Error Are Cited Vis-a-Vis Articles that Didn't Contain the Error", 
      align = TRUE,
      digits = 1,
      font.size = "small",
      float.env = "sidewaystable",
      dep.var.labels = "Citations per year",
      column.labels = c("1 year out", "2 years out", "3 years out"), 
      column.separate = c(2, 2, 2), 
      covariate.labels = c("Treatment Date", "Error or Not", "Makes Error*Treatment Date"),
      no.space = TRUE, 
      omit = "as.factor", 
      omit.stat = c("LL", "ser", "f"),
      label = "tab:tab1",
      notes = c("Models (1), (3), and (5) define error as any article making the error.",
                "And Models (2), (4), and (6) refer to error as articles making ``potentially serious errors.''"),
      notes.align = "l",
      out = "tabs/nw_did_tab.tex")

# 6. What % of citations post publication of Nieuwenhuis were approving?
nw_top100 <- read.csv("data/02_are_nw_citations_approving/post_nw_pub_citation_100_approving.csv")
sum(nw_top100$approving == "yes")/sum(nw_top100$approving != "")
