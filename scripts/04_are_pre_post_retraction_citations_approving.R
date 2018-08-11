"
Includes:
  1. Percent of Citations to Retracted Articles Post Retraction That Were approving
  2. Percent of Citations to Retracted Articles Pre Retraction That Were Approving

Pre-Post:

  1. For post: 275
  2. For pre: 100
"

# Global options
options(StringsAsFactors = FALSE)

# Set dir 
setwd(githubdir)
setwd("propagation_of_error/")

# Load lib.
library(tidyverse)

# 1. Post-retaction

# Get data
i_approve        <- read.csv("data/09_are_post_retration_citations_approving/new_post_retraction_approving_citations.csv")

# Total rows
nrow(i_approve)
# [1] 275

# Mean approving
mean(i_approve$approving, na.rm = T)
# [1] 91.41%

# If we discount any article present more than once in our dataset
i_approve_by_article <- i_approve %>%
  left_join(plyr::count(i_approve, "index"))

round(mean(i_approve_by_article[i_approve_by_article$freq < 2, "approving"], na.rm = T), 4)
# 95.32%

# Median time
median(i_approve$diff)
# 3

# Mean time
mean(i_approve$diff)
#  3.50

# Regress on time
approve_time <- with(i_approve, glm(approving ~ diff, family = "binomial"))

# Table 
library(stargazer)
stargazer(approve_time, 
      title = "Relationship Between Years After Retraction Notice and Whether or Not Citation is Approving",
      align = TRUE,
      digits = 1,
      dep.var.labels = c("Approving Citation"), 
      covariate.labels = c("Years After Notice"),
      no.space = TRUE,
      omit = "as.factor", 
      omit.stat = c("LL", "ser", "f"),
      label = "tab:tab3",
      out = "tabs/approving_time_tab.tex")

# Proportion approving per year
approve_prop_by_year <- i_approve %>%
  group_by(diff) %>%
  summarise(mean_approving = mean(approving, na.rm = T))

# Plot
with(i_approve, plot(jitter(diff), approving))
plot(approve_prop_by_year[approve_prop_by_year$diff < 7,], ylim = c(0, 1))

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
      axis.title         = element_text(size = 12, colour = "#333333"),
      axis.text          = element_text(size = 12, colour = "#333333"),
      axis.ticks.y       = element_blank(),
      axis.ticks.x       = element_blank(),
      strip.text.x       = element_text(size = 9),
      legend.text        = element_text(size = 8),
      plot.margin        = unit(c(.5, .5, .5, .5), "cm"))

ggplot(approve_prop_by_year, aes(diff, mean_approving)) + 
  geom_point() +
  theme_minimal() + 
  theme_base

## Pre-retraction
# Get data
i_approve_pre       <- read.csv("data/09_are_post_retration_citations_approving/new_pre_retraction_approving_citations.csv")

# Total rows
nrow(i_approve_pre)
# 100

# Mean approving
mean(i_approve_pre$approving, na.rm = T)
# [1] 97.70%

mean(i_approve_pre$approving[i_approve_pre$diff < 0], na.rm = T)
# 1

# Proportion approving per year
approve_prop_by_year <- i_approve_pre %>%
  group_by(diff) %>%
  summarise(mean_approving = mean(approving, na.rm = T))

# Plot
with(i_approve_pre, plot(jitter(diff), approving))
plot(approve_prop_by_year)

# Standard errors
se_prop <- function (p, n) sqrt(p*(1 - p)/n)

# Merge pre and post to show how rate of approving citations has changed
# Please note that n_citations is also changing. And that is basically the 
# main thing that is happening. Provide approx. adjusted n_approving_citation
# estimate also

fin_approve <- rbind(i_approve, i_approve_pre)

approve_prop_by_year <- fin_approve %>%
  group_by(diff) %>%
  summarise(mean_approving = mean(approving, na.rm = T))

ggplot(approve_prop_by_year, aes(diff, mean_approving)) + 
  geom_point() +
  ylim(0, 1) +
  geom_vline(xintercept = 0, col = "red", linetype = "dotted") + 
  theme_minimal() +
  xlab("Years from Retraction") +
  ylab("Proportion Approving") +
  theme_base

ggsave("figs/pre_post_prop_approving.pdf")
