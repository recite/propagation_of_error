"

1. Plot time to retraction
2. Plot cor (time to retraction, journal impact factor)

"

# Global options
options(StringsAsFactors = FALSE)

# Set dir 
setwd(githubdir)
setwd("propagation_of_error/")

# Read in data 
retracted  <- read.csv("data/05_retracted_articles/new_retracted_articles.csv")
notices    <- read.csv("data/03_retraction_notices/new_retraction_notices.csv")
codebook   <- read.csv("data/citation_codebook.csv")

# Merge files
re_no <- merge(retracted, notices, by = "index",  suffixes = c("_re","_no"))

# Rename cols.
# cbind(names(re_no), codebook$r_label[match(names(re_no), paste0(codebook$label, "_re"))])

new_names_re <- codebook$r_label[match(names(re_no), paste0(codebook$label, "_re"))]
new_names_no <- codebook$r_label[match(names(re_no), paste0(codebook$label, "_no"))]
names(re_no) <- ifelse(!is.na(new_names_re), paste0(new_names_re, "_re"), names(re_no))
names(re_no) <- ifelse(!is.na(new_names_no), paste0(new_names_no, "_no"), names(re_no))

# Time to retraction
re_no$ttr <- re_no$pub_year_no - re_no$year

# Average/median/range time to retraction 
mean(re_no$ttr)
median(re_no$ttr)
range(re_no$ttr)
quantile(re_no$ttr)

# 1. Plot time to retraction
# -----------------------------
library(ggplot2)
library(grid)

# Base
theme_base <- theme_minimal() +
    theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
    panel.border       = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    legend.background  = element_rect(color = "#ffffff"),
    legend.key         = element_rect(color = "#ffffff", fill = "#ffffff"),
    legend.key.size    = unit(.1, "cm"),
    legend.spacing     = unit(.2, "cm"),
    title              = element_text(size = 8, colour = "#333333"),
    axis.title         = element_text(size = 8, colour = "#333333"),
    axis.text          = element_text(size = 8, colour = "#333333"),
    axis.ticks.y       = element_blank(),
    axis.ticks.x       = element_blank(),
    strip.text.x       = element_text(size = 9),
    plot.margin        = unit(c(0, .5, .5, .5), "cm"))

# Plot 
re_no$constant <- 1

ggplot(re_no, aes(y = pub_year_no - year, x = constant)) +
    geom_boxplot(col = "#2b8cbe", fatten = .75) +
    xlab("") +
    geom_segment(aes(x = .7, 
                   y = median(re_no$pub_year_no - re_no$year),
                   xend = .6, yend = median(re_no$pub_year_no - re_no$year)),
                   size = .25,
                   col="#2b8cbe") +
    annotate("text", size = 3, x = .59, y = median(re_no$pub_year_no - re_no$year) - .5,
           label = paste("Median: ", round(median(re_no$pub_year_no - re_no$year), 2))) +
    ylab("Number of years since publication") +
    coord_flip() +
    theme_base +
    theme(axis.text.y = element_blank())


ggplot(re_no, aes(pub_year_no - year)) +
    geom_density() +
    geom_vline(xintercept = median(re_no$pub_year_no - re_no$year),
               col="#2b8cbe") +
    annotate("text", size = 3, y = .3, x = median(re_no$pub_year_no - re_no$year) + 2,
             label = paste("Median: ", round(median(re_no$pub_year_no - re_no$year), 2))) +
    scale_x_continuous("Years from Publication", breaks = round(seq(0, max(re_no$pub_year_no - re_no$year), 1), 0)) + 
    theme_base

ggsave("figs/time_to_retraction.pdf")

# 
# Correlation between journal impact factor and time to retraction

# Read JIF data
jif <- read.csv("data/journal_impact_factor.csv")

# Average by journal 
library(dplyr)

ttr_by_journal <- re_no[, c("source_title_re", "ttr")] %>% 
  group_by(source_title_re) %>%
  summarise(avg_time = mean(ttr))  

# Merge ttr w/ JIF
ttr_jif <- merge(ttr_by_journal, jif, by.x = "source_title_re", by.y = "journal_title")
ttr_jif$jif <- as.numeric(ttr_jif$impact_factor)

ggplot(ttr_jif[ttr_jif$jif < 10, ], aes(y = avg_time, x = jif)) + 
    scale_y_continuous("Average Time to Retraction", expand = c(0, 0)) + 
    geom_point(alpha = 1/10, pch = 16) + 
    scale_x_continuous("Journal Impact Factor", breaks = round(seq(min(ttr_jif$jif), max(ttr_jif$jif), 1), 0)) + 
    geom_smooth(method = "loess", span = .3, size = .2, col = "#2b8cbe") + 
    theme_minimal() +  
    theme_base

ggsave("figs/jif_time_to_retraction.pdf")

# Log JIF to see if anything is going on more clearly
ggplot(ttr_jif[ttr_jif$jif < 10, ], aes(y = avg_time, x = log(jif))) + 
    scale_y_continuous("Average Time to Retraction", expand = c(0, 0)) + 
    geom_point(alpha = 1/10, pch = 16) + 
    scale_x_continuous("Journal Impact Factor", breaks = round(seq(min(ttr_jif$jif), max(ttr_jif$jif), 1), 0)) + 
    geom_smooth(method = "loess", span = .3, size = .2, col = "#2b8cbe") + 
    theme_minimal() +  
    theme_base

ggsave("figs/log_jif_time_to_retraction.pdf")
