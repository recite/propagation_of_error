"

Includes: 

  1. Number of Retraction Notices by Field (as used by Web of Science + some of our own additions)
  2. Number of Retraction Notices by Year
  3. Why are the articles retracted?
  4. Citations to Retraction Notices

"

# Global options
options(StringsAsFactors = FALSE)

# Set dir 
setwd(githubdir)
setwd("propagation_of_error/")

# Load libs for xtable, plot
library(xtable)
library(dplyr)
library(ggplot2)
library(grid)

# 1. Number of Retraction Notices by Field.
# Each article can span multiple fields but we make some simplifying assumptions like
# the first category gives us the primary category. The overall picture we get from this
# simplification seems reasonably accurate. More can be done but the aim of this is to give
# people an idea of the composition rather than to make strong claims.
# ---------------------------------------------------------------------

# Read in the data 
ret_not   <- read.csv("data/03_retraction_notices/new_retraction_notices.csv")
ret_field <- read.csv("data/11_wos/wos_sc_list.csv")

# First field (enough)
ret_not$wc_1 <- sapply(strsplit(ret_not$WC, ";"), "[", 1)
ret_not$wc_2 <- trimws(sapply(strsplit(ret_not$WC, ";"), "[", 2))

# Let's do some translations around phsyics, neurosciences
ret_not$wc_1[grep("Neuroscience|Neurology", ret_not$wc_1)] <- "Neurosciences & Neurology"
ret_not$wc_1[grep("Physics", ret_not$wc_1)]      <- "Physics"
ret_not$wc_1[grep("Chemistry", ret_not$wc_1)]    <- "Chemistry"
ret_not$wc_1[grep("Medicine|Andrology|Gerontology|Peripheral Vascular Disease|Primary Health Care", ret_not$wc_1)]   <- "Medicine"
ret_not$wc_1[grep("Mathematics", ret_not$wc_1)]  <- "Mathematics"
ret_not$wc_1[grep("Cardiac", ret_not$wc_1)]      <- "Cardiovascular System & Cardiology"
ret_not$wc_1[grep("Computer Science", ret_not$wc_1)]   <- "Computer Science"
ret_not$wc_1[grep("Economics|Business", ret_not$wc_1)] <- "Business & Economics"
ret_not$wc_1[grep("Psychology", ret_not$wc_1)]  <- "Psychology"
ret_not$wc_1[grep("Engineering", ret_not$wc_1)] <- "Engineering"
ret_not$wc_1[grep("Ecology|Environmental Sciences|Biodiversity|GREEN & SUSTAINABLE SCIENCE & TECHNOLOGY", ret_not$wc_1)] <- "Environmental Sciences & Ecology"
ret_not$wc_1[grep("Humanities|Literature", ret_not$wc_1)]  <- "Arts & Humanities Other Topics" #Slight kludge but ok
ret_not$wc_1[grep("Agriculture|Agronomy|Horticulture", ret_not$wc_1)] <- "Agriculture"
ret_not$wc_1[grep("Biochemical", ret_not$wc_1)] <- "Biochemistry & Molecular Biology"
ret_not$wc_1[grep("Geography, Physical", ret_not$wc_1)] <- "Physical Geography"
ret_not$wc_1[grep("Materials Science", ret_not$wc_1)] <- "Materials Science"
ret_not$wc_1[grep("Statistics & Probability", ret_not$wc_1)] <- "Mathematics"
ret_not$wc_1[grep("Law", ret_not$wc_1)] <- "Government & Law"
ret_not$wc_1[grep("Education", ret_not$wc_1)] <- "Education & Educational Research"
ret_not$wc_1[grep("Geosciences|Soil Science", ret_not$wc_1)] <- "Geology"

# ret area
ret_not$area <- ret_field$area[match(ret_not$wc_1, ret_field$field)]
ret_not$area <- ifelse(is.na(ret_not$area), ret_field$area[match(ret_not$wc_2, ret_field$field)], ret_not$area)

ret_field_res        <- as.data.frame(table(ret_not$area))
ret_field_res$prop   <- ret_field_res[, 2]*100/sum(ret_field_res[, 2])
names(ret_field_res) <- c("Field", "Number of Notices", "Percentage of Total")

## SI 2

print(
	  xtable(ret_field_res,
         digits = 1,
	  	   caption = "Retraction Notices By Field", 
	  	   label = "tab:ret_field"), 
	    include.rownames = FALSE,
	    include.colnames = TRUE, 
	    floating = TRUE,
	    type = "latex", 
	    caption.placement = "bottom",
	    table.placement = "!htb",
	    file = "tabs/ret_field.tex")

# 2. Number of Retraction Notices by Year
# ----------------------------------------
# Read in the data
notices    <- read.csv("data/03_retraction_notices/new_retraction_notices.csv")

# Number of notices by year
notices_by_year <- notices[, c("index", "PY")] %>% 
  group_by(PY) %>% 
  summarise(n_notices = n())  

summary(notices_by_year$n_notices[notices_by_year$PY < 2000])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   1.000   2.000   4.857   7.500  14.000 

summary(notices_by_year$n_notices[notices_by_year$PY > 2000 & notices_by_year$PY < 2016])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.0    48.0   148.0   184.2   315.5   439.0

notices_by_year$n_notices[notices_by_year$PY == 2001]
# 15

notices_by_year$n_notices[notices_by_year$PY == 2015]
# 439

# Plot citations over time
# Base Theme  
theme_base <-
    theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
    panel.border       = element_blank(),
    legend.position    = "bottom",
    legend.background  = element_rect(color = "#ffffff"),
    legend.key         = element_rect(color = "#ffffff", fill = "#ffffff"),
    legend.key.size    = unit(.1, "cm"),
    legend.margin      = margin(.2),
    title              = element_text(size = 8, colour = "#333333"),
    axis.title         = element_text(size = 8, colour = "#333333"),
    axis.text          = element_text(size = 8, colour = "#333333"),
    axis.ticks.y       = element_blank(),
    axis.ticks.x       = element_blank(),
    strip.text.x       = element_text(size = 9),
    legend.text        = element_text(size = 8),
    plot.margin        = unit(c(0, .5, .5, .5), "cm"))

# Plot 
ggplot(notices_by_year, aes(y = n_notices, x = PY)) + 
  geom_point() + 
  scale_y_continuous("Total Notices") + 
  scale_x_continuous("Year of Publication of Notice", breaks= seq(min(notices_by_year$PY), max(notices_by_year$PY), 5)) + 
  geom_smooth(method = "loess", size = .2, col = "#2b8cbe") + 
  theme_minimal() +  
  theme_base 

ggsave("figs/n_retraction_notices_by_year.pdf")

# 3. Reasons articles are retracted
# -------------------------------------

rfr <- read.csv("data/04_reason_for_retraction/new_reason_for_retraction.csv")

table(rfr$reason_label)[order(table(rfr$reason_label))]

length(grep("plagiarism", rfr$reason_label))
# [1] 37

length(grep("error|fraud", rfr$reason_label))
# [1] 49

length(grep("fraud", rfr$reason_label))
# [1] 23

# 4. Citations to Retraction Notices
cite_notice <- read.csv("data/08_citations_to_retraction_notices/new_retraction_notice_citations.csv")
nrow(cite_notice)
# 1897
