#
# Describe the Retracted Article Sample
# 

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
library(readr)
library(goji)

# 1. Number of Retraction Notices by Field.
# Each article can span multiple fields but we make some simplifying assumptions like
# the first category gives us the primary category. The overall picture we get from this
# simplification seems reasonably accurate. More can be done but the aim of this is to give
# people an idea of the composition rather than to make strong claims.
# ---------------------------------------------------------------------

# Read in data 
retracted  <- read.csv("data/05_retracted_articles/new_retracted_articles.csv")
notices    <- read.csv("data/03_retraction_notices/new_retraction_notices.csv")
codebook   <- read.csv("data/citation_codebook.csv")

# Merge files
re_no <- merge(retracted, notices, by = "index",  suffixes = c("_re","_no"))

# Rename cols.
new_names_re <- codebook$r_label[match(names(re_no), paste0(codebook$label, "_re"))]
new_names_no <- codebook$r_label[match(names(re_no), paste0(codebook$label, "_no"))]
names(re_no) <- ifelse(!is.na(new_names_re), paste0(new_names_re, "_re"), names(re_no))
names(re_no) <- ifelse(!is.na(new_names_no), paste0(new_names_no, "_no"), names(re_no))

# Time to retraction
re_no$ttr <- re_no$pub_year_no - re_no$year

# Read in the data 
ret_field <- read.csv("data/11_wos/wos_sc_list.csv")

# First field (enough)
re_no$wc_1 <- sapply(strsplit(re_no$wos_categories_no, ";"), "[", 1)
re_no$wc_2 <- trimws(sapply(strsplit(re_no$wos_categories_no, ";"), "[", 2))

# Let's do some translations around phsyics, neurosciences
re_no$wc_1[grep("Neuroscience|Neurology", re_no$wc_1)] <- "Neurosciences & Neurology"
re_no$wc_1[grep("Physics", re_no$wc_1)]      <- "Physics"
re_no$wc_1[grep("Chemistry", re_no$wc_1)]    <- "Chemistry"
re_no$wc_1[grep("Medicine|Andrology|Gerontology|Peripheral Vascular Disease|Primary Health Care", re_no$wc_1)]   <- "Medicine"
re_no$wc_1[grep("Mathematics", re_no$wc_1)]  <- "Mathematics"
re_no$wc_1[grep("Cardiac", re_no$wc_1)]      <- "Cardiovascular System & Cardiology"
re_no$wc_1[grep("Computer Science", re_no$wc_1)]   <- "Computer Science"
re_no$wc_1[grep("Economics|Business", re_no$wc_1)] <- "Business & Economics"
re_no$wc_1[grep("Psychology", re_no$wc_1)]  <- "Psychology"
re_no$wc_1[grep("Engineering", re_no$wc_1)] <- "Engineering"
re_no$wc_1[grep("Ecology|Environmental Sciences|Biodiversity|GREEN & SUSTAINABLE SCIENCE & TECHNOLOGY", re_no$wc_1)] <- "Environmental Sciences & Ecology"
re_no$wc_1[grep("Humanities|Literature", re_no$wc_1)]  <- "Arts & Humanities Other Topics" #Slight kludge but ok
re_no$wc_1[grep("Agriculture|Agronomy|Horticulture", re_no$wc_1)] <- "Agriculture"
re_no$wc_1[grep("Biochemical", re_no$wc_1)] <- "Biochemistry & Molecular Biology"
re_no$wc_1[grep("Geography, Physical", re_no$wc_1)] <- "Physical Geography"
re_no$wc_1[grep("Materials Science", re_no$wc_1)] <- "Materials Science"
re_no$wc_1[grep("Statistics & Probability", re_no$wc_1)] <- "Mathematics"
re_no$wc_1[grep("Law", re_no$wc_1)] <- "Government & Law"
re_no$wc_1[grep("Education", re_no$wc_1)] <- "Education & Educational Research"
re_no$wc_1[grep("Geosciences|Soil Science", re_no$wc_1)] <- "Geology"

# ret area
re_no$area <- ret_field$area[match(re_no$wc_1, ret_field$field)]
re_no$area <- ifelse(is.na(re_no$area), ret_field$area[match(re_no$wc_2, ret_field$field)], re_no$area)


# Read in data on citations per year for retracted articles
cites_by_article_all_years_2016 <- read_csv("data/cites_by_article_all_years_2016.csv")
cites_by_article_all_years_2016$pre_post <- cites_by_article_all_years_2016$diff > 0

# Total cites per retracted article in the db till 2016 (the db we use)
sum_cites <- cites_by_article_all_years_2016 %>% 
	group_by(index) %>% 
	summarise(sum_cites = sum(n_cites))

# Get relevant columns from re_no including columns that allow us to merge to JIF
sum_cites_jno <- sum_cites %>%
	left_join(re_no[, c("index", "source_title_re", "pub_year_re", "pub_year_no", "area")], by = "index")

## Read in data on journal impact factor
jif <- read.csv("data/journal_impact_factor.csv")

# Merge sum_cites with JIF
sum_cites_jif <- sum_cites_jno %>%
		inner_join(jif, by = c("source_title_re" = "journal_title"))

# Let's describe retracted articles
# a) academic disciplines, b) average number of citations, c) journal impact factor, d) average years since published

retracted_desc <- sum_cites_jif %>%
	summarise(avg_cites = mean(sum_cites), avg_jif = mean(as.numeric(impact_factor)), avg_year_published = mean(2016 - pub_year_re)) %>%
	stack()

ret_field_res  <- as.data.frame(table(re_no$area))
ret_field_res$prop   <- round(ret_field_res[, 2]*100/sum(ret_field_res[, 2]), 2)
ret_field_res_t <- ret_field_res[, c(1, 3)]
colnames(ret_field_res_t) <- c("ind", "values")

ret_desc_tab <- rbind(retracted_desc, ret_field_res_t)

# Let's describe articles that cite retracting articles
# First load and rename 
recite  <- read_csv("data/07_citations_to_retracted_articles/new_retracted_article_citations.zip")
new_names_re <- codebook$r_label[match(names(recite), codebook$label)]
names(recite) <- ifelse(!is.na(new_names_re), paste0(new_names_re, "_re"), names(recite))

# Merge with JIF
recite_jif <- recite %>%
		inner_join(jif, by = c("source_title_re" = "journal_title"))

recite_cite_desc <- recite_jif %>%
	summarise(avg_jif = mean(as.numeric(impact_factor), na.rm = T), avg_year_published = mean(2016 - pub_year_re)) %>%
	stack()

# Let's try crosswalk from journals to SC/WC 
wos_crosswalk <- read_csv("data/11_wos/wos_journal_resarea_subcat_crosswalk.csv")
recite_wc <- recite %>% inner_join(wos_crosswalk, by = c("source_title_re" = "SO"))
recite_wc$wos_categories_re <- recite_wc$WC

# Fields
# First field (enough)
recite_wc$wc_1 <- sapply(strsplit(recite_wc$wos_categories_re, ";"), "[", 1)
recite_wc$wc_2 <- trimws(sapply(strsplit(recite_wc$wos_categories_re, ";"), "[", 2))

# Let's do some translations around phsyics, neurosciences
recite_wc$wc_1[grep("Neuroscience|Neurology", recite_wc$wc_1)] <- "Neurosciences & Neurology"
recite_wc$wc_1[grep("Physics", recite_wc$wc_1)]      <- "Physics"
recite_wc$wc_1[grep("Chemistry", recite_wc$wc_1)]    <- "Chemistry"
recite_wc$wc_1[grep("Medicine|Andrology|Gerontology|Peripheral Vascular Disease|Primary Health Care", recite_wc$wc_1)]   <- "Medicine"
recite_wc$wc_1[grep("Mathematics", recite_wc$wc_1)]  <- "Mathematics"
recite_wc$wc_1[grep("Cardiac", recite_wc$wc_1)]      <- "Cardiovascular System & Cardiology"
recite_wc$wc_1[grep("Computer Science", recite_wc$wc_1)]   <- "Computer Science"
recite_wc$wc_1[grep("Economics|Business", recite_wc$wc_1)] <- "Business & Economics"
recite_wc$wc_1[grep("Psychology", recite_wc$wc_1)]  <- "Psychology"
recite_wc$wc_1[grep("Engineering", recite_wc$wc_1)] <- "Engineering"
recite_wc$wc_1[grep("Ecology|Environmental Sciences|Biodiversity|GREEN & SUSTAINABLE SCIENCE & TECHNOLOGY", recite_wc$wc_1)] <- "Environmental Sciences & Ecology"
recite_wc$wc_1[grep("Humanities|Literature", recite_wc$wc_1)]  <- "Arts & Humanities Other Topics" #Slight kludge but ok
recite_wc$wc_1[grep("Agriculture|Agronomy|Horticulture", recite_wc$wc_1)] <- "Agriculture"
recite_wc$wc_1[grep("Biochemical", recite_wc$wc_1)] <- "Biochemistry & Molecular Biology"
recite_wc$wc_1[grep("Geography, Physical", recite_wc$wc_1)] <- "Physical Geography"
recite_wc$wc_1[grep("Materials Science", recite_wc$wc_1)] <- "Materials Science"
recite_wc$wc_1[grep("Statistics & Probability", recite_wc$wc_1)] <- "Mathematics"
recite_wc$wc_1[grep("Law", recite_wc$wc_1)] <- "Government & Law"
recite_wc$wc_1[grep("Education", recite_wc$wc_1)] <- "Education & Educational Research"
recite_wc$wc_1[grep("Geosciences|Soil Science", recite_wc$wc_1)] <- "Geology"

# ret area
recite_wc$area <- ret_field$area[match(recite_wc$wc_1, ret_field$field)]
recite_wc$area <- ifelse(is.na(recite_wc$area), ret_field$area[match(recite_wc$wc_2, ret_field$field)], recite_wc$area)

recite_field_res  <- as.data.frame(table(recite_wc$area))
recite_field_res$prop   <- round(recite_field_res[, 2]*100/sum(recite_field_res[, 2]), 2)
recite_field_res_t <- recite_field_res[, c(1, 3)]
colnames(recite_field_res_t) <- c("ind", "values")

recite_desc_tab <- rbind(recite_cite_desc, recite_field_res_t)

# Now join the tables
recite_ret_res <- ret_desc_tab  %>% 
	left_join(recite_desc_tab, by = "ind") %>%
	relocate(ind, values.x, values.y) %>%
	rename(Retracted = values.x, Citing = values.y, Variable = ind)

recite_ret_res$Variable <- recode(recite_ret_res$Variable, avg_cites = "Avg. Number of Citations", 
                                                           avg_jif = "Avg. Journal Impact Factor",
                                                           avg_year_published = "Avg. Number of Years Since Published")

recite_ret_res$Variable <- as.character(recite_ret_res$Variable)

# Fix formatting
recite_ret_res$Retracted <- nice(recite_ret_res$Retracted)
recite_ret_res$Citing[2:9] <- nice(recite_ret_res$Citing[2:9])

# Add %
recite_ret_res$Retracted[4:9]  <- paste0(recite_ret_res$Retracted[4:9], "%")
recite_ret_res$Citing[4:9]  <- paste0(recite_ret_res$Citing[4:9], "%")

# Add dashes for Citing[1]
recite_ret_res$Citing[1] <- "--"

# Add empty line
recite_ret_res_fin <- rbind(recite_ret_res[1:3, ], c("Field", NA, NA), recite_ret_res[4:9, ])

## Desc Table 

print(
	  xtable(recite_ret_res_fin,
         digits = 1,
	  	   caption = "Summary Statistics of Retracted Articles and Articles Citing Retracted Articles", 
	  	   label = "tab:recite_ret_sum"), 
	    include.rownames = FALSE,
	    include.colnames = TRUE, 
	    floating = TRUE,
	    type = "latex", 
	    caption.placement = "bottom",
	    table.placement = "!htb",
	    file = "tabs/recite_ret_sum.tex")
