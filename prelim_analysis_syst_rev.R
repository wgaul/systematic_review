#####################################
## Preliminary analysis of bio recs systematic review
## 
## author: Willson Gaul wgaul@hotmail.com
## created: 7 June 2018
## last modified: 7 June 2018
#####################################

setwd("~/Documents/Data_Analysis/UCD/systematic_review/")
library(wgutil)
library(Hmisc)
library(tidyverse)

rev <- read_csv("~/Documents/UCD/PhD_Project/systematic_review/systematic_review_coded_results.csv")

rev <- rev[which(rev$coding_DONE == T), ]

### tables / bar charts --------------------------------------------------------
an_type <- rev[, c(1, 9, 18:22)]
an_type <- gather(an_type, key = "type", value = "value", 3:7) %>%
  filter(value == TRUE)
table(an_type$type)

table(rev$results_tp_hypothesis_testing)

table(rev$eligible_for_meta_publication_bias_analysis)

table(rev$year)

table(rev$data_tp_what_where_when_only)

table(rev$data_tp_museum)

### end tables / bar charts --------------------------------------------------