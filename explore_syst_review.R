###############################
## first quick look at partial systematic review data
## 
## Willson Gaul wgaul@hotmail.com
## created: 25 May 2018
## last modified: 18 June 2018
###############################

library(wgutil)
library(tidyverse)
setwd("~/Documents/UCD/PhD_Project/systematic_review/")

df <- read_csv("eligibility.csv")

table(df$qualifies_wg)

include_df <- df[df$qualifies_wg == T, ]
include_df <- select(include_df, -disqualification_reason)

write_csv(include_df, "eligible_results.csv")

### ---------------------------------------------------------------------------
### merge the eligibility scoring I've already done with the new (18 June) 
## master list of search results
## ran this on 18 June 2018 and shouldn't need to run it again

search_results <- read_csv("~/Documents/UCD/PhD_Project/systematic_review/search_results/master_combined_search_results_syst_rev.csv")

df <- select(df, title, year, qualifies_wg)

search_results <- left_join(search_results, df, by = c("title", "year"))

for(i in 1:nrow(search_results)) {
  if(is.na(search_results$qualifies[i])) {
    search_results$qualifies[i] <- search_results$qualifies_wg[i]
  }
}

search_results <- select(search_results, -qualifies_wg)
# write out search_results with added eligiblity coding
write_csv(search_results, path = "~/Documents/UCD/PhD_Project/systematic_review/search_results/master_combined_search_results_syst_rev.csv")
### end add eligibility scoring to search results -----------------------------