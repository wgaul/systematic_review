######################################
## Combine search results for systematic review of biological records 
## Search results from Web of Science, Google Scholar, Scopus, and GBIF.
## 
## Also, I did some eligiblility evaluating for Web of Science results but then
## had to expand the search to include more UK place names, so this script also
## merges the new search results into my existing eligibility scoring sheet.
## 
## NOTES: * Google Scholar search results do not include year for some records.
##          After merging Google Scholar results in this script and writing the
##          combined GS results to GoogleScholar_searchResults_merged.csv, I 
##          added years in that file for records that were missing year.  After
##          doing so, I saved a backup copy of that in the "old" folder under
##          the file name 
##          GoogleScholar_searchResults_merged_18June2018_GSyears_added_by_hand.csv
##          (just in case this script overwrites the merged csv at some point).
##        * I ran this script on 18 June 2018 to produce 
##          master_combined_search_results_syst_rev.csv and now I shouldn't need
##          to run this script again. 
## 
## inputs: * search results from 
##         * Web of Science
##         * Scopus
##         * ProQuest
##         * Google Scholar
##         * GBIF
##         
## outputs: * master_combined_search_results_syst_rev.csv - a file with all the
##          selected search results in the order that they should be used
## 
## author: Willson Gaul wgaul@hotmail.com
## created: 29 May 2018
## last modified: 19 Feb 2020
#######################################

library(wgutil)
library(Hmisc)
library(tidyverse)

scopus <- read_csv("scopus_15June2018_searchResults.csv", 
                   skip = 4)
scopus <- scopus[which(scopus$year >= 2014), ]
gbif <- read_csv("GBIF_publications.csv")
gbif <- gbif[which(gbif$year >= 2014), ] # keep only 2014 to present
wos <- read_csv("WebOfScience_15June2018.csv", 
                skip = 4)

## combine proquest files if not done yet, othewise read in combined results
if("ProQuest_combined.csv" %nin% list.files("")) {
  pq_1 <- read_csv("ProQuest_01.csv", 
                   skip = 4)
  pq_2 <- read_csv("ProQuest_02.csv")
  pq_3 <- read_csv("ProQuest_03.csv")
  pq_4 <- read_csv("ProQuest_04.csv")
  pq_5 <- read_csv("ProQuest_05.csv")
  pq_6 <- read_csv("ProQuest_06.csv")
  pq_7 <- read_csv("ProQuest_07.csv")
  pq_8 <- read_csv("ProQuest_08.csv")
  pq_9 <- read_csv("ProQuest_09.csv")
  pq_10 <- read_csv("ProQuest_10.csv")
  
  proquest <- bind_rows(list(pq_1, pq_2, pq_3, pq_4, pq_5, pq_6, pq_7, pq_8, 
                             pq_9, pq_10))
  
  proquest <- select(proquest, -URL)
  colnames(proquest) <- c("title", "author", "documentURL", "PublicationDate", 
                          "database")
  proquest$year <- NA
  proquest$search_venue <- rep("ProQuest", nrow(proquest))
  
  for(i in 1:nrow(proquest)) {
    s <- proquest$PublicationDate[i]
    if(grepl(".*2014.*", s)) y <- 2014
    if(grepl(".*2015.*", s)) y <- 2015
    if(grepl(".*2016.*", s)) y <- 2016
    if(grepl(".*2017.*", s)) y <- 2017
    if(grepl(".*2018.*", s)) y <- 2018
    proquest$year[i] <- y
  }
  
  proquest$dup <- NA
  for(i in 1:nrow(proquest)) { # find duplicate titles within proquest results
    if(proquest$title[i] %in% proquest$title[-i]) proquest$dup[i] <- TRUE
  }
  
  dups <- proquest[which(proquest$dup == T), ] # proquest duplicates
  proquest <- proquest[which(is.na(proquest$dup)), ] # all other proquest
  
  # add only a single instance of duplicate titles back into th proquest
  # results df
  for(i in 1:length(unique(dups$title))) {
    keep <- which(dups$title == unique(dups$title)[i])[1]
    proquest <- bind_rows(proquest, dups[keep, ])
  }

  proquest <- proquest[, -which(colnames(proquest) %in% c("PublicationDate", 
                                                         "dup"))]
  
  write_csv(proquest, "ProQuest_combined.csv")
} else proquest <- read_csv("ProQuest_combined.csv")

## combine Google Scholar results if not done yet, otherwise read in combined
if("GoogleScholar_searchResults_merged.csv" %nin% 
   list.files()) { 
  gs_1 <- read_csv("PoP_GS_BiologicalrecordsIreland_no_BookNewreportNewspeciesNewrecord_2014_2018.csv", 
                   skip = 4)
  gs_2 <- read_csv("PoP_GS_BiologicalrecordsUnitedkingdom_no_bookNewreportNewspeciesNewrecord.csv", 
                   skip = 4)
  gs_3 <- read_csv("PoP_GS_CitizenscienceSpeciesIreland_no_DiseaseBookNewreportNewspeciesNewrecord_2014_2018.csv", 
                   skip = 4)
  gs_4 <- read_csv("PoP_GS_CitizenscienceSpeciesUnitedkingdom_no_DiseaseBookNewreportNewspeciesNewrecord.csv", 
                   skip = 4)
  
  all_gs <- full_join(gs_1, gs_2,
                      by = c("Authors", "Title", "Year", "ArticleURL", 
                             "Publisher", "DOI", "ISSN", "Volume", "Issue", 
                             "StartPage", "EndPage", "AuthorCount",
                             "Age"))
  all_gs <- select(all_gs, -Cites.y, -CitesURL.y, -GSRank.y, -QueryDate.y, 
                   -CitationURL.y, -ECC.y, -CitesPerYear.y, -CitesPerAuthor.y, 
                   -Type.y, -Source.y)
  names(all_gs) <- gsub("\\.x", "", names(all_gs))
  
  all_gs <- full_join(all_gs, gs_3, 
                      by = c("Authors", "Title", "Year", "ArticleURL", 
                             "Publisher", "DOI", "ISSN", "Volume", "Issue", 
                             "StartPage", "EndPage", "AuthorCount",
                             "Age"))
  all_gs <- select(all_gs, -Cites.y, -CitesURL.y, -GSRank.y, -QueryDate.y, 
                   -CitationURL.y, -ECC.y, -CitesPerYear.y, -CitesPerAuthor.y, 
                   -Type.y, -Source.y)
  names(all_gs) <- gsub("\\.x", "", names(all_gs))
  
  all_gs <- full_join(all_gs, gs_4, 
                      by = c("Authors", "Title", "Year", "ArticleURL", 
                             "Publisher", "DOI", "ISSN", "Volume", "Issue", 
                             "StartPage", "EndPage", "AuthorCount",
                             "Age"))
  all_gs <- select(all_gs, -Cites.y, -CitesURL.y, -GSRank.y, -QueryDate.y, 
                   -CitationURL.y, -ECC.y, -CitesPerYear.y, -CitesPerAuthor.y, 
                   -Type.y, -Source.y)
  names(all_gs) <- gsub("\\.x", "", names(all_gs))
  
  # remove citations
  all_gs <- all_gs[-which(all_gs$Type == "CITATION"), ]
  
  # test for duplicates
  all_gs <- all_gs[order(all_gs$Title), ]
  which(as.numeric(table(all_gs$Title)) > 1)
  
  # evaluate and delete Google Scholar duplicates by hand I guess
  all_gs$dup_title <- all_gs$Title %in% names(table(all_gs$Title))[
    which(as.numeric(table(all_gs$Title)) > 1)] # make index of duplicate titles
  all_gs <- all_gs[order(all_gs$dup_title, decreasing = T), ] # put dups on top
  #write_csv(data.frame((all_gs[1:sum(all_gs$dup_title), c(2, 3, 7)])), "temp_dublicates.csv") # view
  
  to_remove <- c(2, 4, 6, 8, 9, 12, 13, 16, 18, 19, 22, 26, 27, 29, 32, 36, 38, 
                 40, 41, 44) # based on evaluation by going to article URLs 15 June 2018
  all_gs <- all_gs[-to_remove, ] # remove true duplicates
  all_gs <- select(all_gs, Authors, Title, Year, ArticleURL, DOI)
  colnames(all_gs) <- c("author", "title", "year", "articleURL", "doi")
  
  write_csv(all_gs, "GoogleScholar_searchResults_merged.csv")
  
  gs <- all_gs # NOTE: then need to add years by hand to this
  rm(all_gs)
} else gs <- read_csv("GoogleScholar_searchResults_merged.csv")


### calculate percent of each search results that were returned by the
## other searches -------------------------------------------------------------
# WOS in other searches
wos_dup <- which(wos$title %in% scopus$title) # WOS in scopus
wos_dup <- c(wos_dup, which(wos$title %in% gbif$title)) # WOS in GBIF
wos_dup <- c(wos_dup, which(wos$title %in% proquest$title)) # WOS in ProQuest
wos_dup <- c(wos_dup, which(wos$title %in% gs$title)) # WOS in GS

wos_dup <- unique(wos_dup) 
length(wos_dup) / nrow(wos) # proportion of WOS results in other results

# scopus in other searches
scop_dup <- which(scopus$title %in% wos$title) # scopus in WOS
scop_dup <- c(scop_dup, which(scopus$title %in% gbif$title)) # scopus in GBIF
scop_dup <- c(scop_dup, which(scopus$title %in% proquest$title)) # scopus in ProQuest
scop_dup <- c(scop_dup, which(scopus$title %in% gs$title)) # scopus in GS
scop_dup <- unique(scop_dup)
length(scop_dup) / nrow(scopus) # percent of scopus results in other results

# ProQuest in other searches
pq_dup <- which(proquest$title %in% wos$title) # proquest in WOS
pq_dup <- which(proquest$title %in% gbif$title) # proquest in GBIF
pq_dup <- which(proquest$title %in% scopus$title) # proquest in Scopus
pq_dup <- which(proquest$title %in% gs$title) # proquest in GS
pq_dup <- unique(pq_dup)
length(pq_dup) / nrow(proquest)

# GS in other searches
gs_dup <- which(gs$title %in% wos$title) # GS in WOS
gs_dup <- c(gs_dup, which(gs$title %in% gbif$title)) # GS in GBIF
gs_dup <- c(gs_dup, which(gs$title %in% scopus$title)) # GS in scopus
gs_dup <- unique(gs_dup)
length(gs_dup) / nrow(gs)

### end calculate overlap ----------------------------------------------------

### get authors into consistent format --------------------------------------
auth_wos <- strsplit(wos$author, "; ")
auth_wos <- lapply(auth_wos, 
                   FUN = function(x) gsub("[[:lower:]]*[[:punct:]]*$", "", x))
auth_wos <- lapply(auth_wos, FUN = function(x) gsub("\\. ", "", x))
# use parenthetical subexpressions to return parts of the matched pattern
auth_wos <- lapply(auth_wos, 
                   FUN = function(x) gsub(
                     "(, [[:upper:]])([[:lower:]]* )([[:upper:]]*$)", 
                     "\\1\\3", x))
auth_wos <- lapply(auth_wos, FUN = function(x) paste(x, collapse = "; "))
for(i in 1:nrow(wos)) {
  wos$author[i] <- auth_wos[[i]]
}

auth_scop <- strsplit(scopus$author, "\\.,")
auth_scop <- lapply(auth_scop, FUN = function(x) gsub("\\.", "", x))
auth_scop <- lapply(auth_scop, FUN = function(x) paste(x, collapse = ";"))
for(i in 1:nrow(scopus)) {
  scopus$author[i] <- auth_scop[[i]]
}

auth_pq <- strsplit(proquest$author, ";")
auth_pq <- lapply(auth_pq, 
                   FUN = function(x) gsub(
                     "(, [[:upper:]])([[:lower:]]* )([[:upper:]]*$)", 
                     "\\1\\3", x))
auth_pq <- lapply(auth_pq, 
                  FUN = function(x) gsub(
                    "(, [[:upper:]])([[:lower:]]*$)", 
                    "\\1", x))
auth_pq <- lapply(auth_pq, FUN = function(x) paste(x, collapse = "; "))
for(i in 1:nrow(proquest)) {
  proquest$author[i] <- auth_pq[[i]]
}

auth_gs <- strsplit(gs$author, ", ")
auth_gs <- lapply(auth_gs, FUN = function(x) gsub(
  "([[:upper:]]+ )([[:upper:]][[:lower:]]+)", "\\2, \\1", x))


rm(auth_wos, auth_scop, auth_pq, auth_gs)
### end format authors -------------------------------------------------------
### merge datasets -------------------------------------------------------------
## I will merge only title, author, and year, then once I have the mastser 
# df of all articles, will add in doi, link, publication, etc. columns, then
# loop through the individual search engine dfs (wos, socups, gs, etc) to fill
# in those columns from whichever individual search engine df has the 
# info for a give line.  This will work better than trying to merge/join those
# columns, b/c I don't know of a good automated way to choose to keep the 
# filled and not NA value when the values of those columns don't match.

## merge WOS and Scoups
search_results <- full_join(wos[, c("title", "author", "year")], 
                            scopus[, c("title", "author", "year")])

## merge ProQuest onto others 
search_results <- full_join(search_results, 
                            proquest[, c("title", "author", "year")])

## merge GBIF onto others
search_results <- full_join(search_results, 
                            gbif[, c("title", "author", "year")])

## merge Google Scholar onto others
search_results <- full_join(search_results, 
                            gs[, c("title", "year")], 
                            by = c("title", "year"))

### end merge ----------------------------------------------------------------

### remove duplicate titles in search_results ---------------------------------
# find possible duplicates
search_results$title_std <- search_results$title # standardize titles to lower case
search_results$title_std <- str_replace_all(
  search_results$title_std, "\\s", "") %>%
  str_replace_all(., "[[:punct:]]", "") %>%
  str_to_lower()
# count number of duplicates
n_duplicate_titles <- table(search_results$title_std)[table(
  search_results$title_std) > 1]

# remove duplicate results
possible_dups <- search_results[which(
  search_results$title %in% names(table(search_results$title)[which(
    table(search_results$title) > 1)])), ]
possible_dups <- possible_dups[order(possible_dups$title), ]
#View(possible_dups) # evaluate possible duplicates manually 
drop_dups <- c(1, 3, 6, 8, 10, 12, 14, 16, 17, 19, 21, 23, 25, 27, 28, 30, 32, 
               34, 36, 38, 40, 41, 43, 45, 47, 49, 51, 52, 54, 56, 58, 59, 61, 
               63, 65, 67, 69, 71, 73) # this list manually created
keep_dups_df <- possible_dups[-drop_dups, ]

# remove all possible duplicates from search_results, then add back in only a 
# single record for each duplicate
search_results <- search_results[-which(
  search_results$title %in% names(table(search_results$title)[which(
    table(search_results$title) > 1)])), ] # remove all possible duplicates
search_results <- bind_rows(search_results, keep_dups_df)

any(table(search_results$title) > 1) # check to make sure it worked

rm(keep_dups_df, possible_dups)
### end remove duplicates ----------------------------------------------------

### order search results for investigation -----------------------------------
## most recent year will be used first, but within years order will be random
set.seed(42)
search_results$rand_order <- sample(1:nrow(search_results))
search_results <- arrange(search_results, desc(year)) %>%
  group_by(desc(year)) %>%
  arrange(desc(rand_order), .by_group = TRUE) 

search_results <- search_results[, -which(
  colnames(search_results) == "desc(year)")]
search_results$rand_order <- 1:nrow(search_results)
### end order search results ---------------------------------------------------

### add additional info onto search_results df from original search dfs
search_results$publication <- NA
search_results$doi <- NA
search_results$link <- NA

if(all(is.na(search_results$publication))) { # fill publication column
  for(i in 1:nrow(search_results)) {
    ttl <- search_results$title[i]
    if(ttl %in% wos$title && !is.na(wos$publication[which(wos$title == ttl)])) {
      search_results$publication[i] <- wos$publication[which(wos$title == ttl)]
    } else
      if(ttl %in% scopus$title && !is.na(scopus$`Source title`[which(
        scopus$title == ttl)])) {
        search_results$publication[i] <- scopus$`Source title`[which(
          scopus$title == ttl)]
      } else
        if(ttl %in% gbif$title && !is.na(gbif$publication[which(
          gbif$title == ttl)])) {
          search_results$publication[i] <- gbif$publication[which(
            gbif$title == ttl)]
        }
  }
}

if(all(is.na(search_results$doi))) { # fill doi column
  for(i in 1:nrow(search_results)) {
    ttl <- search_results$title[i]
    if(ttl %in% wos$title && !is.na(wos$doi[which(wos$title == ttl)])) {
      search_results$doi[i] <- wos$doi[which(wos$title == ttl)]
    } else
      if(ttl %in% scopus$title && !is.na(scopus$doi[which(
        scopus$title == ttl)])) {
        search_results$doi[i] <- scopus$doi[which(
          scopus$title == ttl)]
      } else
        if(ttl %in% gbif$title && !is.na(gbif$doi[which(
          gbif$title == ttl)])) {
          search_results$doi[i] <- gbif$doi[which(
            gbif$title == ttl)]
        } else
          if(ttl %in% gs$title && !is.na(gs$doi[which(gs$title == ttl)])) {
            search_results$doi[i] <- gs$doi[which(gs$title == ttl)]
          }
  }
}

#wos, scopus, gbif, proquest, gs
if(all(is.na(search_results$link))) { # fill link column
  for(i in 1:nrow(search_results)) {
    ttl <- search_results$title[i]
    if(ttl %in% scopus$title && !is.na(scopus$link[which(
      scopus$title == ttl)])) {
      search_results$link[i] <- scopus$link[which(scopus$title == ttl)]
    } else
      if(ttl %in% proquest$title && !is.na(proquest$documentURL[which(
        proquest$title == ttl)])) {
        search_results$link[i] <- proquest$documentURL[which(
          proquest$title == ttl)]
      } else
        if(ttl %in% gs$title && !is.na(gs$articleURL[which(gs$title == ttl)])) {
          search_results$link[i] <- gs$articleURL[which(gs$title == ttl)[1]]
        }
  }
}

search_results <- select(search_results, rand_order, title, author, publication, 
                         doi, year, link)
# rename "author" column to "authors"
colnames(search_results)[which(
  colnames(search_results) == "author")] <- "authors"
colnames(search_results)[which(
  colnames(search_results) == "rand_order") ] <- "inclusion_order"

rm(scopus, gbif, wos, proquest, drop_dups, gs_dup, pq_dup, 
   scop_dup, ttl, wos_dup)
### write out merged, ordered search results ----------------------------------
if("master_combined_search_results_syst_rev.csv" %nin% 
   list.files("./")){
  try(write_csv(search_results,
            path = "~/Documents/UCD/PhD_Project/systematic_review/search_results/master_combined_search_results_syst_rev.csv"))
} else warning("File not written.  A file named master_combined_search_results_syst_rev.csv alread exists.  If you want to overwrite it, run the write_csv command manually.")

