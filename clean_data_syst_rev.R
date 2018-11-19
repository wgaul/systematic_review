###########################################
## Clean systematic review data
## 
## inputs:  * master_eligibility_results.csv - a csv with eligibility coding
##          * wg_systematic_review_coding.csv - wg's coding of articles
## 
## outputs: * elig - the original eligibility scoring
##          * wg - wg's coding of articles.  
##              This is the object to use for analyses
## 
## 
## author: Willson Gaul wgaul@hotmail.com
## created: 27 Aug 2018
## last modified: 19 Nov 2018
###########################################

library(tidyverse)

# setwd("~/Documents/Data_Analysis/UCD/systematic_review/")

# read in data
elig <- read_csv("~/Documents/UCD/PhD_Project/systematic_review/master_eligibility_results.csv")
wg <- read_csv("./data/wg_systematic_review_coding.csv")
er <- read_csv("./data/ellie_systematic_review_coding_04.csv")


## remove columns which are used as visual separators with no data ------------
# wg data
if(any(grepl("X.*", colnames(wg)))) {
  wg <- wg[, -which(grepl("X.*", colnames(wg)))]
}
colnames(wg) <- make.names(colnames(wg))

# er data
if(any(grepl("X.*", colnames(er)))) {
  er <- er[, -which(grepl("X.*", colnames(er)))]
}
colnames(er) <- make.names(colnames(er))

## clean some column names and data values ------------------------------------
# names ok right now
er$data.type...photo[which(er$data.type...photo == "f")] <- "FALSE"

dfs <- list(er = er, wg = wg) # make a list of data for cleaning
# define which columns should be logical
logic_cols <- colnames(wg)[which(sapply(wg, class) == "logical")]
for(i in 1:length(dfs)) {
  # standardize phrases for "unclear" and similar entries
  dfs[[i]] <- data.frame(apply(dfs[[i]], MARGIN = 2, FUN = function(x) {
    gsub("not stated|unknown|not clear|unclear", replacement = "unknown", x = x, 
         ignore.case = TRUE)
  }), stringsAsFactors = FALSE)
  
  # get T/F columns into logical class (must deal with "0" "1" entries in er)
  # TODO 19 NOV
  for (j in 1:ncol(dfs[[i]])) {
    if (colnames(dfs[[i]])[j] %in% logic_cols) {
      dfs[[i]][, j] <- parse_logical(dfs[[i]][, j])
    }
  }
  
  # clean data.type...gridded separately
  dfs[[i]]$data.type...gridded <- gsub("1|T", "TRUE", 
                                       x = dfs[[i]]$data.type...gridded)
  dfs[[i]]$data.type...gridded <- gsub("0|F", "FALSE", 
                                       x = dfs[[i]]$data.type...gridded)
}

list2env(dfs, envir = environment()) # unpack the data from the list
## end clean column names and data values -------------------------------------

## subset to qualifying articles for which coding is done ---------------------
# wg data
if (any(wg$coding.DONE != T, na.rm = T)) {
  warning("Some articles in wg do not have coding.DONE marked as TRUE.  Subsetting to only the articles marked as coding.DONE == TRUE.")
  wg <- wg[which(wg$coding.DONE == T), ] 
}
if (any(wg$qualifies != T)) {
  warning("Some articles in wg are not marked as qualifying.  Subsetting wg to only those articles marked as qualifing.")
  wg <- wg[which(wg$qualifies == T), ]
}

wg <- wg[, -which(colnames(wg) %in% c(
  "link", "inclusion.order", "doi", "other.notes", 
  "aspects.not.yet.coded", 
  "eligible.for.meta.analysis", 
  "wg.needs.to.read"))]

# er data
if (any(er$qualifies != T)) {
  warning("Some articles in er are not marked as qualifying.  Subsetting er to only those articles marked as qualifing.")
  er <- er[which(er$qualifies == T), ]
}

er <- er[, -which(colnames(er) %in% c(
  "link", "inclusion.order", "doi", "other.notes", 
  "aspects.not.yet.coded", 
  "eligible.for.meta.analysis", 
  "er.needs.to.read"))]


## add indicator column for studies that used no data types except whwhwh
wg$data.type...exclusively.whwhwh <- wg$data.type...what.where.when.only == T &
  wg$data.type...abundance == F & 
  wg$data.type...visit.specific.covariates == F & 
  wg$data.type...life.stage == F & 
  wg$data.type...photo == F & 
  wg$data.type...audio == F & 
  wg$data.type...video == F & 
  wg$data.type...voucher.of.some.kind.necessary.for.analysis == F


wg_not_desc <- wg$results.type...inference == T | 
  wg$results.type...prediction == T
if(any(wg_not_desc == T & wg$results.type...descriptive.only == T, na.rm = T)) {
  stop("Descriptive data only and either inference or prediction are both coded TRUE in wg.  This should not be possible.  This may be due to assigning one of the values manually as part of error correction.  wgisit that assignment and change the corresponding mutually exclusive value also.")
}

rm(wg_not_desc)

