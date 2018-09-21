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
## last modified: 17 Sep 2018
###########################################

library(tidyverse)

setwd("~/Documents/Data_Analysis/UCD/systematic_review/")

# read in data
elig <- read_csv("~/Documents/UCD/PhD_Project/systematic_review/master_eligibility_results.csv")
wg <- read_csv("./data/wg_systematic_review_coding.csv")
er <- read_csv("./data/fake_ellie.csv")


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

## tidy some column names manually --------------------------------------------
# (not needed right now)


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

