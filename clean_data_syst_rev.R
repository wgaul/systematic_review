###########################################
## Clean systematic review data
## 
## inputs:  * master_eligibility_results.csv - a csv with eligibility coding
##          * systematic_review_coded_results.csv - wg's original coding (not
##              corrected based on comparison to second reader)
##          * casey_coded_results.csv - cp's codings after the first 10 or 11
##              article callibration period.  This includes corrections entered
##              after the callibration talk.
##          * casey_coded_results_pre_calibration.csv - cp's original coding of
##              callibration articles before calibration talk and re-evaluation
## 
## outputs: * elig - the original eligibility scoring
##          * rev - wg's coding of articles after correcting errors revealed by
##              double coding. This is the object to use.
##          * rev_orig - wg's original coding of articles before correcting 
##              errors revealed by double coding.  For use only in calculating
##              agreement statistics.
##          * cp - Casey's codings after the calibration discussion and after 
##              correcting obvious errors based on comparison to wg's coding. 
##              This is the object to use.
##          * cp_orig - Casey's codings after the calibration distcussion but 
##              before correcting obvious errors based on comparison to wg's 
##              coding.  For use only in calculating agreement statistics.
##          * cp_pre_cal - Casey's codings for the 10 or 11 articles before the
##              calibration meeting.
## 
## notes: This script is where all corrections are made based on errors revealed
##  in double coding.
## 
## author: Willson Gaul wgaul@hotmail.com
## created: 27 Aug 2018
## last modified: 27 Aug 2018
###########################################

library(tidyverse)

setwd("~/Documents/Data_Analysis/UCD/systematic_review/")

# read in data
elig <- read_csv("~/Documents/UCD/PhD_Project/systematic_review/master_eligibility_results.csv")
rev_orig <- read_csv("./data/systematic_review_coded_results.csv")
cp_orig <- read_csv("./data/casey_coded_results.csv")
cp_pre_cal <- read_csv("./data/casey_coded_results_pre_calibration.csv")

## remove columns which are used as visual separators with no data ------------
# wg data
if(any(grepl("X.*", colnames(rev_orig)))) {
  rev_orig <- rev_orig[, -which(grepl("X.*", colnames(rev_orig)))]
}
colnames(rev_orig) <- make.names(colnames(rev_orig))

# casey's pre-calibration coding
if(any(grepl("X.*", colnames(cp_pre_cal)))) {
  cp_pre_cal <- cp_pre_cal[, -which(grepl("X.*", colnames(cp_pre_cal)))]
}
colnames(cp_pre_cal) <- make.names(colnames(cp_pre_cal))

# casey's post-calibration coding
if(any(grepl("X.*", colnames(cp_orig)))) {
  cp_orig <- cp_orig[, -which(grepl("X.*", colnames(cp_orig)))]
}
colnames(cp_orig) <- make.names(colnames(cp_orig))


## tidy some column names manually --------------------------------------------
# wg data
colnames(rev_orig)[which(
  colnames(rev_orig) == 
    "X.if.prediction.used..prediction.performance.measure")] <- 
  "predicton.performance.measure"

# casey pre-calibration data
colnames(cp_pre_cal)[which(
  colnames(cp_pre_cal) == 
    "X.if.prediction.used..prediction.performance.measure")] <- 
  "predicton.performance.measure"
colnames(cp_pre_cal)[grep(".*museum.*", colnames(cp_pre_cal))] <- 
  "data.type...physical.specimen" # change to match my new name for this var.

# casey post-callibration data
colnames(cp_orig)[which(
  colnames(cp_orig) == 
    "X.if.prediction.used..prediction.performance.measure")] <- 
  "predicton.performance.measure"
colnames(cp_orig)[grep(".*museum.*", colnames(cp_orig))] <- 
  "data.type...physical.specimen" # change to match my new name for this var.

## subset to qualifying articles for which coding is done ---------------------
# wg data
rev_orig <- rev_orig[which(rev_orig$coding.DONE == T), ] 
rev_orig <- rev_orig[which(rev_orig$qualifies == T), ]
rev_orig <- rev_orig[, -which(colnames(rev_orig) %in% c(
  "link", "inclusion.order", "other.notes", 
  "aspects.not.yet.coded", 
  "eligible.for.meta..or.publication.bias.analysis", 
  "wg.needs.to.read"))]

# casey post-callibration data
cp_orig <- cp_orig[, -which(colnames(cp_orig) %in% c("other.notes"))]
cp_pre_cal <- cp_pre_cal[, -which(colnames(cp_pre_cal) %in% 
                                    c("other.notes"))]

## make voucher available column
rev_orig$data.type...voucher.available <- 
  rev_orig$data.type...physical.specimen == T |
  rev_orig$data.type...photo == T |
  rev_orig$data.type...audio == T | 
  rev_orig$data.type...video ==T

## correct errors revealed by double coding -----------------------------------
# This fixes things that were obvioulsy oversights or errors in coding that were
# revealed by comparing 2 readers' codings.  Things that are legitimately 
# ambiguous are not changed.  Original codings are preserved so that agreement
# statistics can be calculated for before and after correction.  This will 
# inform whether all articles and variables should be double-coded.

# wg codings
rev <- rev_orig
rev[grep(".*Nature protection areas of Europe.*", rev$title), 
    "qualifies"] <- FALSE
rev[grep("Explaining European fungal fruiting.*", rev$title), 
    "data.type...physical.specimen"] <- TRUE
# the above change making physical.specimen TRUE means I need to change wh,wh,wh
rev[grep("Explaining European fungal fruiting.*", rev$title), 
    "data.type...what.where.when.only"] <- FALSE
rev[grep("British phenological records indicate high diversity.*", rev$title), 
    "data.type...sampling.effort.reported"] <- TRUE
# the above change making samp effort to TRUE means I need to change wh,wh,wh
rev[grep("British phenological records indicate high diversity.*", rev$title), 
    "data.type...what.where.when.only"] <- FALSE
rev[grep("Modelling and mapping UK emissions.*", rev$title), 
    "non.detection.inferred.using.taxonomic.group"] <- FALSE
rev[grep("Changes in the geographical distribution of plant.*", rev$title), 
    "non.detection.inferred.using.taxonomic.group"] <- FALSE
rev[grep(".*Scragh.*", rev$title), 
    "policy.focus"] <- TRUE
rev[grep("Evaluating promotional approaches.*", rev$title), 
    "spatial.bias.mentioned"] <- TRUE
rev[grep("Big data integration.*", rev$title), 
    "data.type...physical.specimen"] <- TRUE

# cp codings
cp <- cp_orig
cp[grep("Explaining European fungal fruiting.*", cp$title), 
   "results.type...inference"] <- TRUE
cp[grep("Ocean current conn.*", cp$title), "results.type...inference"] <- FALSE
cp[grep("Congruency in fungal phen.*", cp$title), 
   "data.type...physical.specimen"] <- TRUE
cp[grep("Evaluating promotional app.*", cp$title), 
   "data.type...organized.data.collection.scheme"] <- FALSE
cp[grep(".*assessment of bumblebee.*land use.*", cp$title), 
   "biological.records.in.facilitative.role.but.not.analyzed"] <- FALSE
cp[grep("Modelling and mapping UK emissions.*", cp$title), 
   "biological.records.in.facilitative.role.but.not.analyzed"] <- FALSE
cp[grep("The role of historical environmental.*", cp$title), 
   "biological.records.in.facilitative.role.but.not.analyzed"] <- FALSE
cp[grep("Evaluating promotional app.*", cp$title), 
   "data.type...sampling.effort.reported"] <- FALSE
cp[grep("Large reorganizations in butterfly.*", cp$title), 
   "data.type...sampling.effort.reported"] <- TRUE
cp[grep("Setting temporal baselines for bio.*", cp$title),
   "data.type...sampling.effort.reported"] <- TRUE
cp[grep("An assessment of bumblebee \\(Bombus sp.*", cp$title),
   "diversity"] <- FALSE
cp[grep(".*Scragh Bog.*", cp$title),
   "diversity"] <- FALSE
cp[grep("British phenological records indicate.*", cp$title),
   "diversity"] <- FALSE
cp[grep("Ocean current connectivity.*", cp$title),
   "diversity"] <- FALSE
cp[grep("The interplay of climate and land.*", cp$title),
   "diversity"] <- FALSE
cp[grep("An assessment of bumblebee \\(Bombus sp.*", cp$title),
   "non.detection.inferred.using.taxonomic.group"] <- FALSE
cp[grep("British phenological records indicate.*", cp$title),
   "non.detection.inferred.using.taxonomic.group"] <- FALSE
cp[grep("Evaluating promotional approaches.*", cp$title),
   "non.detection.inferred.using.taxonomic.group"] <- FALSE
cp[grep("Factors driving population recovery.*", cp$title),
   "non.detection.inferred.using.taxonomic.group"] <- FALSE
cp[grep("Nature protection areas of Europe.*", cp$title),
   "non.detection.inferred.using.taxonomic.group"] <- FALSE
cp[grep("Ocean current connectivity.*", cp$title),
   "non.detection.inferred.using.taxonomic.group"] <- FALSE
cp[grep("Population variability in species can be deduced.*", cp$title),
   "testing.using.independent.dataset"] <- TRUE
cp[grep("Potential for coupling the monitoring of bush.*", cp$title),
   "testing.using.independent.dataset"] <- TRUE
cp[grep("Using geographic profiling.*", cp$title),
   "testing.using.independent.dataset"] <- TRUE
cp[grep("Light pollution:.*", cp$title),
   "biological.records.in.facilitative.role.but.not.analyzed"] <- FALSE


## end correct errors ---------------------------------------------------------


## drop any newly disqualified studies ----------------------------------------
# these are the objects to use for analysis
rev <- rev[which(rev$coding.DONE == T & rev$qualifies == T), ]
cp <- cp[which(cp$coding.DONE == T & cp$qualifies == T), ]


## check to make sure I didn't manually assign impossible competing values
# make sure I didn't manually assign a richer data type as TRUE without changing
# what, where, when to FALSE
rev_rich_data <- rev$data.type...detection...non.detection == T |
  rev$data.type...abundance == T | 
  rev$data.type...sampling.effort.reported == T |
  rev$data.type...organized.data.collection.scheme == T |
  rev$data.type...visit.specific.covariates == T | 
  rev$data.type...life.stage == T | 
  rev$data.type...voucher.available == T
  
if(any(rev_rich_data == T & rev$data.type...what.where.when.only == T)) {
  stop("data.type...what.where.when.only and one of the richer data types are both TRUE in rev.  This may be due to assigning one of the data types manually as part of error correction.  Revisit that assignment and change the corresponding mutually exclusive value also.")
}

rev_not_desc <- rev$results.type...inference == T | 
  rev$results.type...prediction == T
if(any(rev_not_desc == T & rev$results.type...descriptive.only == T)) {
  stop("Descriptive data only and either inference or prediction are both coded TRUE in rev.  This should not be possible.  This may be due to assigning one of the values manually as part of error correction.  Revisit that assignment and change the corresponding mutually exclusive value also.")
}

rm(rev_rich_data, rev_not_desc)
## end check for competing values

