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
## last modified: 14 Jan 2020
###########################################

library(tidyverse)

source("combine_search_results.R")

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

### clean some column names and data values ------------------------------------
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
  
  ## get T/F columns into logical class (must deal with "0" "1" entries in er)
  for (j in 1:ncol(dfs[[i]])) {
    if (colnames(dfs[[i]])[j] %in% logic_cols) {
      dfs[[i]][, j] <- parse_logical(dfs[[i]][, j])
    }
  }
  
  ## clean data.type...gridded separately
  dfs[[i]]$data.type...gridded <- gsub("1|T", "TRUE", 
                                       x = dfs[[i]]$data.type...gridded)
  dfs[[i]]$data.type...gridded <- gsub("0|F", "FALSE", 
                                       x = dfs[[i]]$data.type...gridded)
  
  ## clean institution names so they all match
  for(j in which(colnames(dfs[[i]]) %in% c("institution.of.first.author", 
                                          "institution.of.last.author", 
                                          "proximate.data.source"))) {
    # the big unlist(sapply(sapply(strsplit()))) calls here apply gsub to the 
    # split string components of each cell, then paste the components of each
    # cell back together with ";" as a seperator, and finally unlist the results
    # of the last sapply into a single vector which can be assigned to the
    # column.  
    
    # NBN
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "NBN.*|National Biodiversity Network.*", 
             replacement = "National Biodiversity Network"), 
      FUN = paste, collapse = ";"))
    
    # NBDC
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "NBDC|NBDC.*Ireland", 
             replacement = "National Biodiversity Data Centre"), 
      FUN = paste, collapse = ";"))
    # GBIF
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "GBIF", 
             replacement = "Global Biodiversity Information Facility"), 
      FUN = paste, collapse = ";"))
    # BTO
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "BTO.*", 
             replacement = "British Trust for Ornithology"), 
      FUN = paste, collapse = ";"))
    # NPWS
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "NPWS|National Parks and Wild.*Ireland|National Parks and Wildlife Service$", 
             replacement = "National Parks and Wildlife Service Ireland"), 
      FUN = paste, collapse = ";"))
    # BSBI
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "BSBI", 
             replacement = "Botanical Society of Britain and Ireland"), 
      FUN = paste, collapse = ";"))
    # BRC / CEH
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "BRC|Biological Records Centre|.* CEH .*|.*Centre for Ecology & Hy.*|.*Centre for Ecology and Hy.*", 
             replacement = "Centre for Ecology and Hydrology"), 
      FUN = paste, collapse = ";"))
    # UKBMS
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "UKBMS", 
             replacement = "UK Butterfly Monitoring Scheme"), 
      FUN = paste, collapse = ";"))
    # Butterflies for the New Millenium
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Butterflies.*new Milleni.*", 
             replacement = "Butterflies for the New Millenium",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Bumblebee Conservation Trust
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Bumblebee Conservation Trust.*|.*BeeWatch.*", 
             replacement = "Bumblebee Conservation Trust",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # BWARS
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "BWARS|Bees.*Wasps.*Ants.*Record.*", 
             replacement = "Bees, Wasps and Ants Recording Society",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    
    # Scottish Natural Heritage
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Scottish Natural Heritage.*", 
             replacement = "Scottish Natural Heritage",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Project Seahorse
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Project Seahorse.*", 
             replacement = "Project Seahorse",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Naturalis - The Netherlands
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Naturalis Biod.*", 
             replacement = "Naturalis Biodiversity Centre, The Netherlands",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # National Marine Fisheries - Poland
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "National Marine Fisheries.*Poland", 
             replacement = "National Marine Fisheries Institute, Poland",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # RSPB
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "RSPB.*", 
             replacement = "Royal Society for the Protection of Birds",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # BirdWatch Ireland
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*BirdWatch Ireland.*", 
             replacement = "BirdWatch Ireland",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Atlas Hymenoptera
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Atlas Hymenoptera.*", 
             replacement = "Atlas Hymenoptera",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # MARLIN
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*marlin.*", 
             replacement = "MARLIN",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # UK Marine Recorder
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Marine Recorder.*", 
             replacement = "UK Marine Recorder",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # National Wildlife Management Centre
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*National Wildlife Management Centre.*", 
             replacement = "National Wildlife Management Centre",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Helmholtz Centre
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Hemholtz Centre.*", 
             replacement = "Helmholtz Centre for Ocean Research",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Envirometrix Ltd
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Envirometrix.*", 
             replacement = "Envirometrix Ltd.",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Atkins Ireland
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Atkins.*", 
             replacement = "Atkins Ireland",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Amphibian and Reptile Conservation
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Amphibian and Reptile Conservation.*|National.*amph.*repti.*record.*sch.*", 
             replacement = "Amphibian and Reptile Conservation",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Health Protection Research Unit in Emerging and Zoonotic Infections
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Health Prot.* Emerg.* Zoonotic Infections.*", 
             replacement = "Health Protection Research Unit in Emerging and Zoonotic Infections",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Public Health England
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Public Health England.*", 
             replacement = "Public Health England",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Fungal Records Database of Britain and Ireland
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Fungal Records Database of Britain and Ireland.*", 
             replacement = "Fungal Records Database of Britain and Ireland",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    
    # Imperial College London
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Imperial College London.*", 
             replacement = "Imperial College London",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # University of Konstanz
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*University of Konstanz.*", 
             replacement = "University of Konstanz",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # University of British Columbia
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*University of British Columbia.*", 
             replacement = "University of British Columbia",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # University of Sheffield
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*University of Sheffield.*", 
             replacement = "University of Sheffield",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # University of Kansas
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*University of Kansas.*", 
             replacement = "University of Kansas",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Heriot-Watt University
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Heriot-Watt University.*", 
             replacement = "Heriot-Watt University",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # National University of Ireland Galway
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = "Plant systems biol.*University of Ireland", 
             replacement = "National University of Ireland, Galway",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # The Flora of Cornwall
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Flora of Cornwall.*", 
             replacement = "The Flora of Cornwall (Davey 1909)",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # University of Gloucestershire
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*University of Gloucester.*ire.*", 
             replacement = "University of Gloucestershire",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Coventry University
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ".*Coventry University.*", 
             replacement = "Coventry University",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", UK" after institution names
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", UK", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", Norway"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", Norway", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", Belgium"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", Belgium", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", Italy"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", Italy", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", Germany"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", Germany", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", USA"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", USA", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", South Africa"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", South Africa", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    # Drop ", Naples"
    dfs[[i]][, j] <- unlist(sapply(
      sapply(strsplit(dfs[[i]][, j], split = ";"), 
             FUN = gsub, 
             pattern = ", Naples", 
             replacement = "",
             ignore.case = T),
      FUN = paste, collapse = ";"))
    
    # Common mis-spellings
    dfs[[i]][, j] <- gsub("Center", "Centre", x = dfs[[i]][, j], 
                          ignore.case = T)
    dfs[[i]][, j] <- gsub("Program$|Programmeme", "Programme", x = dfs[[i]][, j], 
                          ignore.case = T)
    dfs[[i]][, j] <- gsub("Intitute", "Institute", x = dfs[[i]][, j], 
                          ignore.case = T)
    dfs[[i]][, j] <- gsub("Nationnal", "National", x = dfs[[i]][, j], 
                          ignore.case = T)
    dfs[[i]][, j] <- gsub("Bublebee", "Bumblebee", x = dfs[[i]][, j], 
                          ignore.case = T)
    dfs[[i]][, j] <- gsub("Britsol", "Bristol", x = dfs[[i]][, j], 
                          ignore.case = T)
    dfs[[i]][, j] <- gsub(".*many.*", "many", x = dfs[[i]][, j], 
                          ignore.case = T)
    
    # trim leading and trailing whitespace
    dfs[[i]][, j] <- unlist(sapply(sapply(strsplit(dfs[[i]][, j], split = ";"), 
                                          FUN = str_trim, 
                                          side = "both"), 
                                   FUN = paste, collapse = ";"))
    # order institutions within each cell alphabetically
    dfs[[i]][, j] <- unlist(sapply(sapply(strsplit(dfs[[i]][, j], split = ";"), 
                                          FUN = function(x) {x[order(x)]}), 
                                   FUN = paste, collapse = ";"))
    
  } # end loop through institution columns
  
}

list2env(dfs, envir = environment()) # unpack the data from the list

## check to make sure all institution strings are correct
# inst <- c(wg$institution.of.first.author, wg$institution.of.last.author,
#           wg$proximate.data.source, 
#           er$institution.of.first.author, er$institution.of.last.author, 
#           er$proximate.data.source)
# inst <- unlist(strsplit(inst, split = ";"))
# inst <- unique(inst)[order(unique(inst))]
# inst

### end clean column names and data values -------------------------------------


### subset to qualifying articles for which coding is done ---------------------
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
  "er.needs.to.read", "qualifies", "authors", 
  "publication", "year", 
  "keywords", "coding.DONE"))]


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

### Temporal extent -----------------------------------------------------------
# calculate number of years covered by study
wg$temp_extent <- as.numeric(wg$end.year) - as.numeric(wg$start.year)
# individually assign values to studies with temporal extent < 1 year
if(diag) paste0("The following article has a temporal extent of zero: ", 
                wg$title[which(wg$temp_extent == 0)])
wg$temp_extent[grepl("An assessment of bumblebee .* land use and floral.*", 
                     wg$title)] <- 0.33
wg$temp_extent[which(grepl("The Success of the Horse.*Revealed with.*", 
                           wg$title))] <- 105/365 # 105/365 days
wg$temp_extent[which(grepl("Patterns and causes of covariation in bird and butterfly.*", 
                           wg$title))] <- 5/12 # April through august 5/12 months
wg$temp_extent[grepl("Taxonomic bias in biodiversity data and.*", 
                     wg$title)] <- 2015 - 1900 # at least this long
wg$temp_extent[grepl("Focal Plant Observations.*Pollinator.*", 
                     wg$title)] <- 3/12 # June through August
wg$temp_extent[grepl("Regional vegetation change and implications.*Cornwall.*", 
                     wg$title)] <- 2013 - 1900 # at least this long
wg$temp_extent[grepl("Monitoring abundance and phenology.*novel mixture.*", 
                     wg$title)] <- 21/52 # weeks
wg$temp_extent[grepl("Large-scale citizen.*farms to bats.*", 
                     wg$title)] <- 5/12 # May to September
wg$temp_extent[grepl("Quality control in.*OPAL Water.*", 
                     wg$title)] <- 8/12 # April to November
wg$temp_extent[grepl("The effect of artificial lighting.*missed.*", 
                     wg$title)] <- 9/365 # 9 days from 4 to 12 January
wg$temp_extent[grepl("Evidence for habitat.*distribution trends of UK and.*", 
                     wg$title)] <- 2012 - 1900 # at least this long
wg$temp_extent[grepl("Change and causes of.*flora of Ireland.*", 
                     wg$title)] <- 1999 - 1970 # at least this long
wg$temp_extent[grepl("Garden and landscape-scale.*moths.*", 
                     wg$title)] <- 36/52 # 36 weeks
### end calculate temporal extent ---------------------------------------------

rm(wg_not_desc, dfs)
