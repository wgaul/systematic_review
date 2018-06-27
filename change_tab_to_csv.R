####################################
## change Web of Science exported results from tab separated to csv
## 
## Willson Gaul wgaul@hotmail.com
## 15 June 2018
###################################

library(wgutil)
library(tidyverse)
setwd("~/Documents/UCD/PhD_Project/systematic_review/")

df <- read_tsv("./search_results/WebOfScience_15June2018.txt")

df <- select(df, TI, AU, SO, VL, IS, DI, PY)
names(df) <- c("title", "author", "publication", "volume", "issue", "doi", 
               "year" )

write_csv(df, "WebOfScience_15June2018.csv")
