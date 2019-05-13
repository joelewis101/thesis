library(plyr)
library(tidyverse)
library(reshape2)
library(ape)
library(phytools)

cat("Loading DASSIM 1 and 2 E. coli WGS sample metadata ....")
cat("\n From /Users/joelewis/Documents/Sanger/DASSIM1_sequencing/sample_ids.csv...")
cat("\n And /Users/joelewis/Documents/Sanger/DASSIM2/DASSIM2info.tab...\n")

## get metadata

d1.sample_ids <- read.csv("/Users/joelewis/Documents/Sanger/DASSIM1_sequencing/sample_ids.csv", stringsAsFactors = F)
sub("_1_201803", "", d1.sample_ids$Supplier.Name) -> d1.sample_ids$Supplier.Name
sub("_2_201803", "", d1.sample_ids$Supplier.Name) -> d1.sample_ids$Supplier.Name
d1.sample_ids[,1:3] -> d1.sample_ids
d2.sample_ids <- suppressMessages(read_table("/Users/joelewis/Documents/Sanger/DASSIM2/DASSIM2info.tab"))
names(d2.sample_ids)[3] <- "Supplier.Name"
sub("_1_201808", "", d2.sample_ids$Supplier.Name) -> d2.sample_ids$Supplier.Name
d2.sample_ids[,1:3] -> d2.sample_ids
sample_ids <- rbind(d1.sample_ids, d2.sample_ids)

# correct one error

sample_ids$Supplier.Name[sample_ids$Supplier.Name == "CAF167"] <- "CAF16Z"

# get clinical data

source("/Users/joelewis/Documents/PhD/Thesis/bookdown/final_cleaning_scripts/load_and_clean_followup_and_enroll.R")
source("/Users/joelewis/Documents/PhD/R/PhD/ESBL/load_and_clean_lims.R")

enroll$visit <- 0

names(followup)[names(followup) == "d2visit"] <- "visit"

rbind(select(enroll, pid, visit, data_date ), select(followup, pid, visit, data_date)) -> fudates

#fudates$data_date <- as.Date(fudates$data_date, "%d%b%Y")


unique(fudates) -> fudates
# check for multiple entries and sort em



subset(fudates, !(pid == "DAS1329S" & data_date =="2017-12-06")) -> fudates # seems to be duoplication - pick earliset on all cases
subset(fudates, !(pid == "DAS14870" & data_date =="2018-06-20")) -> fudates
subset(fudates, !(pid == "DAS1533K" & data_date =="2018-12-12")) -> fudates
subset(fudates, !(pid == "DAS1533K" & data_date =="2018-12-12")) -> fudates
subset(fudates, !(pid == "DAS15742" & data_date =="2019-01-24")) -> fudates
#fudates$visit[fudates$pid == "DAS14009" & fudates$data_date == "2018-03-27"] <- 4



# don't worry about NAs for now
fudates %>% dplyr::group_by(pid, visit) %>% dplyr::summarise(n = dplyr::n()) %>% filter(n > 1)

nrow(lims)

merge(lims, fudates, all.x = T, by = c("pid", "visit")) -> lims
# check for dups
nrow(lims)

subset(lims, is.na(data_date))

# entries with no matching 

#DAS1053F - no ODK form wa done
#DAS11705 - can't find form ODK
#DAS1249S - can't find form ODK
#DAS1474A - can't find form ODK
#DAS1540M - can't find form ODK
#DAS1546A - can't find form ODK

nrow(fudates)

nrow(sample_ids)

merge(sample_ids, lims, by.x ="Supplier.Name", by.y = "lab_id", all.x = T) -> sample_ids

sample_ids$data_date[sample_ids$Supplier.Name == "CAC14F"] <- "2018-04-11"
sample_ids$data_date[sample_ids$Supplier.Name == "CAM10H"] <- "2017-09-15"

# add enrollment dates to df

edates <- select(enroll, pid, data_date)
names(edates)[2] <- "enroll_date"

sample_ids <- merge(sample_ids, edates, all.x= T) 
sample_ids$t <- as.numeric(sample_ids$data_date - sample_ids$enroll_date)

cat("\n E. coli WGS Metadata is now in a dataframe called sample_ids.")
cat("\n Share and enjoy!")

