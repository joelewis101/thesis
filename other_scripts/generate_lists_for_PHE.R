
# JL Feb 2020
# Script to load list of acute and convalescent sera and urines shipped to PHE
# clean up the pids as per the lims cleaning script
# (do this by making a lookup table using the lims script to link the raw and cleaned PIDs)
# load diagnoses and link
# Provide wide table of tose to test

library(reshape2)
library(plyr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(grid)
library(ggpubr)
library(pheatmap)
library(RColorBrewer)
library(scales)
library(lubridate)
library(survival)
library(ggsci)
library(survminer)
library(glue)
library(broom)


# lims lookup table
source("final_cleaning_scripts/generate_lims_lookup.R")

# load aetiology data

source("final_cleaning_scripts/generate_visits_df.R")
source("final_cleaning_scripts/load_and_clean_followup_and_enroll_labelled.R")
source("other_scripts/summary_table_functions.R")
source("final_cleaning_scripts/make_composite_hivstatus_variable.R")
#source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_bloods.R")
source("final_cleaning_scripts/load_and_clean_aetiol.R")

aetiol.m <- dcast(aetiol, pid + hivstatus ~ type, value.var = "pathogen")
aetiol.m$tb <- aetiol.m$`mtb bsi`== 1 | aetiol.m$uLAM == 1 | aetiol.m$Xpert == 1
aetiol.m -> a
a[is.na(a)] <- 0
a$no_diagnosis <- as.numeric(apply(a[3:8], 1,sum) == 0)

melt(select(a, pid, hivstatus, tb, bc, malaria, csf, no_diagnosis), id.vars = c("pid", "hivstatus")) %>% dplyr::group_by(variable) %>% dplyr::summarise(n = dplyr::n(), positive = sum(value == 1)) -> t


apply(t[2:3],1, function(x) paste0(x[2], "/",  x[1], " (",
                                   format(round((x[2]*100/x[1]), 0), nsmall = 0),
                                   "% [",
                                   format(round(binom.test(x[2],x[1])$conf.int[[1]] * 100, 0), nsmall = 0),
                                   "-",
                                   format(round(binom.test(x[2],x[1])$conf.int[[2]] * 100, 0), nsmall = 0),
                                   "%])"
)) -> t$prop

t$variable <- c("Tuberculosis", "Bloodstream infection", "Malaria", "Meningitis", "No diagnosis")

### load samples

samples <- read.csv("/Users/joelewis/Documents/PhD/Shipping/PHE/20190122_manifest.csv", stringsAsFactors = FALSE)

##
pid.lookup <- lims.lookup %>% dplyr::select(lab_id.raw,pid.raw, pid.corrected) %>% unique

names(pid.lookup) <- c("lab_id", "pid", "pid.corrected")

merge(samples, pid.lookup, all.x = TRUE) -> samples

samples$pid.corrected[is.na(samples$pid.corrected)] <- samples$pid[is.na(samples$pid.corrected)] 

samples %>% 
  dplyr::select(pid.corrected, sample, lab_id) %>% 
  pivot_wider( names_from = sample, 
               values_from = lab_id
               ) -> samples.wide


merge(samples.wide,a, by.x = "pid.corrected", by.y = "pid") -> df

df$no_diagnosis_except_malaria <- df$bc + df$csf + df$tb
df$no_diagnosis_except_malaria[df$no_diagnosis_except_malaria == 0] <- -1
df$no_diagnosis_except_malaria[df$no_diagnosis_except_malaria > 0] <- 0
df$no_diagnosis_except_malaria[df$no_diagnosis_except_malaria == -1] <- 1
df$tb.alone <- as.numeric(df$tb == 1 & df$bc == 0 & df$csf == 0 & df$malaria == 0)

tb.random.samples <- sample(df$pid.corrected[df$tb.alone == TRUE], 14,replace = FALSE)
tb.random.samples <- data.frame(pid.corrected = tb.random.samples, tb.random.sample = rep("YES", length(tb.random.samples)), stringsAsFactors = FALSE)

df <- merge(df,tb.random.samples, all.x = TRUE)
df$tb.random.sample[is.na(df$tb.random.sample)] <- "NO"

df$for_serology <- ifelse(df$no_diagnosis_except_malaria == 1 | df$tb.random.sample == "YES", "YES", "NO")

array.card.random.sample <- sample(df$pid.corrected[df$for_serology == "YES"], 120,replace = FALSE)
array.card.random.sample <- data.frame(pid.corrected = array.card.random.sample, for_array_card = rep("YES", length(array.card.random.sample)), stringsAsFactors = FALSE)

df <- merge(df,array.card.random.sample, all.x = TRUE)
df$for_array_card[is.na(df$for_array_card)] <- "NO"

df <- dplyr::select(df,
                    pid.corrected,
                    urine,
                    `day 0 serum`,
                    `day 28 serum`,
                    for_serology,
                    for_array_card,
                    hivstatus,
                    bc,
                    csf,
                    malaria,
                    `mtb bsi`,
                    uLAM,
                    Xpert,
                    tb
                    )

# sample 10

write.csv(df,"/Users/joelewis/Documents/PhD/Shipping/PHE/PHE_samples_to_test.csv", row.names = FALSE)
