
library(plyr)
library(reshape2)
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

# get ESBL data into a format where there is a row for each change in status

# tehen make some extractor functions to give number of days of ab etc
source("final_cleaning_scripts/generate_visits_df.R")
source("final_cleaning_scripts/load_and_clean_followup_and_enroll_labelled.R")
source("other_scripts/summary_table_functions.R")
source("final_cleaning_scripts/make_composite_hivstatus_variable.R")
source("final_cleaning_scripts/load_and_clean_hourly.R")
#source("final_cleaning_scripts/load_and_clean_bloods.R")
#source("final_cleaning_scripts/load_and_clean_aetiol.R")
#source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_upto72.R")
source("final_cleaning_scripts/load_and_clean_post72.R")
source("final_cleaning_scripts/load_and_clean_hosp_oc.R")

fum <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_followup_micro_raw.csv", stringsAsFactors = FALSE)

#functions
source("other_scripts/panel_data_helpers/expand_covariates.R")
source("other_scripts/panel_data_helpers/sort_out_tb_rx_on_discharge.R")
source("other_scripts/panel_data_helpers/shuffle_a_in_b2.R")
source("other_scripts/panel_data_helpers/strip_post_dropout_rows.R")
source("other_scripts/panel_data_helpers/extract_covariate_exposure.R")
source("other_scripts/panel_data_helpers/collapse_covariates.R")

recode_abz <- function(upto72) {
  upto72[sapply(upto72, function(x) grepl("AMOX", x))] <- "amoxy"
  upto72[sapply(upto72, function(x) grepl("ARTE", x))] <- "arte"
  upto72[sapply(upto72, function(x) grepl("AUG", x))] <- "coamo"
  upto72[sapply(upto72, function(x) (grepl("CEFT", x) | grepl("CET", x)))] <- "cefo"
  upto72[sapply(upto72, function(x) grepl("CIPRO", x))] <- "cipro"
  upto72[sapply(upto72, function(x) grepl("COTRIM", x))] <- "cotri"
  upto72[sapply(upto72, function(x) grepl("FLUCON", x))] <- "fluco"
  upto72[sapply(upto72, function(x) grepl("METRO", x))] <- "metro"
  upto72[sapply(upto72, function(x) grepl("TB", x))] <- "tb"
  upto72[sapply(upto72, function(x) grepl("PENICI", x))] <- "benzy"
  upto72[sapply(upto72, function(x) grepl("DOXY", x))] <- "doxy"
  upto72[sapply(upto72, function(x) grepl("ERYTH", x))] <- "erythro"
  upto72[sapply(upto72, function(x) grepl("FLUCLOX", x))] <- "fluclox"
  upto72[sapply(upto72, function(x) grepl("LA", x))] <- "coart"
  upto72[sapply(upto72, function(x) grepl("AMPHO", x))] <- "ampho"
  upto72[sapply(upto72, function(x) grepl("GENT", x))] <- "genta"
  upto72[sapply(upto72, function(x) grepl("NYSTATIN", x))] <- NA
  upto72[sapply(upto72, function(x) grepl("ALBENDAZ", x))] <- NA
  return(upto72)
}


names(enroll)[names(enroll) == "data_date"] <- "enroll_date"
followup <- merge(followup, select(enroll, pid, arm, enroll_date), all.x = T)
followup$t <- followup$data_date - followup$enroll_date

enroll <- filter(enroll, arm != 4)

# use the post72 ab categories
# first check for other meds

table(post72$othermed1)
post72$acicl <- 0
post72$acicl[grepl("ACYCLO", post72$othermed1)] <- 1
post72$fluclox <- 0
post72$fluclox[grepl("FLUCLOX", post72$othermed1)] <- 1
post72$metro <- 0
post72$metro[grepl("METRO", post72$othermed1)] <- 1
post72$strepto <- 0
post72$strepto[grepl("STREPTO", post72$othermed1)] <- 1
post72$strepto <- 0
post72$strepto[grepl("STREPTO", post72$othermed1)] <- 1
post72$ampho[grepl("AMPHO", post72$strepto)] <- 1
# nothing in amicro2 or 3

post72 <- dplyr::select(post72, -c(pid_init, staff_init, admit_date, assess_date, assess_time, 
                                   othermed1, othermed2, othermed3, othermed4, row_id, ufid, form_id,
                                   time_stamp, batchno, project_dsid,othermed))
tempstr <- c("pid", "enroll_date", "data_date", "assess_type", "died", "discharged")

post72 <- post72[ c(tempstr,
                   names(post72)[!(names(post72) %in% tempstr)])]

as.numeric(post72$fluco) ->post72$fluco

ab_var_names <- names(post72)[!(names(post72) %in% tempstr)]

## pre 72hr - get into same schape

upto72 <- select(upto72, pid, assess_type, died, discharged, amicro1, amicro2,amicro3,amicro4)

upto72 <- recode_abz(upto72)


upto72 %>% unique %>% pivot_longer(-c(pid, assess_type, died, discharged)) %>%
  filter(!is.na(value)) %>% select(-name) %>% pivot_wider(names_from = value,
                                                          values_fn = list(value = length) )  -> upto72

df <- bind_rows(upto72, post72)

# add in hourly my man
hourly[hourly == ""] <- NA
hourly <- select(hourly, pid, amicro1, amicro2, amicro3, amicro4, amicro5,amicro6)
hourly$assess_type <- 0
hourly <- recode_abz(hourly)
hourly$amicro3 <- as.character(hourly$amicro3)
hourly %>% 
  pivot_longer(-c(assess_type, pid)) %>%
  filter(!is.na(value)) %>% select(-name) %>%
  pivot_wider(names_from = value, values_fn = list(value = length)) -> hourly

bind_rows(df, hourly) -> df
df$hosp <- 1
df <- select(df, -discharged)

#check for doobles:
df %>% group_by(assess_type, pid ) %>% tally() %>% filter(n > 1)
# none

# expand hosp outcome and add in

outcome <- oc

outcome <- merge(outcome, select(enroll, pid, enroll_date), all.x = T)
outcome <- select(outcome, -data_date)
names(outcome)[names(outcome) == "hospoutcomedate"] <- "data_date"
outcome$data_date <- as.Date(outcome$data_date, format = "%d%b%Y")
#outcome$enroll_date <- as.Date(outcome$enroll_date)
outcome$assess_type <- as.numeric(outcome$data_date - outcome$enroll_date)


# ok let's modify outcome so covariate names match

# change names of oitcome to match other covariates

names(outcome)[names(outcome) == "amox"] <- "amoxy"
names(outcome)[names(outcome) == "coamox"] <- "coamo"
names(outcome)[names(outcome) == "fluclo"] <- "fluclox"
names(outcome)[names(outcome) == "penic"] <- "benzy"

outcome$metro <- 0

# which colnames are not in outcome? Add em

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
outcome %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> outcome
rm(col_add)

### sort out ab length times

outcome$tb[grepl("TB", outcome$other_q)] <- outcome$other_sdays[grepl("TB", outcome$other_q)]
outcome$other_q[grepl("TB", outcome$other_q)] <- NA

outcome$tb[grepl("Tb", outcome$other_q)] <- outcome$other_sdays[grepl("Tb", outcome$other_q)]
outcome$other_q[grepl("Tb", outcome$other_q)] <- NA

outcome$tb[grepl("RHZE", outcome$other_q)] <- outcome$other_sdays[grepl("RHZE", outcome$other_q)]
outcome$other_q[grepl("RHZE", outcome$other_q)] <- NA

outcome$acicl[grepl("Acyclovir", outcome$other_q)] <- outcome$other_sdays[grepl("Acyclovir", outcome$other_q)]
outcome$other_q[grepl("Acyclovir", outcome$other_q)] <- NA

outcome$coamo[grepl("Algumentine", outcome$other_q )] <- outcome$other_sdays[grepl("Algumentine", outcome$other_q )]
outcome$coamo[grepl("Augmentin", outcome$other_q)] <- outcome$other_sdays[grepl("Augmentin", outcome$other_q)]
outcome$other_q[grepl("Algumentine", outcome$other_q )] <- NA
outcome$other_q[grepl("Augmentin", outcome$other_q)] <- NA

outcome$coart[grepl("LA", outcome$other_q)] <- outcome$other_sdays[grepl("LA", outcome$other_q)]
outcome$other_q[grepl("LA", outcome$other_q)] <- NA

outcome$cipro[grepl("Cipro", outcome$other_q)] <- outcome$other_sdays[grepl("Cipro", outcome$other_q)]
outcome$other_q[grepl("Cipro", outcome$other_q)] <- NA

outcome$metro[grepl("Metro", outcome$other_q)] <- outcome$other_sdays[grepl("Metro", outcome$other_q)]
outcome$other_q[grepl("Metro", outcome$other_q)] <- NA

# should be nothing here
table(outcome$other_q)


outcome$amoxy[!(is.na(outcome$amoxdays))] <- outcome$amoxdays[!(is.na(outcome$amoxdays))] 
outcome$azithro[!(is.na(outcome$azithronodays))] <- outcome$azithronodays[!(is.na(outcome$azithronodays))] 
outcome$cipro[!(is.na(outcome$cipronodays))] <- outcome$cipronodays[!(is.na(outcome$cipronodays))] 
outcome$cotri[!(is.na(outcome$cotridays))] <- outcome$cotridays[!(is.na(outcome$cotridays))]
outcome$cotri[!(is.na(outcome$cotridays))] <- outcome$cotridays[!(is.na(outcome$cotridays))]
outcome$coamo[!(is.na(outcome$coamoxdays))] <- outcome$coamoxdays[!(is.na(outcome$coamoxdays))]
outcome$doxy[!(is.na(outcome$doxydays))] <- outcome$doxydays[!(is.na(outcome$doxydays))]
outcome$erythro[!(is.na(outcome$erythrodays))] <- outcome$erythrodays[!(is.na(outcome$erythrodays))]
outcome$fluclox[!(is.na(outcome$fluclodays))] <- outcome$fluclodays[!(is.na(outcome$fluclodays))]
outcome$benzy[!(is.na(outcome$penicdays))] <- outcome$penicdays[!(is.na(outcome$penicdays))]
outcome$fluco[!(is.na(outcome$flucodays))] <- outcome$flucodays[!(is.na(outcome$flucodays))]

outcome$hosp <- 1
outcome <- select(outcome, pid, enroll_date, data_date, assess_type,
                  names(outcome)[names(outcome) %in% ab_var_names], hosp, hospoutcome)

outcome$hospoutcome[outcome$hospoutcome !=3] <- 0
outcome$hospoutcome[outcome$hospoutcome ==3] <- 1
names(outcome)[names(outcome) == "hospoutcome"] <- "died"
outcome[is.na(outcome)] <- 0
outcome[4:ncol(outcome)] <- sapply(outcome[4:ncol(outcome)], as.numeric)

# before we expand outcome we need to expand TB rx length to greater than 30 or 60 days (all that the tab would allow)

outcome <- merge(outcome, select(enroll, pid, tbongoing, tbrxstart), by = "pid", all.x = T)
outcome$tbrxstart <- as.Date(outcome$tbrxstart, "%d%b%Y")


outcome$tbongoing <- as.numeric(outcome$tbongoing == "Yes")

# ### bosh
### Gotta love the pipe

outcome %>% group_by(pid) %>%
  do(sort_out_tb_rx_on_discharge(.,df)) %>%
  do(expand_covariates(.,5,27))  -> expanded.outcome

expanded.outcome <- select(expanded.outcome, -tbongoing, -tbrxstart)

#merge(expanded.outcome, select(enroll, pid, arm)) -> expanded.outcome

### shuffles those bad boys together

expanded.outcome <- select(expanded.outcome, -c(enroll_date, data_date))
df <- select(df, -c(enroll_date, data_date))

expanded.outcome[names(df)] -> expanded.outcome
identical(names(expanded.outcome), names(df))

df[is.na(df)] <- 0

# just changed this
merge_longit_dfs(df,expanded.outcome, 3,26, ditch_dfa_after_dc = TRUE) -> out

## now add in followup

# pull out abx from f micro and add each in

names(followup)[names(followup) == "t"] <- "assess_type"

fu <- followup
lookup <- data.frame(names(fu[,29:49]))
names(lookup)[1] <- "antibiotic"
lookup$micro_n <- 11:31
lookup$antibiotic <- as.character(lookup$antibiotic)

fum$data_date <- as.Date(fum$data_date, "%d-%b-%y")
fum$d2visitnnewrxstart <- as.Date(fum$d2visitnnewrxstart, "%d/%m/%Y")
fum <- merge(fum, select(enroll, pid, enroll_date))
fum$assess_type <- as.numeric(fum$d2visitnnewrxstart - fum$enroll_date)

lookup$antibiotic[lookup$antibiotic == "amoxily"] <- "amoxy"
lookup$antibiotic[lookup$antibiotic == "ceft"] <- "cefo"
lookup$antibiotic[lookup$antibiotic == "co_amo"] <- "coamo"
lookup$antibiotic[lookup$antibiotic == "cotrim"] <- "cotri"
lookup$antibiotic[lookup$antibiotic == "quinine"] <- "quin"
lookup$antibiotic[lookup$antibiotic == "coartem"] <- "coart"
lookup$antibiotic[lookup$antibiotic == "artesu"] <- "arte"
lookup$antibiotic[lookup$antibiotic == "chrola"] <- "chlora"

merge(fum, lookup) -> fum
merge(fum, select(fu, pid, data_date,antim_other), by = c("pid", "data_date"), all.x = T) -> fum


fum$antibiotic[fum$antibiotic == "otherab"] <- fum$antim_other[fum$antibiotic == "otherab"]
fum$antibiotic[grepl("Acyclovir", fum$antibiotic)] <- "acicl"
fum$antibiotic[grepl("Algmentine", fum$antibiotic)] <- "coamo"
fum$antibiotic[grepl("Algumentine", fum$antibiotic)] <- "coamo"
fum$antibiotic[grepl("Augmentin", fum$antibiotic)] <- "coamo"
fum$antibiotic[grepl("Augumentin", fum$antibiotic)] <- "coamo"
fum$antibiotic[grepl("Metro", fum$antibiotic)] <- "metro"
fum$antibiotic[grepl("RHZE", fum$antibiotic)] <- "tb"
fum$antibiotic[grepl("TB", fum$antibiotic)] <- "tb"
fum$antibiotic[grepl("Tb", fum$antibiotic)] <- "tb"

fum <- filter(fum, d2visitnnewrxwhy != "CAC14R")

# add back in tb rx form fu

fu$d2visittbstart <- as.Date(fu$d2visittbstart, "%d%b%Y")
tb <- subset(fu, tbstart == 1)
tb$assess_type <- as.numeric(tb$d2visittbstart - tb$enroll_date)
tb$antinum <- 180
tb$antibiotic <- "tb"

# i've checkd the neg ones
# they both are captured in the hosp data
fum <- filter(fum, assess_type >= 0)


fum$antinum[fu$antifreq == 2] <- fum$antinum[fu$antifreq == 2] * 7
fum$antinum[fu$antifreq == 3] <- fum$antinum[fu$antifreq == 3] * 28

fum$antinum[fum$ongmed == 1 & fum$antibiotic != "tb"] <- 5
fum$antinum[fum$ongmed == 1 & fum$antibiotic == "tb"] <- 180

# drop tb where tb is already in

dropem <- merge(tb, fum, by = c("pid", "assess_type"))$pid

tb <- subset(tb, !(pid %in% dropem))
rm(dropem)
fum <- bind_rows(fum, select(tb, pid, assess_type, antibiotic, antinum))


fum %>% unique %>% select(pid, assess_type, antibiotic, antinum ) %>%
  pivot_wider(names_from = antibiotic, values_from = antinum) -> fum

#fum[3:ncol(fum)][fum[3:ncol(fum)] > 1] <- 1

fum$died <- 0
fum$hosp <- 0

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
fum %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> fum
rm(col_add)

fum[is.na(fum)] <- 0

fum[names(out)] -> fum

#just changed this
rowwise_expand_and_shuffle_a_in_b2(fum, out,3, 26) -> out.2

# now pull out hospitalisation and add in

hosp <- subset(fu, d2visithosadm == 1)
hosp <- select(hosp, pid, enroll_date, d2visitnhospadmwhenadm, d2visitnhospaadmwhendisc)
names(hosp)[names(hosp) == "d2visitnhospadmwhenadm"] <- "adm_date"
hosp$adm_date <- as.Date(hosp$adm_date, "%d%b%Y")
names(hosp)[names(hosp) == "d2visitnhospaadmwhendisc"] <- "dc_date"
hosp$dc_date <- as.Date(hosp$dc_date, "%d%b%Y")

hosp$dc_date[is.na(hosp$dc_date)] <- hosp$adm_date[is.na(hosp$dc_date)] + 5
hosp$hosp <-  as.numeric(hosp$dc_date - hosp$adm_date)
hosp$hosp[hosp$hosp == 0] <- 1

hosp$assess_type <- as.numeric(hosp$adm_date - hosp$enroll_date)

hosp <- select(hosp, pid, hosp, assess_type, hosp)
hosp$died <- 0

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
hosp %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> hosp
rm(col_add)

hosp[names(out.2)] -> hosp

# just changed this
rowwise_expand_and_shuffle_a_in_b2(hosp, out.2,3,26) -> out

## add censor/death dates from visits df
vb <- visits

visits$t <- suppressWarnings(apply(visits[7:10], 1,max, na.rm= T ))

visits$t[!is.na(visits$died_date)] <- visits$died_date[!is.na(visits$died_date)]

censor <- select(visits, pid, status, t)
censor$status[censor$status == 0] <- 2

names(censor)[2:3] <- c("died", "assess_type")
censor$hosp <- 0

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
censor %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> censor
rm(col_add)

censor[names(out)] -> censor

censor$died <- as.numeric(censor$died)
censor <- select(censor, pid, assess_type, died, hosp, ab_var_names)
out <- select(out, pid, assess_type, died, hosp, ab_var_names)
rowwise_expand_and_shuffle_a_in_b2(censor, out,3,26) -> out.2

# add arm and strip out a4s

out.2 <- merge(out.2, select(enroll, pid, arm), all.x = T)

subset(out.2, !is.na(arm)) -> out.2
out.2 <- select(out.2, -arm)

# add in d0 hosp = 1 for all arm 1 and 2

d0add <- enroll %>% filter(arm ==1 | arm == 2) %>% select(pid)

d0add$assess_type <- 0
d0add$hosp <- 1
d0add$died <- 0

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
d0add %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> d0add
rm(col_add)

d0add[names(out.2)] -> d0add

rowwise_expand_and_shuffle_a_in_b2(d0add, out.2,3,26) -> out.2

## check dat ting

as.data.frame(subset(out.2, pid == sample(unique(out.2$pid),1)))

# add arm3 day 0

a3.add <- enroll %>% filter(arm == 3) %>% select(pid)

a3.add$assess_type <- 0
a3.add$hosp <- 0
a3.add$died <- 0

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
a3.add %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> a3.add
rm(col_add)

a3.add[names(out)] -> a3.add

rowwise_expand_and_shuffle_a_in_b2(a3.add, out.2,3,26) -> out.2


# add in tb therapy at baseline

tb.bl <- enroll %>% filter(tbongoing == "Yes") %>% select(pid, arm, tbrxstart, tbrxend, enroll_date)

tb.bl$tbrxstart <- as.Date(tb.bl$tbrxstart, "%d%b%Y")
tb.bl$tb <- as.numeric(tb.bl$enroll_date - tb.bl$tbrxstart)
tb.bl <- filter(tb.bl, tb < 180)
tb.bl$tb <- 180 - tb.bl$tb
tb.bl$died <- 0
tb.bl$hosp <- 0

col_add <- rep(0, length(ab_var_names))
names(col_add) <- ab_var_names
tb.bl %>% add_column(!!!col_add[!names(col_add) %in% names(.)]) -> tb.bl
rm(col_add)

tb.bl$assess_type <- 0
tb.bl[names(out.2)] -> tb.bl

rowwise_expand_and_shuffle_a_in_b2(tb.bl, out.2,3,26) -> out



### strip rows post death

out %>% filter(assess_type >= 0) %>% group_by(pid) %>% do(strip_post_dropout_rows(.)) -> out

####


write.csv(out,"data/longit_covariate_data.csv", row.names = FALSE)
