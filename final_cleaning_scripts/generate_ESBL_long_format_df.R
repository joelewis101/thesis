
# get ESBL data into a format where there is a row for each change in status

# tehen make some extractor functions to give number of days of ab etc

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

#functions
source("other_scripts/panel_data_helpers/expand_covariates.R")
source("other_scripts/panel_data_helpers/sort_out_tb_rx_on_discharge.R")
source("other_scripts/panel_data_helpers/shuffle_a_in_b2.R")

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

merge_longit_dfs(df,expanded.outcome, 5,26, ditch_dfa_after_dc = TRUE) -> out

## now add in followup
