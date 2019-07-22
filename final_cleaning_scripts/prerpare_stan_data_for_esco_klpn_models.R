## prepare stan data for ESBL and KLPN models

# set up stan data for real models with mESBLmod_finalV1.0_rk45.stan

# model 1

# ESBL ~ ABx + hosp no decay
# this will use old code and the data passed is slightly different
# where abx includeas all abx and tb rx

# model 2
# ESBL ~ Abx + hosp, exp decay

# model 3 
# ESBL~ tb + cotri + hosp + abx 
# where abx includes all abx except cotri and tb
# cotri and tb with no decay, abx with

# model 4 
# ESBL + tb + cotri + hosp + (cipro + cefo + amoxy)
# those in () have decay, the others not

# use model 2!

### splice in ESBL results to the covariates df
#nerate figures
#source("chapter_5/make_consort_diagram.R")
#source("final_cleaning_scripts/generate_visits_df.R")
source("final_cleaning_scripts/load_and_clean_followup_and_enroll_labelled.R")
source("other_scripts/summary_table_functions.R")
source("final_cleaning_scripts/make_composite_hivstatus_variable.R")
source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_bloods.R")
#source("final_cleaning_scripts/load_and_clean_aetiol.R")
#source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_upto72.R")
source("final_cleaning_scripts/load_and_clean_post72.R")
source("final_cleaning_scripts/load_and_clean_hosp_oc.R")
#source("final_cleaning_scripts/load_and_clean_time_to_ab.R")
#source("final_cleaning_scripts/load_and_clean_fluid_hr1_to_6.R")
source("other_scripts/stan_helpers/arrange_stan_df_functions.R")

# lims




#panel data helper functions
source("other_scripts/panel_data_helpers/expand_covariates.R")
source("other_scripts/panel_data_helpers/sort_out_tb_rx_on_discharge.R")
source("other_scripts/panel_data_helpers/shuffle_a_in_b2.R")
source("other_scripts/panel_data_helpers/strip_post_dropout_rows.R")
source("other_scripts/panel_data_helpers/extract_covariate_exposure.R")
source("other_scripts/panel_data_helpers/collapse_covariates.R")
source("other_scripts/panel_data_helpers/ditch_everything_after_first_1.R")
source("other_scripts/panel_data_helpers/mstate_helper_functions.R")
source("other_scripts/panel_data_helpers/splice_ESBL2.R")

names(enroll)[names(enroll) == "data_date"] <- "enroll_date"
followup <- merge(followup, select(enroll, pid, arm, enroll_date), all.x = T)
followup$t <- followup$data_date - followup$enroll_date

# and load lims
source("final_cleaning_scripts/load_and_clean_lims.R")


read.csv("data/longit_covariate_data.csv") -> df.covs

nrow(lims_orgs)
lims_orgs.esco <- subset(lims_orgs, organism == "Escherichia coli")

# check for double lims_id
lims_orgs.esco %>% group_by(lab_id) %>% tally() %>% filter(n > 1)

# none ! good

lims_dates.esco <- merge(lims_dates, select(lims_orgs.esco, lab_id, organism), all.x = T)

lims_dates.esco$ESBL[lims_dates.esco$organism == "Escherichia coli"] <- "Positive"
lims_dates.esco$ESBL[is.na(lims_dates.esco$organism)] <- "Negative"



select(lims_dates.esco, pid, lab_id, sample_type,ESBL, assess_type) %>% group_by(pid) %>% do(splice_ESBL2(df.covs,. )) ->spliced

# add in cpt

spliced <- merge(spliced, select(enroll, pid, hivcpt), all.x = T)
spliced$hivcpt[is.na(spliced$hivcpt)] <- "No"
spliced$cotri[spliced$hivcpt == "Yes"] <- 1

spliced <- select(spliced, -hivcpt)

as.data.frame(
  subset(spliced, pid == sample(unique(spliced$pid), 1))
)

spliced$ESBL[spliced$ESBL == "Positive"] <- 1
spliced$ESBL[spliced$ESBL == "Negative"] <- 0
spliced$ESBL[is.na(spliced$ESBL)] <- 999

spliced %>%
  rowwise() %>%
  mutate(abx = sum(amoxy, genta, azithro, tb, benzy, cefo, chlora, cipro, coamo, clinda, doxy, erythro, fluclox, metro, strepto, cotri)) %>% ungroup() %>%
  mutate(abx = case_when(abx > 0 ~ 1,
                         abx == 0 ~ 0)) -> spliced

spliced %>%
  rowwise() %>%
  mutate(abx.2 = sum(amoxy, genta, azithro, benzy, cefo, chlora, cipro, coamo, clinda, doxy, erythro, fluclox, metro, strepto)) %>% ungroup() %>%
  mutate(abx.2 = case_when(abx.2 > 0 ~ 1,
                           abx.2 == 0 ~ 0)) -> spliced


### ok all looks good

spliced  %>% group_by(pid) %>% do(generate_stan_df(., "ESBL", c("cipro", "cefo", "amoxy", "abx", "abx.2", "hosp", "cotri", "tb" ))) -> stan.df

testpid <- stan.df$pid[sample.int(nrow(stan.df), size =1)]
#testpid <- "DAS1505S"

test <- as.data.frame(subset(spliced, pid == testpid))
long_to_wide_panel_states(test, "ESBL")
long_to_wide_panel_covariates(test, "cefo")
long_to_wide_panel_covariates(test, "cipro")
long_to_wide_panel_covariates(test, "hosp")
as.data.frame(subset(stan.df, pid == testpid))
test


# zero times
stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] <- 
  stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] - stan.df$tstart


#stan.df$ab_this_step <- 0
#stan.df$ab_this_step[stan.df$abx_start_time != 999] <- 1
#write.csv(stan.df, "chapter_9/stan_df.csv")


stan.df <- as.data.frame(stan.df)
stan.df[!grepl("_exposure", names(stan.df))] -> stan.df
stan.df[grepl("prev", names(stan.df))][is.na(stan.df[grepl("prev", names(stan.df))])] <- 999
stan.df[grepl("start", names(stan.df))][is.na(stan.df[grepl("start", names(stan.df))])] <- -999
stan.df[grepl("end", names(stan.df))][is.na(stan.df[grepl("end", names(stan.df))])] <- -999

write.csv(stan.df,"data/stan_df_esco.csv")


# model 2

stan.df$p0 <- as.numeric(stan.df$ESBL_start == 0)
stan.df$p1 <- as.numeric(stan.df$ESBL_start == 1)

stan_data_m2_esco <- list(N = nrow(stan.df), t = stan.df$tstop,
                     n_covs = c(0,1,1), covs_type = c(3,2),
                     start_state = as.matrix(stan.df[c("p0", "p1")]),
                     end_state = as.numeric(stan.df$ESBL_stop), 
                     cov_mat = stan.df[c("abx_start_time", "abx_end_time","prev_abx_stop_time",
                                         "hosp_start_time", "hosp_end_time", "prev_hosp_stop_time")])

saveRDS(stan_data_m2_esco, "chapter_9/stan_models/model_2_esco/stan_data_m2_esco.rds")


# and again for klpn

lims_orgs.klpn <- subset(lims_orgs, organism == "Klebsiella pneumoniae")

# check for double lims_id
lims_orgs.klpn %>% group_by(lab_id) %>% tally() %>% filter(n > 1)

# none ! good

lims_dates.klpn <- merge(lims_dates, select(lims_orgs.klpn, lab_id, organism), all.x = T)

lims_dates.klpn$ESBL[lims_dates.klpn$organism == "Klebsiella pneumoniae"] <- "Positive"
lims_dates.klpn$ESBL[is.na(lims_dates.klpn$organism)] <- "Negative"

select(lims_dates.klpn, pid, lab_id, sample_type,ESBL, assess_type) %>% group_by(pid) %>% do(splice_ESBL2(df.covs,. )) ->spliced

# add in cpt

spliced <- merge(spliced, select(enroll, pid, hivcpt), all.x = T)
spliced$hivcpt[is.na(spliced$hivcpt)] <- "No"
spliced$cotri[spliced$hivcpt == "Yes"] <- 1

spliced <- select(spliced, -hivcpt)

as.data.frame(
  subset(spliced, pid == sample(unique(spliced$pid), 1))
)

spliced$ESBL[spliced$ESBL == "Positive"] <- 1
spliced$ESBL[spliced$ESBL == "Negative"] <- 0
spliced$ESBL[is.na(spliced$ESBL)] <- 999

spliced %>%
  rowwise() %>%
  mutate(abx = sum(amoxy, genta, azithro, tb, benzy, cefo, chlora, cipro, coamo, clinda, doxy, erythro, fluclox, metro, strepto, cotri)) %>% ungroup() %>%
  mutate(abx = case_when(abx > 0 ~ 1,
                         abx == 0 ~ 0)) -> spliced

spliced %>%
  rowwise() %>%
  mutate(abx.2 = sum(amoxy, genta, azithro, benzy, cefo, chlora, cipro, coamo, clinda, doxy, erythro, fluclox, metro, strepto)) %>% ungroup() %>%
  mutate(abx.2 = case_when(abx.2 > 0 ~ 1,
                           abx.2 == 0 ~ 0)) -> spliced


### ok all looks good

spliced  %>% group_by(pid) %>% do(generate_stan_df(., "ESBL", c("cipro", "cefo", "amoxy", "abx", "abx.2", "hosp", "cotri", "tb" ))) -> stan.df

testpid <- stan.df$pid[sample.int(nrow(stan.df), size =1)]
#testpid <- "DAS1505S"

test <- as.data.frame(subset(spliced, pid == testpid))
long_to_wide_panel_states(test, "ESBL")
long_to_wide_panel_covariates(test, "cefo")
long_to_wide_panel_covariates(test, "cipro")
long_to_wide_panel_covariates(test, "hosp")
as.data.frame(subset(stan.df, pid == testpid))
test


# zero times
stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] <- 
  stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] - stan.df$tstart


#stan.df$ab_this_step <- 0
#stan.df$ab_this_step[stan.df$abx_start_time != 999] <- 1
#write.csv(stan.df, "chapter_9/stan_df.csv")


stan.df <- as.data.frame(stan.df)
stan.df[!grepl("_exposure", names(stan.df))] -> stan.df
stan.df[grepl("prev", names(stan.df))][is.na(stan.df[grepl("prev", names(stan.df))])] <- 999
stan.df[grepl("start", names(stan.df))][is.na(stan.df[grepl("start", names(stan.df))])] <- -999
stan.df[grepl("end", names(stan.df))][is.na(stan.df[grepl("end", names(stan.df))])] <- -999

write.csv(stan.df,"data/stan_df_klpn.csv")


# model 2

stan.df$p0 <- as.numeric(stan.df$ESBL_start == 0)
stan.df$p1 <- as.numeric(stan.df$ESBL_start == 1)

stan_data_m2_klpn <- list(N = nrow(stan.df), t = stan.df$tstop,
                          n_covs = c(0,1,1), covs_type = c(3,2),
                          start_state = as.matrix(stan.df[c("p0", "p1")]),
                          end_state = as.numeric(stan.df$ESBL_stop), 
                          cov_mat = stan.df[c("abx_start_time", "abx_end_time","prev_abx_stop_time",
                                              "hosp_start_time", "hosp_end_time", "prev_hosp_stop_time")])

saveRDS(stan_data_m2_klpn, "chapter_9/stan_models/model_2_klpn/stan_data_m2_klpn.rds")


