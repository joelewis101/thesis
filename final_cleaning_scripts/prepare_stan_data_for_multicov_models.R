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


select(lims_dates, pid, lab_id, sample_type,ESBL, assess_type) %>% group_by(pid) %>% do(splice_ESBL2(df.covs,. )) ->spliced

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
 
write.csv(stan.df,"data/stan_df.csv")

# make lists and save .rdas for each of the four models

# model 1

stan.df$p0 <- as.numeric(stan.df$ESBL_start == 0)
stan.df$p1 <- as.numeric(stan.df$ESBL_start == 1)

stan_data_m1 <- list(N = nrow(stan.df), t = stan.df$tstop,
                     start_state = as.matrix(stan.df[c("p0", "p1")]),
                     end_state = as.numeric(stan.df$ESBL_stop), 
                     covariates = stan.df[c("abx_start_time", "abx_end_time",
                                            "hosp_start_time", "hosp_end_time")])

stan_data_m1$covariates[stan_data_m1$covariates == -999] <- 999

saveRDS(stan_data_m1, "chapter_9/stan_models/model_1/stan_data_m1.rds")

# model 2

stan_data_m2 <- list(N = nrow(stan.df), t = stan.df$tstop,
                     n_covs = c(0,1,1), covs_type = c(3,2),
                     start_state = as.matrix(stan.df[c("p0", "p1")]),
                     end_state = as.numeric(stan.df$ESBL_stop), 
                     cov_mat = stan.df[c("abx_start_time", "abx_end_time","prev_abx_stop_time",
                                            "hosp_start_time", "hosp_end_time", "prev_hosp_stop_time")])


saveRDS(stan_data_m2, "chapter_9/stan_models/model_2/stan_data_m2.rds")

# model 3

stan_data_m3 <- list(N = nrow(stan.df), t = stan.df$tstop,
                     n_covs = c(0,3,1), covs_type = c(3,2,2,2),
                     start_state = as.matrix(stan.df[c("p0", "p1")]),
                     end_state = as.numeric(stan.df$ESBL_stop), 
                     cov_mat = stan.df[c("abx.2_start_time", "abx.2_end_time","prev_abx.2_stop_time",
                                         "cotri_start_time", "cotri_end_time","prev_cotri_stop_time",
                                         "tb_start_time", "tb_end_time","prev_tb_stop_time",
                                         "hosp_start_time", "hosp_end_time", "prev_hosp_stop_time")])

saveRDS(stan_data_m3, "chapter_9/stan_models/model_3/stan_data_m3.rds")

# model 4

stan_data_m4 <- list(N = nrow(stan.df), t = stan.df$tstop,
                     n_covs = c(0,3,3), covs_type = c(3,3,3,2,2,2),
                     start_state = as.matrix(stan.df[c("p0", "p1")]),
                     end_state = as.numeric(stan.df$ESBL_stop), 
                     cov_mat = stan.df[c("cefo_start_time", "cefo_end_time","prev_cefo_stop_time",
                                         "cipro_start_time", "cipro_end_time","prev_cipro_stop_time",
                                         "amoxy_start_time", "amoxy_end_time","prev_amoxy_stop_time",
                                         "cotri_start_time", "cotri_end_time","prev_cotri_stop_time",
                                         "tb_start_time", "tb_end_time","prev_tb_stop_time",
                                         "hosp_start_time", "hosp_end_time", "prev_hosp_stop_time")])

saveRDS(stan_data_m4, "chapter_9/stan_models/model_3/stan_data_m4.rds")






