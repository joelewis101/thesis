
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



select(lims_dates, pid, lab_id, sample_type,ESBL, assess_type) %>% group_by(pid) %>% do(splice_ESBL2(df.covs.cpt,. )) ->spliced

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
  mutate(blact = sum(amoxy, benzy, cefo, coamo, fluclox)) %>% ungroup() %>%
  mutate(blact = case_when(abx > 0 ~ 1,
                         abx == 0 ~ 0)) -> spliced

spliced  %>% group_by(pid) %>% do(generate_stan_df(., "ESBL", c("hosp", "cotri", "cefo", "amoxy", "cipro", "tb"))) -> stan.df

# zero times
stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] <- 
  stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] - stan.df$tstart

stan.df[is.na(stan.df)] <- 999
stan.df$ab_this_step <- 0
stan.df$ab_this_step[stan.df$abx_start_time != 999] <- 1
write.csv(stan.df, "chapter_9/stan_df.csv")


as.data.frame(
  subset(stan.df, pid == sample(unique(stan.df$pid), 1))
)

# zero times


stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] <- 
  stan.df[c("tstart", "tstop", grep("_time", names(stan.df),value = TRUE ))] - stan.df$tstart

