

prep_mod2_stan_data <- function(lims_dates, df.covs, enroll) {
  
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
  
#  testpid <- stan.df$pid[sample.int(nrow(stan.df), size =1)]
  #testpid <- "DAS1505S"
  
 # test <- as.data.frame(subset(spliced, pid == testpid))
#  long_to_wide_panel_states(test, "ESBL")
#  long_to_wide_panel_covariates(test, "cefo")
 # long_to_wide_panel_covariates(test, "cipro")
#  long_to_wide_panel_covariates(test, "hosp")
#  as.data.frame(subset(stan.df, pid == testpid))
 # test
  
  
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
  
  #write.csv(stan.df,"data/stan_df.csv")
  
  # make lists and save .rdas for each of the four models
  
  # model 1
  
  stan.df$p0 <- as.numeric(stan.df$ESBL_start == 0)
  stan.df$p1 <- as.numeric(stan.df$ESBL_start == 1)
  
  #stan_data_m1 <- list(N = nrow(stan.df), t = stan.df$tstop,
                   #    start_state = as.matrix(stan.df[c("p0", "p1")]),
                    #   end_state = as.numeric(stan.df$ESBL_stop), 
                    ##   covariates = stan.df[c("abx_start_time", "abx_end_time",
                      #                        "hosp_start_time", "hosp_end_time")])
  
  #stan_data_m1$covariates[stan_data_m1$covariates == -999] <- 999
  
  #saveRDS(stan_data_m1, "chapter_9/stan_models/model_1/stan_data_m1.rds")
  
  # model 2
  
  stan_data_m2 <- list(N = nrow(stan.df), t = stan.df$tstop,
                       n_covs = c(0,1,1), covs_type = c(3,2),
                       start_state = as.matrix(stan.df[c("p0", "p1")]),
                       end_state = as.numeric(stan.df$ESBL_stop), 
                       cov_mat = stan.df[c("abx_start_time", "abx_end_time","prev_abx_stop_time",
                                           "hosp_start_time", "hosp_end_time", "prev_hosp_stop_time")])
  return(list(stan_data_m2, stan.df))
  
}


source("other_scripts/load_metadata.R")
source("other_scripts/function_parse_cdhitest.R")

# file of WGS sample_ids

sample_ids

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

# get baps.cl

readRDS(paste0( "chapter_7/hierbaps/D2ESCO_core_snp_hbaps_clusts.rda")) -> hb.results

hbdf <- hb.results$partition.df
sub("_1#", "_1_",hbdf$Isolate) -> hbdf$Isolate
sub("_2#", "_2_",hbdf$Isolate) -> hbdf$Isolate

rownames(hbdf) <- hbdf$Isolate
names(hbdf)[2:3] <- c("BAPS.l1", "BAPS.l2")

hbdf$BAPS.l1 <- as.factor(hbdf$BAPS.l1)
hbdf$BAPS.l1 <- LETTERS[hbdf$BAPS.l1]
hbdf$BAPS.l2 <- as.factor(hbdf$BAPS.l2)

# get contig cl

fileses <- list.files(paste0("chapter_7/contig_cluster/results"))
fileses <- grep(".clstr", fileses, value = T)

out <- list()
for (i in 1:length(fileses)) {
  #strsplit(fileses[[i]], "\\.")[1][[1]][[1]]
  #print(gene)
  outtemp <- parse_cd_hit_est_output_to_df(
    paste0("chapter_7/contig_cluster/results/",fileses[i])) 
  outtemp$gene <- strsplit(fileses[[i]], "\\.")[1][[1]][[1]]
  out[[i]] <- outtemp
  
}

df <- do.call(rbind, out)

df$clust.id <- paste0(df$gene, ".", df$cluster)

# make

hbdf <- merge(hbdf, select(df, Lane, clust.id), by.x = "Isolate", by.y = "Lane", all.x = T, all.y = T)


hbdf$barcode <- paste0(hbdf$BAPS.l2, ".", hbdf$clust.id)

hbdf %>%
  group_by(barcode) %>%
  mutate(n = length(barcode)) %>%
  filter(n > 1) %>%
ggplot(aes(fct_rev(fct_infreq(barcode)))) + geom_bar() + coord_flip() + theme_bw() + theme(axis.text.y = element_text(size =6))

## set up datasets to fit model2 to top 6 (all those with > 10 pos samples)

# restrict to <= 14 June 2018 (date of last ESBL sample) 
# and ditch those samples that were sent but sequencing failed

sub("#", "_", sample_ids$Lane ) -> sample_ids$Lane

 merge(sample_ids, dplyr::select(hbdf, Isolate, barcode), by.x = "Lane", by.y = "Isolate", all.x = T, all.y = T) -> sample_ids
 
sample_ids$barcode[is.na(sample_ids$barcode)] <- "seq.fail"
 
lims_dates.crop <- subset(lims_dates, data_date <= "2018-06-14")

# get top barcodes

sample_ids %>% 
  dplyr::group_by(barcode) %>%
  dplyr::summarise(n = length(barcode)) %>%
  arrange(desc(n))

# 1

sids.9.ctxm15.62 <- subset(sample_ids, barcode == "9.ctxm15.62" | barcode == "seq.fail")
lims_9.ctxm15.62 <- merge(lims_dates.crop, select(sids.9.ctxm15.62, Supplier.Name, barcode), by.x = "lab_id", by.y = "Supplier.Name", all.x = TRUE, all.y = TRUE)
lims_9.ctxm15.62 <- subset(lims_9.ctxm15.62 , barcode != "seq.fail" | is.na(barcode) )
lims_9.ctxm15.62$ESBL[lims_9.ctxm15.62$barcode == "9.ctxm15.62"] <- "Positive"
lims_9.ctxm15.62$ESBL[is.na(lims_9.ctxm15.62$barcode)] <- "Negative"
prep_mod2_stan_data(lims_9.ctxm15.62, df.covs, enroll) -> stan_data_9.ctxm15.62

write.csv(stan_data_9.ctxm15.62[[2]], "chapter_9/stan_models/model_2_9_ctxm15_62/stan_data_9_ctxm15_62.csv", row.names = FALSE )

saveRDS(stan_data_9.ctxm15.62[[1]], "chapter_9/stan_models/model_2_9_ctxm15_62/stan_data_9_ctxm15_62.rda")

# 2


sids.23.ctxm15.57 <- subset(sample_ids, barcode == "23.ctxm15.57" | barcode == "seq.fail")
lims_23.ctxm15.57 <- merge(lims_dates.crop, select(sids.23.ctxm15.57, Supplier.Name, barcode), by.x = "lab_id", by.y = "Supplier.Name", all.x = TRUE, all.y = TRUE)
lims_23.ctxm15.57 <- subset(lims_23.ctxm15.57 , barcode != "seq.fail" | is.na(barcode) )
lims_23.ctxm15.57$ESBL[lims_23.ctxm15.57$barcode == "23.ctxm15.57"] <- "Positive"
lims_23.ctxm15.57$ESBL[is.na(lims_23.ctxm15.57$barcode)] <- "Negative"
prep_mod2_stan_data(lims_23.ctxm15.57, df.covs, enroll) -> stan_data_23.ctxm15.57
write.csv(stan_data_23.ctxm15.57[[2]], "chapter_9/stan_models/model_2_23_ctxm15_57/stan_data_23_ctxm15_57.csv", row.names = FALSE )

saveRDS(stan_data_23.ctxm15.57[[1]], "chapter_9/stan_models/model_2_23_ctxm15_57/stan_data_23_ctxm15_57.rda")

# 3 - 6.ctxm27.1

sids.6.ctxm27.1 <- subset(sample_ids, barcode == "6.ctxm27.1" | barcode == "seq.fail")
lims_6.ctxm27.1 <- merge(lims_dates.crop, select(sids.6.ctxm27.1, Supplier.Name, barcode), by.x = "lab_id", by.y = "Supplier.Name", all.x = TRUE, all.y = TRUE)
lims_6.ctxm27.1 <- subset(lims_6.ctxm27.1 , barcode != "seq.fail" | is.na(barcode) )
lims_6.ctxm27.1$ESBL[lims_6.ctxm27.1$barcode == "6.ctxm27.1"] <- "Positive"
lims_6.ctxm27.1$ESBL[is.na(lims_6.ctxm27.1$barcode)] <- "Negative"
prep_mod2_stan_data(lims_6.ctxm27.1, df.covs, enroll) -> stan_data_6.ctxm27.1
write.csv(stan_data_6.ctxm27.1[[2]], "chapter_9/stan_models/model_2_6_ctxm27_1/stan_data_6_ctxm27_1.csv", row.names = FALSE )

saveRDS(stan_data_6.ctxm27.1[[1]], "chapter_9/stan_models/model_2_6_ctxm27_1/stan_data_6_ctxm27_1.rda")

# 4 - 1.ctxm15.16

sids.1.ctxm15.16 <- subset(sample_ids, barcode == "1.ctxm15.16" | barcode == "seq.fail")
lims_1.ctxm15.16 <- merge(lims_dates.crop, select(sids.1.ctxm15.16, Supplier.Name, barcode), by.x = "lab_id", by.y = "Supplier.Name", all.x = TRUE, all.y = TRUE)
lims_1.ctxm15.16 <- subset(lims_1.ctxm15.16 , barcode != "seq.fail" | is.na(barcode) )
lims_1.ctxm15.16$ESBL[lims_1.ctxm15.16$barcode == "1.ctxm15.16"] <- "Positive"
lims_1.ctxm15.16$ESBL[is.na(lims_1.ctxm15.16$barcode)] <- "Negative"
prep_mod2_stan_data(lims_1.ctxm15.16, df.covs, enroll) -> stan_data_1.ctxm15.16
write.csv(stan_data_1.ctxm15.16[[2]], "chapter_9/stan_models/model_2_1_ctxm15_16/stan_data_1_ctxm15_16.csv", row.names = FALSE )

saveRDS(stan_data_1.ctxm15.16[[1]], "chapter_9/stan_models/model_2_1_ctxm15_16/stan_data_1_ctxm15_16.rda")

# 5 - 39.ctxm15.22 

sids.39.ctxm15.22 <- subset(sample_ids, barcode == "39.ctxm15.22" | barcode == "seq.fail")
lims_39.ctxm15.22 <- merge(lims_dates.crop, select(sids.39.ctxm15.22, Supplier.Name, barcode), by.x = "lab_id", by.y = "Supplier.Name", all.x = TRUE, all.y = TRUE)
lims_39.ctxm15.22 <- subset(lims_39.ctxm15.22 , barcode != "seq.fail" | is.na(barcode) )
lims_39.ctxm15.22$ESBL[lims_39.ctxm15.22$barcode == "39.ctxm15.22"] <- "Positive"
lims_39.ctxm15.22$ESBL[is.na(lims_39.ctxm15.22$barcode)] <- "Negative"
prep_mod2_stan_data(lims_39.ctxm15.22, df.covs, enroll) -> stan_data_39.ctxm15.22
write.csv(stan_data_39.ctxm15.22[[2]], "chapter_9/stan_models/model_2_39_ctxm15_22/stan_data_39_ctxm15_22.csv", row.names = FALSE )

saveRDS(stan_data_39.ctxm15.22[[1]], "chapter_9/stan_models/model_2_39_ctxm15_22/stan_data_39_ctxm15_22.rda")

## 6 8.ctxm27.1

sids.8.ctxm27.1 <- subset(sample_ids, barcode == "8.ctxm27.1" | barcode == "seq.fail")
lims_8.ctxm27.1 <- merge(lims_dates.crop, select(sids.8.ctxm27.1, Supplier.Name, barcode), by.x = "lab_id", by.y = "Supplier.Name", all.x = TRUE, all.y = TRUE)
lims_8.ctxm27.1 <- subset(lims_8.ctxm27.1 , barcode != "seq.fail" | is.na(barcode) )
lims_8.ctxm27.1$ESBL[lims_8.ctxm27.1$barcode == "8.ctxm27.1"] <- "Positive"
lims_8.ctxm27.1$ESBL[is.na(lims_8.ctxm27.1$barcode)] <- "Negative"
prep_mod2_stan_data(lims_8.ctxm27.1, df.covs, enroll) -> stan_data_8.ctxm27.1
write.csv(stan_data_8.ctxm27.1[[2]], "chapter_9/stan_models/model_2_8_ctxm27_1/stan_data_8_ctxm27_1.csv", row.names = FALSE )

saveRDS(stan_data_8.ctxm27.1[[1]], "chapter_9/stan_models/model_2_8_ctxm27_1/stan_data_8_ctxm27_1.rda")



