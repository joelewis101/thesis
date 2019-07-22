
library(dplyr)
library(rstan)

df_t0 <- read.csv("chapter_9/stan_model_fits/exp_decay_agg_abx/stan_df.csv")

df_t0$abx_end_time[df_t0$abx_end_time == 999] <- -999
df_t0$abx_start_time[df_t0$abx_start_time == 999] <- -999
df_t0$prev_abx_stop_time[df_t0$prev_abx_stop_time >= 0] <- 999

df_t0$hosp_end_time[df_t0$hosp_end_time == 999] <- -999
df_t0$hosp_start_time[df_t0$hosp_start_time == 999] <- -999


N <- nrow(df_t0)
t <- df_t0$tstop

covariates <- as.matrix(dplyr::select(df_t0,abx_start_time, abx_end_time,prev_abx_stop_time, hosp_start_time, hosp_end_time,prev_hosp_stop_time ))

n_covs <- c(0,1,1)
covs_type <- c(3,2)

df_t0$p0 <- 0
df_t0$p1 <- 0
df_t0$p0[df_t0$ESBL_start == 0] <- 1
df_t0$p1[df_t0$ESBL_start == 1] <- 1

start_state = as.matrix(dplyr::select(df_t0,p0,p1))
end_state = df_t0$ESBL_stop

stan_data <- list(N = N, t = t, cov_mat = covariates, n_covs = n_covs, covs_type = covs_type, start_state = start_state, end_state = end_state)

stan_model <- "other_scripts/stan_helpers/stan_final_rk45_combovarygammas.stan"
saveRDS(stan_data,"chapter_9/stan_model_fits/multicov models/stan_data.rda" )

#fit <- stan(stan_model, data = stan_data, warmup = 1, iter = 2, chains = 1, cores = 1, thin = 1)
#saveRDS(fit,"stan_model_real_data_stan_final_rk45_combovarygammas.rda" )
