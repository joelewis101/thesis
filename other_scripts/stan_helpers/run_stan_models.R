
df_t0 <- read.csv("chapter_9/stan_df.csv")

df_t0$ab

N <- nrow(df_t0)
t <- df_t0$tstop

covariates <- as.matrix(dplyr::select(df_t0,abx_start_time, abx_end_time, hosp_start_time, hosp_end_time, prev_abx_stop_time))
ab_flags <- as.matrix(dplyr::select(df_t0,prev_abx_exposure, ab_this_step))

df_t0$p0 <- 0
df_t0$p1 <- 0
df_t0$p0[df_t0$ESBL_start == 0] <- 1
df_t0$p1[df_t0$ESBL_start == 1] <- 1

start_state = as.matrix(dplyr::select(df_t0,p0,p1))
end_state = df_t0$ESBL_stop

stan_data <- list(N = N, t = t, covariates = covariates, start_state = start_state, end_state = end_state, ab_flags = ab_flags)

stan_model <- "other_scripts/stan_helpers/stan_model_real_data_exp_fn_loglik.stan"
#saveRDS(fit,"/Users/joelewis/Documents/PhD/R/PhD/stan/stan_model_real_data_msm_replica.rda" )

fit <- stan(stan_model, data = stan_data, warmup = 500, iter = 1000, chains = 1, cores = 2, thin = 1)
