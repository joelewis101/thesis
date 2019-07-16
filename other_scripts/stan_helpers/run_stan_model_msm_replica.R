
library(dplyr)
library(rstan)

df_t0 <- read.csv("chapter_9/stan_df.csv")



#### no gamma model ####


## Then set up data

N <- nrow(df_t0)
t <- df_t0$tstop
# no need for t_start as all time zeroed
#t_ab <- as.matrix(dplyr::select(df_t0,abx_begin, abx_end))

#t_hosp <- as.matrix(dplyr::select(df_t0,hosp_begin, hosp_end))

covariates <- as.matrix(dplyr::select(df_t0,abx_start_time, abx_end_time, hosp_start_time, hosp_end_time))

df_t0$p0 <- 0
df_t0$p1 <- 0
df_t0$p0[df_t0$ESBL_start == 0] <- 1
df_t0$p1[df_t0$ESBL_start == 1] <- 1

start_state = as.matrix(dplyr::select(df_t0,p0,p1))
end_state = df_t0$ESBL_stop

stan_data2 <- list(N = N, t = t, covariates = covariates, start_state = start_state, end_state = end_state)


stan_model2 <- "/Users/joelewis/Documents/PhD/R/PhD/stan/final_interim_analysis/stan_model_real_data_msm_replica_loglik.stan"
#saveRDS(fit,"/Users/joelewis/Documents/PhD/R/PhD/stan/stan_model_real_data_msm_replica.rda" )

fit_no_gamma <- stan(stan_model2, data = stan_data2, warmup = 500, iter = 1000, chains = 4, cores = 4, thin = 1)
saveRDS(fit_no_gamma, "/Users/joelewis/Documents/PhD/R/PhD/stan/final_interim_analysis/stan_model_real_data_msm_replica_loglik.rda")
