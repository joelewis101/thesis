
library(rstan)

stan_data_m1 <- readRDS("stan_data_m1.rds")

stan_mod_m1 <- "stan_model_real_data_msm_replica_loglik.stan"

stanfit_m1 <- stan(file = stan_mod_m1, data = stan_data_m1, warmup = 1, iter = 1, chains = 1)

saveRDS(stanfit_m1, "stanfit_m1.rds")
