
library(rstan)

stan_data_m2 <- readRDS("chapter_9/stan_models/model_2/stan_data_m2.rds")

stan_mod_m2 <- "other_scripts/stan_model/ESBLmod_finalV1.0_rk45.stan"

stanfit_m2 <- stan(file = stan_mod_m2, data = stan_data_m2, warmup = 1, iter = 1, chains = 1)

saveRDS(stanfit_m2, "stanfit_m2.rds")
