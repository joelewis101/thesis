
library(rstan)

stan_data_m2 <- readRDS("stan_data_23_ctxm15_57.rda")

stan_mod_m2 <- "ESBLmod_finalV1.0_rk45.stan"

stanfit_m2 <- stan(file = stan_mod_m2, data = stan_data_m2, warmup = 500, iter = 1000, chains = 4, cores = 4)

saveRDS(stanfit_m2, "stanfit_m2_23_ctxm15_57.rds")
