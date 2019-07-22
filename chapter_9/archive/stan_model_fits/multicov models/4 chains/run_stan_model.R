library(rstan)
readRDS("stan_data.rda") -> test_data
stanmod <- "stan_final_rk45_combovarygammas.stan"
test_fit <- stan(stanmod, data = test_data, warmup = 500, iter = 1000, chains = 4, cores = 4, thin = 1)
saveRDS(test_fit, "fit__multicov.rda")

