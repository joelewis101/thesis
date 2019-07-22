# generate simulated dataset to test markov models

# n covariates
# lambda is transition from uncol -> col
# mu is transition from col -< uncol
# betas is vector of length n of covariates for lambda
# alphas is a vector of length n of covariates mu

library(tidyverse)
library(deSolve)
library(rstan)
expose_stan_functions("other_scripts/stan_helpers/stan_test_fns.stan")

n <- 3
n_pat <- 50
alphas <- c(-2,2,0)
betas <- c(0,0,2)
mu0 <- 0.05
lambda0 <- 0.05
gamma <- 30

#state vector is y = (p0,p1)

# dp0/dt =  -lambda * p0 + mu*p1

# dp1/dt =  lambda * p0 - mu*p1

# where lambda =  lambda0 * exp(beta0*x0(t) + beta1*x(1(t) + ...))

# where xm(t) for covariate m = 1 is current exposure, 0 if no exposure and no prior exposure and
#exp(-(t-texp)/gamma) if prev exposure was at texp

# set up data structures

t <- rnorm(n_pat, mean = 30, sd = 20)

t[t < 0] <- abs(t[t < 0])
t <- round(t)
t[t == 0] <- 1

generate_covs <- function(t_func, n_pats, name, startmean, startsd, lengthmean, lengthsd, prevmean, prevsd, prop_noexp = 0, prop999 = 0, prop0 = 0) {
  
cov <- data.frame( cov_start = rnorm(n_pats, startmean, startsd))
cov$cov_start[cov$cov_start < 0] <- abs(cov$cov_start[cov$cov_start < 0])
cov$cov_end <- cov$cov_start + abs(rnorm(n_pats, lengthmean, lengthsd))
cov$cov_end[cov$end < cov$cov_start] <- cov$cov_start[cov$end < cov$cov_start] + abs(cov$cov_end[cov$end < cov$cov_start] - cov$cov_start[cov$end < cov$cov_start])

cov$cov_prev <- -rnorm(n_pats, prevmean, prevsd)
cov$cov_prev[cov$cov_prev > 0] <- -abs(cov$cov_prev[cov$cov_prev > 0])

cov[sample.int(n_pats, round(prop_noexp*n_pats, digits = 0)), 1:2] <- 999
cov[sample.int(n_pats, round(prop999*n_pats, digits = 0)), 3] <- 999
cov[sample.int(n_pats, round(prop0*n_pats, digits = 0)), 3] <- 0


cov$cov_start[cov$cov_start > t_func] <- t[cov$cov_start > t_func]
cov$cov_end[cov$cov_end > t_func] <- t[cov$cov_end > t_func]

sub("cov", name, names(cov)) -> names(cov)

return(round(cov))
}

as.matrix((generate_covs(t,n_pat, "cov1", 3,2, 7,4,5,2, 0,0.1,0.1))) -> m1
as.matrix((generate_covs(t,n_pat, "cov2", 1,4, 2,1,2,1, 0,0.1,0.1))) -> m2
as.matrix((generate_covs(t,n_pat, "cov3", 7,4, 2,1,2,1, 0,0.1,0.1))) -> m3

array(c(m1,m2,m3), dim = c(n_pat,3,3)) -> covs_array

return_time_varying_coefs_exp(c(t(covs_array[1,,])), 5, n_covs = 3, 10)

start_state <- rbinom(n_pat, 1, 0.5)

p0 <- as.numeric(start_state == 0)
p1 <- as.numeric(start_state == 1)
initstates <- matrix(c(p0,p1), nrow = n_pat, ncol = 2)

odeR <- function(t, state, parameters) {
  coefs <- return_time_varying_coefs_exp(parameters, t, 3, gamma)
 # print(coefs)

  dp0 <- -(lambda0 * exp(sum(betas * coefs)) * state[[1]]) + (mu0 * exp(sum(alphas * coefs)) * state[[2]])
  dp1 <- (lambda0 * exp(sum(betas * coefs)) * state[[1]]) - (mu0 * exp(sum(alphas * coefs)) * state[[2]])
  return(list(c(dp0, dp1)))
}

finalstate.prob <- list()
for (i in 1:n_pat) {
  finalstate.prob [i] <- ode(c(p0[i], p1[i]), c(0,t[i]), odeR, c((covs_array[i,,])))[[2,2]]
}

lapply(finalstate.prob, function(x) rbinom(1,1,x)) -> finalstate
finalstate <- unlist(finalstate)

stan_test_data2 <- list(N = n_pat, n_covs = 3 ,t = t, cov_mat = covs_array, start_state = initstates, end_state = finalstate)
#saveRDS(stan_test_data, "chapter_9/test_data/test_data_1.rda")
stanmod <- "other_scripts/stan_helpers/stan_test_fns.stan"
test_fit <- stan(stanmod, data = stan_test_data2, warmup = 1, iter = 2, chains = 1, cores = 1, thin = 1)

