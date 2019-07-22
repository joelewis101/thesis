#### compute posterior predictions of ESBL carriage

library(tidyverse)
library(rstan)
library(deSolve)
expose_stan_functions("other_scripts/stan_helpers/stan_final_rk45_combovarygammas.stan")

readRDS("chapter_9/stan_model_fits/multicov models/4 chains/fit__multicov.rda") -> f.gamma

readRDS("chapter_9/stan_model_fits/piecewise_const_cov/stan_model_real_data_msm_replica_loglik.rda") -> f.nogamma

dat <- readRDS("chapter_9/stan_model_fits/multicov models/stan_data.rda")

posterior.gamma <- rstan::extract(f.gamma, pars = c( "lambda", "mu","gammas", "alphas", "betas"))

ode_finalstateprob <- function(t, state, parameters) {
  coefs <- return_time_varying_coefs_exp_flat (parameters$cov_mat, t, parameters$covs_type, parameters$gammas)
  # print(coefs)
  
  dp0 <- -(parameters$lambda * state[[1]] * exp(sum(parameters$betas * coefs)))   + (parameters$mu * state[[2]] * exp(sum(parameters$alphas * coefs)))
  dp1 <- (parameters$lambda * state[[1]] * exp(sum(parameters$betas *coefs)))  - (parameters$mu * state[[2]] * exp(sum(parameters$alphas * coefs)))
  return(list(c(dp0, dp1)))
}

out <- list()
data.frame(dat$cov_mat, dat$start_state, end_state = dat$end_state, t = dat$t)  -> df

n_replicates <- 2000
out_states <- list()
for (j in 1:n_replicates) {
  cat("\r",paste0("Drawing from posterior and calculating predicted states. Replicate ", j, " of ", n_replicates))
  probs <- list()
  modelparms <- list(lambda = posterior.gamma$lambda[j],
                     mu = posterior.gamma$mu[j],
                     gammas = posterior.gamma$betas[j],
                     alphas = posterior.gamma$alphas[j,],
                     betas = posterior.gamma$alphas[j,]
  )
  
  for (i in 1: nrow(df)) {
    
    probs[[i]] <- ode(y = c(df$p0[i], df$p1[i]), 
        t = c(0, df$t[i]),
        func = ode_finalstateprob, 
        parms = c(modelparms, list(cov_mat =as.matrix(df[i,1:6]), covs_type = c(3,2)))
    )[2,2]
  }
  probs <- as.data.frame(do.call(rbind,probs))
  names(probs)  <- 'pr_1'   
  probs$pred_state <- apply(probs, 1, function(x) rbinom(1,1, x[1]))
  out_states[[j]] <- probs
  }



