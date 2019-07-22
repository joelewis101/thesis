18 Jul 2019
test_data_3.rda contains
test data generated using the following parameters


n <- 2
n_pat <- 400
alphas <- c(-2,0)
betas <- c(0,2)
mu0 <- 0.1
lambda0 <- 0.1
gamma <- 20


BUT covariate 2 set to have no time varying effect

Is the one gamma parameter making things tough on the sampler?

# remember I've flipped the gamma in the stan models
from 

generate_stam_test_data.R

fit this with rk45 and bdf to check out the difference