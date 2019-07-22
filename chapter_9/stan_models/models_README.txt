# set up stan data for real models with mESBLmod_finalV1.0_rk45.stan

# model 1

# ESBL ~ ABx + hosp no decay
# this will use old code and the data passed is slightly different
# where abx includeas all abx and tb rx
stan_model_real_data_msm_replica_loglik.stan

# model 2
# ESBL ~ Abx + hosp, exp decay

# model 3 
# ESBL~ tb + cotri + hosp + abx 
# where abx includes all abx except cotri and tb
# cotri and tb with no decay, abx with

# model 4 
# ESBL + tb + cotri + hosp + (cipro + cefo + amoxy)
# those in () have decay, the others not
