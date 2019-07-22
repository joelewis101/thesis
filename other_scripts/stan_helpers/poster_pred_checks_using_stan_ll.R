#### compare models with loo

library(rstan)
library(loo)

stan.df <- merge(stan.df, select(enroll, pid, arm), all.x = TRUE)

fit_mod1 <- readRDS("chapter_9/stan_models/model_1/stanfit_m1.rds")
log_lik_mod1 <- extract_log_lik(fit_mod1)

log_lik_mod1.df <- as.data.frame(t(log_lik_mod1))
log_lik_mod1.df$pid <- as.character(stan.df$pid)
log_lik_mod1.df$actual_end_state <- stan.df$ESBL_stop
log_lik_mod1.df<- merge(log_lik_mod1.df, select(enroll, pid, arm), all.x = T)



log_lik_mod1.df %>% pivot_longer(-c(pid, actual_end_state, arm)) -> log_lik_mod1.df.long
exp(log_lik_mod1.df.long$value) ->  log_lik_mod1.df.long$pred_prob

log_lik_mod1.df.long$pred_state <- map_int(log_lik_mod1.df.long$pred_prob, ~rbinom(1,1,.))


actuals <- stan.df %>% group_by(arm) %>% dplyr::summarise(n.esbl = sum(ESBL_stop == 1), 
                                                   n = length(ESBL_stop),
                                                   prop = n.esbl/n)

predicted <- log_lik_mod1.df.long %>% group_by(arm, name) %>% dplyr::summarise(n.esbl = sum(pred_state == 1),                                                         n = length(pred_state),
                                                            prop = n.esbl/n) 

ggplot(predicted, aes(prop, fill = as.factor(arm), group = arm)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = actuals, aes(xintercept = prop, color = as.factor(arm)), linetype = "dashed")

ll_m1 <- extract_log_lik(fit_mod1, merge_chains = FALSE)

r_eff_m1 <- relative_eff(exp(ll_m1)) 

loo_mod1 <- loo(ll_m1, r_eff = r_eff_m1, cores = 2 )
