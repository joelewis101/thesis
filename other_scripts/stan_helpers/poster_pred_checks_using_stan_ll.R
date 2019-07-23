#### compare models with loo

library(rstan)
library(loo)
stan.df <- read.csv("data/stan_df.csv", stringsAsFactors = F)
stan.df <- merge(stan.df, select(enroll, pid, arm), all.x = TRUE)

fit_mod1 <- readRDS("chapter_9/stan_models/model_1/stanfit_m1.rds")
fit_mod2 <- readRDS("chapter_9/stan_models/model_2/stanfit_m2.rds")
fit_mod3 <- readRDS("chapter_9/stan_models/model_3/stanfit_m3.rds")
plot(fit_mod2, pars = c("alphas", "betas"))
plot(fit_mod3, pars = c("alphas", "betas"))

##mod 1

plot(fit_mod1, pars = c("ab_alpha0", "ab_beta0", "hosp_alpha1", "hosp_beta1")) + 
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") + scale_y_discrete(limits = c("hosp_beta1", "hosp_alpha1","ab_beta0", "ab_alpha0" ), labels = c('hosp_beta1' =  expression(beta~"[hosp]"), 'hosp_alpha1' =  expression(alpha~"[hosp]"), 'ab_beta0' =  expression(beta~"[abx]"), 'ab_alpha0' =  expression(alpha~"[abx]"))) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) + xlab("Parameter value") +
  coord_cartesian(xlim = c(-3,5))  -> a.mod1

plot(fit_mod1, pars = c("lambda", "mu")) + 
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") + scale_y_discrete(limits = c("lambda", "mu"), labels = c('lambda' =  expression(lambda), 'mu' =  expression(mu))) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) + xlab("Parameter value") + 
  coord_cartesian(xlim = c(0,0.4))-> b.mod1 

ggarrange(a.mod1, b.mod1, NULL, ncol = 3, nrow = 1, labels = c("A", NA, NA), widths = c(1.5,1,1)) -> m1.plot

plot(fit_mod2, pars = c("alphas[1]", "betas[1]", "alphas[2]", "betas[2]")) + 
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") + scale_y_discrete(limits = c("betas[2]", "alphas[2]","betas[1]", "alphas[1]" ), labels = c('betas[2]' =  expression(beta~"[hosp]"), 'alphas[2]' =  expression(alpha~"[hosp]"), 'betas[1]' =  expression(beta~"[abx]"), 'alphas[1]' =  expression(alpha~"[abx]"))) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) + xlab("Parameter value") +
  coord_cartesian(xlim = c(-3,5)) -> a.mod2

plot(fit_mod2, pars = c("lambda", "mu")) + 
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") + scale_y_discrete(limits = c("lambda", "mu"), labels = c('lambda' =  expression(lambda), 'mu' =  expression(mu))) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) + xlab("Parameter value") +
  coord_cartesian(xlim = c(0,0.4))-> b.mod2  

plot(fit_mod2, pars = c("gammas")) + 
  theme_bw() + 
  scale_y_discrete(limits = c("gammas[1]"), labels = c('gammas[1]' = expression(gamma~"[abx]"))) + theme(axis.title.y = element_blank(),axis.text.y = element_text(size = 14)) + xlab("Parameter value") + 
  coord_cartesian(xlim = c(0,250))-> c.mod2  

 ggarrange(a.mod2, b.mod2, c.mod2, ncol = 3, nrow = 1, labels = c("B", NA, NA), widths = c(1.5,1,1)) -> m2.plot
 
 # mod 3
 
 plot(fit_mod3, pars = c("alphas[1]", "betas[1]", "alphas[2]", "betas[2]",
                         "alphas[3]", "betas[3]", "alphas[4]", "betas[4]")) + 
   theme_bw() +
   geom_vline(xintercept = 0, linetype = "dashed") + 
   scale_y_discrete(limits = rev(c("alphas[1]", "betas[1]", "alphas[2]", "betas[2]", 
                                "alphas[3]", "betas[3]", "alphas[4]", "betas[4]")),
                    labels = c('betas[4]' =  expression(beta~"[hosp]"), 
                               'alphas[4]' =  expression(alpha~"[hosp]"),
                               'betas[3]' =  expression(beta~"[tb]"), 
                               'alphas[3]' =  expression(alpha~"[tb]"),
                               'betas[2]' =  expression(beta~"[cotr]"), 
                                'alphas[2]' =  expression(alpha~"[cotr]"),
                                'betas[1]' =  expression(beta~"[abx]"), 
                                'alphas[1]' =  expression(alpha~"[abx]"))
                    ) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
                      xlab("Parameter value") +
                      coord_cartesian(xlim = c(-3,5)) -> a.mod3 
 
 plot(fit_mod3, pars = c("lambda", "mu")) + 
   theme_bw() +
   geom_vline(xintercept = 0, linetype = "dashed") + scale_y_discrete(limits = c("lambda", "mu"), labels = c('lambda' =  expression(lambda), 'mu' =  expression(mu))) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) + xlab("Parameter value") +
   coord_cartesian(xlim = c(0,0.4))-> b.mod3  
 
 plot(fit_mod3, pars = c("gammas")) + 
   theme_bw() + 
   scale_y_discrete(limits = c("gammas[1]"), labels = c('gammas[1]' = expression(gamma~"[abx]"))) + theme(axis.title.y = element_blank(),axis.text.y = element_text(size = 14)) + xlab("Parameter value") + 
   coord_cartesian(xlim = c(0,250))-> c.mod3  
 
 ggarrange(a.mod3, b.mod3, c.mod3, ncol = 3, nrow = 1, labels = c("C", NA, NA), widths = c(1.5,1,1)) -> m3.plot
 
 # mod 4
 
 
 
 
 plot(fit_mod4, pars = c("alphas[1]", "betas[1]", "alphas[2]", "betas[2]",
                         "alphas[3]", "betas[3]", "alphas[4]", "betas[4]",
                         "alphas[5]", "betas[5]", "alphas[6]", "betas[6]")) + 
   theme_bw() +
   geom_vline(xintercept = 0, linetype = "dashed") + 
   scale_y_discrete(limits = rev(c("alphas[1]", "betas[1]", "alphas[2]", "betas[2]", 
                                   "alphas[3]", "betas[3]", "alphas[4]", "betas[4]",
                                   "alphas[5]", "betas[5]", "alphas[6]", "betas[6]")),
                    labels = c('betas[6]' =  expression(beta~"[hosp]"), 
                               'alphas[6]' =  expression(alpha~"[hosp]"),
                               'betas[5]' =  expression(beta~"[tb]"), 
                               'alphas[5]' =  expression(alpha~"[tb]"),
                               'betas[4]' =  expression(beta~"[cotr]"), 
                               'alphas[4]' =  expression(alpha~"[cotr]"),
                               'betas[3]' =  expression(beta~"[amx]"), 
                               'alphas[3]' =  expression(alpha~"[amx]"),
                               'betas[2]' =  expression(beta~"[cip]"), 
                               'alphas[2]' =  expression(alpha~"[cip]"),
                               'betas[1]' =  expression(beta~"[cef]"), 
                               'alphas[1]' =  expression(alpha~"[cef]"))
   ) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
   xlab("Parameter value") +
   coord_cartesian(xlim = c(-3,5)) -> a.mod4
 
 plot(fit_mod4, pars = c("lambda", "mu")) + 
   theme_bw() +
   geom_vline(xintercept = 0, linetype = "dashed") + scale_y_discrete(limits = c("lambda", "mu"), labels = c('lambda' =  expression(lambda), 'mu' =  expression(mu))) + theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) + xlab("Parameter value") +
   coord_cartesian(xlim = c(0,0.4))-> b.mod4
 
 plot(fit_mod4, pars = c("gammas")) + 
   theme_bw() + 
   scale_y_discrete(limits = rev(c("gammas[1]", "gammas[2]", "gammas[3]")), 
                    labels = c('gammas[3]' = expression(gamma~"[amx]"), 
                               'gammas[2]' =  expression(gamma~"[cip]"),
                               'gammas[1]' =  expression(gamma~"[cef]") )) + 
   theme(axis.title.y = element_blank(),axis.text.y = element_text(size = 14)) + 
   xlab("Parameter value") + 
   coord_cartesian(xlim = c(0,250)) -> c.mod4  
 
 ggarrange(a.mod4, b.mod4, c.mod4, ncol = 3, nrow = 1, labels = c("D", NA, NA), widths = c(1.5,1,1)) -> m4.plot
 
 
 
 ggarrange(m1.plot, m2.plot,m3.plot, m4.plot, ncol = 1, nrow = 4, heights = c(1,1,1.3, 1.8))

log_lik_mod1 <- extract_log_lik(fit_mod1)
log_lik_mod1.df <- as.data.frame(t(log_lik_mod1))
log_lik_mod1.df$pid <- as.character(stan.df$pid)
log_lik_mod1.df$actual_end_state <- stan.df$ESBL_stop
log_lik_mod1.df<- merge(log_lik_mod1.df, select(enroll, pid, arm), all.x = T)
log_lik_mod1.df %>% pivot_longer(-c(pid, actual_end_state, arm)) -> log_lik_mod1.df.long
exp(log_lik_mod1.df.long$value) ->  log_lik_mod1.df.long$pred_prob
log_lik_mod1.df.long$pred_prob[log_lik_mod1.df.long$actual_end_state == 0] <- 1 - log_lik_mod1.df.long$pred_prob[log_lik_mod1.df.long$actual_end_state == 0]
log_lik_mod1.df.long$pred_state <- map_int(log_lik_mod1.df.long$pred_prob, ~rbinom(1,1,.))


actuals <- stan.df %>%
  group_by(arm) %>% 
  dplyr::summarise(n.esbl = sum(ESBL_stop == 1),
                   n = length(ESBL_stop),
                   prop = n.esbl/n) %>%
  ungroup() %>%
  mutate(arm = as.character(arm),
         arm = recode(arm, `1` = "Arm 1", `2` = "Arm 2", `3` = "Arm 3")) 
  

predicted <- log_lik_mod1.df.long %>% group_by(arm, name) %>% dplyr::summarise(n.esbl = sum(pred_state == 1),                                                         n = length(pred_state),
                                                            prop = n.esbl/n) 

predicted %>% 
  ungroup() %>% mutate(arm = as.character(arm),
                       arm = recode(arm, `1` = "Arm 1", `2` = "Arm 2", `3` = "Arm 3")) %>%
ggplot(aes(prop, fill = as.factor(arm), group = arm)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = actuals, aes(xintercept = prop, color = as.factor(arm)), linetype = "dashed") +
  coord_cartesian(xlim = c(0.3, 0.7)) +
  xlab("Proportion") + ylab("Denisty") +
  theme_bw() + theme(legend.title = element_blank())-> a


##


log_lik_mod2 <- extract_log_lik(fit_mod2)
log_lik_mod2.df <- as.data.frame(t(log_lik_mod2))
log_lik_mod2.df$pid <- as.character(stan.df$pid)
log_lik_mod2.df$actual_end_state <- stan.df$ESBL_stop
log_lik_mod2.df<- merge(log_lik_mod2.df, select(enroll, pid, arm), all.x = T)
log_lik_mod2.df %>% pivot_longer(-c(pid, actual_end_state, arm)) -> log_lik_mod2.df.long
exp(log_lik_mod2.df.long$value) ->  log_lik_mod2.df.long$pred_prob
log_lik_mod2.df.long$pred_prob[log_lik_mod2.df.long$actual_end_state == 0] <- 1 - log_lik_mod2.df.long$pred_prob[log_lik_mod2.df.long$actual_end_state == 0]
log_lik_mod2.df.long$pred_state <- map_int(log_lik_mod2.df.long$pred_prob, ~rbinom(1,1,.))

predicted_m2 <- log_lik_mod2.df.long %>% group_by(arm, name) %>% dplyr::summarise(n.esbl = sum(pred_state == 1),                                                         n = length(pred_state),
                                                                               prop = n.esbl/n) 

predicted_m2 %>% 
  ungroup() %>% mutate(arm = as.character(arm),
    arm = recode(arm, `1` = "Arm 1", `2` = "Arm 2", `3` = "Arm 3")) %>%
  ggplot(aes(prop, fill = as.factor(arm), group = arm)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = actuals, aes(xintercept = prop, color = as.factor(arm)), linetype = "dashed")+
  coord_cartesian(xlim = c(0.3, 0.7))  +
  xlab("Proportion") + ylab("Denisty") +
  theme_bw() + theme(legend.title = element_blank())-> b

log_lik_mod3 <- extract_log_lik(fit_mod3)
log_lik_mod3.df <- as.data.frame(t(log_lik_mod3))
log_lik_mod3.df$pid <- as.character(stan.df$pid)
log_lik_mod3.df$actual_end_state <- stan.df$ESBL_stop
log_lik_mod3.df<- merge(log_lik_mod3.df, select(enroll, pid, arm), all.x = T)
log_lik_mod3.df %>% pivot_longer(-c(pid, actual_end_state, arm)) -> log_lik_mod3.df.long
exp(log_lik_mod3.df.long$value) ->  log_lik_mod3.df.long$pred_prob
log_lik_mod3.df.long$pred_prob[log_lik_mod3.df.long$actual_end_state == 0] <- 1 - log_lik_mod3.df.long$pred_prob[log_lik_mod3.df.long$actual_end_state == 0]
log_lik_mod3.df.long$pred_state <- map_int(log_lik_mod3.df.long$pred_prob, ~rbinom(1,1,.))

predicted_m3 <- log_lik_mod3.df.long %>% group_by(arm, name) %>% dplyr::summarise(n.esbl = sum(pred_state == 1),                                                         n = length(pred_state),
                                                                                  prop = n.esbl/n) 

predicted_m3 %>% 
  ungroup() %>% mutate(arm = as.character(arm),
                       arm = recode_factor(arm, `1` = "Arm 1", `2` = "Arm 2", `3` = "Arm 3")) %>% 
  ggplot(aes(prop, fill = as.factor(arm), group = arm)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = actuals, aes(xintercept = prop, color = as.factor(arm)), linetype = "dashed")+
  coord_cartesian(xlim = c(0.3, 0.7))  +
  xlab("Proportion") + ylab("Denisty") +
  theme_bw() + theme(legend.title = element_blank())-> c

### mod 4

log_lik_mod4 <- extract_log_lik(fit_mod4)
log_lik_mod4.df <- as.data.frame(t(log_lik_mod4))
log_lik_mod4.df$pid <- as.character(stan.df$pid)
log_lik_mod4.df$actual_end_state <- stan.df$ESBL_stop
log_lik_mod4.df<- merge(log_lik_mod4.df, select(enroll, pid, arm), all.x = T)
log_lik_mod4.df %>% pivot_longer(-c(pid, actual_end_state, arm)) -> log_lik_mod4.df.long
exp(log_lik_mod4.df.long$value) ->  log_lik_mod4.df.long$pred_prob
log_lik_mod4.df.long$pred_prob[log_lik_mod4.df.long$actual_end_state == 0] <- 1 - log_lik_mod4.df.long$pred_prob[log_lik_mod4.df.long$actual_end_state == 0]
log_lik_mod4.df.long$pred_state <- map_int(log_lik_mod4.df.long$pred_prob, ~rbinom(1,1,.))

predicted_m4 <- log_lik_mod4.df.long %>% group_by(arm, name) %>% dplyr::summarise(n.esbl = sum(pred_state == 1),                                                         n = length(pred_state),
                                                                                  prop = n.esbl/n) 

predicted_m4 %>% 
  ungroup() %>% mutate(arm = as.character(arm),
                       arm = recode_factor(arm, `1` = "Arm 1", `2` = "Arm 2", `3` = "Arm 3")) %>% 
  ggplot(aes(prop, fill = as.factor(arm), group = arm)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = actuals, aes(xintercept = prop, color = as.factor(arm)), linetype = "dashed")+
  coord_cartesian(xlim = c(0.3, 0.7))  +
  xlab("Proportion") + ylab("Denisty") +
  theme_bw() + theme(legend.title = element_blank())-> d



ggarrange(a,b,c,d, ncol = 1,nrow = 4, labels = c("A", "B", "C", "D"),
          common.legend = TRUE, legend = "bottom")







ll_m1 <- extract_log_lik(fit_mod1, merge_chains = FALSE)

r_eff_m1 <- relative_eff(exp(ll_m1)) 

ll_m2 <- extract_log_lik(fit_mod2, merge_chains = FALSE)
r_eff_m2 <- relative_eff(exp(ll_m2)) 

ll_m3 <- extract_log_lik(fit_mod3, merge_chains = FALSE)
r_eff_m3 <- relative_eff(exp(ll_m3)) 

ll_m4 <- extract_log_lik(fit_mod4, merge_chains = FALSE)
r_eff_m4 <- relative_eff(exp(ll_m4)) 

loo_mod1 <- loo(ll_m1, r_eff = r_eff_m1, cores = 2 )
loo_mod2 <- loo(ll_m2, r_eff = r_eff_m2, cores = 2 )
loo_mod3 <- loo(ll_m3, r_eff = r_eff_m3, cores = 2 )
loo_mod4 <- loo(ll_m4, r_eff = r_eff_m4, cores = 2 )

loo_compare(loo_mod1, loo_mod2, loo_mod3)
loo::compare(loo_mod2, loo_mod4)
