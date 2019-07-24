## visualise organism

mods.sum <- list()

m.esco.fit <- readRDS("chapter_9/stan_models/model_2_esco/stanfit_m2_esco.rds")

summary(m.esco.fit, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary

mods.sum[[1]] <- as.data.frame(summary(m.esco.fit, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[1]]$vars <- rownames(mods.sum[[1]])
mods.sum[[1]]$model <- "E. coli"
  
m.klpn.fit <- readRDS("chapter_9/stan_models/model_2_klpn/stanfit_m2_klpn.rds")

mods.sum[[2]] <- as.data.frame(summary(m.klpn.fit, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[2]]$vars <- rownames(mods.sum[[1]])
mods.sum[[2]]$model <- "K. Pneumoniae"

m.1.ctxm15.16 <- readRDS("chapter_9/stan_models/model_2_1_ctxm15_16/stanfit_m2_1_ctxm15_16.rds")

mods.sum[[3]] <- as.data.frame(summary(m.1.ctxm15.16, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[3]]$vars <- rownames(mods.sum[[1]])
mods.sum[[3]]$model <- "1.ctxm15.16"


m.6.ctxm15.27 <- readRDS("chapter_9/stan_models/model_2_6_ctxm27_1/stanfit_m2_6_ctxm27_1.rds")

mods.sum[[4]] <- as.data.frame(summary(m.6.ctxm15.27, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[4]]$vars <- rownames(mods.sum[[1]])
mods.sum[[4]]$model <- "6.ctxm15.27"

m.8.ctxm15.27 <- readRDS("chapter_9/stan_models/model_2_8_ctxm27_1/stanfit_m2_8_ctxm27_1.rds")

mods.sum[[5]] <- as.data.frame(summary(m.8.ctxm15.27, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[5]]$vars <- rownames(mods.sum[[1]])
mods.sum[[5]]$model <- "8.ctxm15.27"

m.9.ctxm15.62 <- readRDS("chapter_9/stan_models/model_2_9_ctxm15_62/stanfit_m2_9_ctxm15_62.rds")

mods.sum[[6]] <- as.data.frame(summary(m.9.ctxm15.62, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[6]]$vars <- rownames(mods.sum[[1]])
mods.sum[[6]]$model <- "9.ctxm15.62"

m.23.ctxm15.57 <- readRDS("chapter_9/stan_models/model_2_23_ctxm15_57/stanfit_m2_23_ctxm15_57.rds")

mods.sum[[7]] <- as.data.frame(summary(m.23.ctxm15.57, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[7]]$vars <- rownames(mods.sum[[1]])
mods.sum[[7]]$model <- "23.ctxm15.57"

m.39.ctxm15.57 <- readRDS("chapter_9/stan_models/model_2_39_ctxm15_22/stanfit_m2_39_ctxm15_22.rds")

mods.sum[[8]] <- as.data.frame(summary(m.39.ctxm15.57, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[8]]$vars <- rownames(mods.sum[[1]])
mods.sum[[8]]$model <- "39.ctxm15.57"

fit_mod2 <- readRDS("chapter_9/stan_models/model_2/stanfit_m2.rds")

mods.sum[[9]] <- as.data.frame(summary(fit_mod2, pars = c("alphas", "betas","lambda", "mu", "gammas" ))$summary)
mods.sum[[9]]$vars <- rownames(mods.sum[[1]])
mods.sum[[9]]$model <- "ESBL"


mods.sum <- do.call(rbind, mods.sum)

mods.sum$model<- factor(mods.sum$model, levels = rev(c("ESBL", "E. coli",
                                                   "K. Pneumoniae",
                                                   grep("ctxm", unique(mods.sum$model), 
                                                        value = TRUE))))

mods.sum$vars <- recode_factor(mods.sum$vars,
                               `mu` = "mu",
                               `lambda` = "lambda",
                               `alphas[1]` = "alpha[abx]",
                               `alphas[2]` = "alpha[hosp]",
                               `betas[1]` = "beta[abx]",
                               `betas[2]` = "beta[hosp]",
                               `gammas[1]` = "gamma"
                               )
mods.sum %>% #filter(grepl("ctxm", model) | model == "ESBL_mod2") %>%
ggplot(aes(model, `50%`, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point() + geom_errorbar(width = 0) + theme_bw() + coord_flip() + facet_wrap(~vars, scales = "free_x", ncol = 2) +
  ylab("Parameter value") +
  xlab("Model") +
  theme(panel.spacing.x = unit(1, "lines")) -> p1

mods.sum %>% filter(grepl("ctxm", model), vars %in% c("lambda")) %>%
  ggplot(aes(model, `50%`, ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point() + geom_errorbar(width = 0) + theme_bw() + coord_flip() + facet_wrap(~vars, scales = "free_x", ncol = 2) +
  ylab("Parameter value") +
  xlab("Model") +
  theme(panel.spacing.x = unit(1, "lines")) -> p2

ggarrange(p1,ggarrange(NULL,p2,NULL, widths = c(0.4,1,0.4), 
                       ncol = 3, nrow = 1, labels = c(NA,"B", NA)),
          ncol = 1, nrow = 2, heights = c(4,1), labels = c("A",NA))


