# run the 04-sepsis.Rmd upt o the brms part. you'll need the data frame with the pca components


# up to the point where the d df is defined

d$d28_death <- as.numeric(d$d28_death == "Died")

m1priors <- c(
prior(student_t(3, 0, 2.5), class = "Intercept"),
prior(student_t(3, 0, 2.5), class = "b")
)


brms::brm(as.numeric(d28_death == "Died") ~ Dim.1 +
           Dim.2 + Dim.3 +  
          malaria + bc + csf + tb + 
          any_fung_rx + any_ab + any_tb_rx +
           any_mal_rx + fluid_6hr, data = d.complete, , family = "bernoulli", prior = m1priors) -> m.b2



# check linearity assumption of fluid



## make partial residual
extract_draws(m.b2) -> b
Y <- b$data
draws <- as.data.frame(b$dpars$mu$fe$b) 
draws$.draw <- 1:nrow(draws)
df.data <- as.data.frame(b$dpars$mu$fe$X)
df.data <- bind_cols(df.data, Y)
df.data$.row <- 1:nrow(df.data)

dfout <- bind_cols(
  
  df.data %>% slice(rep(1:n(), each = nrow(draws))),
  draws %>% slice(rep(row_number(), nrow(df.data)))
)

# apply(dfout, 1, function(x) x[22] + sum(x[2:19] * x[23:40])) -> dfout$linear_pred
#as.data.frame(t(apply(dfout[1:10,], 1, function(x) x[2:19] * x[23:40])))

betas <- dfout[,2:13]  * dfout[,17:28]
names(betas) <- paste0("x_beta_", names(betas)) 
dfout <- bind_cols(dfout, betas)

dfout$linear_pred <- apply(dfout,1, function(x) x[16] + sum( x[30:ncol(dfout)]))

dfout$p <- plogis(dfout$linear_pred)
dfout$resid <- (dfout$Y - dfout$p) / (dfout$p*(1- dfout$p))

dfout[30:41] <- dfout[30:41] + dfout$resid

dfout %>% dplyr::select(.row,fluid_6hr,x_beta_fluid_6hr, linear_pred, Y, resid) %>%  group_by(.row) %>%
  summarise(Y = median(Y),
    fluid = median(fluid_6hr), 
            med.par.resid = median(x_beta_fluid_6hr),
            lci = quantile(x_beta_fluid_6hr, 0.025),
            uci = quantile(x_beta_fluid_6hr, 0.975),
            linpred = median(linear_pred, na.rm= TRUE),
            lci.lp = quantile(linear_pred, 0.025),
            uci.lp = quantile(linear_pred, 0.975),
    med.resid = median(resid),
    lci.r = quantile(resid, 0.025),
    uci.r = quantile(resid, 0.975)
    ) -> fluid_part_resid

ggplot(fluid_part_resid, aes(fluid, med.par.resid, ymin = lci, ymax = uci)) + geom_point() + geom_errorbar(width = 0) + 
  geom_smooth() + geom_smooth(method = 'lm', linetype = "dashed", color = "red") + coord_cartesian(ylim = c(-5,10)) + theme_bw()

# looks ok - no need to consider nonlinearlity

saveRDS(m.b2, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/mortality_brms_model_cca.rds")
saveRDS(fluid_part_resid, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/mortality_brms_model_cca_partial_residuals_df.rds")

# now need to impute missing data
d[c("d28_death","hivstatus", "CD4.dic", "calc_age", "ptsex","Haemoglobin", "screentemp", "t0map", "t0hr", "t0rr", "t0spo2", "gcs","lactate_value",
    "WCC" , "Platelets", "Urea",  "Creatinine", "CO2", "ustand")] -> df.famd.vars

for (var in colnames(df.famd.vars)) {
  attr(df.famd.vars[,deparse(as.name(var))], "scaled:center") <- NULL
  attr(df.famd.vars[,deparse(as.name(var))], "scaled:scale") <- NULL
}

as.data.frame(as.matrix( df.famd.vars)) -> df.famd.vars
df.famd.vars[c(4,6,7,8,9,10,13,14,15,16,17,18)] <- sapply(df.famd.vars[c(4,6,7,8,9,10,13,14,15,16,17,18)], function(x) as.numeric(as.character(x)))

#df.famd.vars$missing_data <- (!complete.cases(df.famd.vars))
which(!complete.cases(df.famd.vars)) -> missing_data_rows

mice(df.famd.vars, m = 10) -> df.famd.vars.imp
complete(df.famd.vars.imp, action = "all") -> imp.list

out.famd <- list()
out.coords <- list()


for (i in 1:10) {
  FAMD(imp.list[[i]], graph = FALSE, ind.sup = missing_data_rows, sup.var = 1) -> famd.temp
  out.famd[[i]] <- famd.temp 
  coordtemp <- rbind(famd.temp$ind$coord, famd.temp$ind.sup$coord)
  coordtemp[order(as.numeric(rownames(coordtemp))),] -> coordtemp
  out.coords[[i]] <-   cbind(imp.list[[i]],coordtemp)
   out.coords[[i]] <-   cbind(out.coords[[i]],d[c("any_tb_rx", "any_mal_rx", "any_fung_rx", 
                                                        "any_ab", "bc", "tb", "csf", "malaria", "fluid_6hr")])
   out.coords[[i]] <-   cbind(out.coords[[i]],df.temp[c("time_to_abx", "time_to_tb_rx")])
   out.coords[[i]]$time_to_abx <- scale(out.coords[[i]]$time_to_abx)
}

brm_multiple(d28_death ~ Dim.1 +
               Dim.2 + Dim.3 + tb + 
               malaria + bc + csf + any_tb_rx + 
               any_fung_rx + any_ab +
               any_mal_rx + fluid_6hr, data = out.coords, , family = "bernoulli", prior = m1priors) -> m.imp

saveRDS(m.imp, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/mortality_brms_model_imputed.rds")
saveRDS(imp.list, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/mortality_brms_model_imputed_datasets.rds")

# model time to ab

brm_multiple(d28_death ~ Dim.1 +
               Dim.2 + Dim.3 + tb + 
               malaria + bc + csf + any_tb_rx + 
               any_fung_rx + any_ab +
               any_mal_rx + fluid_6hr , data = out.coords,  family = "bernoulli", prior = m1priors) -> m.imp

d2$time_to_abx <- scale(log(d2$time_to_abx ))

d2.complete <- merge(d.complete, select(df.temp, pid, time_to_abx, time_to_tb_rx))
d2.complete$time_to_abx <- scale(d2.complete$time_to_abx)

brm(as.numeric(d28_death == "Died") ~ time_to_abx + I(time_to_abx^2) + 
      Dim.1 + Dim.2 , data = d2.complete, family = "bernoulli", prior = m1priors ) -> m.poly

brm(as.numeric(d28_death == "Died") ~ time_to_abx + 
      Dim.1 , data = d2.complete, family = "bernoulli", prior = m1priors ) -> m.lin

brm_multiple(d28_death ~  time_to_abx + I(time_to_abx^2) + 
               Dim.1 + Dim.2, data = out.coords, , family = "bernoulli", prior = m1priors) -> m.poly.imp

brm_multiple(d28_death ~  time_to_abx  + 
               Dim.1 + Dim.2, data = out.coords, , family = "bernoulli", prior = m1priors) -> m.lin.imp

data.frame(timetoab.scale.centre = attr(out.coords[[1]]$time_to_abx, "scaled:center"), 
           timetoab.scale.scale = attr(out.coords[[1]]$time_to_abx, "scaled:scale")) -> timetoab.scale

saveRDS(m.lin, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/time_to_ab_linear_cca.rds")
saveRDS(m.poly, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/time_to_ab_poly_cca.rds")
saveRDS(m.lin.imp, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/time_to_ab_linear_imp.rds")
saveRDS(m.poly.imp, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/time_to_ab_poly_imp.rds")
saveRDS(timetoab.scale, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/timetoab.scale.rds")


