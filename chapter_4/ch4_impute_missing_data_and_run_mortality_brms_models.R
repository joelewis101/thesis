# run the 04-sepsis.Rmd upt o the brms part. you'll need the data frame with the pca components


# up to the point where the d df is defined

d$d28_death <- as.numeric(d$d28_death == "Died")

m1priors <- c(
prior(student_t(3, 0, 2.5), class = "Intercept"),
prior(student_t(3, 0, 2.5), class = "b")
)


brms::brm(as.numeric(d28_death) ~ Dim.1 +
           Dim.2 + Dim.3 + tb + 
          malaria + bc + csf + any_tb_rx + 
          any_fung_rx + 
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

betas <- dfout[,2:12]  * dfout[,16:26]
names(betas) <- paste0("x_beta_", names(betas)) 
dfout <- bind_cols(dfout, betas)

dfout$linear_pred <- apply(dfout,1, function(x) x[15] + sum( x[28:ncol(dfout)]))

dfout$p <- plogis(dfout$linear_pred)
dfout$resid <- (dfout$Y - dfout$p) / (dfout$p*(1- dfout$p))

dfout[28:38] <- dfout[28:38] + dfout$resid

dfout %>% dplyr::select(.row,fluid_6hr,x_beta_fluid_6hr, linear_pred, Y, resid) %>%  group_by(.row) %>%
  summarise(Y = median(Y),
    fluid = median(fluid_6hr), 
            med.par.resid = median(x_beta_fluid_6hr),
            lci = quantile(x_beta_fluid_6hr, 0.025),
            uci = quantile(x_beta_fluid_6hr, 0.975),
            linpred = median(linear_pred),
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
   out.coords[[i]] <-   cbind(out.coords[[i]],d[c("any_tb_rx", "any_mal_rx", "any_fung_rx", "any_ab", "bc", "tb", "csf", "malaria", "fluid_6hr")])                         
}

brm_multiple(d28_death ~ Dim.1 +
               Dim.2 + Dim.3 + tb + 
               malaria + bc + csf + any_tb_rx + 
               any_fung_rx + 
               any_mal_rx + fluid_6hr, data = out.coords, , family = "bernoulli", prior = m1priors) -> m.imp

saveRDS(m.imp, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/mortality_brms_model_imputed.rds")
saveRDS(imp.list, "/Users/joelewis/Documents/PhD/Thesis/bookdown/chapter_4/mortality_brms_model_imputed_datasets.rds")


fviz_famd_var(all.famd, axes = c(1,2), shape.var = "circle", repel = TRUE, col.var.sup = NA, col.var = "red", labelsize = 3,) + theme_bw() +ggtitle(element_blank()) -> p1
fviz_famd_var(all.famd, axes = c(1,3), shape.var = "circle", repel = TRUE, col.var.sup = NA, col.var = "red", labelsize = 3, ) + theme_bw() +ggtitle(element_blank()) +ggtitle(element_blank())

fviz_famd_ind(famd.temp, geom = "point")

lapply(imp.list, )

# plan -> mice (10 datasets) -> FAMD with "missing" participants as extra data -> pull out -> brms

