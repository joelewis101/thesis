# univariate table for patient factors


library(tidyverse)
library(pheatmap)
library(viridis)

# associations of mortality in the DASSIM data

source("final_cleaning_scripts/generate_visits_df.R")
source("final_cleaning_scripts/load_and_clean_followup_and_enroll_labelled.R")
source("other_scripts/summary_table_functions.R")
source("final_cleaning_scripts/make_composite_hivstatus_variable.R")
#source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_bloods.R")
source("final_cleaning_scripts/load_and_clean_aetiol.R")
source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_upto72.R")
source("final_cleaning_scripts/load_and_clean_post72.R")
source("final_cleaning_scripts/load_and_clean_hosp_oc.R")
source("final_cleaning_scripts/load_and_clean_time_to_ab.R")
source("final_cleaning_scripts/load_and_clean_fluid_hr1_to_6.R")

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# link enroll, bloods, time to abs, tb rx, malaria rx, aetiol

df <- subset(enroll, arm == 1)
df$gcs <- df$t0gcse + df$t0gcsv + df$t0gcsm
df <- merge(df, bloods, all.x = T)

aetiol.m <- dcast(aetiol, pid + hivstatus ~ type, value.var = "pathogen")
aetiol.m$tb <- aetiol.m$`mtb bsi`== 1 | aetiol.m$uLAM == 1 | aetiol.m$Xpert == 1

df <- merge(df, select(aetiol.m, - hivstatus), all.x = T)

df <- merge(df, select(df.abs, pid, first_ab, time_to_abx))

df <- merge(df, select(df.tb, pid, any_tb_rx, time_to_tb_rx))

df <- merge(df, select(fluid, pid, `6`))
names(df)[names(df) == "6"] <- "fluid_6hr"



#aetiol.m -> a
#a[is.na(a)] <- 0
#a$no_diagnosis <- as.numeric(apply(a[3:8], 1,sum) == 0)




v1 <- visits %>% filter(arm == 1) 

v1$t <- apply(v1[7:10], 1,max, na.rm= T )

v1$t[!is.na(v1$died_date)] <- v1$died_date[!is.na(v1$died_date)]

#v1$t <- v1$t * 7
v1$died <- v1$status
v1$died[v1$died == 2] <- 0

v1 <- select(v1, pid, enrolled, arm, t, died)

#v1$d28_death

v1$d28_death <- NA

v1$d28_death[(v1$t <= 28) & (v1$died == 1)] <- 1
v1$d28_death[(v1$t >= 28)] <- 0

v1$d90_death[(v1$t <= 90) & (v1$died == 1)] <- 1
v1$d90_death[(v1$t >= 90)] <- 0

v1$d180_death[(v1$t <= 180) & (v1$died == 1)] <- 1
v1$d180_death[(v1$t >= 180)] <- 0

df <- merge(df, v1, all.x = T)

df$sbp.dic <- (df$t0sbp <= 90)
df$t.dic <- NA
df$t.dic[df$screentemp < 36] <- "<36"
df$t.dic[df$screentemp >= 36 & df$screentemp < 38] <- "36-38"
df$t.dic[df$screentemp >= 38] <- ">38"

df$hr.dic <-  (df$t0hr > 100)
df$rr.dic <-  (df$t0rr > 30)
df$cd4.dic <- (df$CD4_Absolute <= 200)
df$spo2.dic <- (df$t0spo2 < 90)
df$gcs.dic <- (df$gcs < 15)
df$lact.dic <- (df$lactate_value > 3)
as.numeric(df$WCC) > 11 -> df$WCC.dict
df$CO2.dic <- as.numeric(df$CO2 < 23)

# assess bivariate distributions
df$t0map <- (df$t0sbp + (df$t0dbp * 2))/3 

df$tb[df$tb] <- 1
df$tb[df$tb == FALSE | is.na(df$tb) ] <- 0

df$malaria[is.na(df$malaria)] <- 0
df$bc[is.na(df$bc)] <- 0
df$csf[is.na(df$csf)] <- 0

df$diagnosis <- interaction(df$tb, df$malaria, df$bc, df$csf)

df.temp <- dplyr::select(df, d28_death, hivstatus,calc_age,ptsex,screentemp, t0sbp, t0dbp, t0map, t0hr, t0rr, t0spo2, gcs, lactate_value,
                         WCC, CD4_Absolute, Haemoglobin, Platelets, NA.,Potassium, Urea, Creatinine, CO2, Chloride
) 

df.temp$CD4_Absolute[df.temp$hivstatus != "Reactive"] <- NA
df.temp$hivstatus[df.temp$hivstatus == "Reactive"] <- 1
df.temp$hivstatus[df.temp$hivstatus == "Non reactive"] <- 0
df.temp$hivstatus[df.temp$hivstatus == "Unknown"] <- NA

df.temp$ptsex[df.temp$ptsex == "Male"] <- 1
df.temp$ptsex[df.temp$ptsex == "Female"] <- 0


df.temp %>% select(-c(hivstatus, ptsex)) %>% pivot_longer(-d28_death) %>% filter(!is.na(d28_death)) %>%
  dplyr::group_by(name) %>% dplyr::summarise(pval = kruskal.test(value ~ d28_death)$p.value) -> pvals

df.temp %>% select(-c(hivstatus, ptsex)) %>% pivot_longer(-d28_death) %>% filter(!is.na(d28_death)) %>%
  dplyr::group_by(name, d28_death) %>% dplyr::summarise( median = median(value, na.rm = T),
                                                             LQ = quantile(value, 0.25, na.rm = T),
                                                              UQ = quantile(value, 0.75, na.rm = T)) -> p



paste0(
  specify_decimal(p$median, 1), " (",
  specify_decimal(p$LQ, 1), "-",
  specify_decimal(p$UQ, 1) ,")"
) -> p$med_str

p %>% select(name, d28_death, med_str) %>% pivot_wider(names_from = d28_death, values_from = med_str) -> p

p <- merge(p, pvals)

p$pval <- specify_decimal(p$pval, 3)
p$pval[p$pval == "0.000"] <- "<0.001"

df.temp


df.temp %>% select(d28_death, hivstatus, ptsex) %>% pivot_longer(-d28_death) %>% filter(!is.na(d28_death)) %>%
  dplyr::group_by(name, value) %>%
  dplyr::summarise(died = sum(d28_death == 1, na.rm = T), alive = sum(d28_death == 0, na.rm = T)) %>% filter(!is.na(value)) %>% group_by(name) %>%
   group_by(name) %>% dplyr::summarise(pval = fisher.test(matrix(c(died, alive), ncol =2))$p.value) -> p.catpvals

df.temp %>% select(d28_death, hivstatus, ptsex) %>% pivot_longer(-d28_death) %>% filter(!is.na(d28_death)) %>%
  dplyr::group_by(name, d28_death) %>% dplyr::summarise(`1` = sum(value == 1, na.rm = T), 
                                                        total = sum(!is.na(value)),
                                                        prop = `1`/total) -> p.cat

p.cat$str <- paste0(
  p.cat$`1`, "/", p.cat$total,
  " (", signif(p.cat$prop *100,2), "%)"
)


p.cat %>% select(name, d28_death, str) %>% pivot_wider(names_from = d28_death, values_from = str) -> p.cat
p.cat <- merge(p.cat, p.catpvals)

p.cat$pval <- specify_decimal(p.cat$pval, 3)
#p$pval[p$pval == "0.000"] <- "<0.001"



p <- bind_rows(p,p.cat)

p[match(c("calc_age", "ptsex", "hivstatus", "CD4_Absolute", "Haemoglobin","screentemp", "t0hr", "t0sbp", "t0dbp", "t0map",
        "t0rr", "t0spo2","gcs", "lactate_value" , "WCC", "Platelets", "NA.", "Potassium",
        "Chloride", "CO2", "Urea", "Creatinine"), p$name),] -> p

p <- p[c("name", "1", "0", "pval")]
names(p) <- c("Variable", "Died", "Survived", "p")

p$Variable[p$Variable == "calc_age"] <- "Age (years)"
p$Variable[p$Variable == "ptsex"] <- "Male sex"
p$Variable[p$Variable == "hivstatus"] <- "HIV Infected"
p$Variable[p$Variable == "Haemoglobin"] <- "Haemoglobin (g/dL)"
p$Variable[p$Variable == "CD4_Absolute"] <- "CD4 count (uL)-1"
p$Variable[p$Variable == "screentemp"] <- "Temperature (C)"
p$Variable[p$Variable == "screentemp"] <- "Temperature (C)"



df.back <- df

df$any_ab <- as.numeric(df$first_ab != "none")
df$any_mal_rx <- as.numeric(!is.na(df$first_mal_rx))
df$any_fung_rx <- as.numeric(!is.na(df$first_fung_rx))

df$any_tb_rx[df$any_tb_rx == "on admission"] <- "none"
df$any_tb_rx[df$any_tb_rx == "none"] <- 0
df$any_tb_rx[df$any_tb_rx == "yes"] <- 1

df$CD4_Absolute[df$hivstatus != "Reactive"] <- NA

df$hivstatus[df$hivstatus == "Unknown"] <- NA
df$hivstatus[df$hivstatus == "Reactive"] <- 1
df$hivstatus[df$hivstatus == "Non reactive"] <- 0

df$hivstatus <- as.numeric(df$hivstatus)

df$CD4.scale <- scale(df$CD4_Absolute)
df$CD4.scale <- log(df$CD4.scale)



glm(d28_death ~ calc_age + ptsex + hivstatus  +  Haemoglobin + screentemp + t0hr + t0map + 
      t0rr + t0spo2 + gcs + lactate_value + WCC + Platelets + NA. + Potassium + Chloride + CO2 + Urea +
      tb + malaria + bc + csf + any_tb_rx + any_fung_rx + any_mal_rx, family = 'binomial', data = df) -> m

df$calc_age <- scale(df$calc_age)

df$Haemoglobin <- scale(df$Haemoglobin)
df$screentemp <- scale(df$screentemp)
df$t0hr <- scale(df$t0hr)
df$t0map <- scale(df$t0map)
df$t0rr <- scale(df$t0rr)
df$t0spo2 <- scale(df$t0spo2)
df$gcs <- scale(df$gcs)
df$lactate_value <- scale(df$lactate_value)
df$WCC <- scale(df$WCC)
df$Platelets <- scale(df$Platelets)
df$NA. <- scale(df$NA.)
df$Potassium <- scale(df$Potassium)
df$CO2 <- scale(df$CO2)
df$Urea <- scale(df$Urea)
df$Creatinine <- scale(df$Creatinine)

df$d28_death <- as.factor(df$d28_death)
df$hivstatus <- as.factor(df$hivstatus)
df$tb <- as.factor(df$tb)
df$malaria <- as.factor(df$malaria)
df$bc <- as.factor(df$bc)
df$any_tb_rx <- as.factor(df$any_tb_rx)
df$any_fung_rx <- as.factor(df$any_fung_rx)
df$any_mal_rx <- as.factor(df$any_mal_rx)
df$any_ab <- as.factor(df$any_ab)

df$hivstatus <- as.character(df$hivstatus)

df$hivstatus[df$hivstatus == "UK"] <- NA

df2 <- dplyr::select(df, d28_death, calc_age ,ptsex , hivstatus ,Haemoglobin, screentemp ,t0map,
                     t0spo2, gcs, lactate_value, CO2, Urea, Creatinine,
                     tb, bc, malaria,any_tb_rx, any_fung_rx, any_mal_rx, no_diagnosis)

df2.complete <- df2[complete.cases(df2),]

glm(d28_death ~ calc_age + ptsex + hivstatus + Haemoglobin + 
      t0spo2 + gcs + lactate_value +  Urea +  
      tb + bc +  any_tb_rx + any_fung_rx, family = 'binomial', data = df) -> m 

df3 <- dplyr::select(df, d28_death, calc_age ,ptsex , hivstatus ,Haemoglobin, screentemp ,t0map,
                     t0spo2, gcs, lactate_value, CO2, Urea, Creatinine,
                     tb, bc,csf, malaria,any_tb_rx, any_fung_rx, any_mal_rx, time_to_abx)


df3.complete <- df3[complete.cases(df3),]





brms::brm(d28_death ~ Haemoglobin +
            t0spo2 + gcs + lactate_value +  Urea + 
            tb + malaria + bc + csf + time_to_abx + any_tb_rx + any_fung_rx + any_mal_rx, data = df3.complete, family = "bernoulli", prior = m1priors) -> m.brms3

df2.complete %>% add_fitted_draws(m.brms, scale = "linear") %>% ungroup() %>% dplyr::select( -c(d28_death, ptsex, hivstatus, tb, bc, malaria, any_tb_rx, any_fung_rx, any_mal_rx)) %>% pivot_longer(-c(.row, .chain,.iteration, .draw, .value)) %>% ggplot(aes(x= value, y = .value)) + stat_pointinterval() + geom_smooth() + geom_smooth(method = 'lm', linetype = "dashed", color = "red") + facet_wrap(~name, scales = "free")

## make partial residual
extract_draws(m.brms) -> b
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

betas <- dfout[,2:19] * dfout[,23:40]
names(betas) <- paste0("x_beta_", names(betas)) 
dfout <- bind_cols(dfout, betas)

dfout$linear_pred <- apply(dfout,1, function(x) x[22] + sum( x[42: ncol(dfout)]))

dfout$p <- plogis(dfout$linear_pred)
dfout$resid <- (dfout$Y - dfout$p) / (dfout$p*(1- dfout$p))

dfout[,42:59] <- dfout[,42:59] + dfout$resid

dfout %>% dplyr::select(.row,t0spo2,x_beta_t0spo2) %>%  group_by(.row) %>%
  summarise(spo2 = median(t0spo2), 
            med.par.resid = median(x_beta_t0spo2),
            lci = quantile(x_beta_t0spo2, 0.025),
            uci = quantile(x_beta_t0spo2, 0.975)) -> spo2plot

#dfout[1:10,42:ncol(dfout)] - dfout$linear_pred[1:10]

df2.complete %>% add_fitted_draws(m.brms, scale = "linear") %>% ggplot(aes(x= t0spo2, y = .value)) + stat_pointinterval() + geom_smooth() + geom_smooth(method = 'lm', linetype = "dashed")

df2.complete
