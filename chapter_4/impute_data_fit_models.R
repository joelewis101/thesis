library(plyr)
library(reshape2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(grid)
library(ggpubr)
library(pheatmap)
library(UpSetR)
library(eulerr)
library(RColorBrewer)
library(scales)
library(lubridate)
library(survival)
library(eq5d)
library(ggsci)
library(survminer)
library(brms)
library(FactoMineR)
library(factoextra)
library(epitools)

wd <- "chapter_4/"
output = "latex"
T <- TRUE

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


# generate figures
#source("chapter_4/generate_consort_diagram.R")
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



h1.ustand <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/other_datasets/h1_ustand.csv")



# calc time to recruitment

hourly %>% group_by(pid) %>% summarise(first_ax = min(date_time, na.rm= T)) -> fa

merge(select(df.abs, pid, earliest_arr_time), fa, all.x = T) -> fa
difftime(fa$first_ax, fa$earliest_arr_time, units = "hours") -> fa$dt
fa$dt2 <- fa$dt
fa$dt2[fa$dt2 > 1] <- fa$dt2[fa$dt2 > 1] -1

time_to_rec_str <- median_iqr_str(as.numeric(fa$dt2),1)

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

df.mal.b <-select(df.mal, pid, first_ab, time_to_mal_rx)
names(df.mal.b)[2:3] <- c("first_mal_rx", "time_to_mal_rx")

df <- merge(df, df.mal.b)
rm(df.mal.b)

df.fung.b <-select(df.fung, pid, first_ab, time_to_fung_rx)
names(df.fung.b)[2:3] <- c("first_fung_rx", "time_to_fung_rx")

df <- merge(df, df.fung.b)
rm(df.fung.b)

df[df == 999] <- NA

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

df <- merge(df, h1.ustand, all.x= T)

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

e1 <- subset(enroll, arm == 1)


df.temp <- dplyr::select(df,pid, d28_death, hivstatus,hivonart, calc_age,ptsex,screentemp, t0sbp, t0dbp, t0map, t0hr, t0rr, t0spo2, gcs, ustand, lactate_value,
                         WCC, CD4_Absolute, Haemoglobin, Platelets, NA.,Potassium, Urea, Creatinine, CO2, Chloride, fluid_6hr, any_tb_rx, time_to_tb_rx, first_mal_rx, time_to_mal_rx, first_fung_rx, time_to_fung_rx, first_ab, time_to_abx, fluid_6hr,
                         bc, tb, csf, malaria
) 

df.temp$fluid_6hr <- df.temp$fluid_6hr/1000

apply(df.temp[c("tb", "malaria", "bc", "csf")], 1, sum) -> df.temp$n_diagnoses


#df.temp2 <- subset(df.temp2, n_diagnoses < 2 & csf == 0)

df.temp$no_diagnosis <- as.numeric(df.temp$n_diagnoses == 0)


df.temp$time_to_abx <- as.numeric(df.temp$time_to_abx)
df.temp$time_to_tb_rx <- as.numeric(df.temp$time_to_tb_rx)
df.temp$time_to_mal_rx <- as.numeric(df.temp$time_to_mal_rx)
df.temp$time_to_fung_rx <- as.numeric(df.temp$time_to_fung_rx)

df.temp$any_ab <- as.numeric(df.temp$first_ab != "none")
df.temp$any_mal_rx <- as.numeric(!is.na(df.temp$first_mal_rx))
df.temp$any_fung_rx <- as.numeric(!is.na(df.temp$first_fung_rx))

df.temp$any_tb_rx[df.temp$any_tb_rx == "on admission"] <- "none"
df.temp$any_tb_rx[df.temp$any_tb_rx == "none"] <- 0
df.temp$any_tb_rx[df.temp$any_tb_rx == "yes"] <- 1
df.temp$any_tb_rx <- as.numeric(df.temp$any_tb_rx)

df.temp <- dplyr::select(df.temp, -c(first_mal_rx, first_fung_rx, first_ab, n_diagnoses))

df.temp[df.temp == 999] <- NA

df.temp$CD4_Absolute[df.temp$hivstatus != "Reactive"] <- NA
df.temp$hivstatus[df.temp$hivstatus == "Reactive"] <- 1
df.temp$hivstatus[df.temp$hivstatus == "Non reactive"] <- 0
df.temp$hivstatus[df.temp$hivstatus == "Unknown"] <- NA
df.temp$hivstatus <- as.numeric(df.temp$hivstatus)


df.temp$ptsex[df.temp$ptsex == "Male"] <- 1
df.temp$ptsex[df.temp$ptsex == "Female"] <- 0
df.temp$ptsex <- as.numeric(df.temp$ptsex)

df.temp$hivonart[df.temp$hivonart == "Yes"] <- 1
df.temp$hivonart[df.temp$hivonart == "No"] <- 0
df.temp$hivonart <- as.numeric(df.temp$hivonart)
#df.temp$ustand <- as.character(df.temp$ustand)
df.mod <- dplyr::select(df.temp, screentemp, d28_death, hivstatus, 
                        calc_age, ptsex, screentemp, t0map, t0hr, t0rr, t0spo2,
                        gcs,ustand, lactate_value, WCC, Haemoglobin, Platelets,
                        CO2, Urea, Creatinine, fluid_6hr, any_tb_rx, any_ab, any_mal_rx, any_fung_rx,
                        bc, tb, csf, malaria)



cor(cor(df.mod[-2], method = "spearman", use = "pair")) -> c

pheatmap(abs(c))

df.mod$hivstatus[is.na(df.mod$hivstatus)] <- "uk"

m1priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b")
)

df.mod[c("d28_death", "hivstatus","ptsex","ustand","any_tb_rx", "any_ab", "any_mal_rx", "any_fung_rx", "bc", "tb", "csf", "malaria")] <-
  lapply(df.mod[c("d28_death", "hivstatus","ptsex","ustand","any_tb_rx", "any_ab", "any_mal_rx", "any_fung_rx", "bc", "tb", "csf", "malaria")], as.factor)



mice(df.mod) -> df.mod.imp
saveRDS(df.mod.imp, "chapter_4/imputed_datasets.rda")
brms::brm_multiple(as.numeric(as.character(d28_death))  ~ ., data = df.mod.imp, family = "bernoulli", prior = m1priors, chains = 4) -> m.b.imp

saveRDS(m.b.imp, "chapter_4/brms_fit_imputed_datasets.rda")

df.mod.cc <- df.mod[complete.cases(df.mod),]

#brms::brm(d28_death  ~ ., data = df.mod.cc, , family = "bernoulli", prior = m1priors) -> m.b.cc

## compare imputed and non

## then do FAMD


## Ok so lets reduce dimensions





#df.preds[c("hivstatus", "ustand")] <- sapply(df.preds[c("hivstatus", "ustand")], as.character)

#df.preds.cc <- df.preds[complete.cases(df.preds),]
#df.preds.cc$d28_death <- as.character(df.preds.cc$d28_death)

df.mod.cc <- df.mod[complete.cases(df.mod),]

#FAMD(df.mod.cc, ncp =17,sup.var = c(2,19:27)) -> f

df.mod.cc$bc <- recode(df.mod.cc$bc, '0' = "No BSI", '1' = "BSI" )
df.mod.cc$tb <- recode(df.mod.cc$tb, '0' = "No TB", '1' = "TB" )
df.mod.cc$malaria <- recode(df.mod.cc$malaria, '0' = "No Malaria", '1' = "Malaria" )
df.mod.cc$csf <- recode(df.mod.cc$csf, '0' = "No Meningitis", '1' = "Meningitis" )

df.mod.cc$hivstatus <- recode(df.mod.cc$hivstatus, '0' = "HIV-", '1' = "HIV+" )
df.mod.cc$ptsex <- recode(df.mod.cc$ptsex, '0' = "Female", '1' = "Male" )
df.mod.cc$ustand <- recode(df.mod.cc$ustand, '0' = "Can stand", '1' = "Can't stand" )

FAMD(df.mod.cc, ncp =17,sup.var = c(2,19:23)) -> f2

cbind(df.mod.cc,f$ind$coord) -> d.complete
cbind(df.mod.cc,f2$ind$coord) -> d.complete2


ggplot(d.complete2, aes(Dim.1, Dim.3, color = as.factor(d28_death))) + geom_point(alpha = 0.7) + theme_bw() + geom_hline(yintercept =0,linetype = "dashed", alpha = 0.7) + geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)  + scale_color_manual(values = c("grey", "red")) + theme(legend.title = element_blank()) 

ggplot(d.complete, aes(Dim.1, Dim.2, color = as.factor(tb))) + geom_point(alpha = 0.7) + theme_bw() + geom_hline(yintercept =0,linetype = "dashed", alpha = 0.7) + geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)  + scale_color_manual(values = c("grey", "red")) + theme(legend.title = element_blank()) -> p1

ggplot(d.complete, aes(Dim.1, Dim.2, color = as.factor(malaria))) + geom_point(alpha = 0.7) + theme_bw() + geom_hline(yintercept =0,linetype = "dashed", alpha = 0.7) + geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7)  + scale_color_manual(values = c("grey", "red")) + theme(legend.title = element_blank()) -> p2

ggarrange(p1,p2, ncol = 2, nrow = 1)

fviz_pca_ind(pca, axes = c(1,2), geom= "point", habillage = "d28_death")

fviz_famd_ind(f, habillage = c("malaria", "tb"))

## now fit each model in turn

m.famd1

brms::brm(d28_death  ~ Dim.1 + fluid_6hr + any_tb_rx + any_ab + any_mal_rx + any_fung_rx, data = d.complete2, , family = "bernoulli", prior = m1priors) -> m.famd1

brms::brm(d28_death  ~ Dim.1 + Dim.2 + fluid_6hr + any_tb_rx + any_ab + any_mal_rx + any_fung_rx, data = d.complete2, , family = "bernoulli", prior = m1priors) -> m.famd2

brms::brm(d28_death  ~ Dim.1 + Dim.2 + Dim.3 + fluid_6hr + any_tb_rx + any_ab + any_mal_rx + any_fung_rx, data = d.complete2, , family = "bernoulli", prior = m1priors) -> m.famd3

brms::brm(d28_death  ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + fluid_6hr + any_tb_rx + any_ab + any_mal_rx + any_fung_rx, data = d.complete2, , family = "bernoulli", prior = m1priors) -> m.famd4

brms::brm(d28_death  ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5 + fluid_6hr + any_tb_rx + any_ab + any_mal_rx + any_fung_rx, data = d.complete2, , family = "bernoulli", prior = m1priors) -> m.famd5

brms::brm(d28_death  ~ ., data = d.complete2[-grep("Dim", names(d.complete2) )], family = "bernoulli", prior = m1priors) -> m.b.cc

saveRDS(m.famd1, "chapter_4/brms_fit_famd1.rda")
saveRDS(m.famd2, "chapter_4/brms_fit_famd2.rda")
saveRDS(m.famd3, "chapter_4/brms_fit_famd3.rda")
saveRDS(m.famd4, "chapter_4/brms_fit_famd4.rda")
saveRDS(m.famd5, "chapter_4/brms_fit_famd5.rda")
saveRDS(m.b.cc, "chapter_4/brms_orig_data_cca.rda")


loo(m.famd1, reloo = TRUE) -> m.famd1.loo
loo(m.famd2, reloo = TRUE) -> m.famd2.loo
loo(m.famd3, reloo = TRUE) -> m.famd3.loo
loo(m.famd4, reloo = TRUE) -> m.famd4.loo
loo(m.famd5, reloo = TRUE) -> m.famd5.loo
loo(m.b.cc, reloo = TRUE) ->  loo.m.b.cc

saveRDS(m.famd1.loo, "chapter_4/brms_fit_famd1_loo.rda")
saveRDS(m.famd2.loo, "chapter_4/brms_fit_famd2_loo.rda")
saveRDS(m.famd3.loo, "chapter_4/brms_fit_famd3_loo.rda")
saveRDS(m.famd4.loo, "chapter_4/brms_fit_famd4_loo.rda")
saveRDS(m.famd5.loo, "chapter_4/brms_fit_famd5_loo.rda")
saveRDS(loo.m.b.cc, "chapter_4/brms_orig_data_cca_loo.rda")

#kfold(m.b.cc, K = 10) ->  loo.m.b.cc

loo_compare(m.famd2.loo, loo.m.b.cc)
loo_compare(m.famd2.loo, m.famd1.loo)

as.data.frame(m.famd1.loo$estimates) -> famd1.est
famd1.est$model <- "1"
famd1.est$var <- rownames(famd1.est)

as.data.frame(m.famd2.loo$estimates) -> famd2.est
famd2.est$model <- "2"
famd2.est$var <- rownames(famd2.est)

as.data.frame(m.famd3.loo$estimates) -> famd3.est
famd3.est$model <- "3"
famd3.est$var <- rownames(famd3.est)

as.data.frame(m.famd4.loo$estimates) -> famd4.est
famd4.est$model <- "4"
famd4.est$var <- rownames(famd4.est)

as.data.frame(m.famd5.loo$estimates) -> famd5.est
famd5.est$model <- "5"
famd5.est$var <- rownames(famd5.est)

as.data.frame(loo.m.b.cc$estimates) -> orig.est
orig.est$model <- "Full model"
orig.est$var <- rownames(orig.est)

elpd.loo <- bind_rows(famd1.est, famd2.est, famd3.est, famd4.est, famd5.est, orig.est)
elpd.loo <- subset(elpd.loo, var == "elpd_loo" )

elpd.loo$model <- factor(elpd.loo$model, levels = c("Full model", 1:5))

ggplot(elpd.loo, aes(model, Estimate, ymin = Estimate - (2*SE), ymax = Estimate + (2*SE))) + geom_point() + geom_errorbar(width = 0) + theme_bw()

### fit time to ab models

#saveRDS(df.mod.imp, "chapter_4/imputed_datasets.rda")

##

cbind(d.complete2, data.frame(time_to_abx = df.temp[which(complete.cases(df.mod)), "time_to_abx"])) -> d.complete2

brm(d28_death ~ time_to_abx + Dim.1+ Dim.2 + Dim.3 + fluid_6hr + any_tb_rx + any_mal_rx + any_fung_rx,
     data = d.complete2, family = "bernoulli", prior = m1priors ) -> tta.mod

saveRDS(tta.mod, "chapter_4/tta.cc.mod.rda")

# do with imputed ds

df.mod.imp <-  readRDS("chapter_4/imputed_datasets.rda")

df.temp.cc <- df.temp[complete.cases(df.temp),]

missing_data_rows <- which(!complete.cases(df.mod))

complete(df.mod.imp, action = "all") -> imp.list

out.famd <- list()
out.coords <- list()

for (i in 1:5) {
  FAMD(imp.list[[i]], graph = FALSE, ind.sup = missing_data_rows, sup.var = c(2,19:23)) -> famd.temp
  out.famd[[i]] <- famd.temp 
  coordtemp <- rbind(famd.temp$ind$coord, famd.temp$ind.sup$coord)
  coordtemp[order(as.numeric(rownames(coordtemp))),] -> coordtemp
  out.coords[[i]] <-   cbind(imp.list[[i]],coordtemp)
  out.coords[[i]] <-   cbind(out.coords[[i]],df.temp[c("time_to_abx", "time_to_tb_rx")])
}

brm_multiple(d28_death ~ Dim.1 +
               Dim.2 + Dim.3 +any_tb_rx + 
               any_fung_rx +
               any_mal_rx + fluid_6hr + time_to_abx, data = out.coords, family = "bernoulli", prior = m1priors) -> m.tta.imp

saveRDS(m.tta.imp, "chapter_4/tta.imp.mod.rda")

#

brm_multiple(d28_death ~ Dim.1 +
      Dim.2 + Dim.3 +any_tb_rx + 
      any_fung_rx +
      any_mal_rx + fluid_6hr + time_to_abx + I(time_to_abx^2) 
       , data = out.coords, family = "bernoulli", prior = m1priors ) -> m.poly

saveRDS(m.poly, "chapter_4/tta.imp.mod.poly.rda")

marginal_effects(m.tta.imp, effects = "time_to_abx")$time_to_abx ->  me.lin
marginal_effects(m.poly, effects = "time_to_abx")$time_to_abx ->  me.poly


me.lin$regression <- "Linear"
me.poly$regression <- "Polynomial"

me.all <- bind_rows(
  me.lin,
  me.poly,
)

ggplot(me.all, aes(time_to_abx, estimate__, ymin = lower__, ymax = upper__, color = regression, fill = regression )) +
  geom_line() + 
  geom_ribbon(alpha = 0.3, color = NA) +
  theme_bw() + 
  xlab("Time to antibacterials (hrs)") + 
  ylab("Pr(Death by 28 days)") +
  theme(legend.title = element_blank())

