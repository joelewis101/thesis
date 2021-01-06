# load PhD data


require(plyr)
require(reshape2)
require(tidyverse)
require(lubridate)

# Data is loaded from 
# /Users/joelewis/Documents/PhD/Data/Current/portal_downloads/

scriptdir <- "~/Documents/PhD/Thesis/bookdown/final_cleaning_scripts/"


# this loads the visit df for tracking fu-------------------
source(paste0(scriptdir,
              "generate_visits_df.R")
)

# and adds death dates

visits$t <- suppressWarnings(apply(visits[7:10], 1,max, na.rm= T ))
visits$t[!is.na(visits$died_date)] <- visits$died_date[!is.na(visits$died_date)]
visits$died <- visits$status
visits$died[visits$died == 2] <- 0
visits$d28_death <- NA
visits$d28_death[(visits$t <= 28) & (visits$died == 1)] <- 1
visits$d28_death[(visits$t >= 28)] <- 0
visits$d90_death[(visits$t <= 90) & (visits$died == 1)] <- 1
visits$d90_death[(visits$t >= 90)] <- 0
visits$d180_death[(visits$t <= 180) & (visits$died == 1)] <- 1
visits$d180_death[(visits$t >= 180)] <- 0

# loads enrolment data into enrol df ----------------------------------- 
source(paste0(scriptdir,
              "load_and_clean_followup_and_enroll_labelled.R"))

# loads HIV data, forms composite HIV, variable, merges in to enroll -----
source(paste0(scriptdir,
       "/make_composite_hivstatus_variable.R"))

# loads bloods data into bloods df -----------------------------------
source(paste0(scriptdir,
              "load_and_clean_bloods.R"))

# loads aetiolology data --------------------------------------------
  # ---
  # blood culture data in bc.full 
  # csf data in bc.full csf.full 
  # for both (TRUE = pos, FALSE = neg, NA = not done)
  # with one hot coding of org
  # ---
  # malaria data in malaria.full
  # xpert culture data in xpert.full  
  # these are just the ODK dump
  # ---
  # myco/f culture data in tbbsi.full  
  # uLAM culture data in ulam.full  
  # should be self explanatory
source(paste0(scriptdir,
              "load_and_clean_aetiol.R"))
# loads hourly data - first 6hrs rx -----------------------------------
# into hourly df
# parsing failures of date are ok - just NA cols
suppressWarnings(
  source(paste0(scriptdir,
                "load_and_clean_hourly.R")
  )
)
# loads data upto 72hrs - first 3d rx ---------------------------------
# into upto72hr df
suppressWarnings(
  source(paste0(scriptdir,
                "load_and_clean_upto72.R")
  )
)
# loads data post 72hrs - post 3d rx ----------------------------------
# into upto72hr df
suppressWarnings(
  source(paste0(scriptdir,
                "load_and_clean_post72.R")
  )
)
# loads hospital outcome forms into oc df --------------------------
source(paste0(scriptdir,
              "load_and_clean_hosp_oc.R")
)
# loads time ot abx into --------------------------------------------
# Time to abs in df.abs 
# time to tb now in df.tb 
# time to antifungals now in df.fung 
# time to antimalarials now in df.mal 
suppressWarnings(
  source(paste0(scriptdir,
                "load_and_clean_time_to_ab.R")
  )
)

# loads fluid administered over hr 1-6 into fluid df
source(paste0(scriptdir,
              "load_and_clean_fluid_hr1_to_6.R")
)

# loads ustand
cat("loading whether unable to stand 1st hour, merging into enroll")
h1.ustand <- read.csv(paste0("/Users/joelewis/Documents/PhD/Data/Current",
                             "/portal_downloads/other_datasets/",
                             "h1_ustand.csv")
)
enroll <- left_join(enroll, h1.ustand, by = "pid")

# tidy up dates is df

oc %>% 
  mutate(
    hospoutcomedate = dmy(gsub(" 00:00:00", "", hospoutcomedate)),
    hospoutcome = case_when(
      hospoutcome == 1 ~ "discharged",
      hospoutcome == 2 ~ "absconded",
      hospoutcome == 3 ~ "died"
    )
  ) ->
  oc

# rm all the rubbish --------------------------------------------------------

rm(art,double_scanned,enroll.names, error, i, missing_assess_type,
     multiple_assess_type, negative_assess_type, pids_missing,
     pids_missing_oxy_supl, pids_missing_temp_extr, removed_doubles,
     two_assess_type,two_d1_assess_type, two_d2_assess_type, two_d3_assess_type,
     two.forms.one.day,wd.clean.script, wd.data, 
     d,death_or_withd, df, e1.4, earliest_assess, etemp, extra_deaths, extras,
     f1, f2,f3,f4, fu_tots, h1.ustand, misshiv,
     n_forms, n_forms.not.6, oc.m, oc.m.ab, oc.m.fung, oc.m.mal,
     oc.tb, pids, post72.tb, sm,tf,u.m, u.m.ab, u.m.fung, u.m.mal,
     u.m.tb, um.ab.first, um.fung.first, um.mal.first)

