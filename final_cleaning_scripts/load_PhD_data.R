# load PhD data


require(plyr)
require(reshape2)
require(tidyverse)

# Data is loaded from 
# /Users/joelewis/Documents/PhD/Data/Current/portal_downloads/

scriptdir <- "~/Documents/PhD/Thesis/bookdown/final_cleaning_scripts/"
# this loads the visit df for tracking fu, not needed -------------------
# source("~/Documents/PhD/Thesis/bookdown/
# final_cleaning_scripts/generate_visits_df.R")  

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
