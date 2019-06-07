# mung the data
# mung mung the data
# mung the data
# mung mung the data
# muuuuuuuuuuung the data
# get hourly data into a wide df with ABs 

# This code is a bit of a mess but, life's too short, you know?
require(tidyverse)
require(lubridate)

# needs to have hourly, upto72 and post 72 loaded

# correct errors - missing assume happened halfway through previous hour

# get it to long

 bind_rows( select(hourly,pid, amicro1, amicro1_datetime, amicro_route1),
            select(hourly,pid, amicro2, amicro2_datetime, amicro_route2) %>% 
              dplyr::rename( pid = pid, amicro1= amicro2, amicro1_datetime = amicro2_datetime, amicro_route1 = amicro_route2 )
            ) -> df
           
           
        #   %>% pivot_longer(names_to = "B")

 subset(df, is.na(amicro1) & !is.na(amicro1_datetime))
 # none are missing ab
 
 df.abs <- subset(df, amicro1 != "ARTESUNATE" & amicro1 != "FLUCONAZOLE")
 
 # check the NAs
 
 #duplciate - patient has already recieved cef - delete
 subset(df.abs, !(pid == "DAS1030T" & is.na(amicro1_datetime))) -> df.abs
 
 
 df.mal <- subset(df, amicro1 == "ARTESUNATE")
 
 df.fung <- subset(df, amicro1 == "FLUCONAZOLE")
 # start with df.abs
 
 # how many have more than 1 ab
 
 df.abs %>% group_by(pid) %>% tally() %>% filter(n > 1)
 
 # just 1
 # DAS11553 - both ceftriaxone
 
 # so we can just lose the latest there
 
 df.abs <- subset(df.abs, !(pid == "DAS11553" & amicro1_datetime =="2017-03-06 14:30:00"))
 
 # for antimalarials
 
 df.mal %>% group_by(pid) %>% tally() %>% filter(n > 1)
 
 # get enroll, suset to e1,4
 
 e1.4 <- subset(enroll, arm == 1)
 
 e1.4$arrvivehosp_datetime_str <- paste0(
   sub( " 00:00:00", "", e1.4$datearrivehosp),
   " ",e1.4$timeaarrievhosp
 )
 
 e1.4$triage_datetime_str <- paste0(
   sub( " 00:00:00", "", e1.4$triagedate),
   " ",e1.4$triagetime
 )
 
 e1.4$t0obs_datetime_str <- paste0(
   sub( " 00:00:00", "", e1.4$t0obsdate),
   " ",e1.4$t0obstime
 )
 
 
 e1.4$a4t0obs_datetime_str <- paste0(
    e1.4$t0obsdatea4 ,
   " ",e1.4$t0obstimea4
   
 )
 
 e1.4$t0obs_datetime_str[ e1.4$t0obs_datetime_str == " "] <-  e1.4$a4t0obs_datetime_str[ e1.4$t0obs_datetime_str == " "]
 
 e1.4$arrivehosp_datetime <- parse_datetime(e1.4$arrvivehosp_datetime_str, format = "%d%b%Y %H:%M:%S")
 e1.4$triage_datetime <- parse_datetime(e1.4$triage_datetime_str, format = "%d%b%Y %H:%M:%S")
 e1.4$t0obs_datetime <- parse_datetime(e1.4$t0obs_datetime_str, format = "%d%b%Y %H:%M:%S")
 
 merge(select(e1.4, pid, arm,arrivehosp_datetime ,triage_datetime, t0obs_datetime ), df.abs, all.x= T) -> df.abs

 
 ## ok - add in other forms to see if abd were administered on the following days
 
 select(upto72, pid, assess_type, assess_date, assess_time, amicro1, amicro_route1, amicro2, amicro_route2, 
        amicro3, amicro_route3, amicro4, amicro_route4) -> ut72.ab
 
 ut72.ab[is.na(ut72.ab)] <- "None"
 
 sub(":    ", "09:00",ut72.ab$assess_time ) -> ut72.ab$assess_time
 ut72.ab$assess_datetime <- parse_datetime(
   paste0(ut72.ab$assess_date, " ",ut72.ab$assess_time ), format = "%d-%b-%Y %H:%S")
 
 ut72.ab <- select(ut72.ab, pid, assess_datetime, assess_type, amicro1, amicro2, 
                   amicro3, amicro4)
 
 melt(ut72.ab, id.vars = c("pid", "assess_datetime", "assess_type")) -> u.m
 
 u.m %>% group_by(pid) %>% dplyr::summarise(earliest_assess72 = min(assess_datetime), 
                                     earliest_assess_value72 = value[which.min(assess_datetime)]) -> earliest_assess

 u.m.ab <- filter(u.m, value != "ALBENDAZOLE" & value != "AMPHOTERICIN" & 
                    value != "ARTESUNATE" & value != "FLUCONAZOLE" & value != "NYSTATIN" &
                    value != "TB TREATMENT" & value != "None" )
 
 # check for 2 per pt
#View( u.m.ab %>% group_by(pid) %>% dplyr::mutate(n =length(value)) %>% filter(n > 1) )

 
 u.m.ab %>% filter(value != "None") %>% group_by(pid) %>% 
   dplyr::slice(which.min(assess_datetime)) -> um.ab.first
 
 #
 

 df.abs <- merge(df.abs, um.ab.first, all.x = T)
 
 ## how many are missing amicro1 and have no followup
 
 #nrow(subset(df.abs, is.na(amicro1) & is.na(earliest_assess72)))
 
 names(df.abs)[6:12] <- c("AETC.ab","AETC.ab.datetime", "AETC.ab.route", 
                          "ward.ax.datetime","ward.ax.assess_type", "variable", "ward.ax.ab")
 
 df.abs <- select(df.abs, -variable )
 
 # 13
 
 # get oc
 
oc <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_outcome_raw.csv", stringsAsFactors = F)
 # take ealiest dates
sub("00:00:00", "17:00:00", oc$hospoutcometime) -> oc$hospoutcometime
oc$hoc.datetime <- parse_datetime(paste0(sub(" 00:00:00", "", oc$hospoutcomedate), " ", oc$hospoutcometime), "%d%b%Y %H:%M:%S")

oc %>% group_by(pid) %>% tally() %>% filter(n > 1)

# get earliest

oc %>% group_by(pid) %>% slice(which.min(hoc.datetime)) -> oc

df.abs <- merge(df.abs, select(oc,pid, hoc.datetime, hospoutcome), all.x = T) 
#df.abs

oc$otherantib[is.na(oc$otherantib)] <- as.numeric(grepl("TB", oc$other_q) |
                                                    grepl("Tb", oc$other_q) | 
                                                    grepl("RNHZE", oc$other_q))[is.na(oc$otherantib)] 
oc$coamox[oc$other_q == "Algumentine"] <- 1
oc$coamox[oc$other_q == "Augmentin"] <- 1
oc$cipro[oc$other_q == "Ciprofloxacin"] <- 1

oc$la <- 0
oc$la[oc$other_q == "LA"] <- 1
oc$metro <- as.numeric(grepl("Metro", oc$other_q))

oc.m <- melt(oc[ c(8,16:27, 43:44)], id.vars = c("pid", "abtto_given", "hoc.datetime"))
subset(oc.m, value == 1) -> oc.m

subset(oc.m , variable != "fluco" & variable != "otherantib" & variable != "la") -> oc.m.ab

oc.m.ab %>% group_by(pid) %>% tally() %>% filter(n > 1)
# only 2 have 2 ttos = choose broadest spec for ax

subset(oc.m.ab , !(pid == "DAS1077X" & variable == "amox")) -> oc.m.ab
subset(oc.m.ab , !(pid == "DAS1300H" & variable == "erythro")) -> oc.m.ab

# add back in those who died and what not
#
#subset(df.abs, is.na(amicro1) & is.na(earliest_assess72) & !(date(t0obs_datetime) ==  hospoutcomedate))

bind_rows(
  select(oc, pid,abtto_given) %>% filter(abtto_given != 1),
  oc.m.ab
  ) -> oc.m.ab

oc.m.ab %>% group_by(pid) %>% tally() %>% filter(n > 1)
names(oc.m.ab)[3:4] <- c("tto_datetime", "tto_ab")
oc.m$hoc.datetime[oc.m$abtto_given == 0] <- NA 
merge(df.abs, select(oc.m.ab, pid, abtto_given, tto_datetime, tto_ab), all.x= T) -> df.abs

# ok lets make the final variable

df.abs$first_ab_time <- apply(
  df.abs[c("AETC.ab.datetime","AETC.ab", "ward.ax.datetime", "ward.ax.ab", "tto_datetime","tto_ab" )]
  , 1,
  function(x) min(x[c(1,3,5)], na.rm = T)
  )

parse_datetime(df.abs$first_ab_time ) -> df.abs$first_ab_time

get_ab <- function(x) {
  out <- NA
  if(x[[1]] == x[[7]] & (!is.na(x[[1]]) & !is.na(x[[7]]))) {
    out <- x[[2]]
  } else if (x[[3]] == x[[7]]& (!is.na(x[[3]]) & !is.na(x[[7]]))){
    out <- x[[4]]
  } else if (x[[5]] == x[[7]]& (!is.na(x[[5]]) & !is.na(x[[7]]))) {
    out <- x[[6]]
  }
  return(out)
}

df.abs$first_ab <- apply(
  df.abs[c("AETC.ab.datetime","AETC.ab", "ward.ax.datetime", "ward.ax.ab", "tto_datetime","tto_ab","first_ab_time" )], 
  1,
  get_ab
)
  
# OK so we have four groups

# If there is an ab time in hourly, use that (n = 157) g1

nrow(subset(df.abs, !is.na(AETC.ab))) # 157

# is not, use 9am  on the upto72 hour assessmnet (n = 33)  - but switch to 9a onthe day following admission

nrow(subset(df.abs, is.na(AETC.ab) & !is.na(ward.ax.ab)))

df.abs$ward.ax.datetime[is.na(df.abs$AETC.ab) & !is.na(df$absward.ax.ab)] <- update((df.abs$t0obs_datetime[is.na(df.abs$AETC.ab) & !is.na(df.abs$ward.ax.ab)]) + days(1), hours = 9, minutes = 0)

## if there is nothing THERE use TTO if they were discharged that day or the day after (n=27)

nrow(subset(df.abs, is.na(AETC.ab) & is.na(ward.ax.ab) &
       ((date(t0obs_datetime) ==  date(hoc.datetime)) | ((date(t0obs_datetime) + days(1)) ==  date(hoc.datetime)))
          ))

# checked these:
df.abs$abtto_given[df.abs$hospoutcome == 3] <- 0
df.abs$abtto_given[df.abs$pid %in% c("DAS1127B", "DAS1125F", "DAS1150D", "DAS1482A")] <- 0 # they have a tto for not abs eg LA or tb rx

df.abs$first_ab[is.na(df.abs$AETC.ab) & is.na(df.abs$ward.ax.ab) &
                  ((date(df.abs$t0obs_datetime) ==  date(df.abs$hoc.datetime)) | ((date(df.abs$t0obs_datetime) + days(1)) ==  date(df.abs$hoc.datetime)))
                 & df.abs$abtto_given == 0] <- "none"

# of the remainder

(subset(df.abs, is.na(AETC.ab) & is.na(ward.ax.ab) &
          !((date(t0obs_datetime) ==  date(hoc.datetime)) | ((date(t0obs_datetime) + days(1)) ==  date(hoc.datetime)))
))



# the remainder


# DAS1032P admitted on a friday.

#by tuesday on ceftriaxone. Assume was on ceftriaxone from the day following admission 9am

df.abs$first_ab_time[df.abs$pid == "DAS1032P"] <- 
  update(df.abs$t0obs_datetime[df.abs$pid == "DAS1032P"] + days(1), hours = 9, minutes = 0)

df.abs$first_ab[df.abs$pid == "DAS1032P"] <- "CEFTRIAXONE"

# DAS1024L - on TB rx by d4 - not on day 1. Assume TB rx started D4

df.abs$first_ab[df.abs$pid == "DAS1042L"] <- "none"

## DAS1120P - given LA

df.abs$first_ab[df.abs$pid == "DAS1120P"] <- "none"

# DAS11713 admitted on a monday
# all upto 72 forms missing?
# but by d3 is on ceftriaxone
# assume was on from the morning following admisison 9am

df.abs$first_ab_time[df.abs$pid == "DAS11713"] <- 
  update(df.abs$t0obs_datetime[df.abs$pid == "DAS11713"] + days(1), hours = 9, minutes = 0)

df.abs$first_ab[df.abs$pid == "DAS11713"] <- "CEFTRIAXONE"



# DAS1174Y - no abx at all? Only dc form done. admitted on tues. Stick with the tto though seems unlikley

# DAS1185S - admitted on a fri d/c day ~ 3 on amox/metronidazole - assume was on from day following d/c 9am

df.abs$first_ab_time[df.abs$pid == "DAS1185S"] <- 
  update(df.abs$t0obs_datetime[df.abs$pid == "DAS1185S"] + days(1), hours = 9, minutes = 0)



# DAS1192U - admitted on fri
# by d5 is on cef 
# assume was on by 9am following morning

df.abs$first_ab_time[df.abs$pid == "DAS1192U"] <- 
  update(df.abs$t0obs_datetime[df.abs$pid == "DAS1192U"] + days(1), hours = 9, minutes = 0)

df.abs$first_ab[df.abs$pid == "DAS1192U"] <- "CEFTRIAXONE"


# DAS1481C - outome date is wrong. d/c from AETC after 3hr on cipro

df.abs$first_ab_time[df.abs$pid == "DAS1481C"] <- 
  update(df.abs$t0obs_datetime[df.abs$pid == "DAS1481C"], hours = 13, minutes = 30)

#tidy up

sub("amox", "cipro" ,df.abs$first_ab) -> df.abs$first_ab


#

df.abs$arrivehosp_datetime[df.abs$pid == "DAS10020"] <- update(df.abs$arrivehosp_datetime[df.abs$pid == "DAS10020"], month = 9)
df.abs$arrivehosp_datetime[df.abs$pid == "DAS13990"] <- update(df.abs$arrivehosp_datetime[df.abs$pid == "DAS13990"], year = 2018)



df.abs$earliest_arr_time <- apply(df.abs[3:5], 1, min)
df.abs$earliest_arr_time <- parse_datetime(df.abs$earliest_arr_time )



df.abs$time_to_abx <- difftime(df.abs$first_ab_time, df.abs$earliest_arr_time, units = "hours")
# look at errors
subset(df.abs, time_to_abx < 0)

# 24hr clock error
df.abs$first_ab_time[df.abs$pid =="DAS11545"] <- update(df.abs$t0obs_datetime[df.abs$pid == "DAS11545"], hours = 15, minutes = 45)

# to tim eis wrong - take one from hourly
df.abs$first_ab_time[df.abs$pid =="DAS12177"] <- df.abs$AETC.ab.datetime[df.abs$pid =="DAS12177"] 

# DAS1249S - tto tine is wrong - take the hourly time
df.abs$first_ab_time[df.abs$pid =="DAS1249S"] <- df.abs$AETC.ab.datetime[df.abs$pid =="DAS1249S"] 
df.abs$first_ab[df.abs$pid =="DAS1249S"] <- "CEFTRIAXONE"

# arrival time is wrong DAS1300H

df.abs$earliest_arr_time[df.abs$pid =="DAS1300H"] <- update(df.abs$earliest_arr_time[df.abs$pid =="DAS1300H"], hours = 12, minutes = 30)

# arrival time wrong DAS13225

df.abs$earliest_arr_time[df.abs$pid =="DAS13225"] <- update(df.abs$earliest_arr_time[df.abs$pid =="DAS13225"], hours = 11, minutes = 30)

df.abs$time_to_abx <- difftime(df.abs$first_ab_time, df.abs$earliest_arr_time, units = "hours")
# look at errors
subset(df.abs, time_to_abx < 0)


# yey
#
#median(df.abs$time_to_abx, na.rm = T)

# plot a cuulative incidence curve

#df.abs$hoc.datetime[df.abs$pid =="DAS1295G"] <-update(df.abs$hoc.datetime [df.abs$pid =="DAS1295G"] , hour = 18)
#df.abs$hoc.datetime[df.abs$pid =="DAS1296E"] <-update(df.abs$hoc.datetime [df.abs$pid =="DAS1296E"] , hour = 16, minutes = 30)

#df.abs$time_to_abx[df.abs$event == 0] <- difftime(df.abs$hoc.datetime[df.abs$event == 0], df.abs$earliest_arr_time[df.abs$event == 0], units = "hours")

#subset(df.abs, time_to_abx < 0)
#df.abs$t <- df.abs$time_to_abx
#df.abs$t[df.abs$event == 0] <- 1000


#library(survival)


# nice

# now for TB

# nobody starts TB Rx in the aetc BUT some are already on

select(e1.4, pid, arm,arrivehosp_datetime ,triage_datetime, t0obs_datetime, tbongoing ) -> df.tb

u.m.tb <- subset(u.m, value == "TB TREATMENT")

u.m.tb %>% group_by(pid) %>% slice(which.min(assess_datetime)) -> u.m.tb

merge(df.tb, select(u.m.tb, pid, assess_datetime, value), all.x = T) -> df.tb

names(df.tb)[7:8] <- c("first_tb_date_72", "first_tb_72_val")

# post 72

select(post72, pid, data_date, tb) %>% filter(tb == 1) %>% group_by(pid) %>% slice(which.min(data_date)) -> post72.tb

merge(df.tb, post72.tb, all.x= T) -> df.tb
names(df.tb)[9:10] <- c("first_tb_date_post_72", "first_tb_val_post_72")



# oc

select(oc, pid,  hoc.datetime, otherantib) %>% filter(otherantib == 1) %>% group_by(pid) %>% tally() %>% filter(n > 1)
  
# none have 2 lines

select(oc, pid,  hoc.datetime, otherantib) %>% filter(otherantib == 1) -> oc.tb

df.tb <- merge(df.tb, oc.tb, all.x = T)

df.tb$first_tb_72_val[df.tb$first_tb_72_val == "TB TREATMENT"] <- 1

df.tb$any_tb_rx <- "none"
df.tb$any_tb_rx[df.tb$first_tb_72_val == 1| df.tb$first_tb_val_post_72 == 1 | df.tb$otherantib == 1] <- "yes"
df.tb$any_tb_rx[df.tb$tbongoing == "Yes"] <- "on admission"

df.tb$first_tb_date_72[!is.na(df.tb$first_tb_date_72)] <- update(df.tb$first_tb_date_72[!is.na(df.tb$first_tb_date_72)], hour = 9, minutes = 0 )

df.tb$first_tb_date_post_72 <- as.character(df.tb$first_tb_date_post_72)
df.tb$first_tb_date_post_72[!is.na(df.tb$first_tb_date_post_72 )]  <- paste0(df.tb$first_tb_date_post_72[!is.na(df.tb$first_tb_date_post_72 )] , " 09:00:00")
df.tb$first_tb_date_post_72 <- parse_datetime(df.tb$first_tb_date_post_72)

# assume undated tb rx days occur at 09:00


df.tb$time_of_first_tb_rx <- apply(df.tb[c(7,9,11)], 1, min, na.rm = T) 
df.tb$time_of_first_tb_rx <- parse_datetime(df.tb$time_of_first_tb_rx)
df.tb$arrivehosp_datetime[df.tb$pid == "DAS13990"] <- update(df.tb$arrivehosp_datetime[df.tb$pid == "DAS13990"], year = 2018)

df.tb$earliest_arr_time <- apply(df.tb[3:5], 1, min)
df.tb$earliest_arr_time <- parse_datetime(df.tb$earliest_arr_time)
df.tb$time_of_first_tb_rx[df.tb$pid == "DAS1230D"] <- update(df.tb$time_of_first_tb_rx[df.tb$pid == "DAS1230D"], hour = 17 )
df.tb$time_to_tb_rx <- difftime(df.tb$time_of_first_tb_rx, df.tb$earliest_arr_time, units = "hours")



subset(df.tb, time_to_tb_rx < 0)

# DAS1230D dc date is wrong - assume 1700 following hourly

df.tb %>% filter(any_tb_rx == "yes") %>% summarise(median(time_to_tb_rx)) 
df.tb$time_to_tb_rx[df.tb$any_tb_rx == "on admission"] <- 0

#df.tb$t <- df.tb$time_to_tb_rx
#df.tb$t[df.tb$any_tb_rx == "on admission"] <- 0

# add censor times

#df.tb <- merge(df.tb, select(oc, pid, hoc.datetime), by = "pid", all.x = T)


#df.tb$t[df.tb$any_tb_rx == "none"] <- difftime(df.tb$hoc.datetime.y[df.tb$any_tb_rx == "none"], 
    #                                           df.tb$earliest_arr_time[df.tb$any_tb_rx == "none"], units = "hours")

#subset(df.tb, t < 0)

#df.tb$hoc.datetime.y[df.tb$pid =="DAS12177"] <-update(df.tb$hoc.datetime.y [df.tb$pid =="DAS12177"] , hour = 13, minutes = 30)
#df.tb$hoc.datetime.y[df.tb$pid =="DAS1249S"] <-update(df.tb$hoc.datetime.y [df.tb$pid =="DAS1249S"] , hour = 17, minutes = 00)

#df.tb$hoc.datetime.y[df.tb$pid =="DAS1295G"] <-update(df.tb$hoc.datetime.y [df.tb$pid =="DAS1295G"] , hour = 18)
#df.tb$hoc.datetime.y[df.tb$pid =="DAS1296E"] <-update(df.tb$hoc.datetime.y [df.tb$pid =="DAS1296E"] , hour = 16, minutes = 30)
#df.tb$hoc.datetime.y[df.tb$pid =="DAS13241"] <-update(df.tb$hoc.datetime.y [df.tb$pid =="DAS13241"] , hour = 19, minutes = 0)



### antimalarials

merge(select(e1.4, pid, arm,arrivehosp_datetime ,triage_datetime, t0obs_datetime ), df.mal, all.x= T) -> df.mal

u.m.mal <- filter(u.m,   value == "ARTESUNATE" | value == "LA" )

u.m.mal %>% filter(value != "None") %>% group_by(pid) %>%
  dplyr::slice(which.min(assess_datetime)) -> um.mal.first

df.mal <- merge(df.mal, um.mal.first, all.x = T)

names(df.mal)[6:12] <- c("AETC.ab","AETC.ab.datetime", "AETC.ab.route", 
                         "ward.ax.datetime","ward.ax.assess_type", "variable", "ward.ax.ab")

df.mal <- select(df.mal, -variable )

oc.m.mal <- subset(oc.m, variable == "la")

names(oc.m.mal)[3:4] <- c("tto_datetime", "tto_ab")
merge(df.mal, select(oc.m.mal, pid, abtto_given, tto_datetime, tto_ab), all.x= T) -> df.mal

# generate final variables


df.mal$first_ab_time <- apply(
  df.mal[c("AETC.ab.datetime","AETC.ab", "ward.ax.datetime", "ward.ax.ab", "tto_datetime","tto_ab" )]
  , 1,
  function(x) min(x[c(1,3,5)], na.rm = T)
)

parse_datetime(df.mal$first_ab_time ) -> df.mal$first_ab_time

get_ab <- function(x) {
  out <- NA
  if(x[[1]] == x[[7]] & (!is.na(x[[1]]) & !is.na(x[[7]]))) {
    out <- x[[2]]
  } else if (x[[3]] == x[[7]]& (!is.na(x[[3]]) & !is.na(x[[7]]))){
    out <- x[[4]]
  } else if (x[[5]] == x[[7]]& (!is.na(x[[5]]) & !is.na(x[[7]]))) {
    out <- x[[6]]
  }
  return(out)
}

df.mal$first_ab <- apply(
  df.mal[c("AETC.ab.datetime","AETC.ab", "ward.ax.datetime", "ward.ax.ab", "tto_datetime","tto_ab","first_ab_time" )], 
  1,
  get_ab
)

df.mal$earliest_arr_time <- apply(df.mal[3:5], 1, min)
df.mal$earliest_arr_time <- parse_datetime(df.mal$earliest_arr_time )


df.mal$time_to_mal_rx <- difftime(df.mal$first_ab_time, df.mal$earliest_arr_time, units = "hours")

#median(df.mal$time_to_mal_rx, na.rm= T)




# antifungals

merge(select(e1.4, pid, arm,arrivehosp_datetime ,triage_datetime, t0obs_datetime ), df.fung, all.x= T) -> df.fung

u.m.fung <- filter(u.m,   value == "AMPHOTERICIN" | value == "FLUCONAZOLE" )

u.m.fung %>% filter(value != "None") %>% group_by(pid) %>%
  dplyr::slice(which.min(assess_datetime)) -> um.fung.first

df.fung <- merge(df.fung, um.fung.first, all.x = T)

names(df.fung)[6:12] <- c("AETC.ab","AETC.ab.datetime", "AETC.ab.route", 
                         "ward.ax.datetime","ward.ax.assess_type", "variable", "ward.ax.ab")

df.fung <- select(df.fung, -variable )

oc.m.fung <- subset(oc.m, variable == "fluco")

names(oc.m.fung)[3:4] <- c("tto_datetime", "tto_ab")
merge(df.fung, select(oc.m.fung, pid, abtto_given, tto_datetime, tto_ab), all.x= T) -> df.fung

# generate final variables


df.fung$first_ab_time <- apply(
  df.fung[c("AETC.ab.datetime","AETC.ab", "ward.ax.datetime", "ward.ax.ab", "tto_datetime","tto_ab" )]
  , 1,
  function(x) min(x[c(1,3,5)], na.rm = T)
)

parse_datetime(df.fung$first_ab_time ) -> df.fung$first_ab_time

get_ab <- function(x) {
  out <- NA
  if(x[[1]] == x[[7]] & (!is.na(x[[1]]) & !is.na(x[[7]]))) {
    out <- x[[2]]
  } else if (x[[3]] == x[[7]]& (!is.na(x[[3]]) & !is.na(x[[7]]))){
    out <- x[[4]]
  } else if (x[[5]] == x[[7]]& (!is.na(x[[5]]) & !is.na(x[[7]]))) {
    out <- x[[6]]
  }
  return(out)
}

df.fung$first_ab <- apply(
  df.fung[c("AETC.ab.datetime","AETC.ab", "ward.ax.datetime", "ward.ax.ab", "tto_datetime","tto_ab","first_ab_time" )], 
  1,
  get_ab
)

df.fung$arrivehosp_datetime[df.abs$pid == "DAS13990"] <- update(df.fung$arrivehosp_datetime[df.abs$pid == "DAS13990"], year = 2018)

df.fung$earliest_arr_time <- apply(df.fung[3:5], 1, min)
df.fung$earliest_arr_time <- parse_datetime(df.fung$earliest_arr_time )


df.fung$time_to_fung_rx <- difftime(df.fung$first_ab_time, df.fung$earliest_arr_time, units = "hours")

subset(df.fung, time_to_fung_rx > 100 )

# fine keep as is!

#median(df.fung$time_to_fung_rx, na.rm= T)

cat("\n")
cat("Done! \n")
cat("Time to abs now in df.abs \n  ")
cat("time to tb now in df.tb \n  ")
cat("time to antifungals now in df.fung \n  ")
cat("time to antimalarials now in df.mal \n  ")
cat("Share and enjoy! \n  ")










