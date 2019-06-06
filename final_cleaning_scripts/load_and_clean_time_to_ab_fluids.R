# mung the data
# mung mung the data
# mung the data
# mung mung the data
# muuuuuuuuuuung the data
# get hourly data into a wide df with ABs and fluids 

source("final_cleaning_scripts/load_and_clean_hourly.R")
source("final_cleaning_scripts/load_and_clean_upto72.R")
source("final_cleaning_scripts/load_and_clean_post72.R")

# correct errors - missing assume happened halfway through previous hour

hourly$amicro_time1[hourly$pid == "DAS1140H" & hourly$amicro_time1 == "13:80"] <- "13:50"
hourly$amicro_time1[hourly$pid == "DAS1288E" & hourly$assess_type == 2] <- "16:30"


hourly$date_time_str <- paste0(hourly$assess_date, " ", hourly$assess_time) 
hourly$date_time <- parse_datetime(hourly$date_time_st, format = "%d-%b-%Y %H:%M")
hourly$amicro1_datetime_str <- paste0(hourly$assess_date, " ", hourly$amicro_time1)
hourly$amicro1_datetime <- parse_datetime(hourly$amicro1_datetime_str, format = "%d-%b-%Y %H:%M")
hourly$amicro2_datetime_str <- paste0(hourly$assess_date, " ", hourly$amicro_time2)
hourly$amicro2_datetime <- parse_datetime(hourly$amicro2_datetime_str, format = "%d-%b-%Y %H:%M")


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
 
 nrow(subset(df.abs, is.na(amicro1) & is.na(earliest_assess72)))
 
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
oc$metro <- as.numeric(grepl("Metro", oc$other_q))

oc.m <- melt(oc[ c(8,16:27, 43)], id.vars = c("pid", "abtto_given", "hoc.datetime"))
subset(oc.m, value == 1) -> oc.m

subset(oc.m , variable != "fluco" & variable != "otherantib") -> oc.m.ab

oc.m.ab %>% group_by(pid) %>% tally() %>% filter(n > 1)
# only 2 have 2 ttos = choose broadest spec for ax

subset(oc.m.ab , !(pid == "DAS1077X" & variable == "amox")) -> oc.m.ab
subset(oc.m.ab , !(pid == "DAS1300H" & variable == "erythro")) -> oc.m.ab

# add back in those who died and what not

subset(df.abs, is.na(amicro1) & is.na(earliest_assess72) & !(date(t0obs_datetime) ==  hospoutcomedate))

bind_rows(
  select(oc, abtto_given) %>% filter(abtto_given != 1),
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

