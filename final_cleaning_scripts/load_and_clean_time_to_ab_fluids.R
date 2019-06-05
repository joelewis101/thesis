# mung the data
# mung mung the data
# mung the data
# mung mung the data
# muuuuuuuuuuung the data
# get hourly data into a wide df with ABs and fluids 

source("final_cleaning_scripts/load_and_clean_hourly.R")

# correct errors - missing assume happened halfway through previous hour

hourly$amicro_time1[hourly$pid == "DAS1140H" & hourly$amicro_time1 == "13:80"] <- "13:50"
hourly$amicro_time1[hourly$pid == "DAS1288E" & hourly$assess_type == 2] <- "16:30"

hourly$date_time_str <- paste0(hourly$assess_date, " ", hourly$assess_time) 
hourly$date_time <- parse_datetime(hourly$date_time_st, format = "%d-%b-%Y %H:%M")
hourly$amicro1_datetime_str <- paste0(hourly$assess_date, " ", hourly$amicro_time1)
hourly$amicro1_datetime <- parse_datetime(hourly$amicro1_datetime_str, format = "%d-%b-%Y %H:%M")
hourly$amicro2_datetime_str <- paste0(hourly$assess_date, " ", hourly$amicro_time2)
hourly$amicro2_datetime <- parse_datetime(hourly$amicro2_datetime_str, format = "%d-%b-%Y %H:%M")

hou


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
 
 # so we can cast into wide and take the ealiest
 
 df.ab
 
 # for antimalarials
 
 df.mal %>% group_by(pid) %>% tally() %>% filter(n > 1)
 
 # get enroll, suset to e1,4
 
 e1.4 <- subset(enroll, arm == 1| arm == 4)
 
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

