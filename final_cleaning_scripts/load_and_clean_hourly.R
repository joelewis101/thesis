#### get and clean hr 1-6 data and put in df called hourly ###

### This will output a file that represents the teleform forms - NO interpretation or imputation of missing data

## Cleaning strategy:

### check that forms are not misisng - each patient should have 6 unless discheged or died
### then check that the contents of the box are consistent across treatments
# antibiotic - should all be an antibiotic
# all should have times, and all times should have antibiotic

# fluids should all be a fluid and 
#each fluid volume should have a fluid and each fluid should have a volume

# then check for missing values

# all physiology variables
# all died/discharged are nonmissing that are the last form that we have


# extract from sql server and put into csvs

#library(RMySQL)
#library(plyr)
#library(dplyr)
#source("/Users/joelewis/Documents/PhD/R/PhD/db_extraction/extract_mssql.R")
require(plyr)
require(dplyr)
# get data into csv

#hourly<- read.csv("/Users/joelewis/Documents/PhD/Data/teleform_extraction/hourly.csv", stringsAsFactors = F)
hourly<- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/assessment_hourly_raw.csv", stringsAsFactors = F)

print("cleaning hourly ...")

# ZEROTH check fomrs with no PID

#hourly$pid[hourly$assess_date == "11-JAN-2018" & hourly$pid_ini == "BC "] <- "DAS1265S"
#hourly$pid[hourly$assess_date == "31-JAN-2018" & hourly$pid_ini == "SK "] <- "DAS12361"
#hourly$pid[hourly$assess_date == "02-FEB-2018" & hourly$pid_ini == "CM "] <- "DAS12345"
#hourly$pid[hourly$assess_date == "08-FEB-2018" & hourly$pid_ini == "KC "] <- "DAS1229X"
##hourly$pid[hourly$assess_date == "13-FEB-2018" & hourly$pid_ini == "SM "] <- "DAS1220H"
#hourly$pid[hourly$assess_date == "08-MAR-2018" & hourly$pid_ini == "LF "] <- "DAS1204H"
#hourly$pid[hourly$assess_date == "06-MAR-2018" & hourly$pid_ini == "RL "] <- "DAS1207B"
#hourly$pid[hourly$assess_date == "27-FEB-2018" & hourly$pid_ini == "BC "] <- "DAS1212H"
#hourly$pid[hourly$assess_date == "12-MAR-2018" & hourly$pid_ini == "FM "] <- "DAS1199G"
#hourly$pid[hourly$assess_date == "14-MAR-2018" & hourly$pid_ini == "SK "] <- "DAS1195O"
#hourly$pid[hourly$assess_date == "14-MAR-2018" & hourly$pid_ini == "JS "] <- "DAS1196M"
#hourly$pid[hourly$assess_date == "26-MAR-2018" & hourly$pid_ini == "DC "] <- "DAS1182Y"
#hourly$pid[hourly$assess_date == "26-MAR-2018" & hourly$pid_ini == "CM "] <- "DAS1184U"
#hourly$pid[hourly$assess_date == "26-MAR-2018" & hourly$pid_ini == "JB "] <- "DAS1183W"
#hourly$pid[hourly$assess_date == "26-MAR-2018" & hourly$pid_ini == "MN "] <- "DAS1181X"
#hourly$pid[hourly$assess_date == "21-MAR-2018" & hourly$pid_ini == "MP "] <- "DAS1190Y"
#hourly$pid[hourly$assess_date == "23-MAR-2018" & hourly$pid_ini == "MM "] <- "DAS1185S"
#hourly$pid[hourly$assess_date == "19-MAR-2018" & hourly$pid_ini == "SG "] <- "DAS1191W"
#hourly$pid[hourly$assess_date == "09-MAR-2018" & hourly$pid_ini == "FN "] <- "DAS1200P"
#hourly$pid[hourly$assess_date == "09-MAR-2018" & hourly$pid_ini == "GM "] <- "DAS1201N"
#hourly$pid[hourly$assess_date == "24-APR-2018" & hourly$pid_ini == "KC "] <- "DAS1457C"
#hourly$pid[hourly$assess_date == "05-APR-2018" & hourly$pid_ini == "LW "] <- "DAS1169S"
#hourly$pid[hourly$assess_date == "02-APR-2018" & hourly$pid_ini == "IJ "] <- "DAS11721"
#hourly$pid[hourly$assess_date == "30-MAR-2018" & hourly$pid_ini == "EM "] <- "DAS1174Y"
##hourly$pid[hourly$assess_date == "17-MAY-2018" & hourly$pid_ini == "IU "] <- "DAS14774"
#hourly$pid[hourly$assess_date == "15-MAY-2018" & hourly$pid_ini == "FS "] <- "DAS14766"
#hourly$pid[hourly$assess_date == "10-MAY-2018" & hourly$pid_ini == "PS "] <- "DAS1470I"
#hourly$pid[hourly$assess_date == "14-MAY-2018" & hourly$pid_ini == "MS "] <- "DAS1473C"
#hourly$pid[hourly$pid == "DAS14174A"] <- "DAS1474A"


#print("checking for forms with no PID...")

#print(subset(hourly, !(grepl("DAS", hourly$pid))))

#cont <- readline("Continue and delete these?")
#if (cont == "N") {stop()}

hourly <- subset(hourly, (grepl("DAS", hourly$pid)))


###############################################################
###### CHECK FOR MISSING FORMS ################################
###############################################################


# FIRST CHECK NUMBER OF FORMS PER PATIENT - 
# SHOULD BE 6 for all those in hourly unless died or d/c in AETC

# DAS1077X scanned twice and an error in died

hourly$died[hourly$pid == "DAS1077X" & hourly$died == 1] <- 0
hourly <- subset(hourly, !(row_id <= 298 & row_id >= 293))

# DAS1141F scanned twice

hourly <- subset(hourly, !(row_id <= 400 & row_id >= 395))
hourly$pid[hourly$pid== "DAS1020X" & hourly$assess_date == "07-SEP-2017" ] <- "DAS1012X"

# DAS1141 scanned twice

hourly <- subset(hourly, !(row_id >= 1183 & row_id <= 1188))
hourly$died[hourly$pid == "DAS1283O" & is.na(hourly$died)] <- 0


# DAS 1286I scanned twice

hourly <- subset(hourly, !(row_id >= 1189 & row_id <= 1194))

# DAS1292M hourl 6 scanned 3 times

hourly <- subset(hourly, !(row_id >= 1175 & row_id <= 1176))

#DAS1329S hr 6 scanned twice

hourly <- subset(hourly, !(row_id == 1047))

#DAS1333X hr 6 scanned twice
hourly <- subset(hourly, !(row_id == 1048))

hourly$pid[hourly$pid == "DAS1350Y                      "] <- "DAS1350Y"

#DAS13966 hr 1 scanened twice
hourly <- subset(hourly, !(row_id == 802))

#

hourly$pid[hourly$pid == "DAS12750"] <- "DAS1275O"
hourly$pid[hourly$pid == "DAS1250"] <- "DAS1275O"

hourly$pid[hourly$pid == "DAS1277K                      "] <- "DAS1277K"

hourly$pid[hourly$pid == "DAS1178X"] <- "DAS1173X"

hourly$pid[hourly$pid == "DAS1194Q                      "] <- "DAS1194Q"

hourly$pid[hourly$pid =="DAS1078Y                      "] <- "DAS1078Y"
## DAS1212H has two forms with 2 patient initials
## DAS1212H has initials CK
## DAS1213F recruited on the same day has initials BC, so change ID

hourly$pid[hourly$pid == "DAS1212H" & hourly$pid_ini == "BC "] <- "DAS1213F"

# double scanned DAS1238I

hourly <- subset(hourly, row_id != 1311)

# and DAS1267O

hourly <- subset(hourly, row_id != 1291)
hourly <- subset(hourly, row_id != 1292)

# and DAS1457C

hourly <- subset(hourly, !(row_id >= 1657 & row_id <= 1662))

# and DAS1362I

hourly <- subset(hourly, !(row_id >= 1781 & row_id <= 1791))

# correct DAS1256

hourly$pid[hourly$pid == "DAS1256"] <- "DAS1256U"

# DAS1296E was discharged at hour 4 - correct error

hourly$discharged[hourly$pid == "DAS1296E" & hourly$assess_type == 4] <- 1

# duplicates - ditch

subset(hourly, !(row_id %in% c(1834,1835,1836, 1837, 1975) )) -> hourly

# clean



n_forms <- data.frame(table(hourly$pid))
n_forms.not.6 <- subset(n_forms, Freq != 6)

n_forms.not.6$comments <- "."

n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1032P"] <- "seems to be missing hour 4 - check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1110T"] <- "correct, died at hour 3"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1111R"] <- "correct, discharged at hour 5"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1135B"] <- "correct, died at hour 3"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1149X"] <- "correct, discharged at hour 5"

n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS13886"] <- "Seem to be missing forms; check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1296E"] <- "Correct - discharged at hour 4"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1293K"] <- "DUplicates, leave as is"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS12670"] <- "Seem to be missing forms; check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1276M"] <- "Seem to be missing forms; check paper"
#n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1277K"] <- "Duplicates, leave as is"

n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1173X"] <- "Seem to be missing forms; check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1178Q"] <- "Correct, discharged at hour 4"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1193S"] <- "Correct, discharged at hour 3"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1197K"] <- "Seems to be missing hour 3  check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS12089"] <- "Seems to be missing hour 1  check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS12177"] <- "Correct, discharged at hour 6"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1221F"] <- "Correct, discharged at hour 5"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS12273"] <- "Correct, discharged at hour 3"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1458A"] <- "Seems to be missing hour 3 - check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1464E"] <- "correct, discharged at hour 5"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1474A"] <- "Seems to be missing hour 6 - check paper"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1462I"] <- "Seems to be missing hour 1-5 but present on db - tell data"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1249S"] <- "Correct, discharged at h5"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1481C"] <- "Correct, discharged at h3"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1482A"] <- "Correct, discharged at h2"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS14950"] <- "Correct, discharged at h4"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1506Q"] <- "Correct, discharged at h5"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1507O"] <- "Correct, discharged at h4"
n_forms.not.6$comments[n_forms.not.6$Var1 == "DAS1509K"] <- "Extra hr6 -> hr 7 - leave as is"

#print("number of forms for each pid. SHould be 6. If not, go and do something about it ...")

#print(n_forms.not.6)
#write.csv(n_forms.not.6, "/Users/joelewis/Documents/PhD/datasets/6hr/missing6hr_forms.csv")

#cont <- readline("Continue?")
#if (cont == "N") {stop()}

# what pids are missing entirely?

# get list of arm 1 and 4 pids

#mydb = dbConnect(MySQL(), user='jlewis', password='jlewis@odk', dbname='odk_dassim', host='10.137.18.15')
#dbListTables(mydb)
#rs = dbSendQuery(mydb, "select * from dassim_enrolment")
#enroll = fetch(rs, n=-1)
#dbClearResult(rs)
#dbDisconnect(mydb)

#enroll <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_enrolment_raw.csv", stringsAsFactors = F)


pids <- select(enroll, pid, arm, data_date)
pids <- subset(pids, arm == 1 | arm == 4)
pids <- subset(pids, !(pid %in% hourly$pid))

#print("PIDs completely missing hourly forms")
#print(pids)
#write.csv(pids, "/Users/joelewis/Documents/PhD/datasets/6hr/missing6hr_patients.csv")

#cont <- readline("Continue?")
#if (cont == "N") {stop()}

# now clean up individual fields

###############################################################
###### CHECK FOR MISSING ASSESS_TYPE ################################
###############################################################

hourly$assess_type <- as.numeric(hourly$assess_type)

hourly$assess_type[hourly$pid == "DAS10745" & hourly$assess_time == "12:00"] <- 3
hourly$assess_type[hourly$pid == "DAS1046D" & hourly$assess_time == "17:15"] <- 1
hourly$assess_type[hourly$pid == "DAS1024P" & hourly$assess_time == "14:00"] <- 3
hourly$assess_type[hourly$pid == "DAS1013V" & hourly$assess_time == "14:00"] <- 1
hourly$assess_type[hourly$pid == "DAS1014T" & hourly$assess_time == "13:45"] <- 1
hourly$assess_type[hourly$pid == "DAS1406Y" & hourly$assess_time == "16:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1407W" & hourly$assess_time == "15:30"] <- 1
hourly$assess_type[hourly$pid == "DAS14033" & hourly$assess_time == "15:45"] <- 1
hourly$assess_type[hourly$pid == "DAS14033" & hourly$assess_time == "16:45"] <- 2
hourly$assess_type[hourly$pid == "DAS14033" & hourly$assess_time == "17:45"] <- 3
hourly$assess_type[hourly$pid == "DAS14033" & hourly$assess_time == "20:50"] <- 6
hourly$assess_type[hourly$pid == "DAS1352U" & hourly$assess_time == "19:30"] <- 5
hourly$assess_type[hourly$pid == "DAS1352U" & hourly$assess_time == "20:30"] <- 6
hourly$assess_type[hourly$pid == "DAS1349K" & hourly$assess_time == "20:30"] <- 6
hourly$assess_type[hourly$pid == "DAS1317X" & hourly$assess_time == "19:00"] <- 5
hourly$assess_type[hourly$pid == "DAS1319W" & hourly$assess_time == "14:00"] <- 3
hourly$assess_type[hourly$pid == "DAS1319W" & hourly$assess_time == "17:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1336U" & hourly$assess_time == "13:30"] <- 3
hourly$assess_type[hourly$pid == "DAS13145" & hourly$assess_time == "13:30"] <- 3
hourly$assess_type[hourly$pid == "DAS13153" & hourly$assess_time == "14:30"] <- 6
hourly$assess_type[hourly$pid == "DAS13161" & hourly$assess_time == "11:00"] <- 2
hourly$assess_type[hourly$pid == "DAS13321" & hourly$assess_time == "16:30"] <- 3
hourly$assess_type[hourly$pid == "DAS13313" & hourly$assess_time == "10:00"] <- 1
hourly$assess_type[hourly$pid == "DAS13313" & hourly$assess_time == "15:00"] <- 6
hourly$assess_type[hourly$pid == "DAS13057" & hourly$assess_time == "19:00"] <- 4
hourly$assess_type[hourly$pid == "DAS1297C" & hourly$assess_time == "11:30"] <- 1
hourly$assess_type[hourly$pid == "DAS1297C" & hourly$assess_time == "13:30"] <- 3
hourly$assess_type[hourly$pid == "DAS12998" & hourly$assess_time == "11:00"] <- 2
hourly$assess_type[hourly$pid == "DAS12998" & hourly$assess_time == "14:00"] <- 5
hourly$assess_type[hourly$pid == "DAS1281S" & hourly$assess_time == "15:00"] <- 2
hourly$assess_type[hourly$pid == "DAS1287G" & hourly$assess_time == "18:30"] <- 4
hourly$assess_type[hourly$pid == "DAS1287G" & hourly$assess_time == "18:30"] <- 4
hourly$assess_type[hourly$pid == "DAS1277K" & hourly$assess_time == "11:30"] <- 2
hourly$assess_type[hourly$pid == "DAS1272U" & hourly$assess_time == "18:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1276M" & hourly$assess_time == "14:00"] <- 3
hourly$assess_type[hourly$pid == "DAS1276M" & hourly$assess_time == "17:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1270Y" & hourly$assess_time == "14:06"] <- 3
hourly$assess_type[hourly$pid == "DAS1266Q" & hourly$assess_time == "20:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1231B" & hourly$assess_time == "14:00"] <- 3
hourly$assess_type[hourly$pid == "DAS1230D" & hourly$assess_time == "16:00"] <- 6
hourly$assess_type[hourly$pid == "DAS12417" & hourly$assess_time == "17:00"] <- 4
hourly$assess_type[hourly$pid == "DAS12185" & hourly$assess_time == "13:00"] <- 2
hourly$assess_type[hourly$pid == "DAS1198I" & hourly$assess_time == "15:30"] <- 3
hourly$assess_type[hourly$pid == "DAS1193S" & hourly$assess_time == "16:00"] <- 3
hourly$assess_type[hourly$pid == "DAS1172I" & hourly$assess_time == "19:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1179O" & hourly$assess_time == "12:00"] <- 1
hourly$assess_type[hourly$pid == "DAS14774" & hourly$assess_time == "15:30"] <- 3
hourly$assess_type[hourly$pid == "DAS14766" & hourly$assess_time == "14:00"] <- 1
hourly$assess_type[hourly$pid == "DAS1185S" & hourly$assess_time == "15:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1185S" & hourly$assess_time == "15:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1183W" & hourly$assess_time == "13:00"] <- 2
hourly$assess_type[hourly$pid == "DAS1190Y" & hourly$assess_time == "13:00"] <- 4
hourly$assess_type[hourly$pid == "DAS1458A" & hourly$assess_time == "14:00"] <- 4
hourly$assess_type[hourly$pid == "DAS1458A" & hourly$assess_time == "16:00"] <- 6
hourly$assess_type[hourly$pid == "DAS12409" & hourly$assess_time == "17:00"] <- 4
hourly$assess_type[hourly$pid == "DAS1165X" & hourly$assess_time == "13:14"] <- 1
hourly$assess_type[hourly$pid == "DAS1165X" & hourly$assess_time == "13:14"] <- 1
hourly$assess_type[hourly$pid == "DAS11721" & hourly$assess_time == "19:00"] <- 6
hourly$assess_type[hourly$pid == "DAS14846" & hourly$assess_time == "16:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1481C" & hourly$assess_time == "13:30"] <- 3
hourly$assess_type[hourly$pid == "DAS14870" & hourly$assess_time == "10:40"] <- 1
hourly$assess_type[hourly$pid == "DAS14918" & hourly$assess_time == "14:30"] <- 2
hourly$assess_type[hourly$pid == "DAS1508M" & hourly$assess_time == "19:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1249S" & hourly$assess_time == "17:00"] <- 5
hourly$assess_type[hourly$pid == "DAS1253X" & hourly$assess_time == "20:00"] <- 6
hourly$assess_type[hourly$pid == "DAS1257S" & hourly$assess_time == "15:30"] <- 1
hourly$assess_type[hourly$pid == "DAS1261X" & hourly$assess_time == "16:00"] <- 5
hourly$assess_type[hourly$pid == "DAS12505" & hourly$assess_time == "11:00"] <- 1


#print("checking for missing assess_type...")
#print("should be none!")
#print(subset(hourly, is.na(assess_type)))
#cont <- readline("Continue?")
#if (cont == "N") {stop()}


# clean up errors

# died or discharged

# if there are forms after the ones that are missing or 1 then they must be 0

clean.died.dc.6hr <- function (dfin) {
  dfin <- dfin[order(dfin$assess_type),]
  
  for (i in 1:(nrow(dfin)-1)) {
    dfin$died[i] <- 0
    dfin$discharged[i] <- 0
  }
  return(dfin)
}

hourly <- ddply(hourly, "pid", clean.died.dc.6hr)

# if there are sbp etc then they can't have died

hourly$died[is.na(hourly$died) & !is.na(hourly$sbp)] <- 0
hourly$discharged[is.na(hourly$discharged) & !is.na(hourly$sbp)] <- 0

# if there is an na in died but a 1 in dicharged (or vice-versa) assume the NA is 0

hourly$died[is.na(hourly$died) & hourly$discharged == 1] <- 0
hourly$discharged[is.na(hourly$discharged) & hourly$died == 1] <- 0

#print("missing died ....")

#subset(hourly, is.na(died))
#subset(hourly, is.na(discharged))


###############################################################
###### CLEAN UP ANTIBIOTICS ################################
###############################################################

#print("This is summary of what;s in the amicro vars")
#hourly[hourly == "CEFTRIAZONE"] <- "CEFTRIAXONE"
#hourly[hourly == ""] <- NA

#print("check that there aren't any abx names in anything other than amicro1 and 2")

# rationalise antibiotic names and times, one by one. Each time, check teleform form to make sure OK



# I've checked all the below against the scans

hourly$amicro1[hourly$pid == "DAS1046D" & hourly$assess_type == 4] <- NA
hourly$amicro1[hourly$pid == "DAS1204H" & hourly$assess_type == 5] <- "CEFTRIAXONE"
hourly$amicro1[hourly$pid == "DAS12249" & hourly$assess_type == 3] <- "CEFTRIAXONE"
hourly$amicro1[hourly$pid == "DAS1054D" & hourly$assess_type == 2] <- NA
hourly$amicro1[hourly$pid == "DAS1439G" & hourly$assess_type == 6]<- NA
hourly$amicro1[hourly$pid == "DAS11297" & hourly$assess_type == 4]<- NA
hourly$amicro1[hourly$pid == "DAS1123J" & hourly$assess_type == 5]<- NA
hourly$amicro1[hourly$pid == "DAS12169" & hourly$assess_type == 6]<- "CEFTRIAXONE"
hourly$amicro1[hourly$pid == "DAS1439G" & hourly$assess_type == 1]<- NA
hourly$amicro1[hourly$pid == "DAS1176U" & hourly$assess_type == 3]<- "CEFTRIAXONE"
hourly$amicro1[hourly$pid == "DAS1275O" & hourly$assess_type == 5] <- NA
hourly$amicro1[hourly$pid == "DAS1197K" & hourly$assess_type == 2] <- "CEFTRIAXONE"


#these need checking

hourly$amicro1[hourly$pid == "DAS1277K" & hourly$assess_type == 6] <- NA
hourly$amicro1[hourly$pid == "DAS14870" & hourly$assess_type == 6] <- NA
hourly$amicro1[hourly$pid == "DAS1205F" & hourly$assess_type == 1] <- NA
hourly$amicro1[hourly$pid == "DAS14790" & hourly$assess_type == 5] <- NA
hourly$amicro1[hourly$pid == "DAS1143B" & hourly$assess_type == 2] <- NA


hourly$amicro1[hourly$amicro1 == "AMOXYL"] <- "AMOXICILLIN"
hourly$amicro1[hourly$amicro1 == "AUGMENTINE"] <- "AUGMENTIN"
hourly$amicro1[hourly$amicro1 == "AUGUMENTINE"] <- "AUGMENTIN"
hourly$amicro1[hourly$amicro1 == "AUGUMENTIN"] <- "AUGMENTIN"
hourly$amicro1[hourly$amicro1 == "CEFTRIXONE"] <- "CEFTRIAXONE"
hourly$amicro1[hourly$amicro1 == "CIPROFLOXAXINE"] <- "CIPROFLOXACIN"
hourly$amicro1[hourly$amicro1 == "CIPROFLOXACILLIN"] <- "CIPROFLOXACIN"
hourly$amicro1[hourly$amicro1 == "CIPRO"] <- "CIPROFLOXACIN"
hourly$amicro1[hourly$amicro1 == "DIAZEPUM"] <- NA
hourly$amicro1[hourly$amicro1 == "METRONIDAZOLER"] <- "METRONIDAZOLE"


#print("amicro1")
#print(table(hourly$amicro1))



hourly$amicro2[hourly$pid == "DAS1207B" & hourly$assess_type == 2] <- NA
hourly$amicro2[hourly$pid == "DAS10745" & hourly$assess_type == 2] <- NA
hourly$amicro2[hourly$pid == "DAS11537" & hourly$assess_type == 6] <- NA
hourly$amicro2[hourly$pid == "DAS1121N" & hourly$assess_type == 4] <- NA
hourly$amicro2[hourly$pid == "DAS1457C" & hourly$assess_type == 3] <- NA
hourly$amicro2[hourly$pid == "DAS1236I" & hourly$assess_type == 4] <- NA
hourly$amicro2[hourly$pid == "DAS1190Y" & hourly$assess_type == 5] <- NA
hourly$amicro2[hourly$pid == "DAS12441" & hourly$assess_type == 5] <- NA

# check on scans

hourly$amicro2[hourly$pid == "DAS1268M" & hourly$assess_type == 5] <- NA
hourly$amicro2[hourly$pid == "DAS12361" & hourly$assess_type == 4] <- NA
hourly$amicro2[hourly$pid == "DAS1245X" & hourly$assess_type == 3] <- NA


#print("amicro2")
#print(table(hourly$amicro2))

hourly$amicro3[hourly$pid == "DAS1473C" & hourly$assess_type == 2] <- NA

#print("amicro3")
#print(table(hourly$amicro3))


hourly$amicro4[hourly$pid == "DAS1120P" & hourly$assess_type == 1] <- NA
hourly$amicro4[hourly$amicro4 == "I"] <- NA
hourly$amicro4[hourly$pid == "DAS10577" & hourly$assess_type == 1] <- NA
hourly$amicro4[hourly$pid == "DAS1253X" & hourly$assess_type == 1] <- NA

#print("amicro4")
#print(table(hourly$amicro4))

hourly$amicro5[hourly$pid == "DAS1093X" & hourly$assess_type == 6] <- NA
hourly$amicro5[hourly$pid == "DAS10577" & hourly$assess_type == 2] <- NA
hourly$amicro5[hourly$pid == "DAS1437K" & hourly$assess_type == 6] <- NA
hourly$amicro5[hourly$pid == "DAS10593" & hourly$assess_type == 6] <- NA
hourly$amicro5[hourly$pid == "DAS1261X" & hourly$assess_type == 3] <- NA
hourly$amicro5[hourly$pid == "DAS1435O" & hourly$assess_type == 1] <- NA
hourly$amicro5[hourly$pid == "DAS1055B" & hourly$assess_type == 4] <- NA

#print("amicro5")
#print(table(hourly$amicro5))

hourly$amicro6[hourly$amicro6 == "I" | hourly$amicro6 == " I"] <- NA
hourly$amicro6[hourly$pid == "DAS1293K" & hourly$assess_type == "2"] <- NA

#print("amicro6")
#print(table(hourly$amicro6))

#cont <- readline("Continue?")
#if (cont == "N") {stop()}

# NEXT CHECK ALL TIMES HAVE ABX and ALL ABX have times - amicro1 and 2 only


#hourly$amicro_time2[hourly$pid == "DAS1277K"] <- NA

hourly$amicro_time1[hourly$amicro_time1 == ":    "] <- NA
hourly$amicro_time2[hourly$amicro_time2 == ":    "] <- NA
hourly$amicro_time2[hourly$amicro_time2 == ":1   "] <- NA

#print("forms lacking abx but having time - amicro1")
#print(subset(hourly, is.na(hourly$amicro1) & !is.na(hourly$amicro_time1)))

### check all of these with the data team
#write.csv(subset(hourly, is.na(hourly$amicro1) & !is.na(hourly$amicro_time1)), "/Users/joelewis/Documents/PhD/R/PhD/cleaning_script/data_queries/hourly_missing_amicro1.csv")

#print("forms lacking time but having abx- amicro1")
#print(subset(hourly, !is.na(hourly$amicro1) & is.na(hourly$amicro1)))

#print("forms lacking abx but having time - amicro2")

#print(subset(hourly, is.na(hourly$amicro2) & !is.na(hourly$amicro_time2)))
#print("forms lacking time but having abx- amicro2")
#print(subset(hourly, !is.na(hourly$amicro2) & is.na(hourly$amicro2)))

#cont <- readline("Continue and make all of these ceftriaxone?")
#if (cont == "N") {stop()}

#print("Making all these ceftriaxone")
#hourly$amicro1[is.na(hourly$amicro1) & !is.na(hourly$amicro_time1)] <-  "CEFTRIAXONE"


### check for impossible times


###############################################################
###### CLEAN UP FLUIDS ########################################
###############################################################

#print("This is summary of what;s in the fluid vars")
#print("fluid1")
#print(table(hourly$fluid1))
#print("fluid2")
#print(table(hourly$fluid2))
#print("fluid3")
#print(table(hourly$fluid3))
#print("fluid4")
#print(table(hourly$fluid4))

#print("fluid5 - all of this will be set to NA - check that is what you want")
#print(table(hourly$fluid5))
#hourly$fluid5[!is.na(hourly$fluid5)] <- NA

#print("fluid6 - all of this will be set to NA - check that is what you want")
#print(table(hourly$fluid6))
#hourly$fluid5[!is.na(hourly$fluid6)] <- NA

#cont <- readline("Continue?")
#if (cont == "N") {stop()}

#print("fluid_vol1")
#print(table(hourly$fluid_vol1))

#print("fluid_vol2")
#print(table(hourly$fluid_vol2))

#print("fluid_vol3")
#print(table(hourly$fluid_vol3))

#print("fluid_vol4")
#print(table(hourly$fluid_vol4))

hourly$fluid_vol5[hourly$pid == "DAS1167W" & hourly$assess_type == 1] <- NA

#print("fluid_vol5")
#print(table(hourly$fluid_vol5))

#print("fluid_vol6")
#print(table(hourly$fluid_vol6))

#cont <- readline("Continue?")
#if (cont == "N") {stop()}

###############################################################
###### CLEAN UP PHYSIOLOGY VARS ####################################
###############################################################

# any of these that are misisng should be set to 999 (as per the instructions on teleform)

#print("missing sbp...")

hourly$sbp[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$dbp[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$hrh[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$rr[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$temp[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 99.9
hourly$spo2[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999


hourly$sbp_pslr[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$hr_pslr[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 999
hourly$oxy_suppl[hourly$pid == "DAS1166Y" & hourly$assess_type == 1] <- 9

hourly$sbp[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$dbp[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$hrh[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$rr[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$temp[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 99.9
hourly$spo2[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$sbp_pslr[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$hr_pslr[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 999
hourly$oxy_suppl[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 9



#subset(hourly, is.na(sbp) & !(died == 1 | discharged == 1))

#print("missing dbp...")
#print(subset(hourly, is.na(sbp) & !(died == 1 | discharged == 1)))

#print("missing hrh...")
#print(subset(hourly, is.na(hrh) & !(died == 1 | discharged == 1)))

hourly$rr[hourly$pid == "DAS1020X" & hourly$assess_type == 2] <- 99

#print("missing rr...")
#print(subset(hourly, is.na(rr) & !(died == 1 | discharged == 1)))

hourly$temp[hourly$pid == "DAS1024P" & hourly$assess_type == 6] <- 99.9

#print("missing temp...")
#print(subset(hourly, is.na(temp) & !(died == 1 | discharged == 1)))

hourly$spo2[hourly$pid == "DAS1167W" & (hourly$assess_type == 4 | hourly$assess_type == 5)] <- 999
hourly$spo2[hourly$pid == "DAS1141F" & (hourly$assess_type == 6)] <- 999
hourly$spo2[hourly$pid == "DAS11537" & (hourly$assess_type == 3)] <- 999
hourly$spo2[hourly$pid == "DAS1124H" & (hourly$assess_type == 6)] <- 999
hourly$spo2[hourly$pid == "DAS1107J" & (hourly$assess_type == 5)] <- 999
hourly$spo2[hourly$pid == "DAS11369" & (hourly$assess_type == 6)] <- 999
hourly$spo2[hourly$pid == "DAS10585" & (hourly$assess_type == 6)] <- 999
hourly$spo2[hourly$pid == "DAS1368E" & (hourly$assess_type == 5)] <- 999
hourly$spo2[hourly$pid == "DAS1345S" & (hourly$assess_type == 5)] <- 999
hourly$spo2[hourly$pid == "DAS13057" & (hourly$assess_type == 5)] <- 98
hourly$oxy_supl[hourly$pid == "DAS13057" & (hourly$assess_type == 5)] <- 1
hourly$oxy_flow[hourly$pid == "DAS13057" & (hourly$assess_type == 5)] <- NA
hourly$spo2[hourly$pid == "DAS1446I" & (hourly$assess_type == 6)] <- 96
hourly$spo2[hourly$pid == "DAS1456E" & (hourly$assess_type == 4)] <- 999

hourly$spo2[hourly$pid == "DAS14774" & (hourly$assess_type == 2 | hourly$assess_type == 5)] <- 999
hourly$oxy_tracing[hourly$pid == "DAS14774" & (hourly$assess_type == 2 | hourly$assess_type == 5)] <- 1

# need to check
hourly$spo2[hourly$pid == "DAS11297" & (hourly$assess_type == 4)] <- 999
hourly$spo2[hourly$pid == "DAS1345S" & (hourly$assess_type == 6)] <- 999
hourly$spo2[hourly$pid == "DAS1364M" & (hourly$assess_type == 6)] <- 999

#print("missing spo2...")
#print(subset(hourly, is.na(spo2) & !(died == 1 | discharged == 1)))


# lots of missing oxy_supl
# if the patient is on oxygen for the whole 6hr, or notm, then impute that, look at the rest manually

pids_missing_oxy_supl <- subset(hourly, is.na(oxy_suppl) & !(died == 1 | discharged == 1))
pids_missing_oxy_supl <- unique(pids_missing_oxy_supl$pid)

make.missing.val.same.as.others.if.all.remaining.same <- function(dfin, col) {
  
  n.rows <- nrow(dfin)
  n.missing.rows <- sum(is.na(dfin[,col]))
 #print(unique(dfin$pid))
  if (n.missing.rows > 0 & (n.missing.rows <= n.rows - n.missing.rows) & 
      length(unique((subset(dfin, !is.na(dfin[,col]))[,col]))) == 1) {
    dfin[,col][is.na(dfin[,col])] <- unique((subset(dfin, !is.na(dfin[,col]))[,col]))
  }
  return(dfin)
}

hourly <- ddply(hourly, "pid", make.missing.val.same.as.others.if.all.remaining.same, 16)
hourly$oxy_suppl[hourly$pid == "DAS13974"] <- 1
hourly$oxy_suppl[hourly$pid == "DAS1408U"] <- 1

#pids_missing_oxy_supl <- subset(hourly, is.na(oxy_suppl) & !(died == 1 | discharged == 1))
#pids_missing_oxy_supl <- unique(pids_missing_oxy_supl$pid)

hourly$oxy_suppl[hourly$pid == "DAS1020X"] <- 3
hourly$oxy_flow[hourly$pid == "DAS1020X"] <- 3
hourly$oxy_suppl[hourly$pid == "DAS1336U" & hourly$assess_type == 2] <- 9

#print("missing oxy_suppl...")
#print(subset(hourly, is.na(oxy_suppl) & !(died == 1 | discharged == 1)))

# missing oxy_flow

#pids_missing_oxy_flow <- subset(hourly, is.na(oxy_flow) & oxy_suppl > 1 & !(died == 1 | discharged == 1))
#pids_missing_oxy_flow <- unique(pids_missing_oxy_flow$pid)

hourly$oxy_flow[hourly$pid == "DAS1023R"] <- 5
hourly$oxy_flow[hourly$pid == "DAS1116H" & hourly$assess_type == 6] <- NA
hourly$oxy_suppl[hourly$pid == "DAS1116H" & hourly$assess_type == 6] <- 1
hourly$oxy_suppl[hourly$pid == "DAS1140H" & hourly$assess_type == 4] <- 1
hourly$oxy_flow[hourly$pid == "DAS1142D"] <- 4
hourly$oxy_flow[hourly$pid == "DAS1157X"] <- 5
hourly$oxy_flow[hourly$pid == "DAS11705"] <- 7
hourly$oxy_flow[hourly$pid == "DAS1175W"] <- 99
hourly$oxy_flow[hourly$pid == "DAS12193"] <- 8
hourly$oxy_flow[hourly$pid == "DAS1231B"] <- 8
hourly$oxy_flow[hourly$pid == "DAS1343W"] <- 8
hourly$oxy_flow[hourly$pid == "DAS1447G"] <- 8
hourly$oxy_flow[hourly$pid == "DAS14782" & hourly$assess_type > 1] <- 99

# check on forms

hourly$oxy_flow[hourly$pid == "DAS1511W" & hourly$assess_type == 6] <- 99
hourly$oxy_flow[hourly$pid == "DAS1519G" & hourly$assess_type == 6] <- 99

hourly$oxy_flow[hourly$pid == "DAS1038D" & hourly$assess_type == 5] <- 4
hourly$oxy_flow[hourly$pid == "DAS1247W" & hourly$assess_type == 6] <- 7
hourly$oxy_flow[hourly$pid == "DAS14846" & hourly$assess_type == 4] <- 7



#print("missing oxy_flow")

#print(subset(hourly, is.na(oxy_flow) & oxy_suppl > 1 & oxy_suppl != 9 & !(died == 1 | discharged == 1)))

hourly$gcs_m[hourly$pid == "DAS1020X" & hourly$assess_type == 3] <- 6
hourly$gcs_m[hourly$pid == "DAS13878" & hourly$assess_type == 2] <- 9
hourly$gcs_v[hourly$pid == "DAS13878" & hourly$assess_type == 2] <- 9
hourly$gcs_e[hourly$pid == "DAS13878" & hourly$assess_type == 2] <- 9

hourly$gcs_m[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 6
hourly$gcs_e[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 4
hourly$gcs_v[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 5


#print("missing gcs_m...")
#print(subset(hourly, is.na(gcs_m) & !(died == 1 | discharged == 1)))

hourly$gcs_v[hourly$pid == "DAS1140H" & hourly$assess_type == 2] <- 5
hourly$gcs_e[hourly$pid == "DAS1140H" & hourly$assess_type == 2] <- 4

hourly$gcs_m[hourly$pid == "DAS14033" & hourly$assess_type == 3] <- 9
hourly$gcs_v[hourly$pid == "DAS14033" & hourly$assess_type == 3] <- 9
hourly$gcs_e[hourly$pid == "DAS14033" & hourly$assess_type == 3] <- 9

hourly$gcs_v[hourly$pid == "DAS1290Q" & hourly$assess_type == 1] <- 9
hourly$gcs_e[hourly$pid == "DAS1290Q" & hourly$assess_type == 1] <- 9

hourly$gcs_v[hourly$pid == "DAS11705" & hourly$assess_type == 1] <- 9
hourly$gcs_e[hourly$pid == "DAS11705" & hourly$assess_type == 1] <- 9

hourly$gcs_e[hourly$pid == "DAS1293K" & hourly$assess_type == 2] <- 9

#print("missing gcs_v...")
#print(subset(hourly, is.na(gcs_v) & !(died == 1 | discharged == 1)))

hourly$gcs_e[hourly$pid == "DAS1285K" & hourly$assess_type == 5] <- 4

#print("missing gcs_e...")
#print(subset(hourly, is.na(gcs_e) & !(died == 1 | discharged == 1)))

#print("missing sbp_pslr...")
hourly$sbp_pslr[hourly$pid == "DAS1020X" & hourly$assess_type == 5] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1020X" & hourly$assess_type == 5] <- 999
hourly$hr_pslr[hourly$pid == "DAS1020X" & hourly$assess_type == 5] <- 999

hourly$sbp_pslr[hourly$pid == "DAS1024P" & hourly$assess_type == 1] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1024P" & hourly$assess_type == 1] <- 999
hourly$hr_pslr[hourly$pid == "DAS1024P" & hourly$assess_type == 1] <- 999

hourly$sbp_pslr[hourly$pid == "DAS11457" & hourly$assess_type == 5] <- 999
hourly$dbp_pslr[hourly$pid == "DAS11457" & hourly$assess_type == 5] <- 999
hourly$hr_pslr[hourly$pid == "DAS11457" & hourly$assess_type == 5] <- 999

hourly$sbp_pslr[hourly$pid == "DAS11481" & hourly$assess_type == 6] <- 999
hourly$dbp_pslr[hourly$pid == "DAS11481" & hourly$assess_type == 6] <- 999
hourly$hr_pslr[hourly$pid == "DAS11481" & hourly$assess_type == 6] <- 999

hourly$sbp_pslr[hourly$pid == "DAS1265S" & hourly$assess_type == 4] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1265S" & hourly$assess_type == 4] <- 999
hourly$hr_pslr[hourly$pid == "DAS1265S" & hourly$assess_type == 4] <- 999

hourly$sbp_pslr[hourly$pid == "DAS1337S" & hourly$assess_type == 1] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1337S" & hourly$assess_type == 1] <- 999
hourly$hr_pslr[hourly$pid == "DAS1337S" & hourly$assess_type == 1] <- 999

hourly$sbp_pslr[hourly$pid == "DAS1359G"] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1359G"] <- 999
hourly$hr_pslr[hourly$pid == "DAS1359G"] <- 999

hourly$sbp_pslr[hourly$pid == "DAS1364M" & hourly$assess_type == 3] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1364M" & hourly$assess_type == 3] <- 999
hourly$hr_pslr[hourly$pid == "DAS1364M" & hourly$assess_type == 3] <- 999

hourly$sbp_pslr[hourly$pid == "DAS12521" & hourly$assess_type == 3] <- 999
hourly$dbp_pslr[hourly$pid == "DAS12521" & hourly$assess_type == 3] <- 999
hourly$hr_pslr[hourly$pid == "DAS12521" & hourly$assess_type == 3] <- 999

hourly$sbp_pslr[hourly$pid == "DAS1511W" & hourly$assess_type == 1] <- 999
hourly$dbp_pslr[hourly$pid == "DAS1511W" & hourly$assess_type == 1] <- 999
hourly$hr_pslr[hourly$pid == "DAS1511W" & hourly$assess_type == 1] <- 999

#print(subset(hourly, is.na(sbp_pslr) & !(died == 1 | discharged == 1)))

#print("missing dbp_pslr...")
#print(subset(hourly, is.na(dbp_pslr) & !(died == 1 | discharged == 1)))

#print("missing hr_pslr...")
#print(subset(hourly, is.na(dbp_pslr) & !(died == 1 | discharged == 1)))

## missing volume status vars ##

#print("missing temp_extr...")

hourly <- ddply(hourly, "pid", make.missing.val.same.as.others.if.all.remaining.same, 18)

hourly$temp_extr[hourly$pid == "DAS1012X" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1023R" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS1039B" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1096U" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS1100X" & hourly$assess_type == 3] <- 9
hourly$temp_extr[hourly$pid == "DAS1108H" & hourly$assess_type == 4] <- 2
hourly$temp_extr[hourly$pid == "DAS1120P" & hourly$assess_type == 3] <- 1
hourly$temp_extr[hourly$pid == "DAS1123J" & hourly$assess_type == 4] <- 3
hourly$temp_extr[hourly$pid == "DAS11457" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS11545" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS11545" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1176U" & hourly$assess_type == 3] <- 3
hourly$temp_extr[hourly$pid == "DAS1195O" & hourly$assess_type == 3] <- 3
hourly$temp_extr[hourly$pid == "DAS1196M" & hourly$assess_type == 5] <- 2
hourly$temp_extr[hourly$pid == "DAS1206D" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS12097" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS12185" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS12185" & hourly$assess_type == 4] <- 2

hourly$temp_extr[hourly$pid == "DAS1255W" & hourly$assess_type == 2] <- 9
hourly$temp_extr[hourly$pid == "DAS1255W" & hourly$assess_type == 4] <- 9
hourly$temp_extr[hourly$pid == "DAS1255W" & hourly$assess_type == 6] <- 9

hourly$temp_extr[hourly$pid == "DAS1263W" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS1263W" & hourly$assess_type == 3] <- 2
hourly$temp_extr[hourly$pid == "DAS1294I" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1295G" & hourly$assess_type == 3] <- 2

hourly$temp_extr[hourly$pid == "DAS1297C" & hourly$assess_type == 1] <- 3
hourly$temp_extr[hourly$pid == "DAS1297C" & hourly$assess_type == 5] <- 2

hourly$temp_extr[hourly$pid == "DAS1300H" & hourly$assess_type == 3] <- 2

hourly$temp_extr[hourly$pid == "DAS13145" & hourly$assess_type == 5] <- 2

hourly$temp_extr[hourly$pid == "DAS1317X" & hourly$assess_type == 6] <- 3

hourly$temp_extr[hourly$pid == "DAS1333X" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS1333X" & hourly$assess_type == 4] <- 2

hourly$temp_extr[hourly$pid == "DAS1361S" & hourly$assess_type == 4] <- 2
hourly$temp_extr[hourly$pid == "DAS1362Q" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS13878" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS13878" & hourly$assess_type == 3] <- 2

hourly$temp_extr[hourly$pid == "DAS14009" & hourly$assess_type == 3] <- 2
hourly$temp_extr[hourly$pid == "DAS14009" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS14009" & hourly$assess_type == 6] <- 3

hourly$temp_extr[hourly$pid == "DAS14033" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1408U" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1474A" & hourly$assess_type == 5] <- 3
# 12 March 2019- not checked forms
hourly$temp_extr[hourly$pid == "DAS1078Y" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS12433" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1254Y" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1254Y" & hourly$assess_type == 1] <- 2
hourly$temp_extr[hourly$pid == "DAS1261X" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1435O" & hourly$assess_type == 4] <- 3

hourly$temp_extr[hourly$pid == "DAS1463G" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS1496Z" & hourly$assess_type == 6] <- 3

hourly$temp_extr[hourly$pid == "DAS11297" & hourly$assess_type == 4] <- 2
hourly$temp_extr[hourly$pid == "DAS1207B" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS12177" & hourly$assess_type == 1] <- 2
hourly$temp_extr[hourly$pid == "DAS1257S" & hourly$assess_type == 5] <- 3
hourly$temp_extr[hourly$pid == "DAS1259O" & hourly$assess_type == 5] <- 2
hourly$temp_extr[hourly$pid == "DAS1336U" & hourly$assess_type == 3] <- 2
hourly$temp_extr[hourly$pid == "DAS1350Y" & hourly$assess_type == 4] <- 2
hourly$temp_extr[hourly$pid == "DAS1369C" & hourly$assess_type == 4] <- 2
hourly$temp_extr[hourly$pid == "DAS14121" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS1472E" & hourly$assess_type == 1] <- 2
hourly$temp_extr[hourly$pid == "DAS14758" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS14854" & hourly$assess_type == 2] <- 2
hourly$temp_extr[hourly$pid == "DAS1490A" & hourly$assess_type == 6] <- 3
hourly$temp_extr[hourly$pid == "DAS1511W" & hourly$assess_type == 5] <- 3

pids_missing_temp_extr <- subset(hourly, is.na(temp_extr)  & !(died == 1 | discharged == 1))
pids_missing_temp_extr <- unique(pids_missing_temp_extr$pid)

#print(subset(hourly, is.na(temp_extr) & !(died == 1 | discharged == 1)))


#print("missing sunk eyes...")
## sunk eyes
# 0  = no
# 1 = Y

hourly$sunk_eyes[hourly$pid == "DAS1023R" & hourly$assess_type == 1] <- 1
hourly$sunk_eyes[hourly$pid == "DAS1023R" & hourly$assess_type == 2] <- 1
hourly$sunk_eyes[hourly$pid == "DAS1023R" & hourly$assess_type == 3] <- 1
hourly$sunk_eyes[hourly$pid == "DAS1023R" & hourly$assess_type == 5] <- 0

hourly$sunk_eyes[hourly$pid == "DAS11481" & hourly$assess_type == 2] <- 1
hourly$sunk_eyes[hourly$pid == "DAS11705" & hourly$assess_type == 5] <- 0
hourly$sunk_eyes[hourly$pid == "DAS1183W" & hourly$assess_type == 3] <- 1
hourly$sunk_eyes[hourly$pid == "DAS1275O" & hourly$assess_type == 4] <- 1

hourly$sunk_eyes[hourly$pid == "DAS1287G" & hourly$assess_type == 4] <- 0
hourly$sunk_eyes[hourly$pid == "DAS1287G" & hourly$assess_type == 3] <- 0

# 12 March 2019 haven't checked paper forms

hourly$sunk_eyes[hourly$pid == "DAS14846" & hourly$assess_type == 2] <- 0

hourly <- ddply(hourly, "pid", make.missing.val.same.as.others.if.all.remaining.same, 19)

#print((subset(hourly, is.na(sunk_eyes) & !(died == 1 | discharged == 1))))

# skin turgour

hourly <- ddply(hourly, "pid", make.missing.val.same.as.others.if.all.remaining.same, 20)

# missing
pids_missing <- subset(hourly, is.na(skin_turgor)  & !(died == 1 | discharged == 1))
pids_missing <- unique(pids_missing$pid)

hourly$skin_turgor[hourly$pid == "DAS1023R" & hourly$assess_type == 3] <- 9
hourly$skin_turgor[hourly$pid == "DAS1023R" & hourly$assess_type == 5] <- 1

hourly$skin_turgor[hourly$pid == "DAS1121N" & hourly$assess_type == 2] <- 2
hourly$skin_turgor[hourly$pid == "DAS1143B" & hourly$assess_type == 3] <- 2
hourly$skin_turgor[hourly$pid == "DAS1183W" & hourly$assess_type == 3] <- 2
hourly$skin_turgor[hourly$pid == "DAS1184U" & hourly$assess_type == 2] <- 2
hourly$skin_turgor[hourly$pid == "DAS1206D" & hourly$assess_type == 6] <- 1

# 12 March 2019 haven't checked forms

hourly$skin_turgor[hourly$pid == "DAS1245X" & hourly$assess_type == 3] <- 1
hourly$skin_turgor[hourly$pid == "DAS1249S" & hourly$assess_type == 3] <- 2
hourly$skin_turgor[hourly$pid == "DAS14854" & hourly$assess_type == 1] <- 2

#print("missing skin turgor")
#print((subset(hourly, is.na(skin_turgor) & !(died == 1 | discharged == 1))))

# ustand

hourly <- ddply(hourly, "pid", make.missing.val.same.as.others.if.all.remaining.same, 21)

pids_missing <- subset(hourly, is.na(ustand)  & !(died == 1 | discharged == 1))
pids_missing <- unique(pids_missing$pid)

hourly$ustand[hourly$pid == "DAS1013V" & hourly$assess_type == 5] <- 0
hourly$ustand[hourly$pid == "DAS1020X" & hourly$assess_type == 4] <- 0
hourly$ustand[hourly$pid == "DAS1200P" & hourly$assess_type == 2] <- 1
hourly$ustand[hourly$pid == "DAS1343W" & hourly$assess_type == 1] <- 1
hourly$ustand[hourly$pid == "DAS14113" & hourly$assess_type == 5] <- 0

#print((subset(hourly, is.na(ustand) & !(died == 1 | discharged == 1))))



## now switch the 999s and 9s back to NAs
hourly$assess_type[hourly$assess_type == 9] <- NA
hourly$died[hourly$died == 9] <- NA
hourly$discharged[hourly$discharged == 9] <- NA
hourly$sbp[hourly$sbp == 999] <- NA
hourly$dbp[hourly$dbp == 999] <- NA
hourly$rr[hourly$rr == 999] <- NA
hourly$rr[hourly$rr == 99] <- NA
hourly$hrh[hourly$hrh == 999] <- NA
hourly$temp[hourly$temp == 99.9] <- NA
hourly$oxy_tracing[hourly$oxy_tracing == 9] <- NA
hourly$spo2[hourly$spo2 == 999] <- NA
hourly$oxy_suppl[hourly$oxy_suppl == 9] <- NA
hourly$oxy_flow[hourly$oxy_flow == 9] <- NA
hourly$oxy_flow[hourly$oxy_flow > 10] <- NA
hourly$temp_extr[hourly$temp_extr ==9] <- NA
hourly$sunk_eyes[hourly$sunk_eyes ==9] <- NA
hourly$skin_turgor[hourly$skin_turgor ==9] <- NA
hourly$gcs_m[hourly$gcs_m ==9] <- NA
hourly$gcs_v[hourly$gcs_v ==9] <- NA
hourly$gcs_e[hourly$gcs_e ==9] <- NA

hourly$sbp[hourly$sbp_pslr == 999] <- NA
hourly$dbp[hourly$dbp_pslr == 999] <- NA
hourly$hrh[hourly$hr_pslr == 999] <- NA

hourly[hourly == ":    "] <- NA
hourly[hourly == ":1   "] <- NA

hourly$assess_date[hourly$pid == "DAS1246Y" & hourly$assess_type == 6] <- "26-JAN-2018"


hourly$amicro_time1[hourly$pid == "DAS1140H" & hourly$amicro_time1 == "13:80"] <- "13:50"
hourly$amicro_time1[hourly$pid == "DAS1288E" & hourly$assess_type == 2] <- "16:30"


hourly$assess_date[hourly$pid == "DAS1120P" & hourly$assess_type == 4] <- "18-APR-2017"
hourly$assess_date[hourly$pid == "DAS11801" & hourly$assess_type == 1] <- "27-MAR-2018"
hourly$assess_date[hourly$pid == "DAS1182Y" & hourly$assess_type == 1] <- "26-MAR-2018"
hourly$assess_time[hourly$pid == "DAS1185S" & hourly$assess_type == 1] <- "11:00"
hourly$assess_time[hourly$pid == "DAS1185S" & hourly$assess_type == 2] <- "12:00"
hourly$assess_time[hourly$pid == "DAS1185S" & hourly$assess_type == 3] <- "13:00"
hourly$assess_time[hourly$pid == "DAS1185S" & hourly$assess_type == 4] <- "14:00"
hourly$assess_time[hourly$pid == "DAS1185S" & hourly$assess_type == 5] <- "15:00"
hourly$assess_time[hourly$pid == "DAS1185S" & hourly$assess_type == 6] <- "16:00"
hourly$assess_date[hourly$pid == "DAS1194Q" & hourly$assess_type == 6] <- "18-MAR-2018"

hourly$assess_date[hourly$pid == "DAS1267O" & hourly$assess_type == 1] <- "10-JAN-2018"
hourly$assess_date[hourly$pid == "DAS13241" & hourly$assess_type == 6] <- "13-NOV-2017"
hourly$assess_date[hourly$pid == "DAS1510Y" & hourly$assess_type == 6] <- "09-JUL-2018"

hourly$date_time_str <- paste0(hourly$assess_date, " ", hourly$assess_time) 
hourly$date_time <- parse_datetime(hourly$date_time_st, format = "%d-%b-%Y %H:%M")
hourly$amicro1_datetime_str <- paste0(hourly$assess_date, " ", hourly$amicro_time1)
hourly$amicro1_datetime <- parse_datetime(hourly$amicro1_datetime_str, format = "%d-%b-%Y %H:%M")
hourly$amicro2_datetime_str <- paste0(hourly$assess_date, " ", hourly$amicro_time2)
hourly$amicro2_datetime <- parse_datetime(hourly$amicro2_datetime_str, format = "%d-%b-%Y %H:%M")

cat("Done!")
cat("First 6hr forms now in hourly.  \n  ")
cat("Share and enjoy! \n  ")

#write.csv(hourly, "/Users/joelewis/Documents/PhD/datasets/6hr/hourly_clean.csv")



