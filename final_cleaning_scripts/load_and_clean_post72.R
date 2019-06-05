# load and clean post 72 and put in file called post72

print("cleaning post72hr ......")

library(RMySQL)
library(plyr)
library(dplyr)
library(reshape2)


mydb = dbConnect(MySQL(), user='jlewis', password='jlewis@odk', dbname='odk_dassim', host='10.137.18.15')
#dbListTables(mydb)
rs = dbSendQuery(mydb, "select * from dassim_enrolment")
enroll = fetch(rs, n=-1)
dbClearResult(rs)
dbDisconnect(mydb)


post72 <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/assessment_post72hrs_raw.csv", stringsAsFactors = F)




enroll$enroll_date <- enroll$data_date

post72 <- merge(post72, select(enroll,pid, enroll_date), all.x = T)

post72$data_date <- as.Date(post72$data_date)
post72$enroll_date <- as.Date(post72$enroll_date)

# remove no PIDs


print("checking for forms with no PID...")

print(subset(post72, !(grepl("DAS", post72$pid))))

cont <- readline("Continue and delete these?")
if (cont == "N") {stop()}

hourly <- subset(hourly, (grepl("DAS", post72$pid)))



# remove dates before enrollment

post72$data_date[post72$pid == "DAS1336U" & post72$data_date == "2017-10-13"] <- "2017-11-13"
post72$data_date[post72$pid == "DAS1336U" & post72$data_date == "2017-10-10"] <- "2017-11-10"
post72$data_date[post72$pid == "DAS1337S" & post72$data_date == "2017-10-14"] <- "2017-11-14"
post72$data_date[post72$pid == "DAS1490A" & post72$data_date == "2010-06-19"] <- "2018-06-19"

post72$assess_type <- as.numeric(post72$data_date - post72$enroll_date)



# check for negative ones



negative_assess_type <- post72$pid[post72$assess_type < 0]

print("forms with date before enrollment follow")
print(negative_assess_type)




# find elemeyts that have two forms from the same day

# actually most of these are double scanned I think  with NAs for form_id

removed_doubles <- c("297","296","318", "231","101","232","273","221","145","146",
                     "111","112","113","114","150","148","149","147","12","13","442","463","462", "589","527",
                     "505","676", "552", "551", "549", "735")

post72 <- subset(post72, !(row_id %in% removed_doubles))

sm <- dcast(post72, pid ~ assess_type, fun.aggregate = length)
two.forms.one.day <- sm$pid[apply(sm[, -1], MARGIN = 1, function(x) any(x > 1))]

tf <- subset(post72, pid %in% two.forms.one.day)
#tf[order(tf$pid, tf$assess_type),]

print("forms with two on one day follow")
print(two.forms.one.day)

cont <- readline("Continue?")
if (cont == "N") {stop()}

# how many are missing forms?
# follow the pre 72 hr analysis


mydb = dbConnect(MySQL(), user='jlewis', password='jlewis@odk', dbname='odk_dassim', host='10.137.18.15')
dbListTables(mydb)
rs = dbSendQuery(mydb, "select * from dassim_enrolment")
enroll = fetch(rs, n=-1)
dbClearResult(rs)
rs = dbSendQuery(mydb, "select * from dassim_outcome")
outcome = fetch(rs, n=-1)
dbClearResult(rs)
dbDisconnect(mydb)

outcome$hospoutcomedate[is.na(outcome$hospoutcomedate)] <- outcome$data_date[is.na(outcome$hospoutcomedate)]

missing <- select(enroll, pid, arm, data_date)
missing <- subset(missing, arm == 1 | arm == 2)
forms <- data.frame(table(post72$pid))
names(forms) <- c("pid", "n_forms_in_db")

missing <- merge(missing, forms, all.x = T)
names(missing)[3] <- "enroll_date"
missing$enroll_date <- as.Date(missing$enroll_date, "%Y-%m-%d")

missing<- merge(missing, select(outcome,pid, hospoutcomedate), all.x = T)
missing$hospoutcomedate[missing$pid == "DAS11625"] <- "2017-03-27"

missing$hospoutcomedate <- as.Date(missing$hospoutcomedate, "%Y-%m-%d")

missing$hospoutcomedate[is.na(missing$hospoutcomedate)] <- Sys.Date()

missing$first_post72_form_date <- missing$enroll_date + 3

missing$expected <- NA

missing$first_post72_form_date[missing$first_post72_form_date > missing$hospoutcomedate] <-
  missing$hospoutcomedate[missing$first_post72_form_date > missing$hospoutcomedate] # those with discharge before d3 have no forms
  
missing0 <- subset(missing, missing$first_post72_form_date <= missing$hospoutcomedate)

Nweekdays <- Vectorize(function(a, b) 
  sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday")))


missing0$expected <- Nweekdays( missing0$first_post72_form_date, missing0$hospoutcomedate) 

missing0$n_forms_in_db[is.na(missing0$n_forms_in_db)] <- 0
missing0$missing <- missing0$expected - missing0$n_forms_in_db

# generate a list of missing forms

#names(missing0)[4] <- "n_forms"
missing0 <- unique(missing0)
missing <- (subset(missing0, missing > 0))
missing <- subset(missing, arm == 1 | (arm ==2 & enroll_date >= "2017-09-12"))
missing <- missing[order(missing$enroll_date),]

write.csv(missing,"/Users/joelewis/Documents/PhD/datasets/post72/missing_post72.csv")

write.csv(post72, "/Users/joelewis/Documents/PhD/datasets/post72/post72_clean.csv")
