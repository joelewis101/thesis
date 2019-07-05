# load and clean post 72 and put in file called post72

print("cleaning post72hr ......")

#library(RMySQL)
#library(plyr)
require(dplyr)
require(reshape2)


post72 <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/assessment_post72hrs_raw.csv", stringsAsFactors = F)



enroll.t <- enroll
enroll.t$enroll_date <- enroll.t$data_date

post72 <- merge(post72, select(enroll.t,pid, enroll_date), all.x = T)
rm(enroll.t)

post72$data_date <- as.Date(post72$data_date, "%d%b%Y")
post72$enroll_date <- as.Date(post72$enroll_date, "%d%b%Y")

# remove no PIDs


#print("checking for forms with no PID...")

#print(subset(post72, !(grepl("DAS", post72$pid))))

#cont <- readline("Continue and delete these?")
#if (cont == "N") {stop()}

hourly <- subset(hourly, (grepl("DAS", post72$pid)))



# remove dates before enrollment

post72$data_date[post72$pid == "DAS1336U" & post72$data_date == "2017-10-13"] <- "2017-11-13"
post72$data_date[post72$pid == "DAS1336U" & post72$data_date == "2017-10-10"] <- "2017-11-10"
post72$data_date[post72$pid == "DAS1337S" & post72$data_date == "2017-10-14"] <- "2017-11-14"
post72$data_date[post72$pid == "DAS1490A" & post72$data_date == "2010-06-19"] <- "2018-06-19"
post72$data_date[post72$pid == "DAS1247W" & post72$data_date == "2018-01-01"] <- "2018-02-01"
post72$data_date[post72$pid == "DAS15670" & post72$data_date == "2013-08-23"] <- "2018-08-23"
post72$data_date[post72$pid == "DAS15670" & post72$data_date == "2013-08-24"] <- "2018-08-24"
post72$data_date[post72$pid == "DAS15814" & post72$data_date == "2015-09-17"] <- "2018-09-17"
post72$assess_type <- as.numeric(post72$data_date - post72$enroll_date)



# check for negative ones



negative_assess_type <- post72$pid[post72$assess_type < 0]

#print("forms with date before enrollment follow")
#print(subset(post72, assess_type < 0))




# find elemeyts that have two forms from the same day

# actually most of these are double scanned I think  with NAs for form_id

removed_doubles <- c("297","296","318", "231","101","232","273","221","145","146",
                     "111","112","113","114","150","148","149","147","12","13","442","463","462", "589","527",
                     "505","676", "552", "551", "549", "735", "778", "153", "152", "832","830", "829",
                     "831", "794", "795", "796", "797", "798", "799", "800", "543", "544", "545",
                     "793", "804", "805", "806", "807", "808","809", "810", "812", "811", "813", "814",
                     "815","816","817", "818", "819", "820", "821", "822", "823", "826", "827", "828", "825",
                     "833", "834", "932", "934", "901", "902", "870", "879", "857")

post72 <- subset(post72, !(row_id %in% removed_doubles))

sm <- dcast(post72, pid ~ assess_type, fun.aggregate = base::length)
two.forms.one.day <- sm$pid[apply(sm[, -1], MARGIN = 1, function(x) any(x > 1))]

tf <- subset(post72, pid %in% two.forms.one.day)
#tf[order(tf$pid, tf$assess_type),]

#print("forms with two on one day follow")
#print(two.forms.one.day)


cat("\n")
cat("Done! \n")
cat("Up to 72hr forms now in upto72 df.  \n  ")
cat("Share and enjoy! \n  ")

