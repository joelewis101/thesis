#### get and clean 72hr data and put in df called upto72 ###

library(reshape2)

print("cleaning upto72hr ......")

#source("/Users/joelewis/Documents/PhD/R/PhD/db_extraction/extract_mssql.R")
source("/Users/joelewis/Documents/PhD/R/PhD/cleaning_script/teleform_cleaning_functions.R")

# get data into csv

upto72 <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/assessment_244872hrs_raw.csv", stringsAsFactors = F)

# remove double scanned and consolidate

upto72$fluid1[upto72$row_id == 90  &!is.na(upto72$row_id)] <- upto72$fluid1[upto72$row_id == 95 & !is.na(upto72$row_id) ]
upto72$assess_type[upto72$row_id == 297  &!is.na(upto72$row_id)] <- 2


double_scanned <- c("7","18","19","70","94","95","91","128","130",
                    "244","227","226","233","232","257","256","251","231",
                    "255","195","187","261","276","359","360", "283","332")

upto72 <- subset(upto72, !(upto72$row_id %in% double_scanned))

# check for double scanning

find_double_scanned(upto72)

# ones with no PID

print("checking for forms with no PID...")

print(subset(upto72, !(grepl("DAS", upto72$pid))))
print("deleting any that are there")
#cont <- readline("Continue and delete these?")

#if (cont == "N") {stop()}

upto72 <- subset(upto72, (grepl("DAS", upto72$pid)))



# check for multiple of the same assess_type

# multiple forms - take earliest

multiple_assess_type <- c("109", "108")

upto72 <- subset(upto72, !(row_id %in% multiple_assess_type))

# correct wrong assess types

upto72$assess_type[upto72$row_id == 107] <- 3
upto72$assess_type[upto72$row_id == 173] <- 2
upto72$assess_type[upto72$row_id == 111] <- 2
upto72$assess_type[upto72$row_id == 358] <- 3
# DAS1462I has two assess_type 1 - the initials MA are for DA14598 - so change

upto72$pid[upto72$pid == "DAS1462I" & upto72$pid_ini == "MA "] <- "DAS14598"
upto72$assess_date[upto72$pid == "DAS1336U" & upto72$assess_type == 2] <- "02-NOV-2017"
upto72$assess_date[upto72$pid == "DAS1336U" & upto72$assess_type == 2] <- "02-NOV-2017"
upto72$assess_date[upto72$pid == "DAS1246Y" & upto72$assess_type == 3] <- "29-JAN-2018"

two_assess_type <- dcast(upto72, pid ~ assess_type, fun.aggregate = base::length)

two_d1_assess_type <- two_assess_type$pid[two_assess_type[2] > 1]
two_d2_assess_type <- two_assess_type$pid[two_assess_type[3] > 1]
two_d3_assess_type <- two_assess_type$pid[two_assess_type[4] > 1]



two_assess_type <- c(two_d1_assess_type, two_d2_assess_type, two_d3_assess_type)

if (length(two_assess_type) == 0) {
print("no doubled up assess_type found")  
} else if (length(two_assess_type) != 0) {
 print("doubled up assess_type follow")
   two_assess_type <- subset(upto72, pid %in% two_assess_type)
   two_assess_type <- two_assess_type[order(two_assess_type$pid),]
   print(two_assess_type)
}

# check for NA assess_type

# first correct the ones we know about

upto72$assess_type[upto72$row_id == 104] <- 2
upto72$assess_type[upto72$row_id == 206] <- 3
upto72$assess_type[upto72$row_id == 279] <- 1
upto72$assess_type[upto72$row_id == 392] <- 1

missing_assess_type <- upto72$pid[is.na(upto72$assess_type)]

if (length(missing_assess_type) == 0) {
  print("no NA assess_type found")  
} else if (length(missing_assess_type) != 0) {
  print("missing assess_type follows")

  print(missing_assess_type)
}

# validate pids

subset(upto72, !(pid %in% enroll$pid))
# fine

#cont <- readline("Continue?")
#if (cont == "N") {stop()}

upto72[upto72 == ""] <- NA
upto72$amicro1[upto72$row_id == "385"] <- "CEFTRIAXONE"
upto72$amicro1[upto72$row_id == "165"] <- "CEFTRIAXONE"
upto72$amicro1[upto72$row_id == "343"] <- "CEFTRIAXONE"
upto72$amicro1[upto72$row_id == "325"] <- "CEFTRIAXONE"
upto72$amicro1[upto72$row_id %in% c("56", "57")] <- "CEFTRIAXONE"
upto72$amicro1[upto72$row_id %in% c("308", "127", "212", "402")] <- NA
upto72$amicro1[grepl("AMOX", upto72$amicro1)] <- "AMOXICILLIN"
upto72$amicro1[grepl("ARTEUNATE", upto72$amicro1)] <- "ARTESUNATE"
upto72$amicro1[grepl("AUGMENTINE", upto72$amicro1)] <- "CO-AMOXICLAV"
upto72$amicro1[grepl("CEF", upto72$amicro1)] <- "CEFTRIAXONE"
upto72$amicro1[grepl("TRIAXONE", upto72$amicro1)] <- "CEFTRIAXONE"
upto72$amicro1[grepl("CIPRO", upto72$amicro1)] <- "CIPROFLOXACIN"
upto72$amicro1[grepl("FLOCONAZOLE", upto72$amicro1)] <- "FLUCONAZOLE"
upto72$amicro1[grepl("METRO", upto72$amicro1)] <- "METRONIDAZOLE"
upto72$amicro1[grepl("TB", upto72$amicro1)] <- "TB TREATMENT"

# amicro2

upto72$amicro2[upto72$row_id == "82"] <- "COTRIMOXAZOLE"
upto72$amicro2[grepl("AMOX", upto72$amicro2)] <- "AMOXICILLIN"
upto72$amicro2[upto72$row_id %in% c("320", "298", "331", "364", "402", "374")] <- NA
upto72$amicro2[grepl("DOXY", upto72$amicro2) | grepl("DCN", upto72$amicro2)] <- "DOXYCYCLINE"
upto72$amicro2[grepl("ERYTH", upto72$amicro2) | grepl("ERTHYO", upto72$amicro2)] <- "ERYTHROMYCIN"
upto72$amicro2[grepl("FLAGYL", upto72$amicro2) | grepl("METRO", upto72$amicro2)] <- "METRONIDAZOLE"
upto72$amicro2[grepl("TB", upto72$amicro2)] <- "TB TREATMENT"


upto72$amicro3[upto72$row_id %in% c("318", "385", "402", "262", "85")] <- NA
upto72$amicro3[grepl("COTRIM", upto72$amicro3)] <- "COTRIMOXAZOLE"
upto72$amicro3[grepl("DOXY", upto72$amicro3) | grepl("DCN", upto72$amicro3)] <- "DOXYCYCLINE"
upto72$amicro3[upto72$row_id == "83"] <- "LA"
upto72$amicro3[grepl("TB", upto72$amicro3)] <- "TB TREATMENT"

upto72$amicro4[upto72$row_id %in% c("155", "179","269", "273", "348", "325")] <- NA
upto72$amicro4[grepl("FLUCOX", upto72$amicro4)] <- "FLUCLOXACILLIN"

## others

upto72$assess_date[upto72$pid == "DAS12441" & upto72$assess_type == 3] <- "01-FEB-2018"

cat("\n")
cat("Done! \n")
cat("Up to 72hr forms now in upto72 df.  \n  ")
cat("Share and enjoy! \n  ")
