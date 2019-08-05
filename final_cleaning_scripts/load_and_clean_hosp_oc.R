# load and clean outcome


oc <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_outcome_raw.csv", stringsAsFactors = F)

# add in missing from other forms

oc[nrow(oc) + 1,] <- c(rep(NA, 7), "DAS1169S", rep(NA, ncol(oc) -8))
oc$hospoutcomedate[oc$pid == "DAS1169S"] <- "06apr2018"
oc[nrow(oc) + 1,] <- c(rep(NA, 7), "DAS1353S", rep(NA, ncol(oc) -8))
oc$hospoutcomedate[oc$pid == "DAS1353S"] <- "24oct2017"
oc[nrow(oc) + 1,] <- c(rep(NA, 7), "DAS14790", rep(NA, ncol(oc) -8))
oc$hospoutcomedate[oc$pid == "DAS14790"] <- "29may2018"

# take ealiest dates

oc$hospoutcomedate[(oc$hospoutcomedate) == ""] <- NA
oc$hospoutcometime[(oc$hospoutcometime) == ""] <- NA 

oc$hospoutcomedate[is.na(oc$hospoutcomedate)] <- oc$data_date[is.na(oc$hospoutcomedate)]
oc$hospoutcometime[is.na(oc$hospoutcometime)] <- "12:00:00"

sub("00:00:00", "17:00:00", oc$hospoutcometime) -> oc$hospoutcometime




#
oc$hoc.datetime <- parse_datetime(paste0(sub(" 00:00:00", "", oc$hospoutcomedate), " ", oc$hospoutcometime), "%d%b%Y %H:%M:%S")
extra_deaths <- subset(oc, hospoutcome == 3)

extra_deaths %>% group_by(pid) %>% dplyr::slice(which.min(hoc.datetime)) -> extra_deaths

oc %>% group_by(pid) %>% dplyr::slice(which.min(hoc.datetime)) -> oc

subset(extra_deaths, !(pid %in% subset(oc, hospoutcome == 3)$pid)) -> extra_deaths


                     


cat("\n")
cat("Done! \n")
cat("Hosp outcome forms now in oc \n")
cat("All extra deaths in extra_deaths \n")
cat("(They are the deaths that should have been put into followup but weren't. \n")
cat("Share and enjoy! \n  ")



#oc %>% group_by(pid) %>% tally() %>% filter(n > 1)



# get earliest



# DAS1005V - take earliest
# DAS1008P
# DAS10100
#DAS1023R
#DAS1032P - take earliest
#DAS1060H
#DAS1070D
#DAS1078Y
#DAS1108H - take earliest but DEATH

# I think will be ok just to take earliest






