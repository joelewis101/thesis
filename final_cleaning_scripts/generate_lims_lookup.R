## load and clean LIMS.csv ###
### v1 25 Jan 2018 ##
## JL ###

require(tidyverse)

#cat(rep("DON'T PANIC ", 100))

### get .CSVs

### get .CSVs
wd.lims <- "/Users/joelewis/Documents/PhD/Data/ESBL"

cat(paste0("\nLoading lims CSVs from ", wd.lims, "/ESBL2.csv ...  \n  "))


lims <- read.csv(paste0(wd.lims,"/ESBL2.csv"), header = TRUE, stringsAsFactors = FALSE)

## need to sort out missing ESBL fields

lims$ESBL[lims$lab_id == "CAE12Q"] <- "Positive"
lims$ESBL[lims$lab_id == "CAJ10E"] <- "Positive"
lims$ESBL[lims$lab_id == "CAE105"] <- "Positive"
lims$ESBL[lims$lab_id == "CAL11C"] <- "Positive"
lims$ESBL[lims$lab_id == "CAC146"] <- "Positive"
lims$ESBL[lims$lab_id == "CAD13H"] <- "Positive"
lims$ESBL[lims$lab_id == "CAB15V"] <- "Positive"
lims$ESBL[lims$lab_id == "CAD120"] <- "Negative"
lims$ESBL[lims$lab_id == "CAD11U"] <- "Positive"
lims$ESBL[lims$lab_id == "CAC14B"] <- "Positive"
lims$ESBL[lims$lab_id == "CAE11N"] <- "Positive"
lims$ESBL[lims$lab_id == "CAK10X"] <- "Positive"
lims$ESBL[lims$lab_id == "CAF170"] <- "Positive"

lims <- subset(lims, lab_id != "CAC12J")

lims <- select(lims, lab_id, pid, ESBL)

# tidy up lims csv

lims$pid <- gsub("(.*),.*", "\\1", lims$pid) #get rid of stuff after comma
lims$pid <- gsub(" ", "", lims$pid)# get rid of spaces

# sort some errors
lims$pid.raw <- lims$pid
lims$lab_id.raw <- lims$lab_id

# this one - wrong ID - ID is DAS1027J and should be DAS1034L 

error<- c("CAB137","CAC120","CAD13K")

lims$pid[lims$lab_id %in% error] <- "DAS1034L"

# two baseline samples for DAS1008P? Ditch later one

lims <- subset(lims, lab_id != "CAB155")

# DAS1002O should be DAS10020

lims$pid[lims$pid == "DAS1002O"] <- "DAS10020"

# DAS1005Y should be DAS1005Y

lims$pid[lims$pid == "DAS1005Y"] <- "DAS1005V"

# DAS1010O should be DAS10100

lims$pid[lims$pid == "DAS1010O"] <- "DAS10100"

# DAS1021Y should be DAS1021V

lims$pid[lims$pid == "DAS1021Y"] <- "DAS1021V"

# DAS10735 should be DAS10753

lims$pid[lims$pid == "DAS10735"] <- "DAS10753"

# DAS1084I should be DAS10841

lims$pid[lims$pid == "DAS1084I"] <- "DAS10841"

# DAS10964 should be DAS1096U

lims$pid[lims$pid == "DAS10964"] <- "DAS1096U"

# DAS1109X should be 1309X

lims$pid[lims$pid == "DAS1109X"] <- "DAS1309X"

lims$pid[lims$pid == "DAS11695"] <- "DAS1169S"

lims$pid[lims$pid == "DAS1175Y"] <- "DAS1175W"

lims$pid[lims$pid == "DAS12627"] <- "DAS1262Y"

lims$pid[lims$pid == "DAS12655"] <- "DAS1265S"

lims$pid[lims$pid == "DAS1284U"] <- "DAS1248U"

lims$pid[lims$pid == "DAS10895"] <- "DAS1089S"

lims$pid[lims$pid == "DAS1165Y"] <- "DAS1165X"

lims$pid[lims$pid == "DAS11775"] <- "DAS1177S"

lims$pid[lims$pid == "DAS117O5"] <- "DAS11705"

lims$pid[lims$pid == "DAS1199D"] <- "DAS1199G"

lims$pid[lims$pid == "DAS12495"] <- "DAS1249S"

lims$pid[lims$pid == "DAS12670"] <- "DAS1267O"

lims$pid[lims$pid == "DAS1270V"] <- "DAS1267O"

lims$pid[lims$pid == "DAS12830"] <- "DAS1283O"

lims$pid[lims$pid == "DAS12861"] <- "DAS1286I"

lims$pid[lims$pid == "DAS1293E"] <- "DAS1296E"

lims$pid[lims$pid == "DAS12959"] <- "DAS1295G"

lims$pid[lims$pid == "DAS12O7B"] <- "DAS1207B"

lims$pid[lims$pid == "DAS13020"] <- "DAS1302D"

lims$pid[lims$pid == "DAS1308I"] <- "DAS13081"

lims$pid[lims$pid == "DAS1324I"] <- "DAS13241"

lims$pid[lims$pid == "DAS13295"] <- "DAS1329S"

lims$pid[lims$pid == "DAS13550"] <- "DAS1355O"

lims$pid[lims$pid == "DAS13741"] <- "DAS1374I"

lims$pid[lims$pid == "DAS1332I"] <- "DAS13321"

lims$pid[lims$pid == "DAS13375"] <- "DAS1337S"

lims$pid[lims$pid == "DAS13470"] <- "DAS1347O"

lims$pid[lims$pid == "DAS13661"] <- "DAS1366I"

lims$pid[lims$pid == "DAS13710"] <- "DAS1371O"

#lims$pid[lims$pid == "DAS1412I"] <- "DAS14121"

#lims$pid[lims$lab_id == "CAM10M" | lims$lab_id == "CAL10R"] <- "DAS14121"

# add CAG105 DAS1112P Negative


if (!("CAG105" %in% lims$lab_id)) {
  limsnew <- lims[1,]
  limsnew$lab_id[1] <- "CAG105"
  limsnew$pid[1] <- "DAS1112P"
  limsnew$ESBL[1] <- "Not applicable"
  
  lims <- rbind(lims,limsnew)
  
}

# add CAB185 DAS1183W Negative


if (!("CAB185" %in% lims$lab_id)) {
  limsnew <- lims[1,]
  limsnew$lab_id[1] <- "CAB185"
  limsnew$pid[1] <- "DAS1183W"
  limsnew$ESBL[1] <- "Not applicable"
  
  lims <- rbind(lims,limsnew)
  
}



# DAS 1165y should be DAS1165X

lims$pid[lims$pid == "DAS1165Y"] <- "DAS1165X"

# DAS13020 should be DAS1032D

lims$pid[lims$pid == "DAS13020"] <- "DAS1302D"

# add CAB155 DAS13225 positive

if (!("CAB166" %in% lims$lab_id)) {
  limsnew <- lims[1,]
  limsnew$lab_id[1] <- "CAB166"
  limsnew$pid[1] <- "DAS13225"
  limsnew$ESBL[1] <- "Positive"
  
  lims <- rbind(lims,limsnew)
  
}

# DAS1325 should be DAS13225

lims$pid[lims$pid == "DAS1325"] <- "DAS13225"

# DAS1324I should be DAS13241

lims$pid[lims$pid == "DAS1324I"] <- "DAS13241"

# DAS13295 should be DAS13295

lims$pid[lims$pid == "DAS13295"] <- "DAS1329S"

# DAS13375 should be DAS1337S

lims$pid[lims$pid == "DAS13375"] <- "DAS1337S"

# DAS13470 should be DAS1347O

lims$pid[lims$pid == "DAS13470"] <- "DAS1347O"


# DAS13661 should be DAS1366I

lims$pid[lims$pid == "DAS13661"] <- "DAS1366I"

# DAS1366I984 should be DAS1366I

lims$pid[lims$pid == "DAS1366I984"] <- "DAS1366I"

# DAS13710 should be DAS1371O

lims$pid[lims$pid == "DAS13710"] <- "DAS1371O"


# DAS13901 should be DAS1390I

lims$pid[lims$pid == "DAS13901"] <- "DAS1390I"

# DAS14255 should be DAS1425S

lims$pid[lims$pid == "DAS14255"] <- "DAS1425S"



# CAB15V should be DAS12959

lims$pid[lims$lab_id == "CAB15V"] <- "DAS1295G"

# CAB17W PID should be DAS1199G

lims$pid[lims$lab_id == "CAB17W"] <- "DAS1199G"

# CAB194 should be DAS1446I

lims$pid[lims$lab_id == "CAB194"] <- "DAS1446I"

# CAC14I should also be DAS1446I

lims$pid[lims$lab_id == "CAC14I"] <- "DAS1446I"

# DAS1412I should be DAS14121

lims$pid[lims$pid == "DAS1412I"] <- "DAS14121"

# DAS12830 should be DAS1283O

lims$pid[lims$pid == "DAS12830"] <- "DAS1283O"

# CAB18V should be DAS11705

lims$pid[lims$pid == "DAS117O5"] <- "DAS11705"

# DAS11775 shoudl be DAS1177S

lims$pid[lims$pid == "DAS11775"] <- "DAS1177S"

# DAS12O7B should be DAS1207B

lims$pid[lims$pid == "DAS12O7B"] <- "DAS1207B"

# DAS12495 should be DAS1249S

lims$pid[lims$pid == "DAS12495"] <- "DAS1249S"

# DAS12670 should be DAS1267O

lims$pid[lims$pid == "DAS12670"] <- "DAS1267O"

# DAS12861 should be DAS1286I

lims$pid[lims$pid == "DAS12861"] <- "DAS1286I"

# DAS14381 should be 1438I

lims$pid[lims$pid == "DAS14381"] <- "DAS1438I"

# DAS13550 should be DAS1355O

lims$pid[lims$pid == "DAS13550"] <- "DAS1355O"

# DAS10182 shoudl be 1018L - though no form on tablet? Need to check with wit

lims$pid[lims$pid == "DAS10182"] <- "DAS1018L"

# DAS10895 should be 1089S

lims$pid[lims$pid == "DAS10895"] <- "DAS1089S"

# DAS14621 should be DAS1462I

lims$pid[lims$pid == "DAS14621"] <- "DAS1462I"

# DAS1270V should be DAS1270Y

lims$pid[lims$pid == "DAS1270V"] <- "DAS1270Y"

# DAS14461 and DAS1446L should be DAS1446I

lims$pid[lims$pid == "DAS14461"] <- "DAS1446I"

# DAS1308I should be DAS13081

lims$pid[lims$pid == "DAS1308I"] <- "DAS13081"

# CAM16L PID mislabelled as DAS1535G - should be DAS1593X d28

lims$pid[lims$lab_id == "CAM16L"] <- "DAS1593X"

lims$pid[lims$pid == "DAS14190"] <- "DAS1419O"

lims$pid[lims$pid == "DAS14415"] <- "DAS1441S"


lims$pid[lims$pid == "DAS1446G"] <- "DAS1447G"


lims$pid[lims$lab_id == "CAL180"] <- "DAS1534I"

lims$pid[lims$pid == "DAS1332I"] <- "DAS13321"

lims$pid[lims$pid == "DAS14510"] <- "DAS1451O"
lims$pid[lims$pid == "DAS14541"] <- "DAS1454I"
lims$pid[lims$pid == "DAS14664"] <- "DAS1466A"
lims$pid[lims$pid == "DAS1487O"] <- "DAS14870"
lims$pid[lims$pid == "DAS1494"] <- "DAS14942"
lims$pid[lims$pid == "DAS1504Y"] <- "DAS1504U"
lims$pid[lims$pid == "DAS15070"] <- "DAS1507O"
lims$pid[lims$pid == "DAS15181"] <- "DAS1518I"
lims$pid[lims$pid == "DAS15261"] <- "DAS1526I"
lims$pid[lims$pid == "DAS15341"] <- "DAS1534I"
lims$pid[lims$pid == "DAS15421"] <- "DAS1542I"
lims$pid[lims$pid == "DAS15501"] <- "DAS1550I"
lims$pid[lims$pid == "DAS1567O"] <- "DAS15670"
lims$pid[lims$pid == "DAS15682"] <- "DAS1568Z"
lims$pid[lims$pid == "DAS1574Z"] <- "DAS15742"
lims$pid[lims$pid == "DAS1575O"] <- "DAS15750"
lims$pid[lims$pid == "DAS1576X"] <- "DAS1576Z"
lims$pid[lims$pid == "DAS16015"] <- "DAS1601S"
lims$pid[lims$pid == "DS13821"] <- "DAS1382I"

### HERE

# mislabelled?

lims$pid[lims$lab_id == "CAC149"] <- "DAS1174Y"

# dupicate IDs


lims$arm <- NA
lims$visit <- NA

# DAS1332I should be DAS13321



# CAE100 is no growth

if (!("CAE100" %in% lims$lab_id)) {
  limsnew <- lims[1,]
  limsnew$lab_id[1] <- "CAE100"
  limsnew$pid[1] <- "DAS11633"
  limsnew$ESBL[1] <- "Not applicable"
  
  lims <- rbind(lims,limsnew)
  
}

# CAB12H is no growth

if (!("CAB12H" %in% lims$lab_id)) {
  limsnew <- lims[1,]
  limsnew$lab_id[1] <- "CAB12H"
  limsnew$pid[1] <- "DAS10649"
  limsnew$ESBL[1] <- "Not applicable"
  
  lims <- rbind(lims,limsnew)
  
}

# CAE105 is pos

if (!("CAE105" %in% lims$lab_id)) {
  limsnew <- lims[1,]
  limsnew$lab_id[1] <- "CAE105"
  limsnew$pid[1] <- "DAS11545"
  limsnew$ESBL[1] <- "Positive"
  
  lims <- rbind(lims,limsnew)
  
}



#lims$pid[lims$pid == "DAS1518I"] <- "DAS1536E"
#CAB arm 1 baseline


lims$arm[grepl("CAB", lims$lab_id)] <- 1
lims$visit[grepl("CAB", lims$lab_id)] <- 0



#CAC arm 1 d7

lims$arm[grepl("CAC", lims$lab_id)] <- 1
lims$visit[grepl("CAC", lims$lab_id)] <- 1

# CAD arm1 d28

lims$arm[grepl("CAD", lims$lab_id)] <- 1
lims$visit[grepl("CAD", lims$lab_id)] <- 2

# CAE arm 1 m3

lims$arm[grepl("CAE", lims$lab_id)] <- 1
lims$visit[grepl("CAE", lims$lab_id)] <- 3

# CAFarm 1 m6

lims$arm[grepl("CAF", lims$lab_id)] <- 1
lims$visit[grepl("CAF", lims$lab_id)] <- 4

# CAG arm 2 baseline

lims$arm[grepl("CAG", lims$lab_id)] <- 2
lims$visit[grepl("CAG", lims$lab_id)] <- 0

# CAH arm 2 d7

lims$arm[grepl("CAH", lims$lab_id)] <- 2
lims$visit[grepl("CAH", lims$lab_id)] <- 1

# CAI arm 2 d28

lims$arm[grepl("CAI", lims$lab_id)] <- 2
lims$visit[grepl("CAI", lims$lab_id)] <- 2


# CAJ arm2 m3

# correct two errors


lims$arm[grepl("CAJ", lims$lab_id)] <- 2


lims$visit[grepl("CAJ", lims$lab_id)] <- 3

lims$visit[grepl("CAJ109", lims$lab_id)] <- 4
#lims$visit[grepl("CAJ106", lims$lab_id)] <- 4

# CAK arm2 m6

# CAF115 should be arm 2

lims$arm[grepl("CAK", lims$lab_id)] <- 2


lims$visit[grepl("CAK", lims$lab_id)] <- 4

lims$arm[lims$lab_id == "CAF115"] <- 2

lims$arm[lims$lab_id == "CAK116"] <- 1

# CAL arm 3 baseline

lims$arm[grepl("CAL", lims$lab_id)] <- 3
lims$visit[grepl("CAL", lims$lab_id)] <- 0

# CAM arm 3 d28

lims$arm[grepl("CAM", lims$lab_id)] <- 3
lims$visit[grepl("CAM", lims$lab_id)] <- 2

# CAN arm 3 m6

lims$arm[grepl("CAN", lims$lab_id)] <- 3
lims$visit[grepl("CAN", lims$lab_id)] <- 4

#lims$ESBL[lims$ESBL != "Positive"] <- "Negative"

# CAN10W should be arm 1

lims$arm[lims$lab_id == "CAN10W"] <- 1

# CAM116 shoudl be DAS1898X and visit 0

lims$visit[lims$lab_id == "CAM116"] <- 0

lims$pid[lims$lab_id == "CAM116"] <- "DAS1898X"
lims$pid[lims$lab_id == "CAM11B"] <- "DAS1898X"

# CAM115 should be visit 0
lims$visit[lims$lab_id == "CAM115"] <- 0

# AND CAF11H is down as arm 1 but should be arm 2

lims$arm[lims$lab_id == "CAF11H"] <- 2

# DAS1355O CAI10G is realy D7

lims$visit[lims$lab_id == "CAI10G"] <- 1

# DAS1168U CAE10J is really 6 month sample, relabel

lims$visit[lims$lab_id == "CAE10J"] <- 4

# AND CAF11H is down as arm 1 but should be arm 2

lims$arm[lims$lab_id == "CAF11H"] <- 2

# CAE12S should be month 6

lims$visit[lims$lab_id == "CAE12S"] <- 4


# CAF10M and CAE10M are recorded as arm 1 should be arm 2

lims$arm[lims$lab_id == "CAE10M"] <- 2
lims$arm[lims$lab_id == "CAF10M"] <- 2

# CAF16Z DAS13137 should be arm 2 rather than arm 1

lims$arm[lims$lab_id == "CAF16Z"] <- 2

# CAF11M - DAS1328U duplicate? Remove?

lims <- subset(lims, lab_id != "CAF11M")

#Subject DAS1436M  for arm 3 enrolment done on 4-APR-2018 has been changed to DAS1898X

#Subject DAS1437K  for arm 3 enrolment done on 5-APR-2018 has been changed to DAS1899Y

lims$pid[lims$lab_id == "CAL11E"] <- "DAS1899Y"
lims$pid[lims$lab_id == "CAM11A"] <- "DAS1899Y"
lims$pid[lims$lab_id == "CAB18S"] <- "DAS1174Y"

# dupicate sampes? CAC14W, CAC14Y?
# Keep 20 June

lims <- subset(lims, lab_id != "CAC14W")

# CAM106 DAS1536E should be baseline

lims$visit[lims$lab_id == "CAM1A6"] <- 0

# CAL180 pid mislabelled should be DAS1534I



lims$visit[lims$lab_id == "CAJ18S"] <- 4


### NOT SURE WHAT IS GOING ON WITH THIS ONE
#### DAS12257 seems to have two 6m samples
#### The first (CAF18R) doesn't have a matching followup entry
## remove it for now

lims <- subset(lims, lab_id != "CAF18R")

# two visits for DAS15742
# remove one for now

lims <- subset(lims, lab_id != "CAK1DM")

# 2 visits 2s for DAS1570A? remove one - can add back in for final Ax

# seems to have 2 d28 visits - removwe 1

lims <- subset(lims, lab_id != "CAI19C") 

lims$visit[lims$lab_id == "CAC14G"] <- 2
lims$visit[lims$lab_id == "CAD1AG"] <- 4
lims$visit[lims$lab_id == "CAE15W"] <- 4
lims$visit[lims$lab_id == "CAD12W"] <- 3
lims$visit[lims$lab_id == "CAD12Q"] <- 3
lims$visit[lims$lab_id == "CAE134"] <- 4
lims$visit[lims$lab_id == "CAJ10S"] <- 4
lims$visit[lims$lab_id == "CAI113"] <- 4
lims$visit[lims$lab_id == "CAE161"] <- 4
lims$visit[lims$lab_id == "CAE1BH"] <- 4
lims$visit[lims$lab_id == "CAE1BK"] <- 4
lims$visit[lims$lab_id == "CAD1AN"] <- 3
lims$visit[lims$lab_id == "CAJ188"] <- 4
lims$visit[lims$lab_id == "CAC159"] <- 2
lims$visit[lims$lab_id == "CAD1AL"] <- 3
lims$visit[lims$lab_id == "CAK1D2"] <- 2
#lims$visit[lims$lab_id == "CAH147"] <- 2
lims$visit[lims$lab_id == "CAI18Y"] <- 2
#lims$visit[lims$lab_id == "CAH145"] <- 2
#lims$visit[lims$lab_id == "CAI18X"] <- 3
#lims$visit[lims$lab_id == "CAJ18K"] <- 4



lims_orgs <- read.csv(paste0(wd.lims,"/ESBL_orgs.csv"), header = TRUE, stringsAsFactors = FALSE)
lims_sens <-  subset(lims_orgs,profile_name == "DASSIM Culture")

lims_sens <- lims_sens[c(1,4,5:10)]
lims_sens[lims_sens == ""] <- NA
lapply(names(lims_sens),function(x) strsplit(x, "\\.")[[1]][[1]]) -> names(lims_sens)

lims_orgs <- select(lims_orgs,sample_number, organism)
lims_orgs <- unique(lims_orgs)


names(lims_orgs)[names(lims_orgs) == "sample_number"] <- "lab_id"

lims_orgs <- merge(lims, lims_orgs, all.x = T)

lims$ESBL[lims$ESBL != "Positive"] <- "Negative"

lims$arm[lims$lab_id == "CAM172"] <- 2

lims$pid[lims$lab_id == "CAN10C"] <- "DAS1898X"

lims$arm[lims$lab_id == "CAE1BF"] <- 2

lims.lookup <- dplyr::select(lims,lab_id,lab_id.raw,pid.raw,pid)
names(lims.lookup)[names(lims.lookup) == "lab_id"] <- "lab_id.corrected"
names(lims.lookup)[names(lims.lookup) == "pid"] <- "pid.corrected"
lims.lookup <- unique(lims.lookup)

cat("\nLIMS pid lookup tabel now in lims.lookup  \n")
cat("This links the raw PIDs with errors to the corrected ones \n")
cat("Share and enjoy! \n")
