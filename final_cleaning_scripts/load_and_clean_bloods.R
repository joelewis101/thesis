# load and clean blood tests bru

wd.data <- "/Users/joelewis/Documents/PhD/Data/Current/portal_downloads" 

bloods <- read.csv(paste0(wd.data,"/other_datasets/blood_results.csv"), stringsAsFactors = F)

bloods <- subset(bloods, lab_id != "TEST")

bloods$pid <- gsub("(.*),.*", "\\1", bloods$pid) #get rid of stuff after comma
bloods$pid <- gsub(" ", "", bloods$pid)# get rid of spaces

# sort some errors
# this one - wrong ID - ID is DAS1027J and should be DAS1034L 

error<- c("CAB137","CAC120","CAD13K")

bloods$pid[bloods$lab_id %in% error] <- "DAS1034L"
bloods$pid[bloods$pid == "DAS1002O"] <- "DAS10020"
bloods$pid[bloods$pid == "DAS1005Y"] <- "DAS1005V"
bloods$pid[bloods$pid == "DAS1010O"] <- "DAS10100"
bloods$pid[bloods$pid == "DAS1021Y"] <- "DAS1021V"
bloods$pid[bloods$pid == "DAS10735"] <- "DAS10753"
bloods$pid[bloods$pid == "DAS1084I"] <- "DAS10841"
bloods$pid[bloods$pid == "DAS10964"] <- "DAS1096U"
bloods$pid[bloods$pid == "DAS1109X"] <- "DAS1309X"
bloods$pid[bloods$pid == "DAS11695"] <- "DAS1169S"
bloods$pid[bloods$pid == "DAS1175Y"] <- "DAS1175W"
bloods$pid[bloods$pid == "DAS12627"] <- "DAS1262Y"
bloods$pid[bloods$pid == "DAS12655"] <- "DAS1265S"
bloods$pid[bloods$pid == "DAS1284U"] <- "DAS1248U"
bloods$pid[bloods$pid == "DAS10895"] <- "DAS1089S"
bloods$pid[bloods$pid == "DAS1165Y"] <- "DAS1165X"
bloods$pid[bloods$pid == "DAS11775"] <- "DAS1177S"
bloods$pid[bloods$pid == "DAS117O5"] <- "DAS11705"
bloods$pid[bloods$pid == "DAS1199D"] <- "DAS1199G"
bloods$pid[bloods$pid == "DAS12495"] <- "DAS1249S"
bloods$pid[bloods$pid == "DAS12670"] <- "DAS1267O"
bloods$pid[bloods$pid == "DAS1270V"] <- "DAS1267O"
bloods$pid[bloods$pid == "DAS12830"] <- "DAS1283O"
bloods$pid[bloods$pid == "DAS12861"] <- "DAS1286I"
bloods$pid[bloods$pid == "DAS1293E"] <- "DAS1296E"
bloods$pid[bloods$pid == "DAS12959"] <- "DAS1295G"
bloods$pid[bloods$pid == "DAS12O7B"] <- "DAS1207B"
bloods$pid[bloods$pid == "DAS13020"] <- "DAS1302D"
bloods$pid[bloods$pid == "DAS1308I"] <- "DAS13081"
bloods$pid[bloods$pid == "DAS1324I"] <- "DAS13241"
bloods$pid[bloods$pid == "DAS13295"] <- "DAS1329S"
bloods$pid[bloods$pid == "DAS13550"] <- "DAS1355O"
bloods$pid[bloods$pid == "DAS13741"] <- "DAS1374I"
bloods$pid[bloods$pid == "DAS1332I"] <- "DAS13321"
bloods$pid[bloods$pid == "DAS13375"] <- "DAS1337S"
bloods$pid[bloods$pid == "DAS13470"] <- "DAS1347O"
bloods$pid[bloods$pid == "DAS13661"] <- "DAS1366I"
bloods$pid[bloods$pid == "DAS13710"] <- "DAS1371O"
bloods$pid[bloods$pid == "DAS1165Y"] <- "DAS1165X"
bloods$pid[bloods$pid == "DAS13020"] <- "DAS1302D"
bloods$pid[bloods$pid == "DAS1325"] <- "DAS13225"
bloods$pid[bloods$pid == "DAS1324I"] <- "DAS13241"
bloods$pid[bloods$pid == "DAS13295"] <- "DAS1329S"
bloods$pid[bloods$pid == "DAS13375"] <- "DAS1337S"
bloods$pid[bloods$pid == "DAS13470"] <- "DAS1347O"
bloods$pid[bloods$pid == "DAS13661"] <- "DAS1366I"
bloods$pid[bloods$pid == "DAS1366I984"] <- "DAS1366I"
bloods$pid[bloods$pid == "DAS13710"] <- "DAS1371O"
bloods$pid[bloods$pid == "DAS13901"] <- "DAS1390I"
bloods$pid[bloods$pid == "DAS14255"] <- "DAS1425S"
bloods$pid[bloods$lab_id == "CAB15V"] <- "DAS1295G"
bloods$pid[bloods$lab_id == "CAB17W"] <- "DAS1199G"
bloods$pid[bloods$lab_id == "CAB194"] <- "DAS1446I"
bloods$pid[bloods$pid == "DAS1412I"] <- "DAS14121"
bloods$pid[bloods$pid == "DAS12830"] <- "DAS1283O"
bloods$pid[bloods$pid == "DAS117O5"] <- "DAS11705"
bloods$pid[bloods$pid == "DAS11775"] <- "DAS1177S"
bloods$pid[bloods$pid == "DAS12O7B"] <- "DAS1207B"
bloods$pid[bloods$pid == "DAS12495"] <- "DAS1249S"
bloods$pid[bloods$pid == "DAS12670"] <- "DAS1267O"
bloods$pid[bloods$pid == "DAS12861"] <- "DAS1286I"
bloods$pid[bloods$pid == "DAS14381"] <- "DAS1438I"
bloods$pid[bloods$pid == "DAS13550"] <- "DAS1355O"
bloods$pid[bloods$pid == "DAS10182"] <- "DAS1018L"
bloods$pid[bloods$pid == "DAS10895"] <- "DAS1089S"
bloods$pid[bloods$pid == "DAS14621"] <- "DAS1462I"
bloods$pid[bloods$pid == "DAS1270V"] <- "DAS1270Y"
bloods$pid[bloods$pid == "DAS14461"] <- "DAS1446I"
bloods$pid[bloods$pid == "DAS1308I"] <- "DAS13081"
bloods$pid[bloods$pid == "DAS14190"] <- "DAS1419O"
bloods$pid[bloods$pid == "DAS14415"] <- "DAS1441S"
bloods$pid[bloods$pid == "DAS1446G"] <- "DAS1447G"
bloods$pid[bloods$lab_id == "CAL180"] <- "DAS1534I"
bloods$pid[bloods$pid == "DAS1332I"] <- "DAS13321"
bloods$pid[bloods$pid == "DAS14510"] <- "DAS1451O"
bloods$pid[bloods$pid == "DAS14541"] <- "DAS1454I"
bloods$pid[bloods$pid == "DAS14664"] <- "DAS1466A"
bloods$pid[bloods$pid == "DAS1487O"] <- "DAS14870"
bloods$pid[bloods$pid == "DAS1494"] <- "DAS14942"
bloods$pid[bloods$pid == "DAS1504Y"] <- "DAS1504U"
bloods$pid[bloods$pid == "DAS15070"] <- "DAS1507O"
bloods$pid[bloods$pid == "DAS15181"] <- "DAS1518I"
bloods$pid[bloods$pid == "DAS15261"] <- "DAS1526I"
bloods$pid[bloods$pid == "DAS15341"] <- "DAS1534I"
bloods$pid[bloods$pid == "DAS15421"] <- "DAS1542I"
bloods$pid[bloods$pid == "DAS15501"] <- "DAS1550I"
bloods$pid[bloods$pid == "DAS1567O"] <- "DAS15670"
bloods$pid[bloods$pid == "DAS15682"] <- "DAS1568Z"
bloods$pid[bloods$pid == "DAS1574Z"] <- "DAS15742"
bloods$pid[bloods$pid == "DAS1575O"] <- "DAS15750"
bloods$pid[bloods$pid == "DAS1576X"] <- "DAS1576Z"
bloods$pid[bloods$pid == "DAS16015"] <- "DAS1601S"
bloods$pid[bloods$pid == "DS13821"] <- "DAS1382I"
bloods$pid[bloods$pid == "DAS1175W"] <- "DAS1174Y"

art <-
  c("", " -------------- ", "Enjoy ", " --------------", "    \\", 
    "      \\", "        \\", "            |\\___/|", "          ==) ^Y^ (==", 
    "            \\  ^  /", "             )=*=(", "            /     \\", 
    "            |     |", "           /| | | |\\", "           \\| | |_|/\\", 
    "           //_// ___/", "               \\_)", "  ")



cat("Bloods df now in bloods.  \n  ")

cat(art, sep = "\n")



