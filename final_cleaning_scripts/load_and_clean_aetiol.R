# load and clean aetiology data
require(tidyverse)

wd.data <- "/Users/joelewis/Documents/PhD/Data/Current/portal_downloads" 
orgs <- read.csv(paste0(wd.data,"/other_datasets/lims_scrape.csv"), stringsAsFactors = F)
csf_biochem<- read.csv(paste0(wd.data,"/other_datasets/csf_micro_biochem.csv"), stringsAsFactors = F)

csf <- orgs[,c(7,9,14,15:32)]
orgs <- orgs[,c(7,9,13,15:32)]

orgs <- subset(orgs, pid %in% subset(enroll, arm == 1)$pid)
csf <- subset(csf, pid %in% subset(enroll, arm == 1)$pid)

# add in positive crags

names(csf_biochem)[names(csf_biochem) == "lab_id"] <- "lims_fe"
csf <- merge(csf, select(csf_biochem, lims_fe, CrAg_LFA), all.x = T)

csf$csf[!is.na(csf$CrAg_LFA) & csf$CrAg_LFA == "POSITIVE"]<- TRUE

# get list of positive blood culture and positive csf

orgsbc <- subset(orgs, !is.na(bcult))
orgsbc[is.na(orgsbc)] <- 0
orgsbc$anycontam <- 0
orgsbc$n_contam <- orgsbc$dip + orgsbc$ahaemstrep + orgsbc$bacillus +orgsbc$micr + orgsbc$cons 
orgsbc$n_orgs <- apply(orgsbc[4:20], 1, sum)
orgsbc$contam_only <- 0
orgsbc$contam_only[(orgsbc$n_contam == orgsbc$n_orgs) & orgsbc$n_contam > 0] <- 1
orgsbc$pathogen <- orgsbc$bcult
orgsbc$pathogen[orgsbc$contam_only == 1] <- FALSE
orgsbc %>% dplyr::group_by(pid) %>% dplyr::summarise(pathogen = sum(pathogen), type = "bc") -> bc
bc$pathogen[bc$pathogen > 1] <- 1
bc.full <- orgsbc
rm(orgs)
rm(orgsbc)

### csf

(subset(csf, !is.na(csf))) -> csf
csf[is.na(csf)] <- 0  
csf$n_contam <- csf$dip + csf$ahaemstrep + csf$bacillus +csf$micr + csf$cons 
csf$n_orgs <- apply(csf[4:20], 1, sum)
csf$contam_only <- 0
csf$contam_only[(csf$n_contam == csf$n_orgs) & csf$n_contam > 0] <- 1
csf$pathogen <- csf$csf
csf$pathogen[csf$contam_only == 1 & csf$CrAg_LFA != "POSITIVE"] <- FALSE
csf.full <- csf
csf  %>% group_by(pid) %>% dplyr::summarise(pathogen = sum(pathogen), type = "csf") -> csf
csf$pathogen[csf$pathogen > 1] <- 1


### malaria

malaria <- read.csv(paste0(wd.data, "/dassim_malaria_raw.csv"), stringsAsFactors = F)
malaria$any_malaria <- malaria$mal_rdt
malaria$any_malaria[malaria$mal_micr >0 & !is.na(malaria$mal_micr)] <- 1
malaria$any_malaria[malaria$mal_micr ==0 & !is.na(malaria$mal_micr)] <- 0
malaria <- subset(malaria, pid %in% subset(enroll, arm == 1)$pid)
malaria.full <- malaria
malaria %>% group_by(pid) %>% dplyr::summarise(pathogen = sum(any_malaria), type = "malaria") -> malaria
malaria$pathogen[malaria$pathogen > 1] <- 1

# mtb  bsi

tbbsi <- read.csv(paste0(wd.data, "/other_datasets/MTBBSI_data.csv"), stringsAsFactors = F)

tbbsi$pid[tbbsi$pid == "DAS10421"] <- "DAS1042L"
tbbsi$pid[tbbsi$pid == "DAS1055D"] <- "DAS1055B"
tbbsi$pid[tbbsi$pid == "DAS1096H"] <- "DAS1096U"
tbbsi$pid[tbbsi$pid == "DAS1140P"] <- "DAS1140H"
tbbsi$pid[tbbsi$pid == "DAS1142B"] <- "DAS1142D"
tbbsi$pid[tbbsi$pid == "DAS1150B"] <- "DAS1150D"
#tbbsi$pid[tbbsi$pid == "DAS1150B"] <- "DAS1150D"
tbbsi$pid[tbbsi$pid == "DAS1154S"] <- "DAS11545"
tbbsi$pid[tbbsi$pid == "DAS12661"] <- "DAS1366I"
tbbsi$pid[tbbsi$pid == "DAS13689C"] <- "DAS1369C"
tbbsi$pid[tbbsi$pid == "DAS13689C"] <- "DAS1369C"
tbbsi$pid[tbbsi$pid == "DAS13856"] <- "DAS13886"
tbbsi$pid[tbbsi$pid == "DAS14050"] <- "DAS1405X"
tbbsi$pid[tbbsi$pid == "DAS14057"] <- "DAS14017"
tbbsi$Result[tbbsi$pid == "DAS10585"] <- "MTB"
tbbsi$pid[tbbsi$pid == "DAS10421"] <- "DAS1042L"
tbbsi$pid[tbbsi$pid == "DAS1055D"] <- "DAS1055B"
tbbsi$pid[tbbsi$pid == "DAS1096H"] <- "DAS1096U"
tbbsi$pid[tbbsi$pid == "DAS1196S"] <- "DAS1196M"
tbbsi$pid[tbbsi$pid == "DAS1199D"] <- "DAS1199G"
tbbsi$pid[tbbsi$pid == "DAS12417"] <- "DAS12409"
tbbsi$pid[tbbsi$pid == "DAS1259G"] <- "DAS1259O"
tbbsi$pid[tbbsi$pid == "DAS13295"] <- "DAS1329S"
tbbsi$pid[tbbsi$pid == "DAS14381"] <- "DAS1438I"
tbbsi$pid[tbbsi$pid == "DAS1446L"] <- "DAS1446I"
tbbsi$pid[tbbsi$pid == "DAS14621"] <- "DAS1462I"
tbbsi$pid[tbbsi$pid == "DAS14701"] <- "DAS1470I"
tbbsi$pid[tbbsi$pid == "DAS15010"] <- "DAS1501X"

tbbsi <- subset(tbbsi, pid %in% subset(enroll, arm == 1)$pid)
tbbsi <- subset(tbbsi, pid %in% subset(enroll, hivstatus != "Non reactive")$pid)
tbbsi.full <- tbbsi

tbbsi <- subset(tbbsi, Result != "Contam")
tbbsi$Result[tbbsi$Result == "MTB"] <- 1
tbbsi$Result[tbbsi$Result == "Negative"] <- 0
tbbsi %>% group_by(pid) %>% dplyr::summarise(pathogen = sum(as.numeric(Result)), type = "mtb bsi") -> tbbsi

# sputum

xpert <- read.csv(paste0(wd.data, "/dassim_sputum_raw.csv"), stringsAsFactors = F)

# merge smear and xpert
xpert$sputx_result[is.na(xpert$sputx_result)] <- 1
xpert$sputx_result[xpert$sputx_result == 1] <- 0
xpert$sputx_result[xpert$sputx_result > 0] <- 1
xpert.full <- xpert
xpert %>% group_by(pid) %>% dplyr::summarise(pathogen = sum(as.numeric(sputx_result)), type = "Xpert") -> xpert
xpert$pathogen[xpert$pathogen > 0] <- 1


# ulam

ulam <- read.csv(paste0(wd.data, "/other_datasets/lamresults_final.csv"), stringsAsFactors = F)
ulam <- subset(ulam, RESULT != "" & RESULT != "INVALID" & RESULT != "NO SAMPLE") 
ulam$RESULT[ulam$RESULT == "NEG"] <- 0
ulam$RESULT[ulam$RESULT != 0] <- 1
ulam$result2 <- !(grepl("NEG", ulam$JOE) | grepl("NEG", ulam$MADA) | grepl("NEG", ulam$CHRIS) | grepl("NEG", ulam$AJISA))
ulam <- subset(ulam, pid %in% subset(enroll, hivstatus != "Non reactive")$pid)
ulam.full <- ulam
ulam %>% group_by(pid) %>% dplyr::summarise(pathogen = sum(as.numeric(result2)), type = "uLAM") -> ulam

aetiol <- rbind(bc,
                csf,
                malaria,
                tbbsi,
                xpert,
                ulam)

aetiol <- merge(aetiol, select(enroll, pid, hivstatus), all.x = T)

cat("all aetiology data now in aetiol df in long format.  \n  ")
cat("blood culture data in bc.full  \n  ")
cat("csf data in bc.full csf.full \n  ")
cat("malaria data in malaria.full  \n  ")
cat("xpert culture data in xpert.full  \n  ")
cat("myco/f culture data in tbbsi.full  \n  ")
cat("uLAM culture data in ulam.full  \n  ")
cat("Share and enjoy! \n  ")



