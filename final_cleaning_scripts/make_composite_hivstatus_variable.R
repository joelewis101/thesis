# assume yer getting a df called enroll
#

wd.data <- "/Users/joelewis/Documents/PhD/Data/Current/portal_downloads" 


cat(paste0("  Loading hiv CSVs from ", wd.data, " ...  \n  "))
cat("And merging into enroll dataframe ... \n  ")
cat("Bish bash bosh.")



hiv <- read.csv(paste0( wd.data,"/dassim_hiv_raw.csv"), stringsAsFactors = F)

hiv <- subset(hiv, hiv_done == 1)

hiv$hiv_result[hiv$hiv_result == 0] <- 3
hiv$hiv_result[hiv$hiv_result == 1] <- 0
hiv$hiv_result[hiv$hiv_result == 3] <- 1
hiv$hiv_date <- as.Date(hiv$hiv_date, "%d%b%Y")

hiv %>% group_by(pid) %>% dplyr::summarise(hiv_r = sum(hiv_result),
                                    hiv_r_date = max(hiv_date)) -> hiv
hiv <- dplyr::ungroup(hiv)
hiv <- dplyr::ungroup(hiv)

hiv$hiv_r[hiv$hiv_r > 1] <- 1
hiv$hiv_r[hiv$hiv_r == 1] <- "Reactive"
hiv$hiv_r[hiv$hiv_r == 0] <- "Non reactive"

merge(enroll, hiv, all.x = T) -> enroll
enroll$hivstatus[!is.na(enroll$hiv_r)] <- enroll$hiv_r[!is.na(enroll$hiv_r)] 
enroll$hivsatusdate[!is.na(enroll$hiv_r)] <- enroll$hiv_r_date[!is.na(enroll$hiv_r)] 
enroll$hivonart [is.na(enroll$hivonart) & enroll$hiv_r == "Reactive" & !is.na(enroll$hiv_r)] <- "No"
enroll$hivcpt [is.na(enroll$hivcpt) & enroll$hiv_r == "Reactive" & !is.na(enroll$hiv_r)] <- "No"
# add in the final missing ones that witness traced



misshiv <- read.csv(paste0( wd.data,"/other_datasets/missing_hiv_found.csv"), stringsAsFactors = F)
misshiv <- subset(misshiv, hiv_status != "")

misshiv[misshiv == ""] <- NA
misshiv$hiv_status_date <- as.Date(misshiv$hiv_status_date, "%d/%m/%Y")
misshiv$art_start <- as.Date(misshiv$art_start, "%d/%m/%Y")
# i've checked these manually and ok just to merge back in

enroll <- merge(enroll,dplyr::select(misshiv, pid, hiv_status,hiv_status_date, on_art, art_regimen, art_start),
                all.x = T)

enroll$hivstatus[!is.na(enroll$hiv_status)] <- enroll$hiv_status[!is.na(enroll$hiv_status)] 
enroll$hivsatusdate[!is.na(enroll$hiv_status_date)] <- enroll$hiv_statusdate[!is.na(enroll$hiv_status_date)] 
enroll$hivonart[!is.na(enroll$on_art)] <- enroll$on_art[!is.na(enroll$on_art)] 
enroll$hivart[!is.na(enroll$art_regimen)] <- enroll$art_regimen[!is.na(enroll$art_regimen)] 
enroll$hivartstart[!is.na(enroll$art_start)] <- enroll$art_start[!is.na(enroll$art_start)] 

enroll <- dplyr::select(enroll, -hiv_r, -hiv_r_date,-hiv_status,
                        -hiv_status_date, -on_art, -art_regimen, -art_start  )



