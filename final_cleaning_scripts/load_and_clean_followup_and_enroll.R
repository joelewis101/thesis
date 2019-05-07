# load and clean followup and enroll

wd.data <- "/Users/joelewis/Documents/PhD/Data/Current/portal_downloads" 
wd.clean.script <- "/Users/joelewis/Documents/PhD/R/PhD/followup"

cat(paste0("  Loading enroll and followup CSVs from ", wd.data, " ...  \n  "))

enroll <-read.csv(paste0(wd.data,"/dassim_enrolment_raw.csv"), stringsAsFactors = F)
followup <- read.csv(paste0(wd.data,"/dassim_followup_raw.csv"), stringsAsFactors = F)

enroll$data_date <- as.Date(enroll$data_date, "%d%b%Y")
followup$data_date <- as.Date(followup$data_date, "%d%b%Y")

cat(paste0("Running cleaning script at ", wd.clean.script, "/correct_mislabelled_fu.R ... \n  "))

source(paste0(wd.clean.script, "/correct_mislabelled_fu.R"))

cat("Enrollment df now in enroll.  \n  ")
cat("Followup df now in followup.  \n  ")
cat("Share and enjoy! \n  ")

    



