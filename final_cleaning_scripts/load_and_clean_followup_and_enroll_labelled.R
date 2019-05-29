# load and clean followup and enroll

wd.data <- "/Users/joelewis/Documents/PhD/Data/Current/portal_downloads" 
wd.clean.script <- "/Users/joelewis/Documents/PhD/R/PhD/followup"

cat(paste0("  Loading LABELLED enroll and followup CSVs from ", wd.data, " ...  \n  "))

enroll <-read.csv(paste0(wd.data,"/labelled_by_stata/enroll.csv"), stringsAsFactors = F)
followup <- read.csv(paste0(wd.data,"/dassim_followup_raw.csv"), stringsAsFactors = F)

enroll[enroll == ""] <- NA

enroll$data_date <- as.Date(enroll$data_date, "%d%b%Y")
enroll$hivartstart <- as.Date(enroll$hivartstart, "%d%b%Y")
enroll$hivsatusdate <- as.Date(enroll$hivsatusdate, "%d%b%Y")
followup$data_date <- as.Date(followup$data_date, "%d%b%Y")

enroll.names <- c("hhitem1","hhitem2", "hhitem3", "hhitem4", "hhitem5", "hhitem6",
                  "animal1", "animal2", "animal3" , "animal4", "animal5", "animal6", "animal9",
                  "symp1", "symp2", "symp3", "symp4", "symp5",
                  "symp6", "symp7", "symp8", "symp9", "symp10",
                  "symp11", "symp12", "symp13", "symp14", "symp15", 
                  "symp16",  "symp17", "symp18", "symp19", "symp20",
                  "symp21", "symp22", "symp23",  "symp24", "symp25",
                  "symp26",
                  "treatfrom1",  "treatfrom2",  "treatfrom3", "treatfrom4", "treatfrom5" ,
                  "treatfrom6", "treatfrom7", "treatfrom9",
                  "hospcare1","hospcare2", "hospcare3","hospcare4", "hospcare5",
                  "hospcare6", "hospcare7","hospcare9")

names(enroll.names) <- c("own.car", "own.radio","own.cellphone","own.television","own.bicycle","own.refrigirator",
                         "keep.poultry", "keep.cattle","keep.goats","keep.dogs","keep.mules","keep.sheep", "keep.other",
                         "fever","night.sweats","lethargy","weight.loss","cough",
                         "difficulty.in.breathing", "chest.pain","haemoptysis","diarrhoea","vomiting",
                          "abdominal.pain", "urinary.frequency","dysuria","haematuria","genital.discharge",
                          "headache","confusion", "photophobia","neck.stiffness","convulsions",
                          "drowsiness","rash","jaundice","swollen.legs","Joint.pain.or.swelling",
                          "other.symptom",
                          "treatfrom.other.hospital","treatfrom.other.clinic","treatfrom.pharmacy", "treatfrom.private.doctor","treatfrom.other.health.worker",
                           "treatfrom.traditional.practicioner", "treatfrom.friend.or.family", "treatfrom.other",
                          "soughtcare.other.hospital","soughtcare.other.clinic","soughtcare.pharmacy","soughtcare.private.doctor", "soughtcare.other.health.worker" ,
                          "soughtcare.traditional.practicioner","soughtcare.friend.or.family","soughtcare.other")

for (i in 1:length(names(enroll))) {
  names(enroll)[names(enroll) == enroll.names[i]] <- names(enroll.names)[i]
}

cat(paste0("Running cleaning script at ", wd.clean.script, "/correct_mislabelled_fu.R ... \n  "))

source(paste0(wd.clean.script, "/correct_mislabelled_fu.R"))

cat("Enrollment df now in enroll.  \n  ")
cat("Followup df now in followup.  \n  ")
cat("If you wanna merge in HIV tests for composite HIV variable  \n  ")
cat("Run final_cleaning_scripts/make_composite_hivstatus_variable.R  \n  ")
cat("Share and enjoy! \n  ")

    



