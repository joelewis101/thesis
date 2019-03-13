## concatenate old and new withdrawal files and put into clean data folder 
##

library(dplyr)

db.withd <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_withdraw_raw.csv", stringsAsFactors = F)
man.withd <- read.csv("/Users/joelewis/Documents/PhD/Data/withdrawals/DASSIM_Withdrawals.csv", stringsAsFactors = F)

as.Date(db.withd$withdraw_dte, "%d%b%Y") -> db.withd$withdraw_dte
as.Date(db.withd$data_date, "%d%b%Y") -> db.withd$data_date

man.withd <- subset(man.withd, !(pid %in% db.withd$pid))

man.withd$withdraw_reason <- "not interested"
man.withd$data_date <- NA
man.withd$withdrawal_date
as.Date(man.withd$withdrawal_date, "%d/%m/%Y") -> man.withd$withdrawal_date



withdfinal <- dplyr::select(db.withd, pid, data_date, withdraw_dte, withdraw_reason)
names(withdfinal)[names(withdfinal) == "withdraw_dte"] <- "withdrawal_date"

rbind(withdfinal, dplyr::select(man.withd, pid, data_date, withdrawal_date, withdraw_reason)) -> withdfinal


# rationalise double entries

withdfinal <- subset(withdfinal, !(pid == "DAS1470I" & grepl("THE OTHER IS AN ERROR", withdraw_reason )))

withdfinal <- subset(withdfinal, !(pid == "DAS1259O" & (withdrawal_date == "2018-03-15" | withdrawal_date == "2018-12-10") ))

withdfinal$transfer_out <- grepl("transfer", withdfinal$withdraw_reason, ignore.case = T) | 
  grepl("moved", withdfinal$withdraw_reason, ignore.case = T) 
withdfinal$withd <- grepl("not interested", withdfinal$withdraw_reason, ignore.case = T) |
                         grepl("Husband", withdfinal$withdraw_reason, ignore.case = T) |
                         grepl("Didn't give one", withdfinal$withdraw_reason, ignore.case = T) |
                         grepl("Doesn't want to", withdfinal$withdraw_reason, ignore.case = T) |
                         grepl("none given", withdfinal$withdraw_reason, ignore.case = T) |
                         grepl("withdraw", withdfinal$withdraw_reason, ignore.case = T) 

withdfinal$ltfu <- grepl("loss to", withdfinal$withdraw_reason, ignore.case = T) | 
  grepl("lost to", withdfinal$withdraw_reason, ignore.case = T) 

# should all have one reason

if (any(apply(withdfinal[5:7],1, sum ) > 1)) {
  stop("two withdrawal entries in final df - rationalise")
}

if (any(apply(withdfinal[5:7],1, sum ) == 0)) {
  stop("one withdrawal entry has no reason- rationalise")
}

# great - write to disk
print("All seemed to work out. Share and enjoy")

write.csv(withdfinal, "/Users/joelewis/Documents/PhD/datasets/withdrawals/withdrawals_clean.csv", row.names = F)

