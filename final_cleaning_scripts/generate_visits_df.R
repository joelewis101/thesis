


remove_dups <- function(df) {
  df[order(df$hospoutcomedate),]
  return(df[1,])
}


enroll <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_enrolment_raw.csv", stringsAsFactors = F)
followup <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_followup_raw.csv", stringsAsFactors = F)
outcome <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_outcome_raw.csv", stringsAsFactors = F)

withd <- read.csv("/Users/joelewis/Documents/PhD/Data/withdrawals/DASSIM_Withdrawals.csv", header = TRUE, stringsAsFactors = FALSE)

as.Date(withd$withdrawal_date, "%d/%m/%Y") -> withd$withdrawal_date

followup$foutcome[is.na(followup$foutcome) & !is.na(followup$foutcomearm4)] <- followup$foutcomearm4[is.na(followup$foutcome) & !is.na(followup$foutcomearm4)]

# if outcome date not present put a sdata date
outcome$hospoutcomedate[outcome$hospoutcome ==3 & outcome$hospoutcomedate == ""] <- outcome$data_date[outcome$hospoutcome ==3 & outcome$hospoutcomedate == ""]


# chop out rubbish

enroll <- enroll[c("pid","data_date","arm")]
outcome <- outcome[c("pid","hospoutcome","hospoutcomedate")]
followup4 <- followup[c("pid","data_date","foutcomearm4","d2visitndeathdate")]
followup4 <- subset(followup4, !is.na(foutcomearm4))
followup <- followup[c("pid", "d2visit","foutcome","data_date", "d2visitndeathdate")]

### recode to be intelligible

followup$foutcome[followup$foutcome == 2] <- "Died"
followup$foutcome[followup$foutcome == 1] <- "Alive"
followup$data_date <- as.Date(followup$data_date, "%d%b%Y")
followup$d2visitndeathdate <- as.Date(followup$d2visitndeathdate,"%d%b%Y")


followup4$foutcomearm4[followup4$foutcome == 2] <- "Died"
followup4$foutcomearm4[followup4$foutcome == 1] <- "Alive"
followup4$data_date <- as.Date(followup4$data_date, "%d%b%Y")
followup4$d2visitndeathdate <- as.Date(followup4$d2visitndeathdate,"%d%b%Y")


enroll$data_date <- as.Date(enroll$data_date, "%d%b%Y")

outcome$hospoutcomedate <- as.Date(outcome$hospoutcomedate, "%d%b%Y")
outcome$hospoutcome[outcome$hospoutcome == 3] <- "Died"
outcome$hospoutcome[outcome$hospoutcome == 1] <- "Discharge"

names(enroll)[names(enroll) == "data_date"] <- "enrolled"

enroll$pid[enroll$pid == "DAS1437K" & enroll$arm == 3] <- "TEMP1"
enroll$pid[enroll$pid == "DAS1436M" & enroll$arm == 3] <- "TEMP2"



# correct mislabelled followup

source("/Users/joelewis/Documents/PhD/R/PhD/followup/correct_mislabelled_fu.R")

# add extra_deaths back into followup

enroll$d2visit <- 0
followup <- merge(followup, select(enroll, pid, arm))

followup$d2visit[followup$arm == 4] <- 2

followup <- unique(followup)

followup %>% group_by(pid, d2visit) %>% summarise(n = n()) %>% filter(n > 1)

# manually sort these - too important
# the rules are: pick the latest, unless one is death

subset(followup, !(pid == "DAS1009N" & (data_date =="2017-10-19" | data_date == "2017-10-18"))) -> followup
subset(followup, !(pid == "DAS1025N" & (data_date =="2017-10-10"))) -> followup
subset(followup, !(pid == "DAS1268M" & (data_date =="2018-02-06"))) -> followup
subset(followup, !(pid == "DAS1292M" & (data_date =="2018-01-04"))) -> followup
subset(followup, !(pid == "DAS1329S" & (data_date =="2017-12-05"))) -> followup
subset(followup, !(pid == "DAS1406Y" & (data_date =="2017-11-02"))) -> followup
subset(followup, !(pid == "DAS14870" & (data_date =="2018-06-18"))) -> followup
subset(followup, !(pid == "DAS1533K" & (data_date =="2018-11-21"))) -> followup
subset(followup, !(pid == "DAS15742" & (data_date =="2018-12-05"))) -> followup


etemp <- enroll
names(etemp)[names(etemp) == "enrolled"] <- "data_date"

fu_tots <- rbind(subset(etemp), select(subset(followup), pid,arm, d2visit, data_date))

## find double follow up entries

followup %>% group_by(pid, d2visit) %>% summarise(n = n()) %>% filter(n > 1)

fu_tots$data_date <- as.character(fu_tots$data_date)
dcast(fu_tots, pid + arm ~ d2visit, value.var = "data_date") -> d
names(d)[3:7] <- c("enrolled", "w1", "w4", "w12", "w24")

as.Date(d$enrolled) -> d$enrolled
as.Date(d$w1) -> d$w1
as.Date(d$w4) -> d$w4
as.Date(d$w12) -> d$w12
as.Date(d$w24) -> d$w24

# add outcomes
death_or_withd <- subset(outcome, hospoutcome == "Died") 
death_or_withd <- unique(death_or_withd)

death_or_withd %>% group_by(pid) %>% summarise(n = n()) %>% filter(n > 1)

# deal with doubles ones manually
# take earliest

subset(death_or_withd, !(pid == "DAS1465C" & hospoutcomedate == "2018-12-10")) -> death_or_withd
subset(death_or_withd, !(pid == "DAS14846" & hospoutcomedate == "2018-10-26")) -> death_or_withd

# add from followup

fupdeath <- subset(followup, foutcome == "Died")
names(fupdeath)[3] <- "hospoutcome"
names(fupdeath)[5] <- "hospoutcomedate"

fupdeath <- select(fupdeath, pid, hospoutcome, hospoutcomedate)

fupdeath <- unique(fupdeath)

fupdeath %>% group_by(pid) %>% summarise(n = n()) %>% filter(n > 1)

#none

# check withds

withd %>% group_by(pid) %>% summarise(n = n()) %>% filter(n > 1)

# none 

names(withd)[4] <- "hospoutcomedate"
withd$hospoutcome <- "Withdraw"

rbind(death_or_withd, select(withd, pid, hospoutcome, hospoutcomedate)) -> death_or_withd
rbind(death_or_withd, fupdeath) -> death_or_withd

death_or_withd <- unique(death_or_withd)
death_or_withd %>% group_by(pid) %>% summarise(n = n()) %>% filter(n > 1)

# for two death dates, choose erliest

choose_earlier_death <- function(dfin) {
  if (length(unique(dfin$hospoutcome)) != 1) {
    stop(paste0("Death and withdrawal recorded for pid ", dfin$pid[1]))
  }
  
  
  dfin[order(dfin$hospoutcomedate, decreasing = T),] -> dfin
  dfin[1,] -> dfin
  return(dfin)
}

ddply(death_or_withd, "pid", choose_earlier_death) -> death_or_withd

d <- merge(d, death_or_withd, all.x = T)

d$hospoutcome[is.na(d$hospoutcome)] <- 0
d$hospoutcome[d$hospoutcome =="Died"] <- 1
d$hospoutcome[d$hospoutcome =="Withdraw"] <- 2


# populate next visit

d$nextvisit <- -1

choose_next_visit <- function(df) {
  
  if (nrow(df) > 1) {
    stop(paste0("Nextvist error - two rows! ", dfin$pid[1] ) )
  }
  
  if (df$arm == 1 | df$arm == 2) {
    
    if (df$hospoutcome != 0 | !is.na(df$w24)) {
      df$nextvisit <- NA
    } else if (!is.na(df$w12)) {
      df$nextvisit <- 24
    } else if (!is.na(df$w4)) {
      df$nextvisit <- 12
    } else if (!is.na(df$w1)) {
      df$nextvisit <- 4
    } else if (is.na(df$w1)) {
      df$nextvisit <- 1    
    }
    
  } else if (df$arm == 3) {
    
    if (df$hospoutcome != 0 | !is.na(df$w24)) {
      df$nextvisit <- NA
    } else if (!is.na(df$w4)) {
      df$nextvisit <- 24
    } else if (is.na(df$w4)) {
      df$nextvisit <- 4    
    }
  } else if (df$arm == 4) {
    if (df$hospoutcome != 0 | !is.na(df$w4)) {
      df$nextvisit <- NA
    } else if (is.na(df$w4)) {
      df$nextvisit <- 4    
    }
    
  }
  
  return(df)
}

ddply(d, "pid", choose_next_visit) -> d

visits <- d

names(visits)[names(visits) == "hospoutcome" ] <-  "status"
names(visits)[names(visits) == "hospoutcomedate" ] <-  "died_date"
names(visits)[names(visits) == "nextvisit" ] <-  "next_visit"

# convert to weeks

today <- Sys.Date()
visits$w1 <- as.numeric(difftime(as.Date(visits$w1,"%Y-%m-%d"),visits$enrolled, units = "days"))
visits$w1 <- round(visits$w1, digits = 1)

visits$w4 <- as.numeric(difftime(as.Date(visits$w4,"%Y-%m-%d"),visits$enrolled, units = "days"))
visits$w12 <- as.numeric(difftime(as.Date(visits$w12,"%Y-%m-%d"),visits$enrolled, units = "days"))
visits$w24 <- as.numeric(difftime(as.Date(visits$w24,"%Y-%m-%d"),visits$enrolled, units = "days"))

visits$died_date <- as.numeric(difftime(as.Date(visits$died_date,"%Y-%m-%d"),visits$enrolled, units = "days"))

visits$fu <- as.numeric(difftime(today,visits$enrolled, units = "weeks"))

#visits$w4 <- round(visits$w4, digits = 1)
#visits$w12 <- round(visits$w12, digits = 1)
#visits$w24 <- round(visits$w24, digits = 1)
#visits$died_date <- round(visits$died_date, digits = 1)
#visits$fu <- round(visits$fu, digits = 1)  

visits$visitdue <- visits$next_visit - visits$fu 

visits <- visits[c("pid","enrolled","arm","status","died_date", "fu", "w1","w4","w12","w24","next_visit","visitdue")]
