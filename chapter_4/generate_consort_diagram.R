### generate consort diagram

library(DiagrammeR)

library(DiagrammeRsvg) #Needed if you want to export the image
library(rsvg) #Needed if you want to export the image
library(reshape2)

# functions to remove duplicates

remove_dups <- function(df) {
  df[order(df$hospoutcomedate),]
  return(df[1,])
}

remove_dups_data_date <- function(df) {
  df[order(df$data_date, decreasing = T),]
  return(df[1,])
}

remove_dups_died <- function(df) {
  df[order(df$outcome_date, decreasing = T),]
  return(df[1,])
}

library(RMySQL)
library("rmarkdown")
library(plyr)
library(dplyr)

enroll <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_enrolment_raw.csv", stringsAsFactors = F)
followup <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_followup_raw.csv", stringsAsFactors = F)
outcome <- read.csv("/Users/joelewis/Documents/PhD/Data/Current/portal_downloads/dassim_outcome_raw.csv", stringsAsFactors = F)
withd <- read.csv("/Users/joelewis/Documents/PhD/datasets/withdrawals/withdrawals_clean.csv", header = TRUE, stringsAsFactors = FALSE)


outcome$hospoutcomedate <- as.Date(outcome$hospoutcomedate, "%d%b%Y")
outcome$data_date<- as.Date(outcome$data_date, "%d%b%Y")
outcome$hospoutcomedate[is.na(outcome$hospoutcomedate)] <- outcome$data_date[is.na(outcome$hospoutcomedate)]

# now collapse outcome to a unique dataframe taking earliest of each

unique(outcome) %>% group_by(pid) %>%
  dplyr::summarise(n=n(), n_died = sum(hospoutcome == 3)) %>% filter(n > 1) -> two_oc
two_oc <- subset(two_oc, n_died > 0 & n != n_died)
extra_deaths <- subset(outcome, hospoutcome == 3 & pid %in% two_oc$pid)
ddply(extra_deaths, "pid", remove_dups) -> extra_deaths
outcome <- ddply(outcome, "pid", remove_dups) 

# chop out rubbish

followup$foutcome[is.na(followup$foutcome)] <- followup$foutcomearm4[is.na(followup$foutcome)]

enroll <- enroll[c("pid","data_date","arm")]
outcome <- outcome[c("pid","hospoutcome","hospoutcomedate")]
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
outcome$hospoutcome[outcome$hospoutcome == 2] <- "Discharge"
names(enroll)[names(enroll) == "data_date"] <- "enrolled"
enroll$pid[enroll$pid == "DAS1437K" & enroll$arm == 3] <- "TEMP1"
enroll$pid[enroll$pid == "DAS1436M" & enroll$arm == 3] <- "TEMP2"


# correct mislabelled followup

source("/Users/joelewis/Documents/PhD/R/PhD/followup/correct_mislabelled_fu.R")

followup$d2visit[is.na(followup$d2visit)] <- 2
names(enroll)[names(enroll) == "enrolled"] <- "data_date"
enroll$d2visit <- 0
enroll$foutcome<- "Alive"


allfu <- rbind(select(enroll, -arm), select(followup, -d2visitndeathdate))
allfu <- unique(select(allfu, -foutcome))

ddply(allfu, c("pid", "d2visit"), remove_dups_data_date) -> allfu
as.character(allfu$data_date) -> allfu$data_date
dcast(allfu, pid ~ d2visit, value.var = "data_date") -> fu.matrix
as.Date(fu.matrix$`0`) -> fu.matrix$`0`
as.Date(fu.matrix$`1`) -> fu.matrix$`1`
as.Date(fu.matrix$`2`) -> fu.matrix$`2`
as.Date(fu.matrix$`3`) -> fu.matrix$`3`
as.Date(fu.matrix$`4`) -> fu.matrix$`4`

# add outcome

died <- subset(outcome, hospoutcome == "Died")
names(died) <- c("pid","outcome","outcome_date")
fu.died <- subset(followup, foutcome == "Died")
fu.died <- select(fu.died, pid, foutcome, d2visitndeathdate)
names(fu.died) <- c("pid","outcome","outcome_date")

died <- rbind(fu.died, died)
rm(fu.died)

extra_deaths <- select(extra_deaths, pid, hospoutcome, hospoutcomedate)
extra_deaths$hospoutcome <- "Died"
names(extra_deaths) <- c("pid","outcome","outcome_date")

died <- rbind(died, extra_deaths)

# add withd

withd$outcome <- NA
withd$outcome[withd$ltfu] <- "LTFU"
withd$outcome[withd$withd] <- "Withdrew"
withd$outcome[withd$transfer_out] <- "Transfer out"
withd <- select(withd, pid, outcome, withdrawal_date)
names(withd) <- c("pid","outcome","outcome_date")

died <- rbind(died, withd)
died <- unique(died)

# rem dups
died <- ddply(died, "pid", remove_dups_died)
merge(fu.matrix, died, all.x = T) -> fu.matrix
merge(fu.matrix, select(enroll, pid,arm), all.x= T) -> fu.matrix
fu.matrix <- subset(fu.matrix, arm != 4)
fu.matrix$outcome[is.na(fu.matrix$outcome) & !is.na(fu.matrix$`4`)] <- 'completed'
fu.matrix$outcome[is.na(fu.matrix$outcome)] <- 'followup ongoing'
as.Date(fu.matrix$`0`) -> fu.matrix$`0`
as.Date(fu.matrix$`1`) -> fu.matrix$`1`
as.Date(fu.matrix$`2`) -> fu.matrix$`2`
as.Date(fu.matrix$`3`) -> fu.matrix$`3`
as.Date(fu.matrix$`4`) -> fu.matrix$`4`
as.Date(fu.matrix$outcome_date) -> fu.matrix$outcome_date
fu.matrix$censor_date <- fu.matrix$outcome_date
(apply(fu.matrix[2:6], 1, function(x) max(x, na.rm = T) )) -> fu.matrix$max
fu.matrix$censor_date[is.na(fu.matrix$outcome_date)]  <- fu.matrix$max[is.na(fu.matrix$outcome_date)]
fu.matrix$fu_days <- as.numeric(fu.matrix$censor_date - fu.matrix$`0`)

## populate consort diagram schizzle

fu.matrix <- subset(fu.matrix, arm == 1)

enrolled <- paste0("Enrolled \n n = ", nrow(fu.matrix))

b4_d28_excluded.lab <- paste0('Excluded n = ', 
    sum((fu.matrix$outcome == "LTFU" |
          fu.matrix$outcome == "Transfer out" |
          fu.matrix$outcome == "Withdrew" |
          fu.matrix$outcome == "followup ongoing") & 
          fu.matrix$fu_days <28), " \n  \n  ",
           "Withdrawn = ", sum(fu.matrix$outcome == "Withdrew" & 
                                fu.matrix$fu_days <28),
            " \n Transfer out = ", sum(fu.matrix$outcome == "Transfer out" & 
                                 fu.matrix$fu_days <28),
            " \n LTFU = ", sum(fu.matrix$outcome == "LTFU" & 
                               fu.matrix$fu_days <28),
             " \n Ongoing fu = ", sum(fu.matrix$outcome == "followup ongoing" & 
                             fu.matrix$fu_days <28)
    )

inc.d28.lab <- paste0("Included in D28 analysis \n  n = ", 
                      sum( (fu.matrix$outcome == "Died" &
                            fu.matrix$fu_days <= 28) |
                             fu.matrix$fu_days >= 28))

d28.died <- paste0("Died \n  n = ", 
                   sum( fu.matrix$outcome == "Died" &
                           fu.matrix$fu_days <= 28))


d28_d90_excluded.lab <- paste0('Excluded n = ', 
                              sum((fu.matrix$outcome == "LTFU" |
                                     fu.matrix$outcome == "Transfer out" |
                                     fu.matrix$outcome == "Withdrew" |
                                     fu.matrix$outcome == "followup ongoing") & 
                                    fu.matrix$fu_days <90 & fu.matrix$fu_days >= 28), " \n  \n  ",
                              "Withdrawn = ", sum(fu.matrix$outcome == "Withdrew" & 
                                                    fu.matrix$fu_days <90 & fu.matrix$fu_days >= 28),
                              " \n Transfer out = ", sum(fu.matrix$outcome == "Transfer out" & 
                                                           fu.matrix$fu_days <90 & fu.matrix$fu_days >= 28),
                              " \n LTFU = ", sum(fu.matrix$outcome == "LTFU" & 
                                                   fu.matrix$fu_days <90 & fu.matrix$fu_days >= 28),
                              " \n Ongoing fu = ", sum(fu.matrix$outcome == "followup ongoing" & 
                                                         fu.matrix$fu_days <90 & fu.matrix$fu_days >= 28)
)

d90.died <- paste0("Died \n  n = ", 
                   sum( fu.matrix$outcome == "Died" &
                          fu.matrix$fu_days > 28 & fu.matrix$fu_days <= 90))


inc.d90.lab <- paste0("Included in D90 analysis \n  n = ", 
                      sum( (fu.matrix$outcome == "Died" &
                              fu.matrix$fu_days <= 90) |
                             fu.matrix$fu_days >= 90))

final.tpoint <- 160

fu.matrix$outcome[fu.matrix$fu_days < final.tpoint & fu.matrix$outcome == "completed" ] <- "Transfer out"

d90_d180_excluded.lab <- paste0('Excluded n = ', 
                               sum((fu.matrix$outcome == "LTFU" |
                                      fu.matrix$outcome == "Transfer out" |
                                      fu.matrix$outcome == "Withdrew" |
                                      fu.matrix$outcome == "followup ongoing") & 
                                     fu.matrix$fu_days <final.tpoint & fu.matrix$fu_days >= 90), " \n  \n  ",
                               "Withdrawn = ", sum(fu.matrix$outcome == "Withdrew" & 
                                                     fu.matrix$fu_days <final.tpoint & fu.matrix$fu_days >= 90),
                               " \n Transfer out = ", sum(fu.matrix$outcome == "Transfer out" & 
                                                            fu.matrix$fu_days <final.tpoint & fu.matrix$fu_days >= 90),
                               " \n LTFU = ", sum(fu.matrix$outcome == "LTFU" & 
                                                    fu.matrix$fu_days <final.tpoint & fu.matrix$fu_days >= 90),
                               " \n Ongoing fu = ", sum(fu.matrix$outcome == "followup ongoing" & 
                                                          fu.matrix$fu_days <final.tpoint & fu.matrix$fu_days >= 90)
)


inc.d180.lab <- paste0("Included in D",final.tpoint , " analysis \n  n = ", 
                      sum( (fu.matrix$outcome == "Died" &
                              fu.matrix$fu_days <= final.tpoint) |
                             fu.matrix$fu_days >= final.tpoint))

d180.died <- paste0("Died \n  n = ", 
                   sum( fu.matrix$outcome == "Died" &
                          fu.matrix$fu_days > 90 & fu.matrix$fu_days <= final.tpoint))



  


export_svg(grViz(diagram =" digraph {
  graph [fontsize=12]
      node [shape=box, width = 2, fontname = Arial] 
  Screened[pos='1,1',pin=true,label = 'Participants Screened \n n = xxx']
  Screened -> A[arrowhead='none']
  A -> Enrolled[length = 1]
  Enrolled -> B[arrowhead='none'] 
  B-> D28 
  D28 -> C[arrowhead='none'] 
  C-> D90
  D90-> D[arrowhead='none'] 
  D-> D180
  E[width = 3.5]
  E-> Excl[style='invis'] 
  Excl-> G[style = 'invis']
  G -> LTFU_B4_D28[style='invis']
  LTFU_B4_D28-> Died_by_D28[style = 'invis']
  Died_by_D28-> LTFU_B4_D90 [style='invis']
  LTFU_B4_D90 -> Died_D28_to_90[style = 'invis'] 
  Died_D28_to_90-> LTFU_B4_D180[style = 'invis']
  LTFU_B4_D180 -> Died_D90_to_180[style = 'invis']
A[shape='point', width = 0.001]
B[shape ='point', width = 0.001]
C[shape = 'point', width = 0.001]
D[shape = 'point', width = 0.001]
  E[style='invis']
  G[style = 'invis']

  edge [constraint=false]
  A -> Excl
  B ->  LTFU_B4_D28
  C -> LTFU_B4_D90

D -> LTFU_B4_D180






 Excl[label = 'Excluded n = xxx \n reason 1 xxx \n reason 2 xxx']
  Enrolled[label = '@@1']
  LTFU_B4_D28[label = '@@2']
  D28[label = '@@3']
  Died_by_D28[label = '@@4', style = 'invis' ]
  LTFU_B4_D90[label = '@@5']
  D90[label = '@@7']
  Died_D28_to_90[label = '@@6', style = 'invis' ]
  LTFU_B4_D180[label = '@@8']
  D180[label = '@@9']
  Died_D90_to_180[label = '@@10', style = 'invis' ]
    
    
        }
      
      [1]:  enrolled
      [2]: b4_d28_excluded.lab
      [3]: inc.d28.lab
      [4]: d28.died
      [5]: d28_d90_excluded.lab
      [6]: d90.died
      [7]: inc.d90.lab
      [8]: d90_d180_excluded.lab
      [9]: inc.d180.lab
      [10]: d180.died
")) -> sv

writeLines(sv, "chapter_4/figures/consort_diagram.svg")

rsvg_pdf("chapter_4/figures/consort_diagram.svg", "chapter_4/figures/consort_diagram.pdf", height = 600)
rsvg_png("chapter_4/figures/consort_diagram.svg", "chapter_4/figures/consort_diagram.png")

