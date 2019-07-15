

# splicing in ESBL results (handed as esbl df grouped by pid with dplyr do(...))
# wth df (handed as full df)

# both dfs need an assess_typ and a pid

# it will
# subset df to the esbl$pid
# for each row in esbl
# duplictae the nearest row before the esb;$assess_type from df and change assess_type to match esbl if 
#esbl$assess_type is not in df
# then merge in esbl

splice_ESBL2 <- function(df, esbl) {
  if (!esbl$pid %in% df$pid) {stop("Dude, there are no entries for pid ", 
                                   esbl$pid[1], "
                                  in df handed to splice_ESBL2")}
  if (length(unique(esbl$pid)) > 1) {stop("Dude,more than 2 pids in esbl handed to splice_ESBL2") }
  dfout <- subset(df, pid == esbl$pid[1])
  for (i in 1:nrow(esbl)) {
    # if esbl row assess type is not in df then add a row to df
    #print(i)
    if (!esbl$assess_type[i] %in% dfout$assess_type) {
      #then get the closest row before
      dftemp <- subset(dfout, assess_type < esbl$assess_type[i])
      
      matchrow <- dftemp[which.min(esbl$assess_type[i] - dftemp$assess_type),]
      matchrow$assess_type <- esbl$assess_type[i]
      dfout <- bind_rows(dfout, matchrow)
    } 
    
    
  }
  dfout <- merge(dfout, esbl, by = c("pid", "assess_type"), all.x = T, all.y = T)
  return(dfout)
  
}
