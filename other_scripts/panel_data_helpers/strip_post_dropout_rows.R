# this will drop the rows after a death (1) or censor (2)
# if ther eare two it will drop the later
# tidy up
# tidy up
# tidy up


strip_post_dropout_rows <- function(dfin, verbose = TRUE) {
  dfin <- as.data.frame(dfin)
  dfin <- dfin[order(dfin$assess_type),]
  censor_row <- which(dfin$died == 1 | dfin$died == 2)
  if (length(censor_row) > 1) {
    if (verbose) { 
      print(paste0("> 1 row with censor or death: ", dfin$pid[1], " chooing earliest"))
      print(dfin)
    }
    dfin <- dfin[1:min(censor_row),]
  } else if (length(censor_row) == 0){
    stop(paste0("pid with no with censor or death: ", dfin$pid[1]))
  } else if (censor_row < nrow(dfin)) {
    if (verbose) { 
      print(paste0("Dropping rows post censor/death for ", dfin$pid[1]))
      print(dfin)
      dfin <- dfin[1:censor_row,]
    }
  }
  return(dfin)
}