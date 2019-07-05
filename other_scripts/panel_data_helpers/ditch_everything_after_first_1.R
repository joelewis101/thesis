
# this function needs to be passed a df (one patient)
# and a covariate col no a

# it will hand back the first row that contains a 1
# or the row with the largest assess_type if all rows are 0

# for doing a time-to-event with survival package



ditch_everything_after_first_1 <- function(dfin, a) {
  dfin <- as.data.frame(dfin)
  dfin <- dfin[order(dfin$assess_type),]
  if (all(dfin[,a] == 0)) {
    return(dfin[nrow(dfin),])
  } else {
    dfin <- dfin[(dfin[,a] != 0),] 
    dfin <- subset(dfin, assess_type == max(dfin$assess_type))
    return(dfin)
  }
}
