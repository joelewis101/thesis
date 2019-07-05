## sort out tb treatment ##

# this function exists because th elength of ongoimg TB rx on discharge is not propoerly recorded
# it calculaes the true stop date of tb rx and puts that number the tb covariate row

## hand this functiona single row (using ddply)
# first, if tb == 0, return the row you got
# if tb != 0 and check tb_rx_on_adm == 1 tehn set tb = 168 - tbrxtimedays - assess-type
# if tb !=0 and tb_rx_on_adm == 0 then set tb rx to 168 - tb startdate


# safety net - tb rx can't be longer than 168 days

sort_out_tb_rx_on_discharge <- function(dfa, dfb){
  dfa <- as.data.frame(dfa)
  if (nrow(dfa) > 1) stop(paste0("Bro >1 row passed to sort_out_tb_rx_on_dc: ", dfa$pid[1] ))
  if (dfa$tb == 0) {
    
    # if treatment was started as ip - continue post dc
    
    temp <- subset(dfb, pid == dfa$pid[1])
    tbrxstartindex <- suppressWarnings( min(which(temp$tb == 1)))
    if (!is.infinite(tbrxstartindex)) {
      tbrxstart <- temp$assess_type[tbrxstartindex]
      dfa$tb <- 180 - (dfa$assess_type - tbrxstart) 
      }
    return(dfa)
    
  } else if (dfa$tb > 0) {
    if (dfa$tbongoing == 1 & !is.na(dfa$tbongoing)) {
      dfa$tb <- 180 - as.numeric(as.Date(dfa$enroll_date) - as.Date(dfa$tbrxstart)) - dfa$assess_type
    } else if (dfa$tbongoing == 0 | is.na(dfa$tbongoing)) {
      temp <- subset(dfb, pid == dfa$pid[1])
      tbrxstartindex <- suppressWarnings( min(which(temp$tb == 1)))
      if (!is.infinite(tbrxstartindex)) {
        tbrxstart <- temp$assess_type[tbrxstartindex]
      } else {tbrxstart <- dfa$assess_type}
      dfa$tb <- 180 - (dfa$assess_type - tbrxstart)
    }
  }
  if (dfa$tb > 180) {dfa$tb <- 180}
  return(dfa)
  
}
