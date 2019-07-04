## extract covariate exposure

extract_covariate_exposure <- function(dfin, a,b, endtime) {
  dfin <- as.data.frame(dfin)
  pid <- dfin$pid[1]
  dftemp <- uncompress_covariates(dfin)
  dftemp <- subset(dftemp, assess_type <= endtime)
  #apply(dftemp[,a:b],2, sum)
  cbind(pid,as.data.frame(t(apply(dftemp[,a:b],2, sum)))) -> dftemp
  dftemp$pid <- as.character(dftemp$pid)
  return(dftemp)
  
}