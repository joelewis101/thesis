## extract covariate exposure

# expand df between covariate rows a and b (inclusive) and return a single row with
# the total exposure between starttime and endtime

extract_covariate_exposure <- function(dfin, a,b, starttime, endtime) {
  dfin <- as.data.frame(dfin)
  pid <- dfin$pid[1]
  dftemp <- uncompress_covariates(dfin)
  dftemp <- subset(dftemp, assess_type <= endtime & assess_type >= starttime)
  #apply(dftemp[,a:b],2, sum)
  cbind(pid,as.data.frame(t(apply(dftemp[,a:b],2, sum)))) -> dftemp
  dftemp$pid <- as.character(dftemp$pid)
  return(dftemp)
  
}
