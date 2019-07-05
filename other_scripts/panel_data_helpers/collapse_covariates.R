
# this fuction takes a df of covariates and collapses them if the covariates remain unchanged over different assess_types

# df is the df
# a is the start of covariates
# b the end
# assumes pid, assess_type exist
#


collapse_covariates <- function(dfin, a, b ) {

  
  rownames(dfin) <- NULL
  dfin <- dfin[order(dfin$pid, dfin$assess_type),]
  rows.to.remove <- NULL
  if (nrow(dfin) == 1) { 
    return(dfin) } else {
      
  for (i in 1:(nrow(dfin)-1)) {
    
    if (all(dfin[i,a:b] == dfin[i+1,a:b])) {
      dfin$assess_type[i+1] <- dfin$assess_type[i]
      rows.to.remove <- c(rows.to.remove, i)
    }
  }
  if (!is.null(rows.to.remove)) { return(dfin[-rows.to.remove,]) } else { return(dfin)}
    }
  
  
  
}
  



  