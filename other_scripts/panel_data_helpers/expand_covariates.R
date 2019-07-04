### take a row of hospoutcome and put into MSM format ###

## This function expands a row of covariates with numbers that indicate how many days 
# that covariate holds for
# so if amoxy == 7 on assess_typ= 3, it will create a new row at assess_type 3 with amoxy = 1
# a new row at assess_type + 7 with amoxy = 0
# it starts with the lowest covariate and builds up
# a is the start row of covariates, b the end
# hand it the whole outcome df using ddply (or a %>% do() for dplyr pipes)

# it wil take the outcome df and expand the rows based on days of abx
# it is expecting a number at each covariate position and will use that to expand rows
# i haven't tested handing it more than one row - it will probable do weird stuff

# remember to include hosp in the a:b call for this one when sorting out outcome

# though the name is outcome, it will work for a generic dataframe
# it needs a died variable in the passed df

# If anybody except me ever looks at this code, I just want you to know
# that this function is RECURSIVE and CALLS itself
# I'm so proud
# I'm basically Bill Gates or that CERN guy that invented the web

expand_covariates <- function(dfin, a, b) {
  dfin <- as.data.frame(dfin) # sort out tibble badness
 # print(nrow(dfin))
  #if (nrow(dfin) > 1) {stop(paste0("Dawg more than one row passed to expand_covariates: ", dfin$pid[1]))}
  if (all(dfin[nrow(dfin),a:b]== 0) | dfin$died[nrow(dfin)])  {
    
    return(rbind(dfin))
    
  } else {
    
    dfin[a:b][dfin[a:b] == 0] <- NA
    mincol <- which.min(apply(dfin[a:b],MARGIN=2,min))
    newrow <- dfin[nrow(dfin),]
    
    newrow$assess_type[1] = dfin$assess_type[nrow(dfin)] + dfin[nrow(dfin), mincol + a - 1]
    #newrow$hosp[1] <- 0
   # newrow$data_date <- newrow$data_date + newrow$assess_type
    newrow[,a:b] <- newrow[,a:b] - dfin[nrow(dfin), mincol + a -1]
    dfin[nrow(dfin), a:b][!is.na(dfin[nrow(dfin), a:b])] <- 1
    dfout <- rbind(dfin,newrow)
    dfout[,a:b][is.na(dfout[,a:b])] <- 0
    return(expand_covariates(dfout, a, b))
  }
}

