## shufflle outcome into df ###

## hand this function a short dfa of all the entries of one pid (using ddply) from the assembled hourly/pre72/upto72
# dataframe and the whole dfb of the expanded outcome

# it makes some assumptions i
# it assumes that all covariates are right if there are two rows eith the same assess_type and combines them together
# it assumes that hosp outcome (dfb) form is right about date of discharge and gets
# rid of any entries from after that date if ditch_dfa_records_after_dfb_finishes = T


#dfa <- subset(df, pid == "DAS1008P")
#dfb <- outcome.expanded

#a <- 5
#b <- 28

#dfb should be expanded.outcome
#dfa should be the hospital data

# if there is no match in dfb, hand back dfa as is


shuffle_a_in_b2 <- function(dfa, dfb,a, b, ditch.a.after.dc = FALSE) {
    dfa <- as.data.frame(dfa)
    dfb <- as.data.frame(dfb)
 
   if (!(dfa$pid[1] %in% dfb$pid)) {
    return (dfa)
   } else {
  dfbsub <- subset(dfb, pid == dfa$pid[1])
  dfbsub <- uncompress_covariates(dfbsub)
  dfbsub <- dfbsub[order(dfbsub$assess_type),]
  dfa <- uncompress_covariates(dfa)
  dfbsub <- dfbsub[order(dfbsub$assess_type),]
  
  if (ditch.a.after.dc == TRUE) {
    dfa <- subset(dfa, assess_type <= min(dfbsub$assess_type))
  }

  for (i in 1:nrow(dfbsub)) {
    if (!dfbsub$assess_type[i] %in% dfa$assess_type) {
      dfa <- rbind(dfa, dfbsub[i,])
    } else {
      dfa[dfa$assess_type==dfbsub$assess_type[i],][,a:b] <- 
        dfa[dfa$assess_type==dfbsub$assess_type[i],][,a:b] + dfbsub[i,a:b]
      dfa[dfa$assess_type==dfbsub$assess_type[i],][,a:b][dfa[dfa$assess_type==dfbsub$assess_type[i],][,a:b] >= 1 ] <- 1
    }
  }
 dfa <- collapse_covariates(dfa, a, b)
    return(dfa)
  }
}


# this one will expand a df to give a row for each day, keeping covariates the same

uncompress_covariates <- function(dfin) {
  dfin <- as.data.frame(dfin)
   dfin <- dfin[order(dfin$assess_type),]
 # dfin$data_date <- as.Date(dfin$data_date)
#  dfin$enroll_date <- as.Date(dfin$enroll_date)
  dfin$reps <-1
  if (nrow(dfin) == 1) {
    dfin <- select(dfin, -reps)
    return(dfin)
  } else {
      for (i in 1:nrow(dfin)) {
        if (i < nrow(dfin)) {
        dfin$reps[i] <- dfin$assess_type[i+1] - dfin$assess_type[i]  
        } 
      }
    dfin <- dfin[rep(1:nrow(dfin), dfin$reps),]
    dfin$assess_type <- dfin$assess_type[1]:(dfin$assess_type[1] + nrow(dfin)-1)
    
  }
  dfin <- select(dfin, -reps)
#  dfin$data_date <- dfin$enroll_date + dfin$assess_type
  return(dfin)
}

# and a wrapper just to save havingto add the extras on at the end

merge_longit_dfs <- function(dfa, dfb, covariate_start_row, covariate_end_row, ditch_dfa_after_dc = FALSE) {
  if (!identical(names(dfa),names(dfb))) {stop("nonidentical row names of dfs in merge_longit_dfs my man")}
  
  dfa %>% group_by(pid) %>%
    do(shuffle_a_in_b2(.,dfb, covariate_start_row, covariate_end_row, ditch_dfa_after_dc)) -> df.out
  
 #  add extras
   bind_rows(df.out,
                   subset(dfb, !(pid %in% dfa$pid))
  ) -> df.out
  return(df.out)
}

# and to it rowwsie on df a

rowwise_expand_and_shuffle_a_in_b2 <- function(dfa, dfb, a, b) {
  for (i in 1:nrow(dfa)) {
    dfb <- merge_longit_dfs(
      expand_covariates(dfa[i,], a, b),
      dfb,
      a,b,
    )
  }
  return(dfb)
}
