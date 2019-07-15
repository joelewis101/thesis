# generate stan df for covariates

# several steps:
# get ESBL status to define segments

# this extracts states as start-stop

# workflow

# fro a given pid in df
# long_to_wide_panel_states(df, "ESBL") -> esbl.df
# pulls out the staes and returns a df with 4 cold - tstart, tstop, ESBL_start and ESBL_stop

# long_to_wide_panel_covariates(df, "abx") -> cov.df
# pulls out abx start and stop times as a two col df with abx_start, abx_stop

# add_covariate_to_state_df(esbl.df, cov.df, "abx")
# sticks em togther

long_to_wide_panel_states <- function(dfin, state_var) {
  dfin <- dfin[c("pid", "assess_type", state_var)]
  dfin <- dfin[order(dfin$assess_type),]
  #print(as.character(dfin$pid[1]))
 # dfin <- collapse_covariates(dfin, 3,3)
  dfin <- dfin[dfin[state_var] != 999,]
  if (nrow(dfin) == 1) {
    print(paste0("One sample only for pid ", dfin$pid[1], ", dropping from analysis y'all."))
    return(NULL)
  } else {
   # print("ballbag")
    out <- list()
    for (i in 1:(nrow(dfin)-1)) {
      #print(i)
      outdf <- data.frame(pid = dfin$pid[1],
                        tstart = dfin[[i, "assess_type"]],
                        tstop = dfin[[(i+1),"assess_type"]],
                        var_start = dfin[[i,state_var]],
                        var_stop = dfin[[i+1,state_var]])
      names(outdf)[4:5] <- c(paste0(state_var, "_start"), paste0(state_var, "_stop"))
      out[[i]] <- outdf
      }
  
  out <- do.call(rbind, out)
  return(out)
  }
}

long_to_wide_panel_covariates <- function(dfin, cov_var) {
  dfin <- dfin[c("pid", "assess_type", cov_var)]
  dfin <- uncompress_covariates(dfin)
  if (all(dfin[cov_var] == 0)) {
    ranges <- data.frame(V1 = NA, V2 = NA)
    names(ranges) <- c(paste0(cov_var, "_start"), paste0(cov_var ,"_end"))
  } else {
    dfin <- dfin[dfin[cov_var] == 1,]
  #dfin <- dfin[order(dfin$assess_type),]
    tapply(dfin["assess_type"], cumsum(c(1, diff(dfin["assess_type"])) != 1), range) -> ranges
    as.data.frame(do.call(rbind, ranges)) -> ranges
    names(ranges) <- c(paste0(cov_var, "_start"), paste0(cov_var ,"_end"))
  }
  return(ranges)
}

add_covariate_to_state_df <- function(esbl.df, cov.df, cov.name) {
  # assumes a two col df of start and stop times for covs -
  esbl.df$cov_start_time <- NA
  esbl.df$cov_end_time <- NA
  esbl.df$prev_cov_exposure <- 0
  esbl.df$prev_cov_stop <- NA
    for (c in nrow(cov.df)) {
      if (all(is.na(cov.df[c,1:2]))) {
      # don't do anything
      } else {
        
      
      # fro each row - does cov start time fall in this interval? If so, add it 
        for (e in 1:nrow(esbl.df)) {
          #print(e)
          # if cov start time is beyond tstop, or cov end time is before tstart do nothing
        
          # else set cov_start to cov start time and cov end to end
          # and then set hem based on tstart and tstopto make sense
          if (cov.df[c,1] > esbl.df$tstop[e] | cov.df[c,2] < esbl.df$tstart[e]) {
            # don't do anthing
          } else {
            if (!is.na(esbl.df$cov_start_time[e]) | !is.na(esbl.df$cov_end_time[e])) { 
              stop(paste0("Overlapping intervals in add_covariate_to_state_df, pid ", esbl.df$pid[1])) }
            esbl.df$cov_start_time[e] <- cov.df[c,1]
            esbl.df$cov_end_time[e] <- cov.df[c,2]
          }
     
        }
      }
      esbl.df$cov_start_time[esbl.df$cov_start_time < esbl.df$tstart & !is.na(esbl.df$cov_start_time )] <- 
        esbl.df$tstart[esbl.df$cov_start_time < esbl.df$tstart & !is.na(esbl.df$cov_start_time )]
      esbl.df$cov_end_time[esbl.df$cov_end_time > esbl.df$tstop & !is.na(esbl.df$cov_end_time)] <- 
        esbl.df$tstop[esbl.df$cov_end_time > esbl.df$tstop &  !is.na(esbl.df$cov_end_time)]
    }
  ## add in prev_exposure
  
  if (nrow(esbl.df) > 1) {
    for (i in 2:nrow(esbl.df)) {
      if (any(!is.na(esbl.df$cov_end_time[1:(i-1)]))) {
        esbl.df$prev_cov_exposure[i] <- 1
        esbl.df$prev_cov_stop[i] <- max(esbl.df$cov_end_time[1:(i-1)], na.rm = TRUE)
      }
    }
  }
  sub("cov", cov.name, names(esbl.df)) -> names(esbl.df)
  return(esbl.df)
  }
}

generate_stan_df <- function(df, state_var, covariate_var_list) {
  # assume handed a df with a single pid's values
  dfout <- long_to_wide_panel_states(df, state_var)
  if (!is.null(dfout)) {
  for (i in 1:length(covariate_var_list)) {
    dfout <- add_covariate_to_state_df(esbl.df = dfout,
                                       cov.df = long_to_wide_panel_covariates(df, covariate_var_list[i]),
                                       cov.name = covariate_var_list[i]
    )
  }
  return(dfout)
  } else {
    return(data.frame(NULL))
  }
}

spliced$ESBL <- as.numeric(spliced$ESBL)  

spliced  %>% group_by(pid) %>% do(generate_stan_df(., "ESBL", c("hosp", "abx"))) -> stan.df

as.data.frame(
  subset(stan.df, pid == sample(unique(stan.df$pid), 1))
)
