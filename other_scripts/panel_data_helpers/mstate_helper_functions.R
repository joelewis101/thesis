# mstate helpers

pointESBLmeas_tostartstop <- function(dfin,censoradd = 0.01) {                       
  # same old trick to pass it ids with ddply so 
  # this function will return a tstart - tstop dataframe
  # it will censor at final time + censortime
  # it will add a new column init.state and init.sample
  
  dfin <- dfin[order(dfin$assess_type),]  
  dfin$tstart <- NA
  dfin$tstop <- NA
  dfin$init.state <- NA
  
  if (dfin$assess_type[1] != 0) {
    stop(paste0("Error: No timezero entry for pid: ", dfin$pid[1]))
  }
  
  dfin$init.state <- dfin$ESBL[1]                  # set init.states to t= 0 value 
  dfin$init.sample.type <- dfin$sample_type[1] 
  
  if (nrow(dfin) == 1) {                           # if there s only one row at t=0 , censor at 0.01
    dfin$tstart[1] <- 0
    dfin$tstop[1] <- dfin$tstart + censoradd 
    dfin$ESBL <- "censor"
  } else {
    dfin <- dfin[2:nrow(dfin),]
    for (i in 1:nrow(dfin)) {
      if (i == 1) {
        dfin$tstart[i] <- 0
        dfin$tstop[i] <- dfin$assess_type[i]
      } else {
        dfin$tstart[i] <- dfin$tstop[i-1] 
        dfin$tstop[i] <- dfin$assess_type[i]
      }
    }
    
    dfin <- rbind(dfin, dfin[nrow(dfin),])        # add final censor row
    dfin$assess_type[nrow(dfin)] <- NA
    dfin$tstart[nrow(dfin)] <-  dfin$tstop[nrow(dfin) - 1] 
    dfin$tstop[nrow(dfin)] <- dfin$tstop[nrow(dfin)] + censoradd
    dfin$ESBL[nrow(dfin)] <- "censor"
  }
  return(dfin)
}


# this function will change the transition times

change_transition_times <- function(dfin, trans_assum = 1) {
  if (dfin$ESBL[nrow(dfin)] != "censor") {stop(paste0("Error: the last state is not censored in ", dfin$pid[1]))}
  i <- 1              # row index 
  nrows <- nrow(dfin)
  dfin <- dfin[order(dfin$tstart),]
  while (i < nrows) {
    # print(i)
    if (i == 1) {
      
      if (dfin$ESBL[i] != dfin$init.state[i]) {
        newrow <- dfin[i,]
        dfin$tstop[i] <- dfin$tstop[i] * trans_assum
        newrow$tstart <- dfin$tstop[i]
        #print("adding new row")
        dfin <- rbind(dfin, newrow)
        dfin <- dfin[order(dfin$tstart),]
        nrows <- nrows + 1
        i <- i + 2
      } else {
        i <- i + 1
      }
      
    } else {
      
      if (dfin$ESBL[i] != dfin$ESBL[i-1]) {
        newrow <- dfin[i,]
        dfin$tstop[i] <- dfin$tstart[i] + (dfin$tstop[i] - dfin$tstart[i]) * trans_assum
        newrow$tstart <- dfin$tstop[i]
        # print("adding new row")
        dfin <- rbind(dfin, newrow)
        dfin <- dfin[order(dfin$tstart),]
        nrows <- nrows + 1
        i <- i + 2
      } else {
        i <- i + 1
      }
      
    }
  }
  return(dfin)
}

pull_state_occ_from_surv_obj <- function(fit, opname = "observed") {
  p1 <- cbind(data.frame(fit[1,]$time), data.frame(fit[1,]$n.risk),
              data.frame(fit[1,]$pstate),data.frame(fit[1,]$lower),
              data.frame(fit[1,]$upper))
  names(p1) <- c("t", "n.risk.ESBL.neg","n.risk.ESBL.pos", "p(ESBL.neg)", 
                 "p(ESBL.pos)", "ll.ESBL.neg", "ll.ESBL.pos",
                 "ul.ESBL.neg", "ul.ESBL.pos")
  
  t0 <- p1[1,]
  t0$t <- 0
  t0[2:9] <- NA
  t0[4:5] <- fit$p0[1,]
  t0$ll.ESBL.neg <- t0$ul.ESBL.neg <- t0$`p(ESBL.neg)`
  t0$ll.ESBL.pos <- t0$ul.ESBL.pos <- t0$`p(ESBL.pos)`
  p1 <- rbind(p1, t0)
  p1$arm <- 1
  pstate <- p1
  p1 <- cbind(data.frame(fit[2,]$time), data.frame(fit[2,]$n.risk),
              data.frame(fit[2,]$pstate),data.frame(fit[2,]$lower), 
              data.frame(fit[2,]$upper))
  names(p1) <- c("t", "n.risk.ESBL.neg","n.risk.ESBL.pos", 
                 "p(ESBL.neg)", "p(ESBL.pos)", "ll.ESBL.neg", 
                 "ll.ESBL.pos", "ul.ESBL.neg", "ul.ESBL.pos")
  
  t0 <- p1[1,]
  t0$t <- 0
  t0[2:9] <- NA
  t0[4:5] <- fit$p0[2,]
  t0$ll.ESBL.neg <- t0$ul.ESBL.neg <- t0$`p(ESBL.neg)`
  t0$ll.ESBL.pos <- t0$ul.ESBL.pos <- t0$`p(ESBL.pos)`
  p1 <- rbind(p1, t0)
  p1$arm <- 2
  pstate <- rbind(pstate, p1)
  p1 <- cbind(data.frame(fit[3,]$time), data.frame(fit[3,]$n.risk),
              data.frame(fit[3,]$pstate),data.frame(fit[3,]$lower), 
              data.frame(fit[3,]$upper))
  names(p1) <- c("t", "n.risk.ESBL.neg","n.risk.ESBL.pos", 
                 "p(ESBL.neg)", "p(ESBL.pos)", "ll.ESBL.neg", 
                 "ll.ESBL.pos", "ul.ESBL.neg", "ul.ESBL.pos")
  
  t0 <- p1[1,]
  t0$t <- 0
  t0[2:9] <- NA
  t0[4:5] <- fit$p0[3,]
  t0$ll.ESBL.neg <- t0$ul.ESBL.neg <- t0$`p(ESBL.neg)`
  t0$ll.ESBL.pos <- t0$ul.ESBL.pos <- t0$`p(ESBL.pos)`
  p1 <- rbind(p1, t0)
  p1$arm <- 3
  pstate <- rbind(pstate, p1)
  pstate$arm <- as.factor(pstate$arm)
  pstate$group <- opname
  return(pstate)
}