
median_iqr_str <- function(x, r= 0) {
  # x is numeric vector
  # r is digits to round to
  if (!is.numeric(x)) {stop("Nonnumeric value in median_iqr_str.")}
  out_str <- paste0(
    format(round(median(x, na.rm =T), r), nsmall = r),
    " (",
    format(round(quantile(x, 0.25, na.rm = T)[[1]], r), nsmall = r),
    "-",
    format(round(quantile(x, 0.75, na.rm = T)[[1]], r), nsmall = r),
    ")"
  )
  return(out_str)
  
}

prop_str <- function(x, r= 0) {
  # x is any vector
  # r is digits to round to
  x <- as.factor(x)
  levs <- levels(x)
  out <- list()
  sortvar <- list()
  for (l in 1:length(levs)) {
    
      n <-  sum(x == levs[[l]], na.rm = T)
      N <-  sum(!is.na(x))
      
      out[[l]] <- paste0(n, 
                         "/",
                         N,
                         " (",
                         format(round((n*100/N), r), nsmall = r),
                         "% [",
                         format(round(binom.test(n,N)$conf.int[[1]] * 100, r), nsmall = r),
                         "-",
                         format(round(binom.test(n,N)$conf.int[[2]] * 100, r), nsmall = r),
                         "%])"
    )
      sortvar[[l]] <- n/N
  } 
  names(out) <- levs
  out[order(unlist(sortvar), decreasing = T)] -> out
  
  return(out)
  
}

prop_str_df <- function(x, r = 0, varname = "Var") {
  out <- as.data.frame(do.call(rbind, prop_str(x, r=r)), stringsAsFactors = F)
  out$V2 <- rownames(out)
  out$V3 <- varname
  out <- out[,c(3,2,1)]
  names(out) <- c("variable", "levels", "value")
  rownames(out) <- NULL
  return(out)
}

median_iqr_str_df <- function(x, r = 0, varname = "Var") {
  out <- data.frame(variable = varname, levels = "Median (IQR)", value = median_iqr_str(x, r=r), stringsAsFactors = F)
  rownames(out) <- NULL
  return(out)
}

prop_confint_str <- function(x, r= 0) {
  # x is binary vector
  # r is digits to round to
 # if (!is.numeric(x)) {stop("Nonnumeric value in median_iqr_str.")}
  n = sum(x == 1, na.rm = T)
  N = sum(!is.na(x))
  out_str <- paste0(
    format(round((n*100/N), r), nsmall = r),
    "% ",
    " (",
    format(round(binom.test(n,N)$conf.int[[1]] * 100, r), nsmall = r),
    "-",
    format(round(binom.test(n,N)$conf.int[[2]] * 100, r), nsmall = r),
    "%)"
  )
  return(out_str)
  
}




pretty_tbl_df <- function(df, vars_to_char = NULL, r = 0) {
  if (!is.null(vars_to_char)) {
    df[vars_to_char] <- as.data.frame(lapply(df[vars_to_char], as.character), stringsAsFactors = F)
  }
  
  out <- list()
  for (i in 1:ncol(df)) {
    col.class <- class(df[[i]])
    if (col.class == "factor") {
      df[[i]] <- as.character(df[[i]])
      col.class <- "character"
    }
    if (col.class == "numeric" | col.class == "integer") {
      out[[i]] <- median_iqr_str_df(df[[i]], r = r, varname = names(df[i]))
    } else if (col.class == "character") {
      out[[i]] <- prop_str_df(df[[i]], r = r, varname = names(df[i]))
    } else {
      stop("Non character, factor, numeric or integer class in pretty_tb_df()")
    }
  }
    out <- do.call(rbind, out)
    return(out)
  }

make_kable_rowgroup_string <- function(df) {
  require(dplyr)
  df %>% dplyr::group_by(variable, .drop = TRUE) %>% dplyr::mutate(n_var = dplyr::n()) -> df
  df <- unique(dplyr::select(df, variable, n_var))
 # df$row_end <- cumsum(df$n_var)
#  df$row_start <- NA
 # for (i in 1:nrow(df)) {
  #  if (i == 1) {
  #    df$row_start[i] <- 1
  #  } else {
  #    df$row_start[i] <- df$row_end[i-1] + 1
  #  }
  #}
  outstring <- df$n_var
  names(outstring) <- df$variable
 # names(outstring)[outstring == 1 ] <- " "
  return(outstring)
}

