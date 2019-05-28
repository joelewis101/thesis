
median_iqr_str <- function(x, r= 0) {
  # x is numeric vector
  # r is digits to round to
  if (!is.numeric(x)) {stop("Nonnumeric value in median_iqr_str.")}
  out_str <- paste0(
    round(median(x), r), 
    " (",
    round(quantile(x, 0.25)[[1]], r),
    "-",
    round(quantile(x, 0.75)[[1]], r),
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
  for (l in 1:length(levs)) {
    
      n <-  sum(x == levs[[l]], na.rm = T)
      N <-  sum(!is.na(x))
      out[[l]] <- paste0(n, 
                         "/",
                         N,
                         " (",
                         round((n*100/N), r), 
                         "%)"
    )
  } 
  names(out) <- levs
  return(out)
  
}

pretty_tbl_df <- function(df) {
  
}

