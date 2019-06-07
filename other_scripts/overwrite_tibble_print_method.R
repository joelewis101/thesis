# get rid of horrid tibble printing


print.tbl_df <- function(x, ...) {
  print.data.frame(x, ...)
  invisible(x)
}
