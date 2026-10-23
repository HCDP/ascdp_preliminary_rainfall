dataDateMkr <- function(dateVar = NA) {
  # 1. Pull date from command-line argument, passed parameter, or default to yesterday
  args <- commandArgs(trailingOnly = TRUE)
  
  ref_date <- if (length(args) > 0) {
    as.Date(args[1])
  } else if (!is.na(dateVar)) {
    as.Date(dateVar)
  } else {
    Sys.Date() - 1 # Defaults to yesterday
  }
  
  # 2. Convert the reference date to the 1st day of the PREVIOUS month
  target_month_date <- floor_date(ref_date, "month") %m-% months(1)
  
  return(target_month_date)
}