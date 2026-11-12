dataDateMkr <- function(dateVar = NA) {
  # 1. Pull date from command-line argument, passed parameter, or default to yesterday
  args <- commandArgs(trailingOnly = TRUE)
  
  ref_date <- if (length(args) > 0) {
    as.Date(args[1])
  } else if (!is.na(dateVar)) {
    as.Date(dateVar)
  } else {
    # Defaults to first of last month
    floor_date(Sys.Date(), "month") %m-% months(1)
  }
  
  return(target_month_date)
}