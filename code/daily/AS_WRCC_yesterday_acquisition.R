###### American Samoa Rainfall Data Acquisition - WRCC/NPS

#load packages
library(httr)
library(stringr)
library(dplyr)
library(lubridate)

#set dirs
codeDir<-Sys.getenv("CODE_DIR")
outputDir<-Sys.getenv("OUTPUT_DIR")
dependencyDir<-Sys.getenv("DEPENDENCY_DIR")

outDir <- paste0(outputDir,"/as_individual_data")

# # create output directory if it doesn't exist
# if (dir.exists(outDir)) {
#   files_to_delete <- list.files(outDir, full.names = TRUE)
#   file.remove(files_to_delete)
# } else {
#   dir.create(outDir, recursive = TRUE)
# }

# Add to existing folder, or create new one if there is none already
if (!dir.exists(outDir)) {
  dir.create(outDir, recursive = TRUE)
}

#function to fetch precipitation for one station for a given date range
fetch_station <- function(stn, start_date, end_date) {
  message("Fetching station: ", stn, " (", start_date, " to ", end_date, ")")  # log progress
  
  #set start and end dates
  smon <- sprintf("%02d", month(start_date))    # start month
  sday <- sprintf("%02d", day(start_date))      # start day
  syea <- substr(year(start_date), 3, 4)        # start year (last 2 digits)
  
  emon <- sprintf("%02d", month(end_date))      # end month
  eday <- sprintf("%02d", day(end_date))        # end day
  eyea <- substr(year(end_date), 3, 4)          # end year (last 2 digits)
  
  #pull station data with parameters
  res <- GET(
    "https://wrcc.dri.edu/cgi-bin/wea_dysimts2.pl",
    query = list(
      stn = stn,
      smon = smon, sday = sday, syea = syea,
      emon = emon, eday = eday, eyea = eyea,
      qBasic = "ON", qPR = "ON",     # request precipitation
      unit = "M",                     # metric units (mm)
      Ofor = "A",                      # ASCII output
      Datareq = "A", qc = "N", miss = "08",
      obs = "N", WsMon = "01", WsDay = "01",
      WeMon = "12", WeDay = "31"
    )
  )
  
  #extract block containing the data
  txt <- content(res, "text")                     # raw HTML response
  pre_block <- str_match(txt, "(?s)<PRE>(.*)</PRE>")[,2]  # extract text between <PRE> tags
  lines <- str_split(pre_block, "\n")[[1]]        # split into lines
  lines <- lines[lines != ""]                     # remove empty lines
  
  #locate header and skip units row
  header_idx <- which(str_detect(lines, "Date"))[1]  # find first line containing "Date"
  data_lines <- lines[(header_idx + 2):length(lines)] # skip units row (2 lines after header)
  
  #parse each line
  df <- lapply(data_lines, function(line) {
    parts <- str_split(str_trim(line), "\\s+")[[1]]  # split line into columns by whitespace
    if(length(parts) < 2) return(NULL)              # skip malformed lines
    data.frame(
      Date = as.Date(parts[1], format="%m/%d/%Y"),  # first column is date
      Precip_mm = as.numeric(parts[length(parts)]), # last column is precipitation in mm
      station = stn
    )
  })
  
  #combine list of rows into a dataframe and remove any NA rows
  df <- bind_rows(df) %>% filter(!is.na(Date))
  
  return(df)
}

#define date
source(paste0(codeDir,"/as_dataDateFunc.R"))
dataDate<-dataDateMkr() #function for importing/defining date as input or as yesterday
currentDate<-dataDate #dataDate as currentDate

#pull yesterday's data for both stations
stations <- c("sam1", "sam2")  # station codes
all_data <- bind_rows(
  lapply(stations, function(stn) {
    fetch_station(stn, start_date = currentDate, end_date = currentDate)
  })
)

#reformat
all_data_formatted <- all_data %>%
  transmute(
    station_id   = station,          # rename station
    date         = Date,             # rename date
    value        = Precip_mm,        # precipitation value
    variable     = "precip_mm",      # add variable column
    completeness = 1                 # full day (since single day, assume complete)
  )

#station metadata
station_meta <- tibble(
  station_id   = c("sam1", "sam2"),
  SKN          = c(1024, 1023),
  Station.Name = c("toa_ridge_WRCC", "siufaga_WRCC"),
  Observer     = c("WRCC"),
  Elev.m       = c(391.7, 146.3),
  LAT          = c(-14.2611, -14.2761),
  LON          = c(-170.687, -170.722)
)

#combine meta and data
all_data_formatted <- all_data_formatted %>%
  left_join(station_meta, by = c("station_id")) %>%
  select(station_id, SKN, Station.Name, Observer, Elev.m, LAT, LON, date, value, variable, completeness)

#write csv
output_file <- paste0(outDir,"/as_wrcc_", gsub("-", "_", currentDate), ".csv")
write.csv(all_data_formatted, output_file, row.names = FALSE)
message("Daily summary with metadata saved to: ", output_file)

#end
