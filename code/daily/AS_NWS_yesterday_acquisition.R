library(jsonlite)
library(dplyr)

#set dirs
codeDir<-Sys.getenv("CODE_DIR")
outputDir<-Sys.getenv("OUTPUT_DIR")
dependencyDir<-Sys.getenv("DEPENDENCY_DIR")
outDir<-paste0(outputDir,"/as_individual_data")

# --- Configuration ---
STATION    <- "NSTU" # Pago Pago Airport Station Code
OUTPUT_CSV <- paste0(outDir,"nws_station_data.csv")
LOG_FILE   <- paste0(outputDir,"nws_fetch_log/fetch_log.csv")

# Add to existing folder, or create new one if there is none already
if (!dir.exists(outDir)) {
  dir.create(outDir, recursive = TRUE)
}

#define date
source(paste0(codeDir,"/as_dataDateFunc.R"))
dataDate<-dataDateMkr() #function for importing/defining date as input or as yesterday
currentDate<-dataDate #dataDate as currentDate
currentDate<-format(as.Date(dataDate +1), "%Y-%m-%d")
currentDate

log_message <- function(msg, level = "INFO") {
  cat(sprintf("[%s] %s - %s\n", level, Sys.time(), msg), file = LOG_FILE, append = TRUE)
}

# Initialize a fallback default value in case the tryCatch encounters a network failure
yesterday_rainfall <- NA

tryCatch({
  log_message("Requesting hourly METAR data from Aviation Weather Center API.")
  
  # 1. Pull the last 48 hours of raw observation data from the official NOAA API
  # Specifying JSON format makes extraction perfectly clean
  url <- sprintf("https://aviationweather.gov/api/data/metar?ids=%s&hours=48&format=json", STATION)
  
  # Set a custom user agent as required by NOAA API guidelines to prevent automated blocking
  req <- url(url, headers = c("User-Agent" = "GuamPagoPagoResearchScript/1.0 (contact@yourdomain.com)"))
  raw_json <- readLines(req, warn = FALSE)
  close(req)

  # 2. Safely parse the valid JSON structure
  obs_dataframe <- fromJSON(raw_json)

  if (!is.data.frame(obs_dataframe) || nrow(obs_dataframe) == 0) {
    stop("API payload returned an empty structure or invalid format.")
  }
  
  # Convert the numeric Unix epoch 'obsTime' directly into American Samoa Local Time.
  # This prevents UTC day-rollover bugs regardless of what time the script runs.
  obs_dataframe$local_datetime <- as.POSIXct(obs_dataframe$obsTime, 
                                             origin = "1970-01-01", 
                                             tz = "Pacific/Pago_Pago")
  
  # Extract just the calendar date component for matching
  obs_dataframe$local_date <- as.Date(obs_dataframe$local_datetime)
  
  obs_dataframe

  # -------------------------------------------------------------------------
  # 4. Filter Data and Extract Previous Day's Total Accumulation
  # -------------------------------------------------------------------------
  # Isolate only the rows that belong to yesterday's calendar date
  yesterday_data <- obs_dataframe %>% 
    filter(local_date == as.Date(currentDate))
  
  yesterday_data
  # Ensure data exists for yesterday before processing
  if (nrow(yesterday_data) > 0) {
    
    # Ensure target precipitation columns are numeric
    yesterday_data$pcp24hr <- as.numeric(yesterday_data$pcp24hr)
    
    # Check if there is a valid 24-hour summary report available
    if (any(!is.na(yesterday_data$pcp24hr))) {
      yesterday_rainfall <- max(yesterday_data$pcp24hr, na.rm = TRUE)
    } else {
      # If no 24hr value is present, explicitly mark the day as NA
      yesterday_rainfall <- NA
    }
    
    # If max() returns -Inf (all values were NA), it means no rainfall occurred or was logged
    if (is.infinite(yesterday_rainfall)) {
      yesterday_rainfall <- 0.00
    }
    log_message(sprintf("Successfully calculated yesterday's rainfall: %s inches", yesterday_rainfall))
    
  } else {
    print("Warning: No observations found in the payload for yesterday's date.")
    yesterday_rainfall <- NA
  }
  
}, error = function(e) {
  # This block catches network drops, API changes, or bad JSON formats without killing the pipeline
  log_message(sprintf("Pipeline failed to fetch or parse NOAA data: %s", e$message), level = "ERROR")
  yesterday_rainfall <<- "NA" # Assign to global environment fallback scope
})

  # -------------------------------------------------------------------------
  # 5. Output Final Value
  # -------------------------------------------------------------------------
  print(paste("Final Yesterday RF Total (inches):", yesterday_rainfall))

#Make final export table
# Create a clean data frame mirroring your template structure
output_row <- data.frame(
  station_id   = STATION,
  SKN          = 1005,
  Station.Name = "airport5101",
  Observer     = "USGS",
  Elev.m       = 3,
  LAT          = -14.33167,
  LON          = -170.7142,
  date         = format(as.Date(currentDate), "%m/%d/%Y"), # Converts to mm/dd/yyyy
  value        = NA,                                       # Placeholder
  variable     = "precip_mm",
  completeness = 0,                                        # Default to 0 (missing/NA)
  stringsAsFactors = FALSE
)

# If yesterday_rainfall is a valid number (not NA), perform calculations
if (!is.na(yesterday_rainfall)) {
  output_row$value        <- round(yesterday_rainfall * 25.4, 2) # Convert inch to mm (e.g., 0.2 -> 5.08)
  output_row$completeness <- 1                                   # Complete daily record
}

output_row

#write csv
output_file <- paste0(outDir,"/as_nws_", gsub("-", "_", currentDate), ".csv")
write.csv(output_row, output_file, row.names = FALSE)
message("Daily summary with metadata saved to: ", output_file)


#end  