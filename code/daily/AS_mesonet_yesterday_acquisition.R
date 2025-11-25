###### American Samoa Rainfall Data Acquisition - UH Mesonet

rm(list = ls())  # Remove all objects
lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)
gc()  # Run garbage collection to clean memory

#load packages
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

#set dirs
mainDir <- Sys.getenv("PROJECT_ROOT")
outDir<-paste0(mainDir,"/as_individual_data")

# create an empty output directory
if (dir.exists(outDir)) {
  files_to_delete <- list.files(outDir, full.names = TRUE)
  file.remove(files_to_delete)
} else {
  dir.create(outDir, recursive = TRUE)
}

#set API
auth_token <- Sys.getenv("HCDP_API_TOKEN")
station_ids <- c("1311", "1312", "1313", "1316", "1319")
var_id <- "RF_1_Tot300s"
tz_local <- "Pacific/Pago_Pago"

#define date
source(paste0(mainDir,"/as_dataDateFunc.R"))
dataDate<-dataDateMkr() #function for importing/defining date as input or as yesterday
currentDate<-dataDate #dataDate as currentDate

#set expected daily data points and threshold
expected <- 288
threshold <- ceiling(expected * 0.95)

#pull station metadata
resp_meta <- GET(
  "https://api.hcdp.ikewai.org/mesonet/db/stations",
  add_headers(Authorization = auth_token),
  query = list(location = "american_samoa")
)

if (status_code(resp_meta) == 200) {
  stations <- fromJSON(content(resp_meta, as = "text", encoding = "UTF-8"))
  # Filter only the stations of interest
  station_meta <- stations %>%
    filter(as.character(station_id) %in% station_ids) %>%
    mutate(
      station_id = as.character(station_id),
      SKN = case_when(
        station_id == "1311" ~ 1021,
        station_id == "1312" ~ 1001,
        station_id == "1313" ~ 1026,
        station_id == "1316" ~ 1004,
        station_id == "1319" ~ 1010
      ),
      Station.Name = case_when(
        station_id == "1311" ~ "poloa_UH",
        station_id == "1312" ~ "aasu_UH",
        station_id == "1313" ~ "vaipito_UH",
        station_id == "1316" ~ "afono_UH",
        station_id == "1319" ~ "aunuu_UH"
      ),
      Elev.m = elevation,
      LAT   = lat,
      LON   = lng
    ) %>%
    select(station_id, SKN, Station.Name, Elev.m, LAT, LON)
  
} else {
  stop("Failed to pull station metadata: ", status_code(resp_meta))
}

#pull yesterday's data for each station
all_daily_summary <- list()

for (station in station_ids) {
  start_utc <- with_tz(as.POSIXct(currentDate, tz = tz_local), tzone = "UTC")
  end_utc   <- with_tz(as.POSIXct(currentDate + 1, tz = tz_local) - 300, tzone = "UTC")
  
  resp <- GET(
    "https://api.hcdp.ikewai.org/mesonet/db/measurements",
    add_headers(Authorization = auth_token),
    query = list(
      station_ids = station,
      var_ids     = var_id,
      start_date  = format(start_utc, "%Y-%m-%dT%H:%M:%S"),
      end_date    = format(end_utc, "%Y-%m-%dT%H:%M:%S"),
      local_tz    = "True",
      location    = "american_samoa",
      row_mode    = "json"
    )
  )
  
  if (status_code(resp) == 200) {
    data_raw <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
    if (length(data_raw) > 0 && "timestamp" %in% names(data_raw)) {
      data_tbl <- as_tibble(data_raw) %>%
        mutate(
          timestamp = ymd_hms(timestamp, tz = tz_local),
          value = as.numeric(value),
          date = as.Date(timestamp)
        ) %>%
        distinct(timestamp, .keep_all = TRUE) %>%
        mutate(station_id = station)
      
      daily_summary <- data_tbl %>%
        group_by(station_id, date) %>%
        summarise(
          n_subdaily = n(),
          daily_total = sum(value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          value = ifelse(n_subdaily >= threshold, daily_total, NA),
          variable = "precip_mm",
          completeness = n_subdaily / expected
        ) %>%
        select(station_id, date, value, variable, completeness)
      
      all_daily_summary[[station]] <- daily_summary
    } else {
      warning("No data for station ", station)
    }
  } else {
    warning("Failed for station ", station, ": ", status_code(resp))
  }
}

#combine all stations and join metadata
all_daily_summary <- bind_rows(all_daily_summary) %>%
  left_join(station_meta, by = "station_id") %>%
  mutate(Observer = "UH") %>%
  select(station_id, SKN, Station.Name, Observer, Elev.m, LAT, LON, date, value, variable, completeness) %>%
  arrange(station_id, date)

all_daily_summary

#write csv
output_file <- paste0(outDir,"/as_mesonet_", gsub("-", "_", currentDate), ".csv")
write.csv(all_daily_summary, output_file, row.names = FALSE)
message("Daily summary with metadata saved to: ", output_file)


#end

