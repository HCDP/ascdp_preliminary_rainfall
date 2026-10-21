#### Monthly IDW Near-Realtime (NRT) Code ####

library(raster)
library(sf)
library(dplyr)
library(gstat)
library(readr)
library(lubridate)

# Explicitly ensure raster extract is used to prevent masking
extract <- raster::extract

# Clear workspace
rm(list = ls())

# ==============================================================================
# ENVIRONMENT & DIRECTORY SETUP
# ==============================================================================
codeDir       <- Sys.getenv("CODE_DIRm")
outputDir     <- Sys.getenv("OUTPUT_DIRm")
dependencyDir <- Sys.getenv("DEPENDENCY_DIRm")

# Source supporting functions
source(file.path(codeDir, "AS_RF_funcs_month.R"))
source(file.path(codeDir, "as_dataDateFunc_month.R"))

# Output directories for NRT Monthly products
ras_dir   <- file.path(outputDir, "as_idw_rf_month_ras_NRT")
meta_dir  <- file.path(outputDir, "as_idw_rf_month_meta_NRT")
maps_dir  <- file.path(outputDir, "as_idw_rf_month_maps_NRT")
table_dir <- file.path(outputDir, "as_idw_rf_month_table_NRT")

outDirs <- c(ras_dir, meta_dir, maps_dir, table_dir)
for (d in outDirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ==============================================================================
# LOAD STATIC DEPENDENCIES
# ==============================================================================
weight_lookup_file <- file.path(dependencyDir, "station_gapfill_weights_month.csv")
weight_lookup      <- read.csv(weight_lookup_file, stringsAsFactors = FALSE)

ASmask  <- raster(file.path(dependencyDir, "as_mask3.tif"))
AScoast <- st_read(file.path(dependencyDir, "as_coastline.shp"))

# Pre-convert spatial grid once for interpolation speedup
grid_sp <- as(ASmask, "SpatialPixelsDataFrame")
names(grid_sp) <- "PRISM_mean"

# Load raingage metadata for cluster ranking and table templates
d_gage       <- read.csv(file.path(dependencyDir, "as_daily_wide_template.csv"), stringsAsFactors = FALSE)
d_gage$range <- as.numeric(d_gage$range)

# ==============================================================================
# DEFINE TARGET MONTH & DOWNLOAD NRT MONTHLY TABLE FROM API
# ==============================================================================
s <- Sys.time()

# Define target monthly date (returns 1st day of previous month)
target_date <- dataDateMkr() # e.g., returns 2026-09-01 when run on 2026-10-01

year_str   <- format(target_date, "%Y")
month_str  <- format(target_date, "%m")
date_str   <- format(target_date, "%Y%m")

message("Processing Monthly NRT Rainfall for: ", format(target_date, "%B %Y"))

# Load PRISM Monthly Climatology Raster
prism_file    <- file.path(dependencyDir, paste0("PRISM_monthly2/monthly_", month_str, "_mm.tif"))
ASmeanRFmonth <- raster(prism_file)

# Dynamic API download URL for the wide partial monthly station table
month_file_name <- paste0("rainfall_day_partial_station_data_", year_str, "_", month_str, ".csv")
url <- paste0("https://api.hcdp.ikewai.org/files/download/ASCDP/production/rainfall/day/partial/station_data/",
              year_str, "/", month_str, "/", month_file_name)

temp_table_path <- file.path(tempdir(), month_file_name)

download_status <- tryCatch({
  download.file(url, destfile = temp_table_path, mode = "wb", quiet = TRUE)
  TRUE
}, error = function(e) { FALSE })

if (!download_status || !file.exists(temp_table_path)) {
  stop(paste("Failed to download monthly station data table from API:", url))
}

# Load wide-format monthly table
nrt_table <- read.csv(temp_table_path, check.names = FALSE, stringsAsFactors = FALSE)

# ==============================================================================
# AGGREGATE DAILY COLUMNS INTO MONTHLY TOTALS
# ==============================================================================
# Identify all daily columns (matching pattern XYYYY.MM.DD)
date_cols <- grep("^X[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}$", names(nrt_table), value = TRUE)

if (length(date_cols) == 0) {
  stop("No valid daily date columns found in the downloaded NRT table.")
}

# Convert daily rainfall values to numeric matrix and sum across row days
daily_mat <- as.matrix(nrt_table[, date_cols])
suppressWarnings(class(daily_mat) <- "numeric")

# Calculate total monthly rainfall per station (ignoring NAs for partial months)
rfSta <- nrt_table %>%
  mutate(
    total_rf_mm = rowSums(daily_mat, na.rm = TRUE),
    valid_days  = rowSums(!is.na(daily_mat)),
    # Force total_rf_mm to NA if station has 0 valid days recorded
    total_rf_mm = if_else(valid_days == 0, NA_real_, total_rf_mm),
    # Assume ALL NRT stations are un-gapfilled (0)
    gapfilled   = 0
  )

# Standardize station name column
if (!"station_name" %in% names(rfSta) && "Station.Name" %in% names(rfSta)) {
  rfSta$station_name <- rfSta$Station.Name
}

# Attach record length metadata for cluster priority ranking
idx <- match(trimws(rfSta$Station.Name), trimws(d_gage$Station))
rfSta$station_range <- d_gage$range[idx]

# Filter valid non-NA stations
valid_sta <- rfSta[!is.na(rfSta$total_rf_mm), ]
sta_clean <- tolower(trimws(valid_sta$Station.Name))

# Assign spatial station clusters
valid_sta$cluster_id <- dplyr::case_when(
  sta_clean %in% c("vaipito_uh", "vaipito", "vaipito_res", "vaipito2000") ~ "cluster_vaipito",
  sta_clean %in% c("aasufou90", "aasufou80", "aoloafou", "aasu_uh")       ~ "cluster_aasu",
  sta_clean %in% c("airport5101", "airport80")                           ~ "cluster_airport",
  sta_clean %in% c("aua", "pioa_afono", "afono_uh")                       ~ "cluster_afono_aua",
  sta_clean %in% c("malaeimi", "malaeimi1691")                           ~ "cluster_malaeimi",
  sta_clean %in% c("matatula", "gml_smo")                                ~ "cluster_matatula_gml",
  TRUE ~ valid_sta$Station.Name
)

# Deduplicate clusters (Priority: Longer record range)
if (nrow(valid_sta) > 0) {
  valid_sta <- valid_sta[order(-valid_sta$station_range), ]
  valid_sta <- valid_sta[!duplicated(valid_sta$cluster_id), ]
}

rfSta_proc <- valid_sta
n_total    <- nrow(rfSta_proc)
n_obs      <- sum(rfSta_proc$gapfilled == 0)

# ==============================================================================
# RUN IDW INTERPOLATION
# ==============================================================================
if (n_total < 2) {
  warning(paste("Not enough data points to run IDW for", date_str))
  
  metadata <- data.frame(
    var   = c("date", "runDate", "interpolation_status", "nStationsTotal", "nStationsObs"),
    value = c(as.character(target_date), as.character(Sys.Date()), 
              "Fewer than 2 total stations. No map produced.", 
              as.character(n_total), as.character(n_obs)),
    stringsAsFactors = FALSE
  )
  
  meta_outfile <- file.path(meta_dir, paste0("as_idw_meta_", date_str, ".txt"))
  write.table(metadata, meta_outfile, sep = "\t", row.names = FALSE, quote = FALSE)
  best_idw_rf <- NULL
  
} else {
  testOut <- bestIDWrfFun(rfSta         = rfSta_proc,
                          mask          = ASmask,
                          date          = target_date,
                          meanRFday     = ASmeanRFmonth,
                          weight_lookup = weight_lookup,
                          grid_sp       = grid_sp)
  
  best_idw_rf    <- testOut[["best_idw_rf"]]
  metadata       <- testOut[["metadata"]]
  metadata$value <- as.character(metadata$value)
}

# ==============================================================================
# EXPORT RASTER, METADATA, MAP PNG AND UPDATE YEARLY DATA TABLE
# ==============================================================================
if (!is.null(best_idw_rf)) {
  
  # 1. Save GeoTIFF Raster
  raster_outfile <- file.path(ras_dir, paste0("as_idw_", date_str, ".tif"))
  writeRaster(best_idw_rf, raster_outfile, overwrite = TRUE, NAflag = -9999)
  message("RASTER_OUTFILE=", raster_outfile)
  
  # 2. Save Metadata TXT
  meta_outfile <- file.path(meta_dir, paste0("as_idw_meta_", date_str, ".txt"))
  write.table(metadata, meta_outfile, sep = "\t", row.names = FALSE, quote = FALSE)
  
  # 3. Save Map PNG
  map_outfile <- file.path(maps_dir, paste0("as_idw_map_", date_str, ".png"))
  png(filename = map_outfile, width = 600, height = 400)
  
  par(mar = c(5, 4, 4, 2))
  
  var_col  <- if ("var" %in% names(metadata)) "var" else "variable"
  sub_vars <- c("method", "optimal_idp", "rmse", "mae", "rsq", "loocv_mode")
  sub_idx  <- metadata[[var_col]] %in% sub_vars
  subtext  <- paste(paste(metadata[[var_col]][sub_idx], metadata$value[sub_idx], sep = ": "), collapse = "; ")
  
  max_val <- max(c(1500, maxValue(best_idw_rf)), na.rm = TRUE)
  
  plot(best_idw_rf, col = rainbow(100, end = 0.8), 
       main = paste(format(target_date, "%B %Y"), "IDW Rainfall (mm)"),
       zlim = c(0, max_val))
  
  mtext(subtext, side = 1, line = 4, cex = 0.75)
  plot(AScoast, col = NA, border = "black", add = TRUE)
  
  # Render all station points as observed (black)
  points(rfSta_proc$LON, rfSta_proc$LAT, col = "black", pch = 16, lwd = 1, cex = 1.2)
  
  labels <- paste(rfSta_proc$Station.Name, round(rfSta_proc$total_rf_mm, 1), sep = "\n")
  text(rfSta_proc$LON, rfSta_proc$LAT, labels = labels, pos = 4, cex = 0.75)
  
  dev.off()
  
  # ============================================================================
  # 4. GENERATE AND UPDATE YEARLY STATION DATA TABLE (MONTHLY TOTALS)
  # ============================================================================
  currentDate <- target_date
  year_val    <- format(currentDate, "%Y")
  
  yearly_file_name <- paste0("rainfall_month_partial_station_data_", year_val, ".csv")
  local_path_year  <- file.path(table_dir, yearly_file_name)
  
  # HCDP API URL structure for month/partial station data
  url_year <- paste0("https://api.hcdp.ikewai.org/files/download/ASCDP/production/rainfall/month/partial/station_data/",
                     year_val, "/", yearly_file_name)
  
  # Monthly column naming format: XYYYY.MM
  col_name_month <- paste0("X", format(currentDate, "%Y.%m"))
  
  # Prepare new monthly column from processed station rainfall
  new_month_data <- rfSta_proc %>%
    dplyr::select(SKN, total_rf_mm) %>%
    mutate(SKN = as.character(SKN)) %>%
    dplyr::rename(!!col_name_month := total_rf_mm)
  
  # Download existing yearly file if present on server
  download_status_year <- tryCatch({
    download.file(url_year, destfile = local_path_year, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) { FALSE })
  
  if (download_status_year && file.exists(local_path_year)) {
    message("Yearly table retrieved from API: ", url_year)
    existing_year <- read.csv(local_path_year, check.names = FALSE, stringsAsFactors = FALSE)
    existing_year$SKN <- as.character(existing_year$SKN)
    
    # Remove column if updating an existing month
    if (col_name_month %in% names(existing_year)) {
      existing_year <- existing_year[, names(existing_year) != col_name_month, drop = FALSE]
    }
    
    updated_year <- left_join(existing_year, new_month_data, by = "SKN")
    
  } else {
    message("Could not retrieve existing yearly file. Initializing from metadata template.")
    
    # Fallback: Clean template table (d_gage / nrt_table)
    temp_clean <- nrt_table %>%
      mutate(SKN = as.character(SKN)) %>%
      dplyr::select(-matches("total_rf_mm|X[0-9]{4}|x|y|RF_Mean_Extract|valid_days|gapfilled|cluster_id|station_range"))
    
    updated_year <- left_join(temp_clean, new_month_data, by = "SKN")
  }
  
  # Format final column structure and sort month columns chronologically
  updated_year <- updated_year %>% 
    dplyr::select(-any_of(c("x", "y", "RF_Mean_Extract", "valid_days", "gapfilled", "cluster_id", "station_range")))
  
  month_cols    <- grep("^X[0-9]{4}\\.[0-9]{2}$", names(updated_year), value = TRUE)
  static_cols   <- setdiff(names(updated_year), month_cols)
  sorted_months <- month_cols[order(as.Date(paste0(gsub("^X", "", month_cols), ".01"), format = "%Y.%m.%d"))]
  
  final_year_table <- updated_year[, c(static_cols, sorted_months)]
  final_year_table[final_year_table == ""] <- NA
  
  # Save yearly CSV output file
  write.csv(final_year_table, local_path_year, row.names = FALSE, na = "NA")
  message("Yearly table updated and saved to: ", local_path_year)
  
  message("Monthly NRT processing complete for ", date_str)
}

e <- Sys.time()
runtime_outfile <- file.path(outputDir, paste0("tt_monthly_NRT_run.txt"))
write(difftime(e, s, units = "hours"), runtime_outfile)