### Make daily RF maps from climatologically-aided interpolation using IDW and 
### optimized through LOOCV. Requires AS_RF_funcs.R functions code.

#load packages
library(raster)
library(sf)
library(dplyr)
library(gstat)
library(Metrics)

rm(list = ls())#remove all objects in R

#set dirs
codeDir<-Sys.getenv("CODE_DIR")
outputDir<-Sys.getenv("OUTPUT_DIR")
dependencyDir<-Sys.getenv("DEPENDENCY_DIR")

inDir <- paste0(outputDir,"/as_gapfilled_data")
source(paste0(codeDir,"/AS_RF_funcs.R")) # calls functions code

# Create output directories if they don't exist
outDirs <- c(paste0(outputDir, "/as_idw_rf_ras_NRT"), paste0(outputDir,"/as_idw_rf_meta_NRT"), paste0(outputDir,"/as_idw_rf_table_NRT"), paste0(outputDir,"/as_idw_rf_maps_NRT"))
for(outDir in outDirs) {
  if (!dir.exists(outDir)) 
    dir.create(outDir)
}

# Load static data
ASmask <- raster(paste0(dependencyDir,"/as_mask3.tif"))
AScoast <- st_read(paste0(dependencyDir,"/as_coastline.shp"))
temp <- read.csv(paste0(dependencyDir,"/as_rf_idw_input_template.csv"))

# List all CSV files for each day
csv_files <- list.files(inDir, pattern = "\\.csv$", full.names = TRUE)

### Work this to start right before loop and end after
s<-Sys.time()

csv_file <- csv_files[1]

#define date
source(paste0(codeDir,"/as_dataDateFunc.R"))
date<-dataDateMkr() #function for importing/defining date as input or as yesterday
date_str <- format(as.Date(date), "%Y%m%d")

# Load PRISM raster for corresponding month
month <- format(date, "%m")
ASmeanRFday <- raster(paste0(dependencyDir,"/as_prism_monthday/daily_", month, "_mm.tif"))

# Load daily rainfall station data
rfSta <- read.csv(csv_file)

# Conditional skips interpolation if there's less than 2 stations available
if (sum(!is.na(rfSta$total_rf_mm)) < 2) {
  warning("Not enough data points to run IDW")
  # return(NULL)  # or return a dummy raster
  best_idw_rf <- NULL  # produce no map 
  # Create metadata explaining fallback
  metadata <- data.frame(
    variable = "interpolation_status",
    value = "Not enough data points to run IDW. No map produced.",
    stringsAsFactors = FALSE
  )
  
} else {
  testOut <- bestIDWrfFun(rfSta = rfSta,
                          mask = ASmask,
                          date = date,
                          meanRFday = ASmeanRFday)

# Extract outputs
best_idw_rf <- testOut[["best_idw_rf"]]
plot(best_idw_rf)
metadata <- testOut[["metadata"]]
metadata$value <- as.character(metadata$value)
}

if (!is.null(best_idw_rf)) {
  # Set raster background value to -9999
  best_idw_rf[is.na(best_idw_rf[])] <- -9999
  
  # Save raster
  raster_outfile <- paste0(outDirs[1], "/as_idw_", date_str, ".tif")
  writeRaster(best_idw_rf, raster_outfile, NAflag = -9999, overwrite = TRUE)

  message("RASTER_OUTFILE=", raster_outfile)
  
  # Save metadata as a tab-separated text file
  meta_outfile <- paste0(outDirs[2],"/as_idw_meta_", date_str, ".txt")
  write.table(metadata, meta_outfile, sep = "\t", row.names = FALSE, quote = FALSE)
  
  # --- START OF MONTHLY TABLE PROCESSING ---
  # 1. Define monthly file name
  month_file <- paste0(outDirs[3], "/daily_rainfall_station_AS_", 
                       format(date, "%Y_%m"), ".csv")
  
  # 2. Prepare the new day's data
  col_name <- paste0("X", format(date, "%Y.%m.%d"))
  new_day_data <- rfSta %>%
    dplyr::select(SKN, total_rf_mm) %>%
    mutate(SKN = as.character(SKN)) %>%
    dplyr::rename(!!col_name := total_rf_mm)
    
  # 3. Handle the Monthly File (Join to existing or start clean)
  if (file.exists(month_file)) {
    existing_month <- read.csv(month_file, check.names = FALSE, stringsAsFactors = FALSE)
    
    # Force SKN to character to match new_day_data
    existing_month$SKN <- as.character(existing_month$SKN)
    
    # Hard deduplication: remove TODAY's column if it already exists
    if (col_name %in% names(existing_month)) {
      existing_month <- existing_month[, names(existing_month) != col_name, drop = FALSE]
    }
    
    # DIAGNOSTIC LOG: Print existing columns before join
    message("Existing file found with ", ncol(existing_month), " columns.")
    
    # JOIN
    updated_month <- left_join(existing_month, new_day_data, by = "SKN")
    message("Appending data for: ", col_name)
    
  } else {
    # No file exists: Start clean from template
    temp_clean <- temp %>% 
      mutate(SKN = as.character(SKN)) %>%
      dplyr::select(-matches("total_rf_mm|X[0-9]{4}|x|y|RF_Mean_Extract"))
    
    updated_month <- left_join(temp_clean, new_day_data, by = "SKN")
    message("No existing file found. Created new monthly file from template.")
  }

  # 4. Final Formatting: Remove unwanted columns, sort, and force NA values
  updated_month <- updated_month %>% 
    dplyr::select(-any_of(c("x", "y", "RF_Mean_Extract")))
  
  date_cols <- grep("^X[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}$", names(updated_month), value = TRUE)
  static_cols <- setdiff(names(updated_month), date_cols)
  sorted_dates <- date_cols[order(as.Date(gsub("^X", "", date_cols), format = "%Y.%m.%d"))]
  
  final_table <- updated_month[, c(static_cols, sorted_dates)]
  
  # Final NA force: Handles empty strings and logical NAs
  final_table[final_table == ""] <- NA
  
  # 5. Save with explicit NA strings
  write.csv(final_table, month_file, row.names = FALSE, na = "NA")
  message("Monthly file update complete. Saved columns: ", ncol(final_table))
  # --- END OF MONTHLY TABLE PROCESSING ---
  
  # Prepare plot
  png(filename = paste0(outDirs[4],"/as_idw_map_", date_str, ".png")
      , width = 600, height = 400
      )
  # par(mar = c(4, 4, 4, 6))  # Give some room for subtext
  par(mar = c(5, 4, 4, 2))  # bottom margin slightly larger for subtext
  
  rfSta_non_na <- rfSta[!is.na(rfSta$total_rf_mm), ]
  subtext <- paste(paste(metadata$var[7:12], metadata$value[7:12], sep=": "), collapse = "; ")
  
  plot(best_idw_rf, col=rainbow(100, end=0.8), 
       main = paste(date, "IDW"),
       zlim = c(0, 200))
  
  # Add subtext under the plot using mtext
  mtext(subtext, side = 1, line = 4, cex = 0.8)  # side = 1 = bottom, line = 4 pushes it down
  
  # Overlay coast and points
  plot(AScoast, col=NA, border="black", add=TRUE)
  points(rfSta_non_na$LON, rfSta_non_na$LAT, col = "black", pch = 16)
  
  # Add station names + rainfall
  labels <- paste(rfSta_non_na$Station.Name, round(rfSta_non_na$total_rf_mm, 2), sep = "\n")
  if(sum(!is.na(rfSta$total_rf_mm)) > 0){
    text(rfSta_non_na$LON, rfSta_non_na$LAT, labels = labels, pos = 4, cex = 0.8)
  }
  
  dev.off()
  
  message("Processed and saved: ", date_str)
}

# finish day map
e<-Sys.time()
write(difftime(e,s,units="hours"),"tt.txt")



