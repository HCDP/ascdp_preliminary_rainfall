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
  
  # Append to monthly data file for map-viewer display
    # Define monthly file name (based on year + month)
    month_file <- paste0(outDirs[3], "/daily_rainfall_station_AS_",
                         format(date, "%Y_%m"), ".csv")

    # Column name from date
    col_name <- paste0("X", format(date, "%Y.%m.%d"))

    # Join daily data into template
    temp_out <- temp
    temp_out$total_rf_mm <- rfSta$total_rf_mm[match(temp_out$SKN, rfSta$SKN)]
    names(temp_out)[names(temp_out) == "total_rf_mm"] <- col_name
    temp_out <- temp_out[, -((ncol(temp_out)-3):ncol(temp_out))]  # remove trailing metadata columns
  
    # If monthly file already exists, update it
    if (file.exists(month_file)) {
      existing <- read.csv(month_file, check.names = FALSE)

      # If column for this date already exists, drop it first (to overwrite cleanly)
      if (col_name %in% names(existing)) {
        existing[[col_name]] <- NULL
      }

      # Merge new data (keeps order by SKN)
      combined <- merge(existing, temp_out[, c("SKN", col_name)], by = "SKN", all.x = TRUE)

      # Identify columns in format XYYYY.MM.DD
      date_cols <- grep("^X[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}$", names(combined), value = TRUE)
      static_cols <- setdiff(names(combined), date_cols)
      
      # Convert to Date for proper sorting
      date_values <- as.Date(gsub("^X", "", date_cols), format = "%Y.%m.%d")
      
      # Sort by date
      sorted_dates <- date_cols[order(date_values)]
      
      # Recombine static and sorted date columns
      combined <- combined[, c(static_cols, sorted_dates)]

      # Overwrite file with updated data
      write.csv(combined, month_file, row.names = FALSE)
      message("Updated existing monthly file (", col_name, " overwritten or added): ", month_file)
      
    } else {
      # If file doesn't exist yet, create new one
      write.csv(temp_out, month_file, row.names = FALSE)
      message("Created new monthly file: ", month_file)
    }
    
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



