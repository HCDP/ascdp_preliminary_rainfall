###### Combine NRT AS daily data from Mesonet and WRCC for use in IDW ######

#load packages
library(dplyr)
library(readr)
library(stringr)

#set dirs
codeDir<-Sys.getenv("CODE_DIR")
outputDir<-Sys.getenv("OUTPUT_DIR")
dependencyDir<-Sys.getenv("DEPENDENCY_DIR")

inDir<-paste0(outputDir,"/as_individual_data")
outDir<-paste0(outputDir,"/as_combined_data")
outDir2<-paste0(outputDir,"/as_gapfill_input")
outDir3<-paste0(outputDir,"/as_qc_logs")

#ensure empty output dirs
if (dir.exists(outDir)) {
  file.remove(list.files(outDir, full.names = TRUE))
} else {
  dir.create(outDir, recursive = TRUE)
}

if (dir.exists(outDir2)) {
  file.remove(list.files(outDir2, full.names = TRUE))
} else {
  dir.create(outDir2, recursive = TRUE)
}

# QC log output folder
if (!dir.exists(outDir3)) {
  dir.create(outDir3, recursive = TRUE)
}

#list csvs in input folder
files <- list.files(
  inDir, 
  pattern = "\\.csv$", 
  full.names = TRUE
)

#define date
source(paste0(codeDir,"/as_dataDateFunc.R"))
dataDate<-dataDateMkr() #function for importing/defining date as input or as yesterday
file_date <-dataDate #dataDate as currentDate

# USE DATE FUNCTION
#convert date to YYYYMMDD format
file_date_fmt <- gsub("-", "", file_date)
file_date_fmt
#read csvs as characters to avoid type issues
meso_list <- lapply(files, function(f) {
  read_csv(f, col_types = cols(.default = col_character()))
})

#combine csvs
meso_combined <- bind_rows(meso_list)

#convert columns to numeric
meso_combined <- meso_combined %>%
  mutate(
    SKN = as.numeric(SKN),
    Elev.m = as.numeric(Elev.m),
    LAT = as.numeric(LAT),
    LON = as.numeric(LON),
    value = as.numeric(value),
    completeness = as.numeric(completeness)
  )

#reformat
meso_goal <- meso_combined %>%
  transmute(
    SKN = SKN,
    Station.Name = Station.Name,
    Observer = Observer,
    Network = NA_character_,
    Island = NA_character_,
    ELEV.m. = Elev.m,
    LAT = LAT,
    LON = LON,
    NCEI.id = NA_character_,
    NWS.id = NA_character_,
    NESDIS.id = NA_character_,
    SCAN.id = NA_character_,
    SMART_NODE_RF.id = NA_character_,
    total_rf_mm = value,
    x = NA_real_,
    y = NA_real_,
    RF_Mean_Extract = NA_real_,
    total_rf_mm_logC = NA_real_
  )

###Identify and screen for unlikely rainfall values 
  # Add QC flags
  meso_goal <- meso_goal %>%
    mutate(
      qc_flag = case_when(
        total_rf_mm < 0 ~ "NEGATIVE",
        total_rf_mm > 800 ~ "OVER_800MM",
        total_rf_mm > 500 ~ "OVER_500MM",
        TRUE ~ "OK"
      )
  )

  # Extract flagged rows (anything not OK)
  qc_log <- meso_goal %>%
    filter(qc_flag != "OK") %>%
    mutate(date = file_date) %>%
    select(date, SKN, Station.Name, total_rf_mm, qc_flag, LAT, LON)
  
  # Only write file if there are flagged rows
  if (nrow(qc_log) > 0) {
    qc_log_file <- paste0(outDir3, "/as_qc_flagged_", file_date_fmt, ".csv")
    write.csv(qc_log, qc_log_file, row.names = FALSE)
    message("QC log written: ", qc_log_file)
  } else {
    message("No QC issues found for ", file_date)
  }
  
# Replace negative and over 800mm values with NA for interpolation
meso_goal <- meso_goal %>%
  mutate(
    total_rf_mm = ifelse(
      qc_flag %in% c("NEGATIVE", "OVER_800MM"),
      NA,
      total_rf_mm
    )
  )

#write csv
output_file <- paste0(outDir,"/",file_date_fmt, "_as_rf_idw_input.csv")
write_csv(meso_goal, output_file)
cat("Combined file saved to:", output_file, "\n")

###reformat for gapfilling input
  #read template
  temp<-read.csv(paste0(dependencyDir,"/as_daily_wide_template.csv"))

  #convert date to MM/DD/YYYY format and replace column name
  file_date_fmt2<-format(as.Date(file_date),"%m/%d/%Y")
  colnames(temp)[colnames(temp) == "MM.DD.YYYY"] <- file_date_fmt2
  
  #fill table with rf values
    #set column to fill
    date_col <- file_date_fmt2
    
    #fill
    temp[, date_col] <- meso_goal$total_rf_mm[match(temp$station_name, meso_goal$Station.Name)]

  #write csv
  output_file <- paste0(outDir2,"/",file_date_fmt, "_wide.csv")
  write_csv(temp, output_file)
  cat("File saved to:", output_file, "\n")
  
#end
