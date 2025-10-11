#load packages
library(dplyr)
library(matrixStats)
library(geosphere)
library(tidyr)

rm(list = ls())

#set dirs
mainDir<-Sys.getenv("PROJECT_ROOT")
inDir<-paste0(mainDir,"/as_static_files/as_gapfill_correlation_inputs")
inDir2<-paste0(mainDir,"/as_gapfill_input")
outDir<-paste0(mainDir,"/as_gapfilled_data")

#ensure empty output dir
unlink(file.path(outDir, "*"), recursive = TRUE)

#Path to Input Files 
Files =list.files(inDir) #Gets the names of all the files in your WorkingFolder
nFiles=length(Files) #Counts the number of files to be processed

#define date
source(paste0(mainDir,"/as_dataDateFunc.R"))
dataDate<-dataDateMkr() #function for importing/defining date as input or as yesterday
currentDate<-dataDate #dataDate as currentDate
file_date<-format(as.Date(currentDate), "%Y%m%d")

#Daily data for gapfilling
#List CSV files in the folder
csv_file <- list.files(inDir2, pattern = "\\.csv$", full.names = TRUE)

#read csv
Daily_RF<-read.csv(csv_file)
colnames(Daily_RF)

# Remove the first column if it's named "X"
if ("X" %in% names(Daily_RF)) {
  Daily_RF <- Daily_RF[ , !(names(Daily_RF) %in% "X")]
}

# Identify and rename date columns
date_columns <- grep("^X\\d{2}\\.\\d{2}\\.\\d{4}$", names(Daily_RF), value = TRUE)
new_date_columns <- gsub("^X", "", date_columns)
names(Daily_RF)[names(Daily_RF) %in% date_columns] <- new_date_columns

# Get all column names
all_colnames <- colnames(Daily_RF)

# Convert only the ones from 14 onward to Date format
all_colnames[14:length(all_colnames)] <- as.character(as.Date(all_colnames[14:length(all_colnames)], format = "%m.%d.%Y"))

# Assign back
colnames(Daily_RF) <- all_colnames

# Get the number of stations
N_Sta <- nrow(Daily_RF)
    
# Set up Gapfilled output   
# should be a mirror of the Input file    
RF_Filled <- Daily_RF

# Cbind SKN and daily RF
D<-ncol(Daily_RF)
RF_DAY <- cbind(Daily_RF[1],Daily_RF[D])
RF_DAY
colnames(RF_DAY)[1] <- "Name"
      
      # Station Loop 
      for (S in 1:N_Sta) {

        print(S)
 
        # Get SKN for the TARGET STAION
        TARG_SKN<-Daily_RF[S,1]  

        # Get Value to be filled     
        TARG_Value<-Daily_RF[S,D]

        # If Target Value is missing (NA) then proceed
        if(is.na(TARG_Value)) { 

            # Read in input file 
            StaInfo <- read.csv(paste0(inDir,"/Name.",TARG_SKN,".Input.csv" ))

            # Merge Input data and Rainfall Data 
            INFO_RF <- merge(x = StaInfo,y = RF_DAY,by = "Name")

            # Organize the input by Spearmann Rank correlation 
            NR_Input1 <- INFO_RF[order(-INFO_RF$Spear),] #Organize by strongest correlation
            
            # Sort By NA for rainfall  
            NR_Input2 <- NR_Input1[order(is.na(NR_Input1[12])),]
            
            # Multiply Ratio by RF Day 
            NR_RAT <- NR_Input2[8] * NR_Input2[12]
            
            # Add to DF
            NR_Input2[13]<-NR_RAT
        
          if(!is.na(NR_Input2[1,13])) {
            # Predicted rainfll is the average of predictions at 3 predictor stations
            NR_PRED  <- mean(NR_Input2[1:3,13],na.rm=TRUE) 
            
            # Add precautionary measure to avoid artificial RF
            # If the highest correlated station is zero RF and st target station to zero  
            if(NR_Input2[1,12] < 0.15){NR_RF <- 0}
            
            # Add NR predicted value to Daily time series
            RF_Filled[S,D] <- NR_PRED 
            
          } #end NR Fill Statement
        } #end missing RF value statement
      } #end station loop

    #wide to long format
    RF_Filled2<-RF_Filled[c(1,ncol(RF_Filled))] #remove meta columns
    RF_long<-gather(RF_Filled2, key="date", value="precip_mm", -station_name) #wide to long

    #format for interpolation input
      #read in template
      temp<-read.csv(paste0(mainDir,"/as_static_files/as_rf_idw_input_template.csv"))
      
      #add rf values in
      temp$total_rf_mm <- RF_long$precip_mm[match(temp$Station.Name, RF_long$station_name)]
      
    #write csv
    output_file <- paste0(outDir,"/",file_date, "_as_rf_idw_input_gapfilled.csv")
    write_csv(temp, output_file)
    cat("File saved to:", output_file, "\n")
