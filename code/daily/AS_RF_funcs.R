#data input
statInput<-function(rfSta,meanRFday){
  
  rfSta<-rfSta[,c("SKN","LAT","LON","total_rf_mm")]
  rfStaSP <- st_as_sf(rfSta, coords = c("LON", "LAT")) #make points
  st_crs(rfStaSP) <- 4326
  rfSta$meanRF<-extract(meanRFday,rfStaSP)

  
  rfSta<-rfSta[!is.na(rfSta$total_rf_mm),] #remove NA values
  #add other rf var cols
  rfSta$rf1_Anom<-(rfSta$total_rf_mm+1)/(rfSta$meanRF) #calc cai from daily rf
  rfSta$rf_log1<-log((rfSta$total_rf_mm+1)) #calc log rf 
  rfSta$rf1_AnomLog<-log(rfSta$rf1_Anom)#calc log cai anom rf 
  return(rfSta)
}

#LOOCV validation table func
loocvValidDF<-Fixed<-function(df){
  require(Metrics)
  #get obs and pred
  pred<-df$predRF 
  obs<-df$obsRF
  validOut<-data.frame(rmse=rmse(obs,pred),
                       bias=bias(obs,pred),
                       mae=mae(obs,pred),
                       rsq=summary(lm(obs~pred))$r.squared)
  return(validOut)
}

#back transform best idw to RF mm func
best_idw_backtrans<-function(bestMethod,best_idw_raster,meanRFday){
  require(raster)
  if(bestMethod=="total_rf_mm"){
    bestRFidw<-best_idw_raster
  }else if(bestMethod=="rf_log1"){
    bestRFidw<-exp(best_idw_raster)-1 
  }else if(bestMethod=="rf1_Anom"){
    bestRFidw<-round((best_idw_raster*meanRFday)-1,10)
  }else if(bestMethod=="rf1_AnomLog"){
    bestRFidw<-round((exp(best_idw_raster)*meanRFday)-1,10)
  }else if(max(bestMethod %in% c("rf_log1","rf1_Anom","rf1_AnomLog","total_rf_mm"))){
    message("no conversion method typr is wrong!")
    return(NULL)
  }
  bestRFidw[bestRFidw < 0] <- 0
  message(paste(bestMethod,"back transformed!"))
  return(bestRFidw)  
}

#optimized IDW with other outputs function
bestIDWrfFun<-function(rfSta,mask,date,meanRFday){
  require(sf)
  require(raster)
  require(gstat)
  
  #format and add pred vars
  rfSta<-statInput(rfSta=rfSta,meanRFday=ASmeanRFday)
  
  #make spatial SF points
  points_sf <- st_as_sf(rfSta, coords = c("LON", "LAT"))
  st_crs(points_sf) <- 4326
  
  # Create a template raster
  template_raster <- mask # Example, adjust as needed
  crs(template_raster) <- "+init=epsg:4326"  # Set CRS to WGS84
  
  # Convert sf object to sp for gstat
  points_sp <- as_Spatial(points_sf)
  
  #blank df to store loocv results
  loocvDF<-data.frame() 
  
  #loop through all days LOOCV
  for(i in 1:length(points_sp)){
    # IDW interpolation using total_rf_mm
    idw_gstat <- gstat(formula = total_rf_mm ~ 1,  # z is the variable to interpolate
                       locations = points_sp[-i,],
                       nmax =  nrow(points_sp[-i,]),       # Number of nearest neighbors to use (optional)
                       set = list(idp = 2))  # Inverse distance power (idp); default is 0
    
    idw_loo <- predict(idw_gstat, points_sp[i,])
    predRF<-round(idw_loo$var1.pred,10) #idw pred rf mm
    obsRF<-points_sp$total_rf_mm[i]
    looRow<-data.frame(predRF,obsRF,SKN=points_sp$SKN[i],method="total_rf_mm")
    loocvDF<-rbind(loocvDF,looRow)
    
    # IDW interpolation using rf1_Anom
    idw_gstat <- gstat(formula = rf1_Anom ~ 1,  # z is the variable to interpolate
                       locations = points_sp[-i,],
                       nmax =  nrow(points_sp[-i,]),       # Number of nearest neighbors to use (optional)
                       set = list(idp = 2))  # Inverse distance power (idp); default is 0
    
    idw_loo <- predict(idw_gstat, points_sp[i,])
    predRF<-round((idw_loo$var1.pred*points_sp$meanRF[i])-1,10) #back transform rf1_Anom to pred rf mm
    obsRF<-points_sp$total_rf_mm[i]
    looRow<-data.frame(predRF,obsRF,SKN=points_sp$SKN[i],method="rf1_Anom")
    loocvDF<-rbind(loocvDF,looRow)
    
    # IDW interpolation using rf_log1 
    idw_gstat <- gstat(formula = rf_log1 ~ 1,  # z is the variable to interpolate
                       locations = points_sp[-i,],
                       nmax =  nrow(points_sp[-i,]),       # Number of nearest neighbors to use (optional)
                       set = list(idp = 2))  # Inverse distance power (idp); default is 0
    
    idw_loo <- predict(idw_gstat, points_sp[i,])
    predRF<-round(exp(idw_loo$var1.pred)-1,10) #back transform rf_log1 to rf mm
    obsRF<-points_sp$total_rf_mm[i]
    looRow<-data.frame(predRF,obsRF,SKN=points_sp$SKN[i],method="rf_log1")
    loocvDF<-rbind(loocvDF,looRow)
    
    # IDW interpolation using rf1_AnomLog 
    idw_gstat <- gstat(formula = rf1_AnomLog ~ 1,  # z is the variable to interpolate
                       locations = points_sp[-i,],
                       nmax =  nrow(points_sp[-i,]),       # Number of nearest neighbors to use (optional)
                       set = list(idp = 2))  # Inverse distance power (idp); default is 0
    
    idw_loo <- predict(idw_gstat, points_sp[i,])
    predRF<-  round((exp(idw_loo$var1.pred)*rfSta$meanRF[i])-1,10) #back transform of rf1_AnomLog to pred rf mm
    obsRF<-points_sp$total_rf_mm[i]
    looRow<-data.frame(predRF,obsRF,SKN=points_sp$SKN[i],method="rf1_AnomLog")
    loocvDF<-rbind(loocvDF,looRow)
  }
  
  #make loocv validation df per method
  loocvDF$predRF[loocvDF$predRF<0]<-0 #make predicted neg RF into 0 bc thats how it will be transformed in the end
  validationDF<-t(sapply(split(loocvDF,loocvDF$method),loocvValidDF))
  validationDF<-data.frame(method=row.names(validationDF),validationDF)
  row.names(validationDF)<-NULL #erase verbose row names
  #print(validationDF) #check validation df
  
  #select best method with lowest rmse
  bestMethod<-validationDF[which.min(validationDF$rmse),"method"] #best method selected based on lowest RMSE
  bestValidation<-validationDF[validationDF$method==bestMethod,]
  bestValidation$nStations<-nrow(points_sp)
  bestloocvDF<-loocvDF[loocvDF$method==bestMethod,]
  
  #re predict rf raster using best method
  bestFormula<-formula(paste(bestMethod,"~ 1"))
  idw_gstat_best <- gstat(formula = bestFormula,  # z is the variable to interpolate
                          locations = points_sp,
                          nmax =  nrow(points_sp),       # Number of nearest neighbors to use (optional)
                          set = list(idp = 2))  # Inverse distance power (idp); default is 0
  
  best_idw_pred <- predict(idw_gstat_best, as(template_raster, "SpatialPixels"))
  best_idw_raster <- raster(best_idw_pred)# Convert the prediction to a raster
  projection(best_idw_raster) <- projection(template_raster) # Ensure same projection (redundant but safe)
  
  #back transform best method
  best_idw_rf<-best_idw_backtrans(bestMethod=bestMethod,
                                  best_idw_raster=best_idw_raster,
                                  meanRFday=ASmeanRFday)
  
  best_idw_rf <-mask(best_idw_rf, ASmask) # Matty said to add this to make sure it crops to mask

  #make best rf idw meta data
  metaValid<-t(bestValidation)
  metaValid<-data.frame(var=row.names(metaValid),val=metaValid)
  row.names(metaValid)<-NULL
  names(metaValid)[2]<-"value"
  metaValid$value[2:5]<-round(as.numeric(metaValid$value[2:5]),3)
  mapextent<-data.frame(var=c("xmin","xmax","ymin","ymax"),value=as.character(extent(template_raster)[1:4]))
  dateMeta<-data.frame(var=c("date","runDate"),value=as.character(c(date,runDate=Sys.Date())))
  metadata<-rbind(dateMeta,mapextent,metaValid)
  row.names(metadata)<-NULL
  
  #put everything in list
  outList<-list() #make a blank list to store everything
  outList[["best_idw_rf"]]<-best_idw_rf #best rf raster
  outList[["allLOOCV"]]<-loocvDF #all methods LOOCV
  outList[["allValidation"]]<-validationDF #all methods validation
  outList[["bestLOOCV"]]<-bestloocvDF #all methods LOOCV
  outList[["bestValidation"]]<-bestValidation #best only methods validation
  outList[["metadata"]]<-metadata #best idw metadata
  return(outList)
}