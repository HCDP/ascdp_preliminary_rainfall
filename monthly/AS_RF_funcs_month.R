library(raster)
library(sf)
library(gstat)
library(dplyr)
library(readr)

# Explicitly ensure raster extract is used to prevent masking
extract <- raster::extract

# ---------------------------------------------------------------------------
# HELPER 1: Prepare Spatial Data, Anomaly Calculations, & Dynamic Weights
# ---------------------------------------------------------------------------
statInput <- function(rfSta, meanRFday, weight_lookup = NULL) {
  rfSta <- rfSta[!is.na(rfSta$total_rf_mm), ]
  if (nrow(rfSta) < 2) return(NULL)
  
  if ("is_gapfilled" %in% names(rfSta) && !"gapfilled" %in% names(rfSta)) {
    rfSta$gapfilled <- rfSta$is_gapfilled
  }
  
  rfSta_sf <- st_as_sf(rfSta, coords = c("LON", "LAT"), crs = 4326)
  
  # Join station weights
  if ("station_name" %in% names(rfSta_sf) && !is.null(weight_lookup)) {
    rfSta_sf <- rfSta_sf %>% left_join(weight_lookup, by = "station_name")
  } else if ("Station.Name" %in% names(rfSta_sf) && !is.null(weight_lookup)) {
    rfSta_sf <- rfSta_sf %>% left_join(weight_lookup, by = c("Station.Name" = "station_name"))
  }
  
  # Extract PRISM mean in batch
  coords <- st_coordinates(rfSta_sf)
  extracted_mean <- raster::extract(meanRFday, coords)
  
  # Coastal boundary fallback: 150m/250m buffer search for 92m resolution
  na_idx <- which(is.na(extracted_mean) | extracted_mean <= 0)
  if (length(na_idx) > 0) {
    for (idx in na_idx) {
      buf_val <- raster::extract(meanRFday, rfSta_sf[idx, ], buffer = 150, fun = mean, na.rm = TRUE)
      
      if (is.na(buf_val) || buf_val <= 0) {
        buf_val <- raster::extract(meanRFday, rfSta_sf[idx, ], buffer = 250, fun = mean, na.rm = TRUE)
      }
      
      extracted_mean[idx] <- if (!is.na(buf_val) && buf_val > 0) buf_val else 0.01
    }
  }
  
  # Dynamically pull weight column safely
  w_gap_vec <- if ("w_gapfill_best" %in% names(rfSta_sf)) {
    rfSta_sf$w_gapfill_best
  } else if ("w_gapfill_top3" %in% names(rfSta_sf)) {
    rfSta_sf$w_gapfill_top3
  } else {
    rep(0.36, nrow(rfSta_sf))
  }
  
  # Compute dynamic weights & spatial transformations
  rfSta_sf <- rfSta_sf %>%
    mutate(
      PRISM_mean    = extracted_mean,
      station_w_gap = coalesce(w_gap_vec, 0.36),
      obs_weight    = if_else(gapfilled == 0, 1.0, station_w_gap),
      total_rf_mm   = total_rf_mm,
      rf_log1       = log1p(total_rf_mm),
      rf1_Anom      = (total_rf_mm + 1) / PRISM_mean,
      rf1_AnomLog   = log1p(rf1_Anom)
    )
  
  return(rfSta_sf)
}

# ---------------------------------------------------------------------------
# HELPER 2: Fast LOOCV Evaluation (NO RASTER PREDICTION)
# ---------------------------------------------------------------------------
evaluate_idw_candidate <- function(sta_sf, trans_var, idp_val) {
  
  n_pts <- nrow(sta_sf)
  loocv_preds <- numeric(n_pts)
  obs_raw <- sta_sf$total_rf_mm
  weights_vec <- sta_sf$obs_weight
  prism_vals <- sta_sf$PRISM_mean
  
  formula_str <- as.formula(paste(trans_var, "~ 1"))
  
  # Leave-One-Out Cross-Validation Loop
  for (i in 1:n_pts) {
    train_pts <- sta_sf[-i, ]
    test_pt   <- sta_sf[i, ]
    train_wts <- weights_vec[-i]
    
    idw_mod <- gstat(
      formula   = formula_str,
      locations = train_pts,
      weights   = train_wts,
      nmax      = nrow(train_pts),
      set       = list(idp = idp_val)
    )
    
    pred_val <- predict(idw_mod, newdata = test_pt, debug.level = 0)$var1.pred
    
    # Fast vectorized back-transformations
    pred_raw <- switch(trans_var,
                       "total_rf_mm" = pred_val,
                       "rf_log1"     = expm1(pred_val),
                       "rf1_Anom"    = (pred_val * prism_vals[i]) - 1,
                       "rf1_AnomLog" = (expm1(pred_val) * prism_vals[i]) - 1
    )
    
    loocv_preds[i] <- max(0, pred_raw)
  }
  
  # Compute Metrics
  errors   <- loocv_preds - obs_raw
  rmse_val <- sqrt(mean(errors^2, na.rm = TRUE))
  mae_val  <- mean(abs(errors), na.rm = TRUE)
  bias_val <- mean(errors, na.rm = TRUE)
  rsq_val  <- if (sd(loocv_preds) == 0 || sd(obs_raw) == 0) 0 else cor(loocv_preds, obs_raw)^2
  
  return(list(
    rmse = round(rmse_val, 5),
    mae  = round(mae_val, 5),
    bias = round(bias_val, 5),
    rsq  = round(rsq_val, 5)
  ))
}

# ---------------------------------------------------------------------------
# HELPER 3: Fit & Predict Surface Raster (CALLED ONLY ONCE FOR WINNING MODEL)
# ---------------------------------------------------------------------------
fit_and_predict_surface <- function(sta_sf, trans_var, idp_val, mask, meanRFday, grid_sp) {
  full_idw_mod <- gstat(
    formula   = as.formula(paste(trans_var, "~ 1")),
    locations = sta_sf,
    weights   = sta_sf$obs_weight,
    nmax      = nrow(sta_sf),
    set       = list(idp = idp_val)
  )
  
  pred_raster_raw <- predict(full_idw_mod, newdata = grid_sp, debug.level = 0)
  pred_raster     <- raster(pred_raster_raw, layer = "var1.pred")
  
  # Back-transform Full Surface Raster
  if (trans_var == "total_rf_mm") {
    final_raster <- pred_raster
  } else if (trans_var == "rf_log1") {
    final_raster <- expm1(pred_raster)
  } else if (trans_var == "rf1_Anom") {
    final_raster <- (pred_raster * meanRFday) - 1
  } else if (trans_var == "rf1_AnomLog") {
    final_raster <- (expm1(pred_raster) * meanRFday) - 1
  }
  
  final_raster[final_raster < 0] <- 0
  final_raster <- mask(final_raster, mask)
  return(final_raster)
}

# ---------------------------------------------------------------------------
# MAIN FUNCTION: Optimized IDW RF Mapping with Dynamic Fallback Rules
# ---------------------------------------------------------------------------
bestIDWrfFun <- function(rfSta, mask, date, meanRFday, weight_lookup = NULL, grid_sp = NULL) {
  
  rfSta_sf <- statInput(rfSta = rfSta, meanRFday = meanRFday, weight_lookup = weight_lookup)
  if (is.null(rfSta_sf) || nrow(rfSta_sf) < 2) return(NULL)
  
  n_pts_total <- nrow(rfSta_sf)
  r_ext       <- extent(mask)
  
  # Pre-convert spatial grid if not passed in
  if (is.null(grid_sp)) {
    grid_sp <- as(mask, "SpatialPixelsDataFrame")
    names(grid_sp) <- "PRISM_mean"
  }
  
  # Extract observed (non-gapfilled) and candidate (gapfilled) points
  obs_sf <- rfSta_sf %>% filter(gapfilled == 0)
  gap_sf <- rfSta_sf %>% filter(gapfilled == 1)
  
  n_obs <- nrow(obs_sf)
  n_gap <- nrow(gap_sf)
  
  # Dynamic fallback strategy to guarantee N >= 3 for LOOCV where possible
  if (n_obs >= 3) {
    eval_sf    <- obs_sf
    loocv_mode <- "non-gapfilled only (N>=3)"
    
  } else if (n_obs == 2) {
    if (n_gap >= 1) {
      target_val <- mean(obs_sf$total_rf_mm, na.rm = TRUE)
      
      dist_matrix <- st_distance(gap_sf, obs_sf)
      gap_sf$mean_dist <- apply(dist_matrix, 1, mean)
      
      selected_gap <- gap_sf %>%
        mutate(diff_from_target = abs(total_rf_mm - target_val)) %>%
        arrange(diff_from_target, mean_dist) %>%
        slice(1)
      
      eval_sf    <- bind_rows(obs_sf, selected_gap)
      loocv_mode <- "2 non-gapfilled + 1 closest gapfilled (val dist tie-break)"
    } else {
      eval_sf    <- obs_sf
      loocv_mode <- "2 non-gapfilled only (insufficient gapfilled)"
    }
    
  } else if (n_obs == 1) {
    if (n_gap >= 2) {
      target_val <- obs_sf$total_rf_mm[1]
      
      dist_vec <- st_distance(gap_sf, obs_sf)
      gap_sf$mean_dist <- as.numeric(dist_vec)
      
      selected_gap <- gap_sf %>%
        mutate(diff_from_target = abs(total_rf_mm - target_val)) %>%
        arrange(diff_from_target, mean_dist) %>%
        slice(1:2)
      
      eval_sf    <- bind_rows(obs_sf, selected_gap)
      loocv_mode <- "1 non-gapfilled + 2 closest gapfilled (val dist tie-break)"
    } else {
      eval_sf    <- bind_rows(obs_sf, gap_sf)
      loocv_mode <- "1 non-gapfilled + all available gapfilled"
    }
    
  } else {
    eval_sf    <- rfSta_sf
    loocv_mode <- "all stations included (fallback)"
  }
  
  n_eval <- nrow(eval_sf)
  
  # Optimization search loop across candidate transformations & IDP values
  if (n_eval >= 3) {
    transformations <- c("total_rf_mm", "rf_log1", "rf1_Anom", "rf1_AnomLog")
    idp_candidates  <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
    
    best_adjusted_rmse <- Inf
    best_run           <- NULL
    
    for (trans in transformations) {
      for (p_val in idp_candidates) {
        
        res <- evaluate_idw_candidate(
          sta_sf    = eval_sf,
          trans_var = trans,
          idp_val   = p_val
        )
        
        if (!is.na(res$rmse)) {
          
          # Apply a 20% RMSE penalty to non-PRISM methods
          adjusted_rmse <- res$rmse
          if (trans %in% c("total_rf_mm", "rf_log1")) {
            adjusted_rmse <- adjusted_rmse * 1.20 
          }
          
          if (adjusted_rmse < best_adjusted_rmse) {
            best_adjusted_rmse <- adjusted_rmse
            
            best_run <- list(
              method      = trans,
              optimal_idp = p_val,
              rmse        = res$rmse, # Store the REAL (unpenalized) RMSE for the log
              mae         = res$mae,
              bias        = res$bias,
              rsq         = res$rsq
            )
          }
        }
      }
    }
  } else {
    # Safe fallback when total station sample < 3
    best_run <- list(
      method      = "total_rf_mm",
      optimal_idp = 2.0,
      rmse        = NA_real_,
      mae         = NA_real_,
      bias        = NA_real_,
      rsq         = NA_real_
    )
  }
  
  # Predict surface EXACTLY ONCE for winning model using the RULE-FILTERED stations
  final_raster <- fit_and_predict_surface(
    sta_sf    = eval_sf,      
    trans_var = best_run$method,
    idp_val   = best_run$optimal_idp,
    mask      = mask,
    meanRFday = meanRFday,
    grid_sp   = grid_sp
  )
  
  metadata <- data.frame(
    var = c(
      "date", "runDate", "loocv_mode", 
      "xmin", "xmax", "ymin", "ymax", 
      "method", "optimal_idp", "rmse", "bias", "mae", "rsq", 
      "nStations", "loocv_n_stations"
    ),
    value = c(
      format(as.Date(date), "%Y-%m-%d"),
      format(Sys.Date(), "%Y-%m-%d"),
      loocv_mode,
      r_ext@xmin, r_ext@xmax, r_ext@ymin, r_ext@ymax,
      best_run$method,
      best_run$optimal_idp,
      best_run$rmse,
      best_run$bias,
      best_run$mae,
      best_run$rsq,
      n_pts_total,
      n_eval
    ),
    stringsAsFactors = FALSE
  )
  
  return(list(best_idw_rf = final_raster, metadata = metadata))
}