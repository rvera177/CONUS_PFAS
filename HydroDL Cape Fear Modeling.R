# HydroDL ML Modeling of Cape fear basin
# RV 5/29/26
# To be run in Unity Cluster

# Cape Fear watershed — get from MERIT basins already on cluster
# pfaf codes covering Cape Fear are in the 73xxx range
# You can also use the HUC8 boundary via nhdplusTools locally
# =============================================================
# HydroDL + StreamCat ML Model — Cape Fear Basin
# RV 5/29/26
# Unity Cluster
# =============================================================

library(tidyverse)
library(sf)
library(ncdf4)
library(nhdplusTools)
library(randomForest)
library(xgboost)
library(blockCV)
library(FNN)
library(zoo)
library(readr)

# =============================================================
# STEP 1: LOAD YOUR LOCAL DATA FROM GITHUB
# =============================================================

conus_data <- readRDS("conus_data.rds")

# Filter to Petre Cape Fear sites only for training
petre_data <- conus_data %>%
  filter(dataset_source == "Petre_2022") %>%
  mutate(date = as.Date(`Sample Date (MM/DD/YYY)`, 
                        format = "%m/%d/%Y"))

cat("Petre sites:", n_distinct(petre_data$COMID), "\n")
cat("Petre observations:", nrow(petre_data), "\n")

# Site coordinates with MERIT IDs
site_coords_clean <- read_csv("site_coords.csv")
# =============================================================
# STEP 2: GET CAPE FEAR WATERSHED BOUNDARY
# =============================================================

cape_fear_hucs <- c("03030002")
#only doing the Haw SubBasin for now,
#since my 13 Petre sites are all in this basin
# It's 1,707.6 sq miles, and includes Greensboro

cape_fear_wbd <- get_huc(id   = cape_fear_hucs, 
                         type = "huc08")

cape_fear_wbd_4326 <- st_transform(cape_fear_wbd, 4326)

# =============================================================
# STEP 3: LOAD MERIT RIVER NETWORK FOR CAPE FEAR
# =============================================================

cape_fear_rivers <- st_read(
  "/nas/cee-ice/data/MERIT_Basins/MERIT_Hydro_v07_Basins_v01_bugfix/pfaf_level_02/riv_pfaf_73_MERIT_Hydro_v07_Basins_v01_bugfix1.shp",
  quiet = TRUE
)

# Reproject watershed to match MERIT CRS
cape_fear_wbd_proj <- st_transform(cape_fear_wbd, 
                                   st_crs(cape_fear_rivers))

# Clip MERIT network to Cape Fear watershed
cape_fear_network <- st_intersection(cape_fear_rivers, 
                                     cape_fear_wbd_proj)

cat("Total MERIT reaches in Cape Fear:", 
    nrow(cape_fear_network), "\n")

# =============================================================
# STEP 4: BUILD NHD → MERIT CROSSWALK
# Key: Multiple NHD catchments per MERIT reach
# =============================================================

# Union the 4 HUC8s into one Cape Fear polygon
cape_fear_wbd_union <- cape_fear_wbd %>%
  st_union() %>%
  st_as_sf()

cape_fear_wbd_4326 <- st_transform(cape_fear_wbd_union, 4326)

# Get NHDPlus catchments for Cape Fear
cape_fear_nhd <- get_nhdplus(
  AOI         = cape_fear_wbd_4326,
  realization = "catchment"
)
cat("NHDPlus catchments retrieved:", nrow(cape_fear_nhd), "\n")

# Reproject NHD catchments to match MERIT CRS
cape_fear_nhd_proj <- st_transform(cape_fear_nhd, 
                                   st_crs(cape_fear_network))

# BUILD CROSSWALK: For each NHD catchment, find its nearest MERIT reach
# This gives many NHD → one MERIT (correct direction)
nhd_centroids <- cape_fear_nhd_proj %>%
  st_centroid()

nearest_idx <- st_nearest_feature(
  st_geometry(cape_fear_nhd_proj),
  cape_fear_network
)
nhd_to_merit <- tibble(
  nhd_COMID   = nhd_centroids$featureid,
  merit_COMID = cape_fear_network$COMID[nearest_idx]
)

cat("NHD-to-MERIT crosswalk built:", nrow(nhd_to_merit), "NHD catchments\n")
cat("Mapping to", n_distinct(nhd_to_merit$merit_COMID), "unique MERIT reaches\n")

# Save crosswalk
write.csv(nhd_to_merit, 
          "/home/rvera_umass_edu/nhd_to_merit_capefear.csv",
          row.names = FALSE)

# =============================================================
# STEP 5: EXTRACT HYDRODL DISCHARGE FOR ALL CAPE FEAR REACHES
# =============================================================

nc <- nc_open(
  "/nas/cee-ice/data/GRADES_hydroDL/output_pfaf_07_1979_2023.nc",
  readunlim = FALSE
)

all_rivids <- ncvar_get(nc, "rivid")
time_vals  <- ncvar_get(nc, "time")
dates      <- as.Date(time_vals, origin = "1979-01-01")

# Cape Fear MERIT reach IDs
cape_fear_rivids <- unique(c(
  cape_fear_network$COMID))
target_idx       <- which(all_rivids %in% cape_fear_rivids)

cat("Cape Fear reaches found in NetCDF:", 
    length(target_idx), "of", length(cape_fear_rivids), "\n")

# Extract discharge — single block read (~4 min, ~664 MB)
discharge_matrix <- ncvar_get(
  nc, "Qout",
  start = c(min(target_idx), 1),
  count = c(max(target_idx) - min(target_idx) + 1, -1)
)
nc_close(nc)

# Convert to long dataframe
discharge_long <- as.data.frame(t(discharge_matrix)) %>%
  setNames(as.character(all_rivids[min(target_idx):max(target_idx)])) %>%
  mutate(date = dates) %>%
  select(date, any_of(as.character(cape_fear_rivids))) %>%
  pivot_longer(
    -date,
    names_to  = "merit_COMID",
    values_to = "Flow_cms"
  ) %>%
  mutate(merit_COMID = as.integer(merit_COMID))

cat("Discharge records:", nrow(discharge_long), "\n")
cat("Unique reaches:", n_distinct(discharge_long$merit_COMID), "\n")
rm(discharge_matrix)  # free memory

# =============================================================
# STEP 6: BUILD FLOW FEATURES — ALL 308 REACHES
# (Used for both training and prediction)
# =============================================================

discharge_features_all <- discharge_long %>%
  mutate(Flow_cms = pmax(Flow_cms, 0)) %>%
  arrange(merit_COMID, date) %>%
  group_by(merit_COMID) %>%
  mutate(
    log_flow       = log1p(Flow_cms),
    flow_7day      = rollmean(Flow_cms, 7,  fill = NA, align = "right"),
    flow_30day     = rollmean(Flow_cms, 30, fill = NA, align = "right"),
    log_flow_7day  = log1p(flow_7day),
    doy            = as.integer(format(date, "%j"))
  ) %>%
  group_by(merit_COMID, doy) %>%
  mutate(
    flow_seas_mean = mean(Flow_cms,  na.rm = TRUE),
    flow_seas_sd   = sd(Flow_cms,    na.rm = TRUE),
    flow_anomaly   = (Flow_cms - flow_seas_mean) / (flow_seas_sd + 1e-6)
  ) %>%
  ungroup() %>%
  mutate(
    sin_doy = sin(2 * pi * doy / 365),
    cos_doy = cos(2 * pi * doy / 365),
    month   = as.integer(format(date, "%m")),
    year    = as.integer(format(date, "%Y"))
  ) %>%
  select(merit_COMID, date, Flow_cms, log_flow,
         flow_7day, log_flow_7day, flow_30day,
         flow_anomaly, sin_doy, cos_doy, month, year)

cat("All Cape Fear flow features:", nrow(discharge_features_all), "\n")

# =============================================================
# STEP 7: BUILD TRAINING DATA
# petre_data (NHD COMID) → MERIT COMID → flow + StreamCat
# =============================================================

petre_crosswalk <- nhd_to_merit %>%
  filter(nhd_COMID %in% unique(petre_data$COMID))

petre_with_flow <- petre_data %>%
  left_join(
    petre_crosswalk,
    by = c("COMID" = "nhd_COMID")
  ) %>%
  left_join(
    discharge_features_all,
    by = c("merit_COMID", "date")
  )

cat("Petre obs with flow:", 
    sum(!is.na(petre_with_flow$Flow_cms)), 
    "of", nrow(petre_with_flow), "\n")
cat("Petre obs with StreamCat:", 
    sum(!is.na(petre_with_flow$npdesdensws)), 
    "of", nrow(petre_with_flow), "\n")

# =============================================================
# STEP 8a: GET STREAMCAT FOR ALL CAPE FEAR NHD REACHES
# =============================================================

library(StreamCatTools)

cape_fear_nhd_comids <- unique(na.omit(nhd_to_merit$nhd_COMID))
cat("Pulling StreamCat for", length(cape_fear_nhd_comids), "Cape Fear COMIDs\n")

sc_chunks <- split(cape_fear_nhd_comids, 
                   ceiling(seq_along(cape_fear_nhd_comids) / 150))

streamcat_request <- c(
  "pctagdrainage", "pctimp2019",  "pcturblo2019", "pcturbhi2019",
  "pcturbmd2019",  "pctgrs2019",  "runoff",        "precip9120",
  "pctcrop2019",   "pctmxfst2019","pcthbwet2019",
  "npdesdens",     "huden2010",   "canaldens",     "minedens",
  "coalminedens",  "manure",      "fert",          "damdens",
  "septic",        "bfi",         "pctow2019",     "tmax9120",
  "superfunddens", "tridens",     "wwtpminordens", "wwtpmajordens",
  "popden2010",    "elev",        "pctshrb2019",   "pctconif2019"
)

# Pull WS scale
sc_list_ws <- list()
#testing the time it takes to get these chunks
#started at 4:54pm, finished at 4:59 pm
for (i in seq_along(sc_chunks)) {
  message("StreamCat WS chunk ", i, " / ", length(sc_chunks))
  res <- tryCatch(
    sc_get_data(metric = paste(streamcat_request, collapse = ","),
                aoi    = "watershed",
                comid  = paste(sc_chunks[[i]], collapse = ",")),
    error = function(e) { message("error: ", e$message); NULL })
  if (!is.null(res)) {
    names(res) <- tolower(names(res))
    sc_list_ws[[i]] <- res
  }
  Sys.sleep(2)
}

# Pull CAT scale for point sources
#testing the time it takes to get these chunks
#started at 5:00pm, finished at ... session ran out before it could finish. more than 5 minutes tho
sc_list_cat <- list()
for (i in seq_along(sc_chunks)) {
  message("StreamCat CAT chunk ", i, " / ", length(sc_chunks))
  res <- tryCatch(
    sc_get_data(metric = paste(c("npdesdens", "tridens", "superfunddens",
                                 "wwtpmajordens", "wwtpminordens"),
                               collapse = ","),
                aoi    = "catchment",
                comid  = paste(sc_chunks[[i]], collapse = ",")),
    error = function(e) { message("error: ", e$message); NULL })
  if (!is.null(res)) {
    names(res) <- tolower(names(res))
    sc_list_cat[[i]] <- res
  }
  Sys.sleep(2)
}

# Combine WS and CAT. Need to combine chuncks from each first
streamcat_ws  <- bind_rows(sc_list_ws)
streamcat_cat <- bind_rows(sc_list_cat)

names(streamcat_ws)  <- tolower(names(streamcat_ws))
names(streamcat_cat) <- tolower(names(streamcat_cat))

streamcat_ws$comid  <- as.integer(streamcat_ws$comid)
streamcat_cat$comid <- as.integer(streamcat_cat$comid)

streamcat_ws  <- streamcat_ws[!duplicated(streamcat_ws$comid), ]
streamcat_cat <- streamcat_cat[!duplicated(streamcat_cat$comid), ]

# Join WS and CAT, compute ratios
cape_fear_streamcat <- streamcat_ws %>%
  left_join(streamcat_cat, by = "comid") %>%
  mutate(
    npdes_ratio     = npdesdenscat     / (npdesdensws     + 1e-6),
    tri_ratio       = tridenscat       / (tridensws       + 1e-6),
    superfund_ratio = superfunddenscat / (superfunddensws + 1e-6),
    wwtp_ratio      = wwtpmajordenscat / (wwtpmajordensws + 1e-6)
  )

cat("StreamCat retrieved for", nrow(cape_fear_streamcat), "reaches\n")
saveRDS(cape_fear_streamcat, "/home/rvera_umass_edu/cape_fear_streamcat.rds")

# =============================================================
# STEP 7 (REDO with StreamCat now available): TRAINING DATA
# =============================================================

petre_with_flow <- petre_data %>%
  left_join(merit_lookup_petre,
            by = c("COMID" = "nhd_COMID")) %>%
  left_join(discharge_features_all,
            by = c("merit_COMID", "date")) %>%
  left_join(cape_fear_streamcat,
            by = c("COMID" = "comid"))

cat("Petre obs with flow:", 
    sum(!is.na(petre_with_flow$Flow_cms)), 
    "of", nrow(petre_with_flow), "\n")
cat("Petre obs with StreamCat:", 
    sum(!is.na(petre_with_flow$npdesdensws)), 
    "of", nrow(petre_with_flow), "\n")

# =============================================================
# STEP 8b: BUILD PREDICTION GRID AT NHD CATCHMENT LEVEL
# Each NHD catchment gets MERIT flow + its own StreamCat
# =============================================================

prediction_grid <- discharge_features_all %>%
  filter(date >= as.Date("2018-09-01"),
         date <= as.Date("2021-03-01")) %>%
  left_join(nhd_to_merit, by = "merit_COMID",
            relationship = "many-to-many") %>%
  left_join(cape_fear_streamcat,
            by = c("nhd_COMID" = "comid")) %>%
  filter(!is.na(nhd_COMID))

cat("Prediction grid rows:", nrow(prediction_grid), "\n")
cat("Unique NHD catchments:", n_distinct(prediction_grid$nhd_COMID), "\n")
cat("Unique MERIT reaches:", n_distinct(prediction_grid$merit_COMID), "\n")
# =============================================================
# STEP 9: DEFINE COMPOUNDS AND PREDICTOR SET
# =============================================================

compounds <- c("PFOA", "PFOS", "PFHxA", "PFHxS")

flow_preds <- c(
  "log_flow", "log_flow_7day", "flow_30day",
  "flow_anomaly", "sin_doy", "cos_doy", "month"
)

sc_preds_found <- intersect(sc_preds, names(petre_with_flow))

all_preds <- c(flow_preds, sc_preds_found)

cat("Flow predictors:      ", length(flow_preds), "\n")
cat("StreamCat predictors: ", length(sc_preds_found), "\n")
cat("Total predictors:     ", length(all_preds), "\n")

# =============================================================
# STEP 10: SPATIAL DENSITY WEIGHTS
# =============================================================

coords <- petre_with_flow %>%
  distinct(Longitude, Latitude) %>%
  as.matrix()

knn_dist      <- FNN::knn.dist(coords, k = 10)
spatial_weight <- rowMeans(knn_dist) / mean(rowMeans(knn_dist))

petre_with_flow <- petre_with_flow %>%
  left_join(
    petre_with_flow %>%
      distinct(Longitude, Latitude) %>%
      mutate(spatial_weight = spatial_weight),
    by = c("Longitude", "Latitude")
  )

# =============================================================
# STEP 11: SPATIAL BLOCK CV — LEAVE ONE SITE OUT
# With only 13 sites, LOSO is better than block CV
# =============================================================

site_ids <- petre_with_flow %>%
  distinct(COMID) %>%
  mutate(site_idx = row_number())

petre_with_flow <- petre_with_flow %>%
  left_join(site_ids, by = "COMID")

n_sites <- nrow(site_ids)
cat("Running Leave-One-Site-Out CV across", 
    n_sites, "sites\n")

# =============================================================
# STEP 12: MODEL FUNCTION — RF AND XGB WITH LOSO CV
# =============================================================

run_cape_fear_model <- function(compound, 
                                model_type = "xgb",
                                data       = petre_with_flow,
                                preds      = all_preds,
                                min_obs    = 30) {
  
  outcome <- log10(data[[compound]] + 1)
  valid   <- !is.na(outcome) & outcome > 0
  
  if (sum(valid) < min_obs) {
    cat(compound, "— skipped (n =", sum(valid), ")\n")
    return(NULL)
  }
  
  df      <- data[valid, ]
  outcome <- outcome[valid]
  weights <- df$spatial_weight
  
  cv_preds <- rep(NA_real_, nrow(df))
  
  # Leave-one-site-out CV
  for (site in unique(df$site_idx)) {
    
    train_idx <- which(df$site_idx != site)
    test_idx  <- which(df$site_idx == site)
    
    if (length(train_idx) < 20) next
    
    train_x <- df[train_idx, preds, drop = FALSE]
    train_y <- outcome[train_idx]
    train_w <- weights[train_idx]
    test_x  <- df[test_idx,  preds, drop = FALSE]
    
    # Impute with training median
    train_medians <- sapply(train_x, median, na.rm = TRUE)
    for (col in names(train_x)) {
      train_x[[col]][is.na(train_x[[col]])] <- train_medians[[col]]
      test_x[[col]][is.na(test_x[[col]])]   <- train_medians[[col]]
    }
    
    if (model_type == "rf") {
      fit   <- randomForest(
        x        = train_x,
        y        = train_y,
        ntree    = 500,
        mtry     = max(1, floor(ncol(train_x) / 3)),
        weights  = train_w
      )
      cv_preds[test_idx] <- predict(fit, newdata = test_x)
    }
    
    if (model_type == "xgb") {
      dtrain <- xgb.DMatrix(as.matrix(train_x),
                            label  = train_y,
                            weight = train_w)
      dtest  <- xgb.DMatrix(as.matrix(test_x))
      
      # Inner CV for early stopping
      fit <- xgb.train(
        data    = dtrain,
        nrounds = 500,
        params  = list(
          objective        = "reg:squarederror",
          eta              = 0.05,
          max_depth        = 4,
          subsample        = 0.8,
          colsample_bytree = 0.8,
          min_child_weight = 5
        ),
        watchlist          = list(train = dtrain),
        early_stopping_rounds = 20,
        verbose            = 0
      )
      cv_preds[test_idx] <- predict(fit, dtest)
    }
  }
  
  # Metrics
  has_both <- !is.na(outcome) & !is.na(cv_preds)
  err      <- outcome[has_both] - cv_preds[has_both]
  r2       <- 1 - sum(err^2) / 
    sum((outcome[has_both] - mean(outcome[has_both]))^2)
  rmse     <- sqrt(mean(err^2))
  spearman <- cor(outcome[has_both], cv_preds[has_both], 
                  method = "spearman")
  
  cat(compound, model_type,
      "| R2 =",      round(r2,       3),
      "| RMSE =",    round(rmse,     3),
      "| Spearman =",round(spearman, 3),
      "| n =",       sum(has_both), "\n")
  
  # Refit final model on all data
  full_x <- df[, preds, drop = FALSE]
  full_medians <- sapply(full_x, median, na.rm = TRUE)
  for (col in names(full_x)) {
    full_x[[col]][is.na(full_x[[col]])] <- full_medians[[col]]
  }
  
  if (model_type == "xgb") {
    dtrain_full <- xgb.DMatrix(as.matrix(full_x),
                               label  = outcome,
                               weight = weights)
    final_fit <- xgb.train(
      data    = dtrain_full,
      nrounds = fit$best_iteration,
      params  = list(
        objective        = "reg:squarederror",
        eta              = 0.05,
        max_depth        = 4,
        subsample        = 0.8,
        colsample_bytree = 0.8,
        min_child_weight = 5
      ),
      verbose = 0
    )
  }
  
  if (model_type == "rf") {
    final_fit <- randomForest(
      x       = full_x,
      y       = outcome,
      ntree   = 1000,
      mtry    = max(1, floor(ncol(full_x) / 3)),
      weights = weights
    )
  }
  
  list(
    compound   = compound,
    model_type = model_type,
    final_model = final_fit,
    train_medians = full_medians,
    cv_preds   = cv_preds,
    actual     = outcome,
    site_ids   = df$site_idx,
    r2         = r2,
    rmse       = rmse,
    spearman   = spearman,
    n          = sum(has_both)
  )
}

# =============================================================
# STEP 13: RUN MODELS
# =============================================================

results_xgb <- map(compounds, 
                   ~run_cape_fear_model(.x, model_type = "xgb"))
names(results_xgb) <- compounds

results_rf  <- map(compounds,
                   ~run_cape_fear_model(.x, model_type = "rf"))
names(results_rf) <- compounds

# =============================================================
# STEP 14: PREDICT ACROSS ALL CAPE FEAR REACHES
# =============================================================

predict_cape_fear <- function(compound, model_result, 
                              pred_grid = prediction_grid,
                              preds     = all_preds) {
  
  if (is.null(model_result)) return(NULL)
  
  pred_x <- pred_grid[, intersect(preds, names(pred_grid)), 
                      drop = FALSE]
  
  # Add any missing columns as NA then impute
  missing_cols <- setdiff(preds, names(pred_x))
  for (col in missing_cols) pred_x[[col]] <- NA
  pred_x <- pred_x[, preds, drop = FALSE]
  
  # Impute with training medians
  for (col in names(pred_x)) {
    pred_x[[col]][is.na(pred_x[[col]])] <- 
      model_result$train_medians[[col]]
  }
  
  if (model_result$model_type == "xgb") {
    dpred <- xgb.DMatrix(as.matrix(pred_x))
    preds_out <- predict(model_result$final_model, dpred)
  } else {
    preds_out <- predict(model_result$final_model, 
                         newdata = pred_x)
  }
  
  pred_grid %>%
    select(merit_COMID, nhd_COMID, date, 
           month, year, Flow_cms) %>%
    mutate(
      compound          = compound,
      log10_conc_pred   = preds_out,
      conc_pred_ngL     = 10^preds_out - 1
    )
}

# Generate predictions for all compounds
cape_fear_preds <- map2_dfr(
  compounds,
  results_xgb[compounds],
  predict_cape_fear
)

cat("Prediction rows:", nrow(cape_fear_preds), "\n")

# =============================================================
# STEP 15: JOIN PREDICTIONS TO SPATIAL NETWORK AND SAVE
# =============================================================

# Join back to MERIT geometries for mapping
cape_fear_pred_spatial <- cape_fear_network %>%
  rename(merit_COMID = COMID) %>%
  left_join(
    cape_fear_preds %>%
      group_by(merit_COMID, compound) %>%
      summarise(
        mean_conc    = mean(conc_pred_ngL,  na.rm = TRUE),
        median_conc  = median(conc_pred_ngL, na.rm = TRUE),
        .groups      = "drop"
      ),
    by = "merit_COMID"
  )

# Save everything
saveRDS(results_xgb,         
        "/home/rvera_umass_edu/cape_fear_xgb_results.rds")
saveRDS(results_rf,          
        "/home/rvera_umass_edu/cape_fear_rf_results.rds")
saveRDS(cape_fear_preds,     
        "/home/rvera_umass_edu/cape_fear_predictions.rds")
saveRDS(cape_fear_pred_spatial,
        "/home/rvera_umass_edu/cape_fear_pred_spatial.rds")
saveRDS(merit_to_nhd,
        "/home/rvera_umass_edu/merit_to_nhd_capefear.rds")

cat("All saved successfully!\n")

# Quick performance summary
map_dfr(compounds, function(cmp) {
  res <- results_xgb[[cmp]]
  if (is.null(res)) return(NULL)
  tibble(compound = cmp, r2 = res$r2, 
         rmse = res$rmse, spearman = res$spearman,
         n = res$n)
}) %>% print()