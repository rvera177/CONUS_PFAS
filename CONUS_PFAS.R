# Conus Machine Learning model for PFAS concentrations
# Created by Raul V. May 6 2026
# this is my code edits to original ML models created by Teja S. in April 2026

# Updated May 6, 2026 by Raul V. 

setwd("C:/Users/Marston User/Documents/CONUS_PFAS")

library(readr)
library(dplyr)
library(tibble)
library(sf)
library(nhdplusTools)
library(StreamCatTools)
library(ggplot2)
library(randomForest)

# compounds
all_pfas <- c(
  "PFOS", "PFOA", "PFDA", "PFNA", "PFHxS", "PFHxA", "PFHpA",
  "PFPeA", "PFBS", "PFUnDA", "PFDoA", "PFBA", "PFHxDA",
  "PFTeDA", "PFTrDA", "PFNS", "GenX", "6:2 FTS", "8:2 FTS"
)

# dataset urls
urls <- list(
  Zhang      = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Zhang_et_al_2016_RI_NY.csv",
  Bai        = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Bai_and_Son_2021_Renoe_LasVegas.csv",
  Goodrow    = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Goodrow_et_al_2020_New_Jersey.csv",
  Breitmeyer = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Breitmeyer_et_al_2023_Pennsylvania.csv",
  Camacho    = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Camacho_et_al_2024_Florida.csv",
  NH_DES     = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/NewHampshire_DES_PFAS_Data_Dump.csv",
  Maine      = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/MaineDEP_2026_Datadump_cleaned.csv",
  WQP        = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/WQP_USA_Data_complete.csv",
  Hayworth   = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Hayworth_et_al_2022_Alabama_cleaned.csv",
  Dunn       = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Dunn_et_al_2023_RhodeIsland_complete.csv",
  Forster    = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Forster_et_al_2024_SouthCarolina_cleaned.csv",
  Penland    = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Penland_2020_SC_NC_cleaned.csv",
  Sims       = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sims_et_al_2025_%20Western_United_States.csv",
  Webb       = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Webb_et_al_2026_Savannah.csv",
  Labad      = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Labad_et_al_2025_Georgia.csv",
  CO_DPH   = "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Colorado_DPH.csv"
)

# streamcat variable names
streamcat_request <- c(
  "pctagdrainage", "pctimp2019",  "pctbl2019",   "pctcrop2019",
  "pctdecid2019",  "pcthbwet2019","npdesdens",   "huden2010",
  "canaldens",     "minedens",    "coalminedens","manure",
  "fert",          "damdens", "septic", "bfi",
  "superfunddens", "tridens"
)

# variables at a watershed scale "_ws"
streamcat_metrics <- c(
  "pctagdrainagews", "pctimp2019ws",   "pctbl2019ws",    "pctcrop2019ws",
  "pctdecid2019ws",  "pcthbwet2019ws", "npdesdensws",    "huden2010ws",
  "canaldensws",     "minedensws",     "coalminedensws", "manurews",
  "fertws",          "damdensws", "septicws", "bfiws",
  "superfunddensws", "tridensws"
)

# colors and labels
source_colors <- c(
  "Zhang"      = "blue", "Bai"        = "orange", "Goodrow"    = "red",
  "Breitmeyer" = "green", "Camacho"    = "purple", "NH_DES"     = "cyan",
  "Maine"      = "pink", "WQP"        = "brown", "Hayworth"   = "yellow",
  "Dunn"       = "darkblue", "Forster"    = "darkgreen", "Penland"    = "darkred",
  "Sims"       = "steelblue", "Webb"       = "magenta", "Labad"      = "maroon",
  "CO_DPH"     = "forestgreen"
)

source_labels <- c(
  "Zhang"      = "Zhang et al. (2016) - RI/NY",
  "Bai"        = "Bai & Son (2021) - NV/CA",
  "Goodrow"    = "Goodrow et al. (2020) - NJ",
  "Breitmeyer" = "Breitmeyer et al. (2023) - PA",
  "Camacho"    = "Camacho et al. (2024) - FL",
  "NH_DES"     = "NH DES (2026) - NH",
  "Maine"      = "Maine DEP (2026) - ME",
  "WQP"        = "WQP USA (2026)",
  "Hayworth"   = "Hayworth et al. (2022) - AL",
  "Dunn"       = "Dunn et al. (2023) - RI",
  "Forster"    = "Forster et al. (2024) - SC",
  "Penland"    = "Penland (2020) - SC/NC",
  "Sims"       = "Sims et al. (2025) - West",
  "Webb"       = "Webb et al. (2026) - GA/Savannah",
  "Labad"      = "Labad et al. (2025) - GA",
  "CO_DPH"     = "Colorado_DPH (2026)"
)

# themes
theme_publication <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      text              = element_text(family = "serif", color = "black"),
      plot.title        = element_text(size = base_size + 1, face = "bold",
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size - 1, color = "grey35",
                                       margin = margin(b = 8)),
      plot.caption      = element_text(size = base_size - 2, color = "grey50",
                                       hjust = 0),
      axis.title        = element_text(size = base_size, face = "plain"),
      axis.text         = element_text(size = base_size - 1, color = "black"),
      axis.line         = element_line(color = "black", linewidth = 0.4),
      axis.ticks        = element_line(color = "black", linewidth = 0.4),
      axis.ticks.length = unit(2.5, "pt"),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.border      = element_blank(),
      panel.spacing     = unit(8, "pt"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA),
      legend.title      = element_text(size = base_size - 1, face = "bold"),
      legend.text       = element_text(size = base_size - 1),
      legend.position   = "right",
      strip.background  = element_rect(fill = "grey96", color = "grey80",
                                       linewidth = 0.3),
      strip.text        = element_text(size = base_size - 1, face = "bold",
                                       margin = margin(3, 3, 3, 3)),
      plot.margin       = margin(10, 14, 8, 10)
    )
}

# loading PFAS data from my github url's
load_dataset <- function(url, source_name) {
  df <- read_csv(url, show_col_types = FALSE)
  if ("Sample Type" %in% names(df)) {
    df <- df %>% filter(`Sample Type` == "Surface Water")
  }
  if (!"year" %in% names(df))  df$year  <- NA_integer_
  if (!"month" %in% names(df)) df$month <- NA_integer_
  if ("Sample Date (MM/DD/YYY)" %in% names(df)) {
    parsed_date <- as.Date(df$`Sample Date (MM/DD/YYY)`, format = "%m/%d/%Y")
    df <- df %>%
      mutate(
        year  = coalesce(year,  as.integer(format(parsed_date, "%Y"))),
        month = coalesce(month, as.integer(format(parsed_date, "%m")))
      ) #only grabs month or year info if it's column is not already filled in
  }
  if ("Sampling Year" %in% names(df)) {
    df <- df %>%
      mutate(
        year = coalesce(year, as.integer(`Sampling Year`))
      )}
  df %>%
    dplyr::select(any_of(all_pfas), Latitude, Longitude, year, month) %>%
    mutate(
      across(any_of(all_pfas), ~ as.numeric(.)),
      source = source_name
    )}

datasets <- mapply(load_dataset, url = urls,
                   source_name = names(urls), SIMPLIFY = FALSE)

water <- bind_rows(datasets)

for (compound in all_pfas)
  if (!compound %in% names(water)) water[[compound]] <- NA_real_

print(table(water$source))
cat("total rows:", nrow(water), "\n\n")

# remove all-zero rows
# sites where every compound = 0 are probably non-detects not true zeros
zero_mask <- water %>%
  rowwise() %>%
  mutate(all_zero = all(c_across(any_of(all_pfas)) == 0, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(all_zero)

water_nozero <- water %>% filter(!zero_mask)

cat("all-zero rows:", sum(zero_mask), "\n")
cat("rows after removal:", nrow(water_nozero), "\n\n")


library(sf)
library(dplyr)
library(future.apply)
library(nhdplusTools)

# reuse saved COMIDs
if (file.exists("water_13datasets_streamcat.rds")) {
  
  saved <- readRDS("water_13datasets_streamcat.rds") %>%
    select(Latitude, Longitude, COMID) %>%
    filter(!is.na(COMID)) %>%
    distinct(Latitude, Longitude, .keep_all = TRUE)
  
  water <- water %>%
    left_join(saved, by = c("Latitude", "Longitude"))
  
  cat("COMIDs from saved file:",
      sum(!is.na(water$COMID)),
      "/", nrow(water), "\n")
  
} else {
  
  water$COMID <- NA_integer_
}

# indices needing COMIDs
missing_idx <- which(is.na(water$COMID))

cat("Fetching COMIDs for",
    length(missing_idx),
    "new points\n")


valid_coords <- which(
  !is.na(water$Longitude) &
    !is.na(water$Latitude)
)

# make sf object only for valid rows
water_sf <- st_as_sf(
  water[valid_coords, ],
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

# rows still needing COMIDs AND valid coords
missing_idx <- which(
  is.na(water$COMID) &
    !is.na(water$Longitude) &
    !is.na(water$Latitude)
)

# map original row indices -> sf row indices
sf_match <- match(missing_idx, valid_coords)

pts_missing <- water_sf[sf_match, ]

get_comid <- function(pt,
                          max_tries = 3,
                          base_sleep = 0.2) {
  
  for (t in seq_len(max_tries)) {
    
    Sys.sleep(base_sleep * t)
    
    res <- tryCatch(
      nhdplusTools::discover_nhdplus_id(pt),
      error = function(e) NULL
    )
    
    if (!is.null(res) && length(res) > 0) {
      
      if (is.atomic(res))
        return(as.integer(res[[1]]))
      
      if (is.data.frame(res) &&
          "comid" %in% names(res))
        return(as.integer(res$comid[1]))
      
      if (is.list(res) &&
          !is.null(res$comid))
        return(as.integer(res$comid))
    }
  }
  
  NA_integer_
}

library(sf)
library(dplyr)
library(future.apply)
library(progressr)

handlers(global = TRUE)
handlers("progress")

plan(multisession, workers = 4)

# saving comid in chunks of 100
# so if the code crashes, it still gets saved
chunk_size <- 100
save_file <- "water_partial_comids.rds"

# rows still missing COMIDs
missing_idx <- which(
  is.na(water$COMID) &
    !is.na(water$Longitude) &
    !is.na(water$Latitude)
)

# split into chunks
chunks <- split(
  missing_idx,
  ceiling(seq_along(missing_idx) / chunk_size)
)

# progress bar
with_progress({
  
  p <- progressor(steps = length(missing_idx))
  
  # loop through chunks
  for (j in seq_along(chunks)) {
    
    idx_chunk <- chunks[[j]]
    
    # map original indices to sf rows
    sf_match <- match(idx_chunk, valid_coords)
    
    pts_chunk <- water_sf[sf_match, ]
    
    # parallel fetch
    comids <- future_sapply(
      seq_len(nrow(pts_chunk)),
      function(i) {
        
        result <- get_comid(pts_chunk[i, ])
        
        p()
        
        result
      },
      future.seed = TRUE
    )
    
    # write back to water
    water$COMID[idx_chunk] <- comids
    
    # autosave after every chunk
    saveRDS(water, save_file)
    
    cat(
      "Chunk", j, "/",
      length(chunks),
      "saved\n"
    )
  }
})

plan(sequential)

cat("Finished\n")

cat("COMIDs assigned:", sum(!is.na(water$COMID)), "/", nrow(water), "\n\n")
saveRDS(water, "water_15datasets_COMID.rds")

water_nozero <- water_nozero %>%
  left_join(water %>% dplyr::select(Latitude, Longitude, COMID) %>%
              distinct(Latitude, Longitude, .keep_all = TRUE),
            by = c("Latitude", "Longitude"))

unique_comids <- unique(na.omit(water$COMID))
cat("pulling StreamCat for", length(unique_comids), "COMIDs\n")

sc_chunks <- split(unique_comids, ceiling(seq_along(unique_comids) / 150))
sc_list   <- list()

# pull streamcat data. Doesn't take too long as long as the streamcat website isn't down
# if you get an http or url error, it is probably because the website is temporarily down
# try again in an hour, usually it's only down for a little bit.

for (i in seq_along(sc_chunks)) {
  message("StreamCat chunk ", i, " / ", length(sc_chunks))
  res <- tryCatch(
    sc_get_data(metric = paste(streamcat_request, collapse = ","),
                aoi    = "watershed",
                comid  = paste(sc_chunks[[i]], collapse = ",")),
    error = function(e) { message("error: ", e$message); NULL })
  if (!is.null(res)) {
    names(res) <- tolower(names(res))
    sc_list[[i]] <- res
  }
  Sys.sleep(2)
}

streamcat_df        <- bind_rows(sc_list)
names(streamcat_df) <- tolower(names(streamcat_df))
streamcat_df$comid  <- as.integer(streamcat_df$comid)
streamcat_df        <- streamcat_df[!duplicated(streamcat_df$comid), ]

water <- water %>% left_join(as.data.frame(streamcat_df), by = c("COMID" = "comid"))
water_nozero <- water_nozero %>% left_join(as.data.frame(streamcat_df), by = c("COMID" = "comid"))

sc_preds_found <- intersect(streamcat_metrics, names(water))
cat("StreamCat metrics joined:", length(sc_preds_found), "/", length(streamcat_metrics), "\n\n")

saveRDS(water,        "water_15datasets_streamcat_all.rds")
saveRDS(water_nozero, "water_15datasets_streamcat_nozero.rds")

#setting up the rf and xgboost model functions
library(doParallel)
library(foreach)

run_model <- function(water_input,
                      suffix = "all",
                      model_type = c("rf", "xgb"),
                      hotspot_weight = FALSE,
                      hotspot_quantile = 0.9,
                      hotspot_boost = 5,
                      nfolds = 10,
                      seed = 2026) {
  
  library(dplyr)
  model_type <- match.arg(model_type)
  
  # Setup parallel backend
  n_cores <- detectCores() - 2  # leave 2 free for system stability
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))  # ensures cluster closes even if function errors
  
  cat("Running on", n_cores, "cores\n")
  
  water_input <- water_input %>%
    group_by(source) %>%
    mutate(weight = 1 / sqrt(n())) %>%
    ungroup()
  
  water_input <- water_input %>%
    mutate(across(all_of(all_pfas), ~ log10(.x + 1)))
  
  preds_all <- c(sc_preds_found)
  preds_df <- water_input %>%
    dplyr::select(all_of(preds_all))
  
  set.seed(seed)
  folds <- sample(rep(1:nfolds, length.out = nrow(water_input)))
  
  cat("\n", model_type, "|", nfolds, "-fold CV [", suffix, ", n =", nrow(water_input), "]\n")
  
  # PARALLEL COMPOUND LOOP
  results_list <- foreach(
    compound = all_pfas,
    .packages = c("dplyr", "randomForest", "xgboost"),
    .errorhandling = "pass"  # if one compound fails, others continue
  ) %dopar% {
    
    outcome <- water_input[[compound]]
    
    if (sum(!is.na(outcome)) < 150) {
      return(list(compound = compound, skipped = TRUE))
    }
    
    weights <- water_input$weight
    
    if (hotspot_weight) {
      thresh <- quantile(outcome, hotspot_quantile, na.rm = TRUE)
      hotspot_mult <- ifelse(outcome >= thresh, hotspot_boost, 1)
      weights <- weights * hotspot_mult
    }
    
    cv_preds <- rep(NA_real_, nrow(water_input))
    
    for (fold in 1:nfolds) {
      
      test_idx <- which(folds == fold)
      train_idx <- which(folds != fold)
      
      train_x <- preds_df[train_idx, , drop = FALSE]
      train_y <- outcome[train_idx]
      train_w <- weights[train_idx]
      
      valid <- complete.cases(train_x) & !is.na(train_y)
      train_x <- train_x[valid, , drop = FALSE]
      train_y <- train_y[valid]
      train_w <- train_w[valid]
      
      if (length(unique(train_y)) < 2 || nrow(train_x) < 20) next
      
      for (col in names(train_x)) {
        med_val <- median(train_x[[col]], na.rm = TRUE)
        train_x[[col]][is.na(train_x[[col]])] <- med_val
      }
      
      test_x <- preds_df[test_idx, , drop = FALSE]
      for (col in names(test_x)) {
        med_val <- median(train_x[[col]], na.rm = TRUE)
        test_x[[col]][is.na(test_x[[col]])] <- med_val
      }
      
      if (model_type == "rf") {
        fit <- randomForest::randomForest(
          x = train_x, y = train_y,
          ntree = 500,
          mtry = max(1, floor(ncol(train_x) / 3)),
          importance = TRUE,
          weights = train_w
        )
        preds <- predict(fit, newdata = test_x)
      }
      
      if (model_type == "xgb") {
        dtrain <- xgboost::xgb.DMatrix(
          data = as.matrix(train_x), label = train_y, weight = train_w
        )
        dtest <- xgboost::xgb.DMatrix(data = as.matrix(test_x))
        fit <- xgboost::xgb.train(
          data = dtrain, nrounds = 300,
          params = list(
            objective = "reg:squarederror",
            eta = 0.05, max_depth = 6,
            subsample = 0.8, colsample_bytree = 0.8
          ),
          verbose = 0
        )
        preds <- predict(fit, dtest)
      }
      
      cv_preds[test_idx] <- preds
    }
    
    # Metrics
    has_obs <- !is.na(outcome) & !is.na(cv_preds)
    err <- outcome[has_obs] - cv_preds[has_obs]
    rmse <- sqrt(mean(err^2))
    mae <- mean(abs(err))
    r2 <- 1 - sum(err^2) / sum((outcome[has_obs] - mean(outcome[has_obs]))^2)
    spearman <- cor(outcome[has_obs], cv_preds[has_obs], method = "spearman")
    
    thresh <- quantile(outcome[has_obs], hotspot_quantile, na.rm = TRUE)
    actual_hot <- outcome[has_obs] >= thresh
    pred_hot <- cv_preds[has_obs] >= thresh
    hotspot_acc <- mean(actual_hot == pred_hot, na.rm = TRUE)
    
    # Final full model
    full_valid <- complete.cases(preds_df) & !is.na(outcome)
    full_x <- preds_df[full_valid, , drop = FALSE]
    full_y <- outcome[full_valid]
    full_w <- weights[full_valid]
    
    for (col in names(full_x)) {
      med_val <- median(full_x[[col]], na.rm = TRUE)
      full_x[[col]][is.na(full_x[[col]])] <- med_val
    }
    
    if (model_type == "rf") {
      final_fit <- randomForest::randomForest(
        x = full_x, y = full_y,
        ntree = 1000,
        mtry = max(1, floor(ncol(full_x) / 3)),
        importance = TRUE,
        weights = full_w
      )
    }
    
    if (model_type == "xgb") {
      dtrain_full <- xgboost::xgb.DMatrix(
        data = as.matrix(full_x), label = full_y, weight = full_w
      )
      final_fit <- xgboost::xgb.train(
        data = dtrain_full, nrounds = 300,
        params = list(
          objective = "reg:squarederror",
          eta = 0.05, max_depth = 6,
          subsample = 0.8, colsample_bytree = 0.8
        ),
        verbose = 0
      )
    }
    
    list(
      compound = compound,
      skipped = FALSE,
      actual = outcome,
      predicted = cv_preds,
      source = water_input$source,
      r2 = r2, rmse = rmse, mae = mae,
      spearman = spearman,
      hotspot_acc = hotspot_acc,
      final_model = final_fit
    )
  }
  
  # Unpack results
  cv_results <- list()
  final_models <- list()
  
  for (res in results_list) {
    if (inherits(res, "error")) next
    if (res$skipped) {
      cat(res$compound, "skipped (too few observations)\n")
      next
    }
    compound <- res$compound
    final_models[[compound]] <- res$final_model
    cv_results[[compound]] <- res[
      c("actual", "predicted", "source", "r2", "rmse", "mae", "spearman", "hotspot_acc")
    ]
    cat(
      compound,
      "| R2 =", round(res$r2, 3),
      "| RMSE =", round(res$rmse, 3),
      "| MAE =", round(res$mae, 3),
      "| Spearman =", round(res$spearman, 3),
      "| HotspotAcc =", round(res$hotspot_acc, 3),
      "\n"
    )
  }
  
  return(list(
    cv_results = cv_results,
    final_models = final_models,
    water_input = water_input,
    preds_df = preds_df,
    model_type = model_type,
    suffix = suffix
  ))
}

#runing model for random forest
results_all_rf <- run_model(
  water,
  suffix = "all",
  model_type = "rf"
)
results_all_xgb <- run_model(
  water,
  suffix = "all",
  model_type = "xgb"
)
#now running ML but only on values above BDL of 0.2
#since values below detection limit are technically inaccurate
water_above_BDL <- water %>%
  mutate(across(all_of(all_pfas), ~ ifelse(.x < 0.2, NA, .x)))

results_all_rf_BDL <- run_model(
  water_above_BDL,
  suffix = "all",
  model_type = "rf"
)

results_all_xgb_BDL <- run_model(
  water_above_BDL,
  suffix = "all",
  model_type = "xgb"
)

saveRDS(results_all_rf,    "cv_results_all_rf.rds")
saveRDS(results_all_xgb, "cv_results_all_xgb.rds")
saveRDS(results_all_rf_BDL, "cv_results_all_rf_BDL.rds")
saveRDS(results_all_xgb_BDL, "cv_results_all_xgb_BDL.rds")

extract_metrics_table <- function(results_obj) {
  
  library(dplyr)
  
  metrics <- lapply(names(results_obj$cv_results), function(compound) {
    r <- results_obj$cv_results[[compound]]
    data.frame(
      compound     = compound,
      r2           = round(r$r2, 3),
      rmse         = round(r$rmse, 3),
      mae          = round(r$mae, 3),
      spearman     = round(r$spearman, 3),
      hotspot_acc  = round(r$hotspot_acc, 3)
    )
  }) %>% bind_rows()
  
  metrics <- metrics %>% arrange(desc(hotspot_acc))
  
  return(metrics)
}

# usage
metrics_rf  <- extract_metrics_table(results_all_rf)
metrics_xgb <- extract_metrics_table(results_all_xgb)
metrics_BDL_rf  <- extract_metrics_table(results_all_rf)
metrics_BDL_xgb <- extract_metrics_table(results_all_xgb)
metrics_BDL_rf
# side by side comparison
metrics_combined <- metrics_rf %>%
  rename_with(~ paste0(.x, "_rf"),  -compound) %>%
  left_join(
    metrics_xgb %>% rename_with(~ paste0(.x, "_xgb"), -compound),
    by = "compound") %>%
  left_join(
    metrics_BDL_rf %>% rename_with(~ paste0(.x, "_rf_BDL"), -compound),
    by = "compound") %>%
  left_join(
    metrics_BDL_xgb %>% rename_with(~ paste0(.x, "_xgb_BDL"), -compound),
    by = "compound")

print(metrics_combined)


detach("package:randomForest", unload = TRUE)
# get top 3 compounds by R²
top3 <- metrics_BDL_rf %>%  # or extract from cv_results
  arrange(desc(r2)) %>%
  slice(1:3) %>%
  pull(compound)

# build plot dataframe for top 3
top3_df <- bind_rows(lapply(top3, function(cmp) {
  r <- results_all_rf_BDL$cv_results[[cmp]]
  thresh <- quantile(r$actual, 0.9, na.rm = TRUE)
  data.frame(
    compound  = cmp,
    actual    = r$actual,
    predicted = r$predicted,
    r2        = round(r$r2, 2),
    hotspot_acc = round(r$hotspot_acc, 3)
  ) %>%
    filter(!is.na(actual) & !is.na(predicted)) %>%
    mutate(
      hotspot_class = case_when(
        actual >= thresh & predicted >= thresh ~ "True Hotspot",
        actual < thresh  & predicted < thresh  ~ "True Non-Hotspot",
        actual >= thresh & predicted < thresh  ~ "Missed Hotspot",
        actual < thresh  & predicted >= thresh ~ "False Alarm"
      ),
      thresh = thresh
    )
}))

# factor for ordering panels
top3_df <- top3_df %>%
  mutate(
    compound = factor(compound, levels = top3),
    hotspot_class = factor(hotspot_class,
                           levels = c("True Hotspot", "True Non-Hotspot",
                                      "Missed Hotspot", "False Alarm"))
  )

# label dataframe
label_df_top3 <- top3_df %>%
  group_by(compound, r2, hotspot_acc) %>%
  summarise(
    max_val = ceiling(max(c(actual, predicted))),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("R² = ", r2, "\nHotspot Acc = ", hotspot_acc)
  )

# anchor points for symmetric axes
anchor_top3 <- label_df_top3 %>%
  rowwise() %>%
  do(data.frame(
    compound     = .$compound,
    actual       = c(0, .$max_val),
    predicted    = c(0, .$max_val),
    hotspot_class = NA_character_,
    r2           = NA_real_,
    hotspot_acc  = NA_real_,
    thresh       = NA_real_
  )) %>%
  ungroup() %>%
  mutate(compound = factor(compound, levels = top3),
         hotspot_class = factor(hotspot_class,
                                levels = c("True Hotspot", "True Non-Hotspot",
                                           "Missed Hotspot", "False Alarm")))

plot_df <- bind_rows(top3_df, anchor_top3)
# convert hotspot_class to factor and drop NA level
plot_df <- bind_rows(top3_df, anchor_top3) %>%
  mutate(hotspot_class = factor(hotspot_class,
                                levels = c("True Hotspot", "True Non-Hotspot",
                                           "Missed Hotspot", "False Alarm"))) %>%
  filter(!is.na(hotspot_class))  # remove anchor rows from legend

p_hotspot <- ggplot(plot_df, aes(x = actual, y = predicted)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", linewidth = 0.5, color = "black") +
  geom_point(aes(color = hotspot_class),
             size = 2, alpha = 0.75, shape = 16,
             na.rm = TRUE) +
  geom_text(data = label_df_top3,
            aes(x = 0, y = max_val * 0.97, label = label),
            hjust = 0, vjust = 1, size = 4,
            family = "serif", color = "black") +
  scale_color_manual(
    values = c("True Hotspot"     = "#8B0000",
               "True Non-Hotspot" = "grey60",
               "Missed Hotspot"   = "#E87D2B",
               "False Alarm"      = "#2196C4"),
    name = NULL,
    na.value = NA
  ) +
  guides(color = guide_legend(
    override.aes   = list(size = 4, alpha = 1),
    na.translate   = FALSE
  )) +
  facet_wrap(~ compound, scales = "free", ncol = 3) +
  labs(
    title    = "Hotspot detection across top PFAS compounds",
    subtitle = "10-fold CV | hotspot = top 10th percentile",
    x        = "Observed log₁₀(concentration + 1) (ng/L)",
    y        = "Predicted log₁₀(concentration + 1) (ng/L)"
  ) +
  theme_publication(base_size = 11) +
  theme(
    aspect.ratio    = 1,
    legend.position = "bottom",
    legend.key.size = unit(10, "pt"),
    strip.text      = element_text(size = 11, face = "bold")
  )
p_hotspot

ggsave("fig_hotspot_top3.png", p_hotspot,
       width = 12, height = 5, dpi = 300, bg = "white")

# plot stuff
make_plots <- function(res) {
  if ("package:randomForest" %in% search()) {
    detach("package:randomForest", unload = TRUE)
  }#need to remove the random forest package before make_plots()
  #because it conflicts with ggplot
  cv_results <- res$cv_results
  suffix     <- res$suffix
  modeled    <- names(cv_results)
  
  perf_df <- data.frame(
    compound = modeled,
    R2   = sapply(modeled, function(c) cv_results[[c]]$r2),
    RMSE = sapply(modeled, function(c) cv_results[[c]]$rmse),
    MAE  = sapply(modeled, function(c) cv_results[[c]]$mae)
  ) %>%
    arrange(desc(R2)) %>%
    mutate(across(c(R2, RMSE, MAE), ~ round(.x, 3)))
  
  write.csv(perf_df, paste0("table_", suffix, ".csv"), row.names = FALSE)
  print(perf_df)
  
  subtitle_tag <- if (suffix == "nozero") " | zero-rows removed" else ""
  
  # R2 bar chart — unchanged
  p_r2 <- ggplot(perf_df,
                 aes(x = reorder(compound, R2), y = R2, fill = R2 > 0)) +
    geom_col(width = 0.6, alpha = 0.88) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "black", linetype = "solid") +
    geom_text(aes(label = sprintf("%.2f", R2),
                  hjust = ifelse(R2 >= 0, -0.15, 1.15)),
              size = 3, family = "serif", color = "grey30") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
    coord_flip() +
    labs(title    = "Cross-validated predictive performance",
         subtitle = paste0("10-fold CV R² | 15 USA datasets | StreamCat + Superfund + TRI",
                           subtitle_tag),
         x = NULL,
         y = "10-fold CV R²") +
    theme_publication(base_size = 11)
  
  ggsave(paste0("fig1_r2_", suffix, ".png"), p_r2,
         width = 7, height = 6, dpi = 300, bg = "white")
  
  # predicted vs actual scatter
  all_pred_df <- bind_rows(lapply(modeled, function(c) {
    data.frame(compound  = c,
               actual    = cv_results[[c]]$actual,
               predicted = cv_results[[c]]$predicted,
               source    = cv_results[[c]]$source)
  })) %>%
    filter(!is.na(actual)) %>%
    left_join(perf_df %>% dplyr::select(compound, R2), by = "compound") %>%
    mutate(label = paste0("R2=", sprintf("%.2f", R2)))
  
  # calculate symmetric max per compound
  per_compound_max <- all_pred_df %>%
    group_by(compound) %>%
    summarise(
      max_val = ceiling(max(c(actual, predicted), na.rm = TRUE)),
      .groups = "drop"
    )
  
  all_pred_df <- all_pred_df %>%
    left_join(per_compound_max, by = "compound")
  
  # invisible anchor points to enforce symmetric axes per panel
  anchor_df <- per_compound_max %>%
    rowwise() %>%
    do(data.frame(
      compound  = .$compound,
      actual    = c(0, .$max_val),
      predicted = c(0, .$max_val),
      source    = NA_character_,
      R2        = NA_real_,
      label     = NA_character_,
      max_val   = .$max_val
    )) %>%
    ungroup()
  
  all_pred_df_plot <- bind_rows(all_pred_df, anchor_df)
  # order compounds by R²
  compound_order <- perf_df %>% arrange(desc(R2)) %>% pull(compound)
  
  all_pred_df_plot <- all_pred_df_plot %>%
    mutate(compound = factor(compound, levels = compound_order))
  
  label_df <- all_pred_df %>%
    group_by(compound, label) %>%
    summarise(
      x = 0,
      y = max(max_val, na.rm = TRUE) * 0.97,
      .groups = "drop"
    )
  label_df <- label_df %>%
    mutate(compound = factor(compound, levels = compound_order))
  
  p_scatter <- ggplot(all_pred_df_plot, aes(x = actual, y = predicted)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", linewidth = 0.5, color = "black") +
    geom_point(aes(color = source),
               size = 2.5, alpha = 0.65, shape = 16,
               na.rm = TRUE) +
    geom_text(data = label_df, aes(x = x, y = y, label = label),
              hjust = 0, vjust = 1, size = 5,
              family = "serif", color = "black") +
    scale_color_manual(values = source_colors,
                       labels = source_labels,
                       name   = NULL,
                       na.value = NA) +
    facet_wrap(~ compound, scales = "free", ncol = 4) +
    labs(
      title    = "Predicted vs observed concentrations",
      subtitle = paste0("10-fold cross validation | dashed = 1:1 relationship",
                        subtitle_tag),
      x = "Observed log₁₀(concentration + 1) (ng/L)",
      y = "Predicted log₁₀(concentration + 1) (ng/L)") +
    theme_publication(base_size = 9) +
    theme(legend.position = "bottom",
          legend.key.size = unit(8, "pt"),
          strip.text      = element_text(size = 15, color = "black"),
          aspect.ratio    = 1)  # square panels without coord_fixed
  ggsave(paste0("fig2_Predicted_Scatter_", suffix, ".png"), p_scatter,
         width = 15, height = 11, dpi = 300, bg = "white")
  cat("figures saved\n")
}
make_plots(results_all_rf_BDL)
make_plots(results_all_rf)
make_plots(results_all_xgb)
make_plots(results_all_xgb_BDL)


# get the indices that were actually used to train the final model
valid_idx <- complete.cases(results_all_rf_BDL$preds_df) & 
  !is.na(results_all_rf_BDL$water_input[["PFOA"]])

train_x <- results_all_rf_BDL$preds_df[valid_idx, ]
train_y <- results_all_rf_BDL$water_input[["PFOA"]][valid_idx]

# predict on exact same rows the model was trained on
library(randomForest)
pfoa_train_pred <- predict(pfoa_model, newdata = train_x)

cat("Length train_y:", length(train_y), "\n")
cat("Length preds:  ", length(pfoa_train_pred), "\n")

ss_res <- sum((train_y - pfoa_train_pred)^2)
ss_tot <- sum((train_y - mean(train_y))^2)
r2_train <- 1 - ss_res / ss_tot

cat("Train R²:", round(r2_train, 3), "\n")
cat("CV R²:   ", 0.671, "\n")
cat("Gap:     ", round(r2_train - 0.671, 3), "\n")

#i'll be proceeding with the results_above_BDL model
#now, mapping PFAS onto HUC region 1 (New england)
library(sf)
flowlines <- st_read("C:/Users/Marston User/Documents/NE PFAS Data/NHDPlusV21_NE_01_NHDSnapshot_04/NHDPlusNE/NHDPlus01/NHDSnapshot/Hydrography/NHDFlowline.shp")
comids <- flowlines$COMID
length(comids) #number of stream reaches for NE

# 1. Get unique valid COMIDs from flowlines
comids_ne <- unique(na.omit(flowlines$COMID))
comids_ne <- comids_ne[comids_ne > 0]  # remove any negative COMIDs
cat("Total NE COMIDs:", length(comids_ne), "\n")

# 2. Download StreamCat for all NE reaches (same method as before)
sc_chunks_ne <- split(comids_ne, ceiling(seq_along(comids_ne) / 150))
sc_list_ne   <- list()

for (i in seq_along(sc_chunks_ne)) {
  message("StreamCat chunk ", i, " / ", length(sc_chunks_ne))
  res <- tryCatch(
    sc_get_data(metric = paste(streamcat_request, collapse = ","),
                aoi    = "watershed",
                comid  = paste(sc_chunks_ne[[i]], collapse = ",")),
    error = function(e) { message("error: ", e$message); NULL })
  if (!is.null(res)) {
    names(res) <- tolower(names(res))
    sc_list_ne[[i]] <- res
  }
  Sys.sleep(2)
}

# 3. Bind and clean
streamcat_ne        <- bind_rows(sc_list_ne)
names(streamcat_ne) <- tolower(names(streamcat_ne))
streamcat_ne$comid  <- as.integer(streamcat_ne$comid)
streamcat_ne        <- streamcat_ne[!duplicated(streamcat_ne$comid), ]

saveRDS(streamcat_ne, "streamcat_NE_allreaches.rds")
cat("StreamCat rows:", nrow(streamcat_ne), "\n")


library(randomForest)

# 1. Join StreamCat to flowlines
flowlines_pred <- flowlines %>%
  left_join(as.data.frame(streamcat_ne), by = c("COMID" = "comid"))

# 2. Prepare predictor dataframe (same columns as training)
sc_preds_found  # check these match exactly what the model was trained on
pred_df <- flowlines_pred %>%
  as.data.frame() %>%
  dplyr::select(all_of(sc_preds_found))

# 3. Impute NAs with training data means
train_means <- colMeans(results_all_rf_BDL$preds_df, na.rm = TRUE)
for (col in names(pred_df)) {
  pred_df[[col]][is.na(pred_df[[col]])] <- train_means[[col]]
}

# 4. Predict for each compound using the final trained models
predictions <- lapply(names(results_all_rf_BDL$final_models), function(compound) {
  predict(results_all_rf_BDL$final_models[[compound]], newdata = pred_df)
})
names(predictions) <- names(results_all_rf_BDL$final_models)

# 5. Back-transform from log10(x + 1) to ng/L
#    log10(x + 1) = y  -->  x = 10^y - 1
predictions_conc <- lapply(predictions, function(y) 10^y - 1)

# 6. Add predictions to flowlines
for (compound in names(predictions_conc)) {
  flowlines_pred[[compound]] <- predictions_conc[[compound]]
}

saveRDS(flowlines_pred, "flowlines_NE_predicted.rds")

library(foreign)

nhd_attrs <- read.dbf("C:/Users/Marston User/Documents/NE PFAS Data/NHDPlusV21_NE_01_NHDPlusAttributes_09/NHDPlusNE/NHDPlus01/NHDPlusAttributes/PlusFlowlineVAA.dbf")

# Check what's available
names(nhd_attrs)

# Join to flowlines
flowlines_pred <- flowlines_pred %>%
  left_join(nhd_attrs %>% 
              dplyr::select(ComID, TotDASqKM) %>%
              rename(COMID = ComID),
            by = "COMID")

# Check join worked
summary(flowlines_pred$TotDASqKM)



library(wesanderson)
library(rlang)
library(scales)
#Plotting ALL the PFAS compounds

# this is the colour palette for legend (dark blue and wes anderson colors)
pal <- colorRampPalette(c("#012A4A",wes_palette("Zissou1", type = "continuous"),
                          "#8B0000"))(256)
obs_sf <- st_as_sf(
  water_nozero %>% filter(!is.na(Latitude), !is.na(Longitude)),
  coords = c("Longitude", "Latitude"), crs = 4326
) %>%
  st_transform(crs = st_crs(flowlines_pred))

library(tigris)
ne_states <- states(cb = TRUE) %>%
  filter(STUSPS %in% c("ME","NH","VT","MA","RI","CT")) %>%
  st_transform(crs = st_crs(flowlines_pred))
out_dir <- "C:/Users/Marston User/Documents/NE PFAS Data/NE_Maps"


for (compound in all_pfas) {
  if (!compound %in% names(predictions)) next
  
  pred_col <- paste0(compound, "_pred")
  obs_col  <- compound
  
  r2_val <- results_all_rf_BDL$cv_results[[compound]]$r2
  r2_label <- if (is.na(r2_val)) "R² = NA" else paste0("R² = ", round(r2_val, 3))
  
  cat_col <- paste0(compound, "_cat")
  flowlines_pred[[cat_col]] <- cut(flowlines_pred[[compound]],
                                   breaks = c(0, 1, 4, 10, 50, 100, Inf),
                                   labels = c("<1", "1-4", "4-10", "10-50", "50-100", ">100"),
                                   include.lowest = TRUE)
  
  p <- ggplot() +
    geom_sf(data = ne_states, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = flowlines_pred,
            aes(color = .data[[cat_col]],
                linewidth = sqrt(TotDASqKM)),  # sqrt dampens extreme values
            show.legend = TRUE) +
    scale_linewidth_continuous(
      name   = "Watershed Area (km²)",
      range  = c(0.1, 1.5),
      breaks = c(0, 40, 80, 120, 160),
      labels = c("0", "1,600", "6,400", "14,400", "25,600")
    )+
    scale_color_manual(
      values = c("<1"     = "#012A4A",
                 "1-4"   = "#2196C4",
                 "4-10"  = "#F5C842",
                 "10-50" = "#E87D2B",
                 "50-100"= "red",
                 ">100"  = "#8B0000"),
      name = paste0(compound, " (ng/L)"),  # dynamic legend title
      na.value = "grey80") +
    guides(color = guide_legend(  # guide_legend not guide_colorbar for discrete
      override.aes = list(linewidth = 3),
      title.position = "top")) +
    labs(title = paste("Predicted",compound,"across New England streams")) +
    # labs(subtitle = r2_label) +
    labs(caption = "EPA Maximum Contamination Level: 4 ng/L for PFOA (2026)")+
    theme_classic() +
    theme(
      plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text     = element_blank(),
      axis.ticks    = element_blank(),
      axis.title    = element_blank(),
      legend.title  = element_text(size = 13, face = "bold"),
      legend.text   = element_text(size = 12),
      legend.position = "right",
      plot.margin = ggplot2::margin(t = 6, r = 6, b = 6, l = 6))+
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())
  ggsave(
    filename = file.path(out_dir, paste0(compound, "_NE_map.png")),
    plot = p,
    width = 8, height = 6, dpi = 300
  )
  
  message("Saved: ", compound)
}


st_write(flowlines_pred, "flowlines_pred.gpkg", driver = "GPKG")

install.packages("fastshap")
library(fastshap)

# Pull the trained model and training data
pfoa_model <- results_all_rf_BDL$final_models[["PFOA"]]
train_data  <- results_all_rf_BDL$preds_df[complete.cases(results_all_rf_BDL$preds_df), ]

# need randomForest loaded for predict to work but detach after
library(randomForest)

# Define prediction wrapper
pred_fun <- function(object, newdata) predict(object, newdata = newdata)

# Compute SHAP values
set.seed(42)
#only doing the shap for 1000 samples instead of 4000+, 
#I should get the general trend with a subset without having to run for as long
train_sample <- train_data[sample(nrow(train_data), 1000), ]

library(doParallel)
cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

shap_values <- explain(
  object       = pfoa_model,
  X            = train_sample,
  pred_wrapper = pred_fun,
  nsim         = 50,
  adjust       = TRUE,
  .parallel    = TRUE
)

stopCluster(cl)

shap_df <- as.data.frame(shap_values)

library(tidyverse)
shap_importance <- shap_df %>%
  summarise(across(everything(), ~ mean(abs(.)))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "mean_shap") %>%
  arrange(desc(mean_shap))

ggplot(shap_importance, aes(x = reorder(predictor, mean_shap), y = mean_shap)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  coord_flip() +
  labs(title = "SHAP Feature Importance — PFOA",
       x     = NULL,
       y     = "Mean |SHAP value|") +
  theme_publication(base_size = 11)

ggsave("fig3_shap_pfoa.png", width = 8, height = 6, dpi = 300, bg = "white")

library(shapviz)
library(randomForest)

# Get the StreamCat values for Manchester hotspot
manchester_nhd <- flowlines_pred %>%
  st_drop_geometry() %>%
  filter(COMID == 6746428) %>%
  dplyr::select(all_of(sc_preds_found)) %>%
  slice(1)

# Verify prediction (log10 scale)
pred_log <- predict(pfoa_model, newdata = manchester_nhd)
cat("Predicted (log10 scale):", round(pred_log, 3), "\n")
cat("Predicted (ng/L):", round(10^pred_log - 1, 1), "\n")

# Compute SHAP for this single observation
set.seed(42)
shap_single <- explain(
  object       = pfoa_model,
  X            = train_data,
  newdata      = manchester_nhd,
  pred_wrapper = function(object, newdata) predict(object, newdata = newdata),
  nsim         = 100,
  adjust       = TRUE
)

rename_map <- c(
  "Superfund Density"      = "superfunddensws",
  "% Impervious Surface"   = "pctimp2019ws",
  "% Wetland"              = "pcthbwet2019ws",
  "Septic Systems"         = "septicws",
  "Housing Density"        = "huden2010ws",
  "% Barren Land"          = "pctbl2019ws",
  "Toxic Release Sites"    = "tridensws",
  "% Deciduous Forest"     = "pctdecid2019ws",
  "Fertilizer Application" = "fertws",
  "Base Flow Index"        = "bfiws",
  "% Ag Drainage"          = "pctagdrainagews",
  "NPDES Density"          = "npdesdensws",
  "Manure Application"     = "manurews",
  "Canal Density"          = "canaldensws",
  "Dam Density"            = "damdensws",
  "% Cropland"             = "pctcrop2019ws",
  "Mine Density"           = "minedensws",
  "Coal Mine Density"      = "coalminedensws"
)

# Rename SHAP and feature values
shap_single_renamed  <- as.data.frame(shap_single) %>% rename(!!!rename_map)
manchester_nhd_renamed <- manchester_nhd %>% rename(!!!rename_map)

# Baseline on log10 scale (mean prediction across training data)
baseline <- mean(predict(pfoa_model, newdata = train_data))

sv_single <- shapviz(
  object   = as.matrix(shap_single_renamed),
  X        = manchester_nhd_renamed,
  baseline = baseline
)

# Correct back-transform: log10 scale -> ng/L
pred_ngL <- round(10^pred_log - 1, 1)

p_wf <- sv_waterfall(sv_single, row_id = 1, max_display = 10)

# force fill color in the geom layer
for (i in seq_along(p_wf$layers)) {
  p_wf$layers[[i]]$aes_params$fill <- "#8B0000"
  p_wf$layers[[i]]$aes_params$colour <- "#5a0000"
}

p_wf +
  scale_x_continuous(
    name   = "Predicted PFOA concentration (ng/L)",
    labels = function(x) round(10^x - 1, 1),
    breaks = log10(c(1, 5, 10, 50, 100, 200, 300) + 1)
  ) +
  labs(title    = "SHAP Waterfall — Manchester, NH Hotspot",
       subtitle = paste0("Predicted PFOA: ", pred_ngL, " ng/L"),
       caption  = "Bar labels show multiplicative effect on predicted concentration (10^SHAP)") +
  theme_classic(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 17, hjust = 0),
    plot.subtitle      = element_text(color = "grey40", size = 14, hjust = 0),
    axis.text.y        = element_text(size = 13, color = "black", face = "italic"),
    axis.text.x        = element_text(size = 12, color = "black"),
    axis.title.x       = element_text(size = 13, margin = ggplot2::margin(t = 8)),
    axis.title.y       = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(color = "black", linewidth = 0.4),
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    plot.margin        = ggplot2::margin(15, 30, 10, 10)
  )

ggsave("fig4_shap_waterfall_manchester.png",
       width = 11, height = 6, dpi = 300, bg = "white")

# Beeswarm importance plot using full training SHAP
shap_df <- as.data.frame(shap_values)
shap_renamed <- shap_df %>% rename(!!!rename_map)
train_renamed <- train_data %>% rename(!!!rename_map)

sv <- shapviz(
  object   = as.matrix(shap_renamed),
  X        = train_renamed,
  baseline = baseline
)

sv_importance(sv, kind = "beeswarm") +
  labs(title = "SHAP analysis of PFOA predictors") +
  theme_classic(base_size = 12)

sv_importance(sv, kind = "beeswarm") +
  labs(
    title   = "SHAP feature importance — PFOA",
    caption = "SHAP values represent contribution to log₁₀(PFOA + 1) prediction"
  ) +
  xlab("SHAP value") +
  theme_classic(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 15, hjust = 0),
    plot.caption       = element_text(color = "grey50", size = 9, hjust = 0),
    axis.text.y        = element_text(size = 11, color = "black", face = "italic"),
    axis.text.x        = element_text(size = 10, color = "black"),
    axis.title.x       = element_text(size = 11, margin = ggplot2::margin(t = 8)),
    axis.title.y       = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(color = "black", linewidth = 0.4),
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 9),
    plot.margin        = ggplot2::margin(15, 20, 10, 10)
  )

ggsave("fig5_shap_beeswarm_pfoa.png",
       width = 10, height = 7, dpi = 300, bg = "white")

# Pull PFOS model and training data
pfos_model <- results_all_rf_BDL$final_models[["PFOS"]]

valid_idx_pfos <- complete.cases(results_all_rf_BDL$preds_df) &
  !is.na(results_all_rf_BDL$water_input[["PFOS"]])

train_data_pfos <- results_all_rf_BDL$preds_df[valid_idx_pfos, ]

# Compute SHAP for PFOS
set.seed(42)
cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)
library(randomForest)

set.seed(42)
sample_idx <- sample(nrow(train_data_pfos), 500)
train_sample_pfos <- train_data_pfos[sample_idx, ]

shap_pfos <- explain(
  object       = pfos_model,
  X            = train_sample_pfos,
  pred_wrapper = function(object, newdata) randomForest:::predict.randomForest(object, newdata = newdata),
  nsim         = 50,
  adjust       = TRUE,
  parallel     = FALSE
)

detach("package:randomForest", unload = TRUE)

# rename using same sample
shap_pfos_df       <- as.data.frame(shap_pfos) %>% rename(!!!rename_map)
train_pfos_renamed <- train_sample_pfos %>% rename(!!!rename_map)

# drop any rows with NA in either
valid <- complete.cases(shap_pfos_df) & complete.cases(train_pfos_renamed)
shap_pfos_df       <- shap_pfos_df[valid, ]
train_pfos_renamed <- train_pfos_renamed[valid, ]

baseline_pfos <- mean(predict(
  results_all_rf_BDL$final_models[["PFOS"]],
  newdata = train_data_pfos
))

sv_pfos <- shapviz(
  object   = as.matrix(shap_pfos_df),
  X        = train_pfos_renamed,
  baseline = baseline_pfos
)

# Plot both beeswarms
p_pfoa_bee <- sv_importance(sv, kind = "beeswarm") +
  labs(title = "PFOA") +
  xlab("SHAP value") +
  xlim(-0.3, 1.1) +
  theme_classic(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y        = element_text(size = 10, color = "black", face = "italic"),
    axis.text.x        = element_text(size = 9),
    axis.title.x       = element_text(size = 10, margin = ggplot2::margin(t = 8)),
    axis.title.y       = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.line.x        = element_line(color = "black", linewidth = 0.4),
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    plot.margin        = ggplot2::margin(10, 15, 10, 10)
  )

p_pfos_bee <- sv_importance(sv_pfos, kind = "beeswarm") +
  labs(title = "PFOS") +
  xlab("SHAP value") +
  xlim(-0.3, 1.1) +  # same x axis as PFOA for direct comparison
  theme_classic(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y        = element_text(size = 10, color = "black", face = "italic"),
    axis.text.x        = element_text(size = 9),
    axis.title.x       = element_text(size = 10, margin = ggplot2::margin(t = 8)),
    axis.title.y       = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.line.x        = element_line(color = "black", linewidth = 0.4),
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    plot.margin        = ggplot2::margin(10, 15, 10, 10)
  )

# combine side by side
library(patchwork)
p_combined <- p_pfoa_bee + p_pfos_bee +
  plot_annotation(
    title    = "SHAP feature importance — PFOA vs PFOS",
    caption  = "SHAP values represent contribution to log_19 (concentration + 1) prediction",
    theme    = theme(
      plot.title   = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.caption = element_text(color = "grey50", size = 9, hjust = 0)
    )
  )
p_combined
ggsave("fig6_shap_pfoa_vs_pfos.png", p_combined,
       width = 16, height = 7, dpi = 300, bg = "white")
detach("package:randomForest", unload = TRUE)

group_map <- list(
  Urban = c("% Impervious Surface", "Housing Density"),
  
  Industrial = c("Superfund Density",
                 "Toxic Release Sites",
                 "NPDES Density",
                 "Mine Density",
                 "Canal Density",
                 "Dam Density"),
  Agriculture = c("Fertilizer Application",
                  "Manure",
                  "% Cropland",
                  "% Ag Drainage"),
  Natural = c("% Deciduous Forest",
              "% Wetland",
              "% Barren Land")
)
group_shap <- sapply(group_map, function(vars) {
  rowSums(shap_renamed[, vars, drop = FALSE], na.rm = TRUE)
})

group_shap <- as.data.frame(group_shap)
sv <- shapviz(
  object = as.matrix(shap_renamed), 
  X      = train_renamed
)
sv_group <- shapviz(
  object = as.matrix(group_shap),
  X      = train_renamed %>% 
    mutate(
      Urban = group_shap$Urban,
      Industrial = group_shap$Industrial,
      Agriculture = group_shap$Agriculture,
      Natural = group_shap$Natural
    )
)
sv_importance(sv_group, kind = "beeswarm") +
  labs(
    title = "Grouped SHAP analysis of PFOA predictors",
    x = "SHAP value (group contribution)"
  ) +
  theme_classic(base_size = 12)

# Find the COMID for the Manchester hotspot
# From your hotspot table, the Merrimack, NH hotspot is COMID 6745812
# Take first manchester index
manchester_idx <- which(results_nozero$water$COMID == 19334991)[1]

# Find its position in the valid training rows
valid_rows <- which(complete.cases(results_nozero$preds_df) & 
                      !is.na(results_nozero$water$PFOA))
manchester_train_idx <- which(valid_rows == manchester_idx)
manchester_train_idx  # check it returns a single value

baseline <- mean(predict(pfoa_model, newdata = train_data))

sv <- shapviz(
  object   = as.matrix(shap_renamed),
  X        = train_renamed,
  baseline = baseline
)

sv_waterfall(sv, row_id = manchester_train_idx, max_display = 16) +
  labs(title    = "SHAP Waterfall — Berlin, NH Hotspot",
       subtitle = paste0("Predicted: 224 ng/L PFOA")) +
  theme_classic(base_size = 12)


# Get StreamCat predictors for COMID 19334991
nhd_19334991 <- flowlines_pred %>%
  st_drop_geometry() %>%
  filter(COMID == 19334991) %>%
  dplyr::select(all_of(sc_preds_found)) %>%
  slice(1)

# Predict in log1p space
log_pred <- predict(pfoa_model, newdata = nhd_19334991)
log_pred

# Back transform to ng/L
expm1(log_pred)  # should be ~224

nhd_19334991 <- flowlines_pred %>%
  st_drop_geometry() %>%
  filter(COMID == 19334991) %>%
  dplyr::select(all_of(sc_preds_found)) %>%
  slice(1)

set.seed(42)
shap_single <- explain(
  object       = pfoa_model,
  X            = train_data,
  newdata      = nhd_19334991,
  pred_wrapper = function(object, newdata) predict(object, newdata = newdata),
  nsim         = 100,
  adjust       = TRUE
)

shap_single_renamed <- as.data.frame(shap_single) %>% rename(!!!rename_map)
nhd_renamed <- nhd_19334991 %>% rename(!!!rename_map)

sv_single <- shapviz(
  object   = as.matrix(shap_single_renamed),
  X        = nhd_renamed,
  baseline = mean(predict(pfoa_model, newdata = train_data))
)

sv_waterfall(sv_single, row_id = 1, max_display = 16) +
  labs(title    = "SHAP Waterfall — Berlin, NH Hotspot",
       subtitle = "Predicted: 224.6 ng/L PFOA") +
  theme_classic(base_size = 12)
# Back-transform baseline and SHAP values
baseline_log  <- mean(predict(pfoa_model, newdata = train_data))
baseline_conc <- expm1(baseline_log)

# Back-transform SHAP values — need to be additive in ng/L space
# Approach: scale SHAP values proportionally to the ng/L prediction
pred_log  <- as.numeric(predict(pfoa_model, newdata = nhd_19334991))
pred_conc <- expm1(pred_log)

# Scale SHAP values from log space to ng/L space proportionally
shap_matrix <- as.matrix(shap_single_renamed)
shap_scaled <- shap_matrix * (pred_conc - baseline_conc) / sum(shap_matrix)

sv_single_conc <- shapviz(
  object   = shap_scaled,
  X        = nhd_renamed,
  baseline = baseline_conc
)

sv_waterfall(sv_single_conc, row_id = 1, max_display = 16) +
  labs(title    = "SHAP Waterfall — Berlin, NH Hotspot",
       subtitle = "Predicted: 224.6 ng/L PFOA") +
  scale_x_continuous(labels = scales::comma) +
  theme_classic(base_size = 12)


library(cowplot)

make_inset <- function(bbox, label) {
  crop_box <- st_bbox(bbox, crs = st_crs(flowlines_pred)) %>% st_as_sfc()
  
  ggplot() +
    geom_sf(data = st_crop(flowlines_pred, crop_box),
            aes(color = PFOA_cat, linewidth = sqrt(TotDASqKM)),
            show.legend = FALSE) +
    geom_sf(data = st_crop(ne_states, crop_box),
            fill = NA, color = "black", linewidth = 0.5) +
    scale_color_manual(
      values = c("<1"     = "#012A4A",
                 "1-4"   = "#2196C4",
                 "4-10"  = "#F5C842",
                 "10-50" = "#E87D2B",
                 "50-100"= "red",
                 ">100"  = "#8B0000"),
      na.value = "grey80") +
    scale_linewidth(range = c(0.3, 2)) +
    labs(title = label) +
    theme_void() +
    theme(
      plot.title   = element_text(size = 10, face = "bold", hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )
}

# Tighter boxes centered on actual hotspot coordinates
maine_bbox <- c(xmin = -70.0, xmax = -69.3, ymin = 44.3, ymax = 44.7)
nh_bbox <- c(xmin = -71.5, xmax = -70.7, ymin = 44.2, ymax = 44.7)
mass_bbox  <- c(xmin = -71.8, xmax = -71.1, ymin = 42.7, ymax = 43.1)


library(maptiles)
library(tidyterra)

# Get satellite tiles for your bbox
maine_sf <- st_as_sfc(st_bbox(maine_bbox, crs = 4326))
# Add terrain/topo background instead
tiles <- get_tiles(maine_sf, provider = "Stadia.StamenTerrain", zoom = 10)

inset_maine <- make_inset(maine_bbox, "Penobscot River, ME")
inset_nh <- make_inset(nh_bbox, "Saco River, NH")
inset_mass  <- make_inset(mass_bbox,  "Merrimack River, MA")

inset_row <- plot_grid(inset_maine, inset_nh, inset_mass, nrow = 1)
inset_row
# Save just the inset row first to check
ggsave("insets_check.png", inset_row,
       width = 12, height = 4, dpi = 300, bg = "white",
)



