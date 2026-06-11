# HydroDL ML Modeling of Cape fear basin
# RV 5/29/26
# To be run in Unity Cluster

# Cape Fear watershed — get from MERIT basins already on cluster
# pfaf codes covering Cape Fear are in the 73xxx range
# You can also use the HUC8 boundary via nhdplusTools locally
# =============================================================
# HydroDL + StreamCat ML Model — Cape Fear Basin
# updated RV 6/11/26
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

conus_data <- readRDS("conus_datasets_COMID.rds")

# Filter to Petre Cape Fear sites only for training
petre_data <- conus_data %>%
  filter(dataset_source == "Petre_2022") %>%
  mutate(date = as.Date(`Sample Date (MM/DD/YYY)`, 
                        format = "%m/%d/%Y"))

cat("Petre sites:", n_distinct(petre_data$COMID), "\n")
cat("Petre observations:", nrow(petre_data), "\n")

# Site coordinates with MERIT IDs
site_coords_clean <- read_csv("site_coords.csv")

compounds <- c("PFOA", "PFOS", "PFBS", "PFHxA", "PFHxS")

# =============================================================
# STEP 2: GET CAPE FEAR WATERSHED BOUNDARY
# =============================================================

cape_fear_hucs <- c("03030002")
# Only doing the Haw SubBasin for now,
# since my 13 Petre sites are all in this basin
# except for one far far downstream.
# It's 1,707.6 sq miles, and includes Greensboro

cape_fear_wbd <- get_huc(id   = cape_fear_hucs,
                         type = "huc08")

cape_fear_wbd_4326 <- st_transform(cape_fear_wbd, 4326)

# =============================================================
# STEP 3: LOAD MERIT RIVER NETWORK FOR CAPE FEAR
# NOTE: using v01 (not bugfix1) to match GRADES-HydroDL NetCDF
# =============================================================

cape_fear_rivers <- st_read(
  "/nas/cee-ice/data/MERIT_Basins/MERIT_Hydro_v07_Basins_v01/pfaf_level_02/pfaf_73_MERIT_Hydro_v07_Basins_v01/riv_pfaf_73_MERIT_Hydro_v07_Basins_v01.shp",
  quiet = TRUE
)

# Reproject watershed to match MERIT CRS
cape_fear_wbd_proj <- st_transform(cape_fear_wbd,
                                   st_crs(cape_fear_rivers))

# Clip MERIT network to Cape Fear watershed
cape_fear_network <- st_intersection(cape_fear_rivers,
                                     cape_fear_wbd_proj)

cat("Total MERIT reaches in Cape Fear:", nrow(cape_fear_network), "\n")

# Fetch NHD flowlines for Cape Fear
cape_fear_flowlines <- get_nhdplus(
  AOI         = cape_fear_wbd_4326,
  realization = "flowline"
)

cape_fear_flowlines_proj <- st_transform(cape_fear_flowlines,
                                         st_crs(cape_fear_network))

cat("NHD flowlines retrieved:", nrow(cape_fear_flowlines_proj), "\n")

save(cape_fear_flowlines, cape_fear_flowlines_proj,
     file = "~/cape_fear_flowlines.RData")

# =============================================================
# STEP 4: EXTRACT HYDRODL DISCHARGE FOR ALL CAPE FEAR REACHES
# NOTE: block read with correct column alignment via actual rivids
# =============================================================

nc <- nc_open(
  "/nas/cee-ice/data/GRADES_hydroDL/output_pfaf_07_1979_2023.nc",
  readunlim = FALSE
)

all_rivids <- ncvar_get(nc, "rivid")
time_vals  <- ncvar_get(nc, "time")
dates      <- as.Date(time_vals, origin = "1979-01-01")

cape_fear_rivids <- unique(cape_fear_network$COMID)
target_idx       <- which(all_rivids %in% cape_fear_rivids)

cat("Cape Fear reaches found in NetCDF:",
    length(target_idx), "of", length(cape_fear_rivids), "\n")

# Single block read
discharge_matrix <- ncvar_get(
  nc, "Qout",
  start = c(min(target_idx), 1),
  count = c(max(target_idx) - min(target_idx) + 1, -1)
)
nc_close(nc)

# Use actual rivids at each position in the block as column names —
# NOT just the Cape Fear rivids, which would misalign across the gaps
actual_rivids_in_block <- all_rivids[min(target_idx):max(target_idx)]

discharge_long <- as.data.frame(t(discharge_matrix)) %>%
  setNames(as.character(actual_rivids_in_block)) %>%
  mutate(date = dates) %>%
  select(date, any_of(as.character(cape_fear_rivids))) %>%
  pivot_longer(
    -date,
    names_to  = "merit_COMID",
    values_to = "Flow_cms"
  ) %>%
  mutate(merit_COMID = as.integer(merit_COMID))

cat("Discharge records:", nrow(discharge_long), "\n")
cat("Unique reaches:   ", n_distinct(discharge_long$merit_COMID), "\n")
rm(discharge_matrix)

# Sanity check — mainstem should show ~60-70 m3/s mean flow
discharge_long %>%
  filter(merit_COMID %in% (cape_fear_network %>%
                             st_drop_geometry() %>%
                             slice_max(uparea, n = 3) %>%
                             pull(COMID))) %>%
  group_by(merit_COMID) %>%
  summarise(mean_flow = mean(Flow_cms, na.rm = TRUE),
            max_flow  = max(Flow_cms,  na.rm = TRUE)) %>%
  left_join(cape_fear_network %>%
              st_drop_geometry() %>%
              select(COMID, uparea),
            by = c("merit_COMID" = "COMID")) %>%
  arrange(desc(uparea))

# =============================================================
# STEP 5: NHD → MERIT CROSSWALK
# Area-aware scoring: geometry * drainage area similarity.
# Large NHD reaches (>500 km²) use area only — geometry unreliable
# at MERIT's coarser resolution for mainstem segments.
# =============================================================

merit_buffered <- st_buffer(cape_fear_network, dist = 100)

nhd_on_merit <- st_filter(cape_fear_flowlines_proj, merit_buffered,
                          .predicate = st_intersects)

cat("NHD flowlines total:          ", nrow(cape_fear_flowlines_proj), "\n")
cat("NHD flowlines on MERIT rivers:", nrow(nhd_on_merit), "\n")
cat("Dropped (off-MERIT):          ",
    nrow(cape_fear_flowlines_proj) - nrow(nhd_on_merit), "\n")

nhd_to_merit <- map_dfr(
  seq_len(nrow(nhd_on_merit)),
  function(i) {
    
    nhd_line   <- nhd_on_merit[i, ]
    nhd_id     <- nhd_line$comid
    nhd_uparea <- nhd_line$totdasqkm
    
    candidates_idx <- which(st_intersects(
      merit_buffered, nhd_line, sparse = FALSE)[, 1])
    
    candidates <- cape_fear_network[candidates_idx, ]
    
    if (nrow(candidates) == 0) return(tibble(
      nhd_COMID   = nhd_id,
      merit_COMID = NA_real_,
      method      = "no_candidate"
    ))
    
    merit_uparea <- candidates$uparea
    
    area_score <- if (!is.null(nhd_uparea) && !is.na(nhd_uparea) && nhd_uparea > 0) {
      exp(-abs(log(merit_uparea / nhd_uparea)))
    } else {
      rep(1, nrow(candidates))
    }
    
    if (!is.null(nhd_uparea) && !is.na(nhd_uparea) && nhd_uparea > 500) {
      # Large reaches: area similarity only
      best   <- which.max(area_score)
      method <- "area_only"
    } else {
      # Small tributaries: geometry * area similarity
      overlap_len <- map_dbl(seq_len(nrow(candidates)), function(j) {
        inter <- st_intersection(
          st_geometry(nhd_line),
          st_buffer(candidates[j, ], dist = 100)
        )
        if (length(inter) == 0 || st_is_empty(inter)) return(0)
        as.numeric(st_length(inter))
      })
      best   <- which.max(overlap_len * area_score)
      method <- if (!is.null(nhd_uparea) && !is.na(nhd_uparea) && nhd_uparea > 0) {
        "combined"
      } else {
        "geometry_only"
      }
    }
    
    tibble(
      nhd_COMID        = nhd_id,
      merit_COMID      = candidates$COMID[best],
      nhd_uparea_km2   = nhd_uparea,
      merit_uparea_km2 = merit_uparea[best],
      area_ratio       = merit_uparea[best] / nhd_uparea,
      method           = method
    )
  }
)

cat("Crosswalk built:", nrow(nhd_to_merit), "NHD reaches\n")
cat("Unique MERIT COMIDs:", n_distinct(nhd_to_merit$merit_COMID), "\n")
cat("Method split:\n")
print(count(nhd_to_merit, method))

save(nhd_to_merit, file = "~/nhd_to_merit.RData")

# =============================================================
# STEP 6: VERIFY CROSSWALK COVERAGE FOR PETRE SITES
# =============================================================

petre_crosswalk <- nhd_to_merit %>%
  filter(nhd_COMID %in% unique(petre_data$COMID))

cat("Petre sites in crosswalk:", n_distinct(petre_crosswalk$nhd_COMID), "\n")
cat("Expected:                ", n_distinct(petre_data$COMID), "\n")

missing <- setdiff(unique(petre_data$COMID), petre_crosswalk$nhd_COMID)
cat("Missing COMIDs:          ", paste(missing, collapse = ", "), "\n")
#need to remove 8834930 and 8896388 sites

# Visual check — NHD reaches colored by MERIT parent
sites_sf_petre <- petre_data %>%
  distinct(COMID, Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_filter(cape_fear_wbd_4326, .predicate = st_within) %>%
  st_transform(st_crs(cape_fear_network)) %>%
  mutate(site_num = row_number())

cat("Petre sites inside HUC8:", nrow(sites_sf_petre), "\n")

nhd_for_plot <- nhd_on_merit %>%
  left_join(nhd_to_merit, by = c("comid" = "nhd_COMID")) %>%
  mutate(merit_COMID_f = factor(merit_COMID))

ggplot() +
  geom_sf(data = cape_fear_network,
          color = "steelblue", linewidth = 1.4, alpha = 0.5) +
  geom_sf(data = cape_fear_flowlines_proj %>%
            filter(!comid %in% nhd_to_merit$nhd_COMID),
          color = "red", linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = nhd_for_plot,
          aes(color = merit_COMID_f), linewidth = 0.4) +
  geom_sf(data = sites_sf_petre,
          shape = 21, size = 3,
          fill = "yellow", color = "black", stroke = 0.8) +
  geom_sf_label(data = sites_sf_petre,
                aes(label = COMID),
                size = 2.5, nudge_y = 0.015,
                label.padding = unit(0.15, "lines")) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  labs(title    = "NHD reaches on MERIT network — Petre monitoring sites",
       subtitle = "Blue = MERIT backbone | Colored = NHD on MERIT | Red = dropped") +
  theme_minimal()

# Join mean flow to the NHD reaches via crosswalk
mean_flow_by_merit <- discharge_long %>%
  group_by(merit_COMID) %>%
  summarise(mean_flow = mean(Flow_cms, na.rm = TRUE))

nhd_flow_plot <- nhd_on_merit %>%
  left_join(nhd_to_merit, by = c("comid" = "nhd_COMID")) %>%
  left_join(mean_flow_by_merit, by = "merit_COMID")

ggplot() +
  # Red = dropped NHD reaches (no flow data)
  geom_sf(data = cape_fear_flowlines_proj %>%
            filter(!comid %in% nhd_to_merit$nhd_COMID),
          color = "grey80", linewidth = 0.2, alpha = 0.5) +
  # Retained NHD reaches colored by mean flow
  geom_sf(data = nhd_flow_plot %>% arrange(mean_flow),
          aes(color = mean_flow, linewidth = mean_flow)) +
  # Petre sites
  geom_sf(data = sites_sf_petre,
          shape = 21, size = 3,
          fill = "yellow", color = "black", stroke = 0.8) +
  geom_sf_label(data = sites_sf_petre,
                aes(label = COMID),
                size = 2.5, nudge_y = 0.015,
                label.padding = unit(0.15, "lines")) +
  scale_color_viridis_c(
    option    = "plasma",
    trans     = "log10",               # log scale — flow spans orders of magnitude
    name      = "Mean flow\n(m³/s)",
    labels    = scales::comma
  ) +
  scale_linewidth_continuous(
    range  = c(0.2, 2.0),             # thin tributaries, thick mainstem
    trans  = "log10",
    guide  = "none"
  ) +
  labs(title    = "Mean HydroDL discharge per NHD reach",
       subtitle = "Via NHD→MERIT crosswalk | log10 color scale") +
  theme_minimal()

#this took so long to get right!
# so #Hashtag Happy!!

# =============================================================
# STEP 7: BUILD FLOW FEATURES FOR ALL MERIT REACHES
# =============================================================

discharge_features_all <- discharge_long %>%
  mutate(Flow_cms = pmax(Flow_cms, 0)) %>%
  arrange(merit_COMID, date) %>%
  group_by(merit_COMID) %>%
  mutate(
    log_flow      = log1p(Flow_cms),
    flow_7day     = rollmean(Flow_cms, 7,  fill = NA, align = "right"),
    flow_30day    = rollmean(Flow_cms, 30, fill = NA, align = "right"),
    log_flow_7day = log1p(flow_7day),
    doy           = as.integer(format(date, "%j"))
  ) %>%
  group_by(merit_COMID, doy) %>%
  mutate(
    flow_seas_mean = mean(Flow_cms, na.rm = TRUE),
    flow_seas_sd   = sd(Flow_cms,   na.rm = TRUE),
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

cat("Flow features built:", nrow(discharge_features_all), "rows\n")

# =============================================================
# STEP 8: GET STREAMCAT FOR ALL CAPE FEAR NHD REACHES
# =============================================================

cape_fear_nhd_comids <- unique(na.omit(nhd_to_merit$nhd_COMID))
cat("Pulling StreamCat for", length(cape_fear_nhd_comids), "COMIDs\n")

sc_chunks <- split(cape_fear_nhd_comids,
                   ceiling(seq_along(cape_fear_nhd_comids) / 150))

streamcat_request <- c(
  "pctagdrainage", "pctimp2019",   "pcturblo2019", "pcturbhi2019",
  "pcturbmd2019",  "pctgrs2019",   "runoff",        "precip9120",
  "pctcrop2019",   "pctmxfst2019", "pcthbwet2019",  "npdesdens",
  "huden2010",     "canaldens",    "minedens",       "coalminedens",
  "manure",        "fert",         "damdens",        "septic",
  "bfi",           "pctow2019",    "tmax9120",       "superfunddens",
  "tridens",       "wwtpminordens","wwtpmajordens",  "popden2010",
  "elev",          "pctshrb2019",  "pctconif2019"
)

# Watershed scale — takes ~2-3 minutes for 10 chunks
sc_list_ws <- list()
for (i in seq_along(sc_chunks)) {
  message("StreamCat WS chunk ", i, " / ", length(sc_chunks))
  res <- tryCatch(
    sc_get_data(
      metric = paste(streamcat_request, collapse = ","),
      aoi    = "watershed",
      comid  = paste(sc_chunks[[i]], collapse = ",")
    ),
    error = function(e) { message("error: ", e$message); NULL }
  )
  if (!is.null(res)) {
    names(res) <- tolower(names(res))
    sc_list_ws[[i]] <- res
  }
  Sys.sleep(2)
}

# Catchment scale (point sources only) — takes ~2-3 minutes
sc_list_cat <- list()
for (i in seq_along(sc_chunks)) {
  message("StreamCat CAT chunk ", i, " / ", length(sc_chunks))
  res <- tryCatch(
    sc_get_data(
      metric = paste(c("npdesdens", "tridens", "superfunddens",
                       "wwtpmajordens", "wwtpminordens"),
                     collapse = ","),
      aoi    = "catchment",
      comid  = paste(sc_chunks[[i]], collapse = ",")
    ),
    error = function(e) { message("error: ", e$message); NULL }
  )
  if (!is.null(res)) {
    names(res) <- tolower(names(res))
    sc_list_cat[[i]] <- res
  }
  Sys.sleep(2)
}

# Combine and compute local vs. watershed point source ratios
streamcat_ws  <- bind_rows(sc_list_ws)
streamcat_cat <- bind_rows(sc_list_cat)

streamcat_ws$comid  <- as.integer(streamcat_ws$comid)
streamcat_cat$comid <- as.integer(streamcat_cat$comid)

streamcat_ws  <- streamcat_ws[!duplicated(streamcat_ws$comid), ]
streamcat_cat <- streamcat_cat[!duplicated(streamcat_cat$comid), ]

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
# STEP 9: BUILD SITES SF (needed for training data join)
# =============================================================

sites_sf <- petre_data %>%
  filter(COMID != 8834930) %>%
  filter(COMID != 8896388) %>%
  distinct(COMID, Latitude, Longitude) %>%
  arrange(COMID) %>%
  mutate(site_idx = row_number()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

sites_proj <- st_transform(sites_sf, 5070)

# =============================================================
# STEP 10: COMPUTE HYDROLOGIC DISTANCE TO UPSTREAM POINT SOURCES
# (WWTPs and TRI surface water dischargers)
# Step 10a: monitoring sites (fast)
# Step 10b: all prediction reaches (slow, checkpoint-based)
# =============================================================

get_upstream_fire_distance <- function(target_comid,
                                       fire_comids,
                                       max_dist_km = 200) {
  tryCatch({
    
    upstream_raw <- navigate_nldi(
      nldi_feature = list(featureSource = "comid",
                          featureID     = as.character(target_comid)),
      mode         = "upstreamMain",
      distance_km  = max_dist_km
    )
    
    upstream <- upstream_raw$UM_flowlines
    
    if (is.null(upstream) || nrow(upstream) == 0) {
      return(tibble(
        nhd_COMID          = target_comid,
        fire_upstream_flag = 0L,
        fire_hydro_dist_km = NA_real_,
        n_fire_upstream    = 0L
      ))
    }
    
    upstream_comids <- as.integer(upstream$nhdplus_comid)
    fire_upstream   <- intersect(upstream_comids, fire_comids)
    
    if (length(fire_upstream) == 0) {
      return(tibble(
        nhd_COMID          = target_comid,
        fire_upstream_flag = 0L,
        fire_hydro_dist_km = NA_real_,
        n_fire_upstream    = 0L
      ))
    }
    
    target_pl <- get_nhdplus(
      comid       = target_comid,
      realization = "flowline"
    ) %>%
      st_drop_geometry() %>%
      pull(pathlength)
    
    upstream_lines <- get_nhdplus(
      comid       = fire_upstream,
      realization = "flowline"
    ) %>%
      st_drop_geometry() %>%
      select(comid, pathlength)
    
    min_dist <- min(abs(target_pl - upstream_lines$pathlength), na.rm = TRUE)
    
    tibble(
      nhd_COMID          = target_comid,
      fire_upstream_flag = 1L,
      fire_hydro_dist_km = round(min_dist, 2),
      n_fire_upstream    = length(fire_upstream)
    )
    
  }, error = function(e) {
    message("Failed for COMID ", target_comid, ": ", e$message)
    tibble(
      nhd_COMID          = target_comid,
      fire_upstream_flag = NA_integer_,
      fire_hydro_dist_km = NA_real_,
      n_fire_upstream    = NA_integer_
    )
  })
}

# ── 10a: Monitoring sites ─────────────────────────────────────
site_comids <- unique(petre_data$COMID[petre_data$COMID != 8834930])

site_fire_hydro <- map_dfr(site_comids, function(c) {
  message("Processing site COMID: ", c)
  get_upstream_fire_distance(c, fire_comids)
})

print(site_fire_hydro %>% arrange(fire_hydro_dist_km))

# ── 10b: All prediction reaches (checkpoint-based) ────────────
pred_comids <- unique(cape_fear_nhd_proj$featureid)
output_file <- "/home/rvera_umass_edu/fire_hydro_dist_all_reaches.rds"

if (file.exists(output_file)) {
  completed        <- readRDS(output_file)
  remaining_comids <- setdiff(pred_comids, completed$nhd_COMID)
  cat("Resuming:", length(remaining_comids), "reaches remaining\n")
} else {
  completed        <- tibble()
  remaining_comids <- pred_comids
}

results_list <- vector("list", length(remaining_comids))

for (i in seq_along(remaining_comids)) {
  results_list[[i]] <- get_upstream_fire_distance(
    remaining_comids[i], fire_comids
  )
  if (i %% 100 == 0) {
    completed <- bind_rows(completed, bind_rows(results_list[1:i]))
    saveRDS(completed, output_file)
    message("Checkpoint: ", i, " of ", length(remaining_comids))
  }
}

nhd_fire_hydro <- bind_rows(completed, bind_rows(results_list)) %>%
  distinct(nhd_COMID, .keep_all = TRUE)

saveRDS(nhd_fire_hydro, output_file)
cat("Complete:", nrow(nhd_fire_hydro), "reaches\n")

# =============================================================
# STEP 11: BUILD TRAINING DATA (petre_with_flow)
# =============================================================
multi_source_hydro_dist <- readRDS("~/CONUS PFAS/multi_source_hydro_dist.rds")

petre_crosswalk <- nhd_to_merit %>%
  filter(nhd_COMID %in% unique(petre_data$COMID))

#8834930 is outside the study area, 8896388 simply doesn't have flow data
petre_with_flow <- petre_data %>%
  filter(COMID != 8834930) %>%
  filter(COMID != 8896388) %>%
  left_join(
    petre_crosswalk %>% select(nhd_COMID, merit_COMID),
    by = c("COMID" = "nhd_COMID")
  ) %>%
  left_join(discharge_features_all, by = c("merit_COMID", "date", "month", "year")) %>%
  left_join(site_ids, by = "COMID") %>%
  left_join(site_coords %>% select(COMID, spatial_weight), by = "COMID") %>%
  #left_join(cape_fear_streamcat, by = c("COMID" = "comid")) %>%   # <-- added
  left_join(
    multi_source_hydro_dist %>%
      select(nhd_COMID,
             wwtp_flag, wwtp_dist_km, wwtp_n_upstream, wwtp_intensity_nearest,
             tri_sw_flag, tri_sw_dist_km, tri_sw_n_upstream, tri_sw_intensity_nearest),
    by = c("COMID" = "nhd_COMID")
  ) %>%
  replace_na(list(
    wwtp_flag = 0, wwtp_n_upstream = 0, wwtp_intensity_nearest = 0,
    tri_sw_flag = 0, tri_sw_n_upstream = 0, tri_sw_intensity_nearest = 0
  )) %>%
  mutate(
    wwtp_dist_km   = if_else(wwtp_flag == 0, 9999, wwtp_dist_km),
    tri_sw_dist_km = if_else(tri_sw_flag == 0, 9999, tri_sw_dist_km)
  ) %>%
  filter(!is.na(log_flow))

dup_cols <- names(petre_with_flow)[grepl("\\.(x|y)$", names(petre_with_flow))]
cat("Duplicate columns:        ", length(dup_cols), "\n")
cat("Sites:                    ", n_distinct(petre_with_flow$COMID), "\n")
cat("Observations:             ", nrow(petre_with_flow), "\n")
cat("Obs with flow:            ", sum(!is.na(petre_with_flow$Flow_cms)), "\n")
cat("Obs with StreamCat:       ", sum(!is.na(petre_with_flow$npdesdensws)), "\n")
cat("Obs with wwtp_dist_km:    ", sum(!is.na(petre_with_flow$wwtp_dist_km)), "\n")
cat("Obs with tri_sw_dist_km:  ", sum(!is.na(petre_with_flow$tri_sw_dist_km)), "\n")

# =============================================================
# STEP 12: BUILD PREDICTION GRID (NHD RESOLUTION, 2020-2021)
# =============================================================

prediction_grid <- discharge_features_all %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2021-12-31")) %>%
  left_join(nhd_to_merit, by = "merit_COMID",         # fixed: was nhd_to_merit_v3
            relationship = "many-to-many") %>%
  left_join(cape_fear_streamcat, by = c("nhd_COMID" = "comid")) %>%
  left_join(
    multi_source_hydro_dist %>%
      select(nhd_COMID,
             wwtp_flag, wwtp_dist_km, wwtp_n_upstream, wwtp_intensity_nearest,
             tri_sw_flag, tri_sw_dist_km, tri_sw_n_upstream, tri_sw_intensity_nearest),
    by = "nhd_COMID"
  ) %>%
  filter(!is.na(nhd_COMID)) %>%
  replace_na(list(
    wwtp_flag = 0, wwtp_n_upstream = 0, wwtp_intensity_nearest = 0,
    tri_sw_flag = 0, tri_sw_n_upstream = 0, tri_sw_intensity_nearest = 0
  )) %>%
  mutate(
    wwtp_dist_km   = if_else(wwtp_flag == 0, 9999, wwtp_dist_km),
    tri_sw_dist_km = if_else(tri_sw_flag == 0, 9999, tri_sw_dist_km)
  )

cat("Prediction grid rows:       ", nrow(prediction_grid), "\n")
cat("Unique NHD reaches:         ", n_distinct(prediction_grid$nhd_COMID), "\n")
cat("NAs in wwtp_dist_km:        ", sum(is.na(prediction_grid$wwtp_dist_km)), "\n")
cat("NAs in tri_sw_dist_km:      ", sum(is.na(prediction_grid$tri_sw_dist_km)), "\n")


# Which NHD reaches are missing StreamCat?
missing_sc <- prediction_grid %>%
  distinct(nhd_COMID, npdesdensws) %>%
  filter(is.na(npdesdensws)) %>%
  pull(nhd_COMID)

cat("Reaches missing StreamCat:", length(missing_sc), "\n")

# Are any of these Petre monitoring site COMIDs?
cat("Missing SC reaches that are Petre sites:", 
    sum(missing_sc %in% unique(petre_data$COMID)), "\n")


# =============================================================
# STEP 13: DEFINE PREDICTOR SET
# =============================================================

flow_preds <- c(
  "log_flow", "log_flow_7day", "flow_30day",
  "flow_anomaly", "sin_doy", "cos_doy", "month"
)

sc_preds <- c(
  "septicws", "pctagdrainagews", "pctimp2019ws", "pctconif2019ws",
  "pctcrop2019ws", "pctgrs2019ws", "pcthbwet2019ws", "pctmxfst2019ws",
  "pctow2019ws", "pctshrb2019ws", "pcturbhi2019ws", "pcturblo2019ws",
  "pcturbmd2019ws", "wwtpmajordensws", "wwtpminordensws", "npdesdensws",
  "huden2010ws", "tridensws", "canaldensws", "minedensws",
  "coalminedensws", "superfunddensws", "popden2010ws", "runoffws",
  "bfiws", "manurews", "fertws", "damdensws", "elevws",
  "precip9120ws", "tmax9120ws", "wwtpmajordenscat", "wwtpminordenscat",
  "tridenscat", "npdesdenscat", "superfunddenscat",
  "npdes_ratio", "tri_ratio", "superfund_ratio", "wwtp_ratio"
)

point_source_preds <- c(
  "wwtp_flag", "wwtp_dist_km", "wwtp_n_upstream", "wwtp_intensity_nearest",
  "tri_sw_flag", "tri_sw_dist_km", "tri_sw_n_upstream", "tri_sw_intensity_nearest"
)

sc_preds_found    <- intersect(sc_preds,           names(petre_with_flow))
point_preds_found <- intersect(point_source_preds, names(petre_with_flow))
all_preds         <- c(flow_preds, sc_preds_found, point_preds_found)

cat("Flow predictors:          ", length(flow_preds), "\n")
cat("StreamCat predictors:     ", length(sc_preds_found), "\n")
cat("Point source predictors:  ", length(point_preds_found), "\n")
cat("Total predictors:         ", length(all_preds), "\n")

missing_preds <- setdiff(all_preds, names(petre_with_flow))
cat("Missing from petre_with_flow:", length(missing_preds), "\n")
if (length(missing_preds) > 0) print(missing_preds)

# =============================================================
# STEP 14: MODEL FUNCTION
# =============================================================

run_cape_fear_model_v2 <- function(compound,
                                   model_type = "rf",
                                   data       = petre_with_flow,
                                   preds      = all_preds,
                                   min_obs    = 30,
                                   mtry_fracs = c(1/10, 1/5, 1/3, 1/2)) {
  
  outcome <- log10(data[[compound]] + 1)
  valid   <- !is.na(outcome) & outcome > 0
  
  if (sum(valid) < min_obs) {
    cat(compound, "— skipped (n =", sum(valid), ")\n")
    return(NULL)
  }
  
  df      <- data[valid, ]
  outcome <- outcome[valid]
  weights <- df$spatial_weight
  
  # Flow x landuse interactions
  flow_col <- intersect(c("Flow_cms", "flow", "discharge"), names(df))
  if (length(flow_col) > 0) {
    df$log_flow_raw <- log10(df[[flow_col[1]]] + 1)
    landuse_preds   <- preds[!preds %in% c(flow_col, "log_flow", "log_flow_raw")]
    top_landuse     <- head(landuse_preds, 5)
    for (lp in top_landuse) {
      df[[paste0("flow_x_", lp)]] <- df$log_flow_raw * df[[lp]]
    }
    preds_use <- c(preds, paste0("flow_x_", top_landuse))
  } else {
    preds_use <- preds
  }
  
  cv_preds <- rep(NA_real_, nrow(df))
  
  for (site in unique(df$site_idx)) {
    train_idx <- which(df$site_idx != site)
    test_idx  <- which(df$site_idx == site)
    if (length(train_idx) < 20) next
    
    train_x <- df[train_idx, preds_use, drop = FALSE]
    train_y <- outcome[train_idx]
    train_w <- weights[train_idx]
    test_x  <- df[test_idx,  preds_use, drop = FALSE]
    
    train_medians <- sapply(train_x, median, na.rm = TRUE)
    for (col in names(train_x)) {
      train_x[[col]][is.na(train_x[[col]])] <- train_medians[[col]]
      test_x[[col]][is.na(test_x[[col]])]   <- train_medians[[col]]
    }
    
    if (model_type == "rf") {
      p               <- ncol(train_x)
      mtry_candidates <- unique(pmax(1, floor(p * mtry_fracs)))
      best_mtry <- mtry_candidates[1]
      best_oob  <- Inf
      for (m in mtry_candidates) {
        tmp <- randomForest(x = train_x, y = train_y,
                            ntree = 300, mtry = m, weights = train_w)
        oob_err <- tail(tmp$mse, 1)
        if (oob_err < best_oob) { best_oob <- oob_err; best_mtry <- m }
      }
      fit <- randomForest(x = train_x, y = train_y,
                          ntree = 500, mtry = best_mtry, weights = train_w)
      cv_preds[test_idx] <- predict(fit, newdata = test_x)
    }
    
    if (model_type == "xgb") {
      set.seed(42)
      val_sites <- sample(unique(df$site_idx[train_idx]),
                          size = max(1, floor(length(unique(df$site_idx[train_idx])) * 0.2)))
      val_idx   <- which(df$site_idx[train_idx] %in% val_sites)
      sub_idx   <- which(!df$site_idx[train_idx] %in% val_sites)
      dsub  <- xgb.DMatrix(as.matrix(train_x[sub_idx, ]),
                           label = train_y[sub_idx], weight = train_w[sub_idx])
      dval  <- xgb.DMatrix(as.matrix(train_x[val_idx, ]),
                           label = train_y[val_idx])
      dtest <- xgb.DMatrix(as.matrix(test_x))
      fit <- xgb.train(
        data = dsub, nrounds = 500,
        params = list(objective = "reg:squarederror", eta = 0.05,
                      max_depth = 4, subsample = 0.8,
                      colsample_bytree = 0.8, min_child_weight = 5),
        evals = list(val = dval),
        early_stopping_rounds = 20, verbose = 0
      )
      cv_preds[test_idx] <- predict(fit, dtest)
    }
  }
  
  has_both <- !is.na(outcome) & !is.na(cv_preds)
  err      <- outcome[has_both] - cv_preds[has_both]
  r2       <- 1 - sum(err^2) / sum((outcome[has_both] - mean(outcome[has_both]))^2)
  rmse     <- sqrt(mean(err^2))
  spearman <- cor(outcome[has_both], cv_preds[has_both], method = "spearman")
  
  cat(compound, model_type,
      "| R2 =", round(r2, 3), "| RMSE =", round(rmse, 3),
      "| Spearman =", round(spearman, 3), "| n =", sum(has_both), "\n")
  
  # Final model on all data
  full_x       <- df[, preds_use, drop = FALSE]
  full_medians <- sapply(full_x, median, na.rm = TRUE)
  for (col in names(full_x)) {
    full_x[[col]][is.na(full_x[[col]])] <- full_medians[[col]]
  }
  
  if (model_type == "rf") {
    final_fit <- randomForest(
      x = full_x, y = outcome, ntree = 1000,
      mtry = max(1, floor(ncol(full_x) / 3)),
      weights = weights, importance = TRUE
    )
  }
  if (model_type == "xgb") {
    dtrain_full <- xgb.DMatrix(as.matrix(full_x),
                               label = outcome, weight = weights)
    final_fit <- xgb.train(
      data = dtrain_full, nrounds = 200,
      params = list(objective = "reg:squarederror", eta = 0.05,
                    max_depth = 4, subsample = 0.8,
                    colsample_bytree = 0.8, min_child_weight = 5),
      verbose = 0
    )
  }
  
  list(compound    = compound,     model_type   = model_type,
       final_model = final_fit,    train_data   = full_x,
       train_outcome = outcome,    train_medians = full_medians,
       preds_used  = preds_use,    cv_preds     = cv_preds,
       actual      = outcome,      site_ids     = df$site_idx,
       log_flow    = if ("log_flow" %in% names(df)) df$log_flow else NULL,
       r2 = r2, rmse = rmse, spearman = spearman, n = sum(has_both))
}

# =============================================================
# STEP 12: RUN MODELS for 5 compounds
# =============================================================

results_rf_all <- setNames(
  map(compounds, ~run_cape_fear_model_v2(
    .x, model_type = "rf",
    data = petre_with_flow, preds = all_preds
  )), compounds)

results_xgb_all <- setNames(
  map(compounds, ~run_cape_fear_model_v2(
    .x, model_type = "xgb",
    data = petre_with_flow, preds = all_preds
  )), compounds)

# =============================================================
# STEP 13: COMPARE MODEL PERFORMANCE
# =============================================================

library(tidyverse)
library(patchwork)  # for combining plots

#----- 1. METRICS COMPARISON TABLE --------

metrics_comparison <- map_dfr(compounds, function(cmp) {
  calc <- function(res) {
    has <- !is.na(res$actual) & !is.na(res$cv_preds)
    d   <- tibble(a = res$actual[has], p = res$cv_preds[has])
    tibble(
      r2       = round(1 - sum((d$a - d$p)^2) / sum((d$a - mean(d$a))^2), 3),
      rmse     = round(sqrt(mean((d$a - d$p)^2)), 3),
      spearman = round(cor(d$a, d$p, method = "spearman"), 3),
      n        = nrow(d)
    )
  }
  bind_rows(
    tibble(compound = cmp, model = "Random Forest", calc(results_rf_all[[cmp]])),
    tibble(compound = cmp, model = "XGBoost",       calc(results_xgb_all[[cmp]]))
  )
})

# Dot plot comparison of R² and Spearman by model
metrics_long <- metrics_comparison %>%
  pivot_longer(cols = c(r2, rmse, spearman),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(metric,
                    levels = c("r2", "spearman", "rmse"),
                    labels = c("R²", "Spearman ρ", "RMSE (log ng/L)"))
  )

p_metrics <- ggplot(metrics_long,
                    aes(x = compound, y = value, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Random Forest" = "#2c7fb8",
                               "XGBoost" = "#d95f02")) +
  labs(title = "Leave-One-Site-Out Cross-Validation Performance",
       subtitle = "Full predictor set (StreamCat + Flow + Point Source)",
       x = NULL, y = NULL, fill = "Model") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

p_metrics

# ---------2. OBSERVED VS PREDICTED — side by side RF and XGB---------

plot_obs_pred_combined <- function(results_rf, results_xgb, compounds,
                                   exclude_site = NULL) {
  
  # Build data for both models
  build_data <- function(results_list, model_name) {
    map_dfr(compounds, function(cmp) {
      res <- results_list[[cmp]]
      if (is.null(res)) return(NULL)
      has_both <- !is.na(res$actual) & !is.na(res$cv_preds)
      tibble(
        compound  = cmp,
        model     = model_name,
        actual    = res$actual[has_both],
        predicted = res$cv_preds[has_both],
        site      = factor(res$site_ids[has_both])
      )
    })
  }
  
  plot_data <- bind_rows(
    build_data(results_rf,  "Random Forest"),
    build_data(results_xgb, "XGBoost")
  )
  
  if (!is.null(exclude_site)) {
    plot_data <- plot_data %>% filter(!site %in% exclude_site)
  }
  
  # Compute annotation labels (R², RMSE, Spearman)
  lab_data <- plot_data %>%
    group_by(compound, model) %>%
    summarise(
      r2   = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
      rmse = sqrt(mean((actual - predicted)^2)),
      rho  = cor(actual, predicted, method = "spearman"),
      n    = n(),
      # Position labels in upper-left
      xpos = min(actual) + 0.02 * diff(range(actual)),
      ypos = max(predicted) - 0.02 * diff(range(predicted)),
      .groups = "drop"
    ) %>%
    mutate(
      label = paste0("R² = ", round(r2, 3),
                     "\nρ = ", round(rho, 3),
                     "\nn = ", n)
    )
  
  ggplot(plot_data, aes(actual, predicted)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey30", linewidth = 0.5) +
    geom_point(aes(color = site), alpha = 0.5, size = 1.5,
               show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE,
                color = "black", linewidth = 0.7, linetype = "solid") +
    geom_text(
      data = lab_data,
      aes(x = xpos, y = ypos, label = label),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1, size = 5, lineheight = 0.9
    ) +
    facet_grid(model ~ compound, scales = "free") +
    labs(
      title = "Observed vs. Predicted PFAS Concentrations",
      subtitle = "Leave-one-site-out CV | Dashed = 1:1 line | Solid = linear fit",
      x = "Observed (log ng/L)",
      y = "Predicted (log ng/L)"
    ) +
    theme_bw(base_size = 16) +
    theme(
      plot.title    = element_text(face = "bold"),
      strip.text    = element_text(face = "bold"),
      panel.spacing = unit(0.8, "lines")
    )
}

plot_obs_pred_combined(results_rf_all, results_xgb_all, compounds)

# 3. SINGLE-MODEL VERSION (if you want RF only, cleaner for the report)

plot_obs_pred_single <- function(results_list, compounds, model_name = "Random Forest",
                                 exclude_site = NULL) {
  
  plot_data <- map_dfr(compounds, function(cmp) {
    res <- results_list[[cmp]]
    if (is.null(res)) return(NULL)
    has_both <- !is.na(res$actual) & !is.na(res$cv_preds)
    tibble(
      compound  = cmp,
      actual    = res$actual[has_both],
      predicted = res$cv_preds[has_both],
      site      = factor(res$site_ids[has_both])
    )
  })
  
  if (!is.null(exclude_site)) {
    plot_data <- plot_data %>% filter(!site %in% exclude_site)
  }
  
  lab_data <- plot_data %>%
    group_by(compound) %>%
    summarise(
      r2   = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
      rmse = sqrt(mean((actual - predicted)^2)),
      rho  = cor(actual, predicted, method = "spearman"),
      n    = n(),
      xmin = min(actual),
      ymax = max(predicted),
      .groups = "drop"
    ) %>%
    mutate(
      label = paste0("R² = ", sprintf("%.3f", r2),
                     "\nRMSE = ", sprintf("%.3f", rmse),
                     "\nSpearman ρ = ", sprintf("%.3f", rho),
                     "\nn = ", n)
    )
  
  ggplot(plot_data, aes(actual, predicted)) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey40", linewidth = 0.6) +
    geom_point(aes(color = site), alpha = 0.45, size = 1.6,
               show.legend = FALSE) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15,
                color = "#2c7fb8", linewidth = 0.8) +
    geom_text(
      data = lab_data,
      aes(x = xmin, y = ymax, label = label),
      inherit.aes = FALSE,
      hjust = 0, vjust = 1, size = 3.2, lineheight = 0.85,
      fontface = "plain"
    ) +
    facet_wrap(~compound, scales = "free", nrow = 1) +
    labs(
      title = paste0(model_name, " — Observed vs. Predicted PFAS Concentrations"),
      subtitle = "Leave-one-site-out cross-validation | Full predictor set",
      x = "Observed (log ng/L)",
      y = "Predicted (log ng/L)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey30"),
      strip.text    = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "grey95"),
      panel.spacing = unit(1, "lines")
    )
}

# Generate individual model plots
p_rf  <- plot_obs_pred_single(results_rf_all,  compounds, "Random Forest")
p_xgb <- plot_obs_pred_single(results_xgb_all, compounds, "XGBoost")

# Display
p_rf
p_xgb

# Or stack them with patchwork
p_rf / p_xgb + plot_annotation(
  title = "Model Comparison: Full Predictor Set Performance",
  theme = theme(plot.title = element_text(face = "bold", size = 14))
)

# ── 4. PERMUTATION VARIABLE IMPORTANCE ───────────────────────────────────

plot_varimp <- function(results_list, compounds, top_n = 20) {
  
  imp_data <- map_dfr(compounds, function(cmp) {
    res <- results_list[[cmp]]
    if (is.null(res) || res$model_type != "rf") return(NULL)
    
    imp <- importance(res$final_model, type = 1)  # %IncMSE (permutation)
    tibble(
      compound  = cmp,
      predictor = rownames(imp),
      pct_inc_mse = imp[, 1]
    )
  }) %>%
    group_by(compound) %>%
    slice_max(pct_inc_mse, n = top_n) %>%
    ungroup() %>%
    mutate(predictor = reorder_within(predictor, pct_inc_mse, compound))
  
  ggplot(imp_data, aes(x = predictor, y = pct_inc_mse, fill = compound)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_x_reordered() +
    facet_wrap(~compound, scales = "free_y") +
    labs(
      title = paste("Top", top_n, "Predictors by Permutation Importance"),
      x     = NULL,
      y     = "% Increase in MSE when permuted"
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

# requires tidytext for reorder_within; or use this base version instead:
# mutate(predictor = fct_reorder(predictor, pct_inc_mse))
library(tidytext)
plot_varimp(results_rf_all, compounds)


# ── 5. PARTIAL DEPENDENCE PLOTS (top 5 predictors) ────────────────────────

plot_pdp <- function(result, top_n = 5) {
  
  if (is.null(result)) return(NULL)
  
  imp      <- importance(result$final_model, type = 1)
  top_vars <- rownames(imp)[order(imp[,1], decreasing = TRUE)][1:top_n]
  
  pd_list <- map(top_vars, function(v) {
    pd <- partial(result$final_model,
                  pred.var = v,
                  train    = result$train_data,
                  grid.resolution = 30,
                  plot     = FALSE)
    pd$variable <- v
    names(pd)[1] <- "x_val"
    pd
  })
  
  pd_all <- bind_rows(pd_list) %>%
    mutate(variable = factor(variable, levels = top_vars))
  
  ggplot(pd_all, aes(x = x_val, y = yhat)) +
    geom_line(color = "steelblue", linewidth = 0.9) +
    geom_rug(data = map_dfr(top_vars, function(v) {
      tibble(variable = v,
             x_val    = result$train_data[[v]])
    }), aes(x = x_val), alpha = 0.15, sides = "b") +
    facet_wrap(~variable, scales = "free_x") +
    labs(
      title = paste("Partial Dependence —", result$compound),
      x     = "Predictor value",
      y     = "Predicted log₁₀(PFAS + 1)"
    ) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

# Run for each compound
pdp_plots <- map(compounds, ~plot_pdp(results_rf_all[[.x]]))
names(pdp_plots) <- compounds
pdp_plots[["PFOA"]]   # view one at a time, or use grid.arrange


# ── 6. RESIDUALS vs LOG(FLOW) ─────────────────────────────────────────────

plot_resid_flow <- function(results_list, compounds) {
  
  resid_data <- map_dfr(compounds, function(cmp) {
    res <- results_list[[cmp]]
    if (is.null(res) || is.null(res$log_flow)) return(NULL)
    
    has_both <- !is.na(res$actual) & !is.na(res$cv_preds)
    tibble(
      compound  = cmp,
      log_flow  = res$log_flow[has_both],
      residual  = (res$actual - res$cv_preds)[has_both],
      site      = factor(res$site_ids[has_both])
    )
  })
  
  ggplot(resid_data, aes(x = log_flow, y = residual, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(alpha = 0.45, size = 1.6) +
    geom_smooth(aes(group = 1), method = "loess", se = TRUE,
                color = "black", linewidth = 0.8, alpha = 0.15) +
    facet_wrap(~compound, scales = "free_x") +
    labs(
      title = "Residuals vs. log₁₀(Flow)",
      x     = "log₁₀(Flow + 1)",
      y     = "Residual (observed − predicted)",
      color = "Site"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())
}

plot_resid_flow(results_rf_all, compounds)



# =============================================================
# STEP 13: ABLATION EXPERIMENTS
# =============================================================

# Define predictor groups
ablation_sets <- list(
  "Full model"       = all_preds,
  "No flow"          = setdiff(all_preds, flow_preds),
  "No StreamCat"     = setdiff(all_preds, sc_preds_found),
  "No point source"  = setdiff(all_preds, point_preds_found),
  "Flow only"        = flow_preds,
  "StreamCat only"   = sc_preds_found,
  "Point source only"= point_preds_found,
  "Flow + point src" = c(flow_preds, point_preds_found),
  "Flow + StreamCat" = c(flow_preds, sc_preds_found)
)

# Run ablation for all compounds × predictor sets
ablation_results <- map_dfr(names(ablation_sets), function(set_name) {
  preds_use <- ablation_sets[[set_name]]
  
  map_dfr(compounds, function(compound) {
    cat(set_name, "|", compound, "\n")
    res <- run_cape_fear_model_v2(
      compound, model_type = "rf",
      data = petre_with_flow, preds = preds_use
    )
    if (is.null(res)) {
      tibble(predictor_set = set_name, compound = compound,
             r2 = NA, rmse = NA, spearman = NA, n = NA, n_preds = length(preds_use))
    } else {
      tibble(predictor_set = set_name, compound = compound,
             r2 = res$r2, rmse = res$rmse, spearman = res$spearman,
             n = res$n, n_preds = length(res$preds_used))
    }
  })
})

# =============================================================
# Visualize ablation results
# =============================================================

# R² comparison across ablation sets
ggplot(ablation_results, aes(x = reorder(predictor_set, r2, FUN = median),
                             y = r2, fill = compound)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Ablation Study: R² by Predictor Group",
       x = NULL, y = "Leave-one-site-out R²") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Delta R² from full model
ablation_delta <- ablation_results %>%
  left_join(
    ablation_results %>% filter(predictor_set == "Full model") %>%
      select(compound, r2_full = r2),
    by = "compound"
  ) %>%
  mutate(delta_r2 = r2 - r2_full)

ggplot(ablation_delta %>% filter(predictor_set != "Full model"),
       aes(x = reorder(predictor_set, delta_r2, FUN = median),
           y = delta_r2, color = compound)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Change in R² relative to Full Model",
       x = NULL, y = expression(Delta~R^2)) +
  theme_minimal()

# =============================================================
# STEP 14: PREDICT ACROSS ALL CAPE FEAR REACHES
# =============================================================

predict_cape_fear <- function(compound, model_result, 
                              pred_grid = prediction_grid) {
  
  if (is.null(model_result)) return(NULL)
  
  preds_use <- model_result$preds_used
  pred_x <- pred_grid
  
  # -------------------------------------------------
  # Recreate engineered variables if needed
  # -------------------------------------------------
  if ("log_flow" %in% preds_use && !"log_flow" %in% names(pred_x)) {
    flow_col <- intersect(c("Flow_cms", "flow", "discharge", "q_cfs"), names(pred_x))
    pred_x$log_flow <- log10(pred_x[[flow_col[1]]] + 1)
  }
  
  if ("log_flow_7day" %in% preds_use && !"log_flow_7day" %in% names(pred_x)) {
    pred_x$log_flow_7day <- log10(pred_x$flow_7day + 1)
  }
  
  # -------------------------------------------------
  # Recreate interaction features
  # -------------------------------------------------
  if ("flow_x_log_flow_7day" %in% preds_use) {
    pred_x$flow_x_log_flow_7day <- pred_x$log_flow * pred_x$log_flow_7day
  }
  
  if ("flow_x_flow_30day" %in% preds_use) {
    pred_x$flow_x_flow_30day <- pred_x$log_flow * pred_x$flow_30day
  }
  
  if ("flow_x_flow_anomaly" %in% preds_use) {
    pred_x$flow_x_flow_anomaly <- pred_x$log_flow * pred_x$flow_anomaly
  }
  
  if ("flow_x_sin_doy" %in% preds_use) {
    pred_x$flow_x_sin_doy <- pred_x$log_flow * pred_x$sin_doy
  }
  
  if ("flow_x_cos_doy" %in% preds_use) {
    pred_x$flow_x_cos_doy <- pred_x$log_flow * pred_x$cos_doy
  }
  # -------------------------------------------------
  # Keep only model variables
  # -------------------------------------------------
  pred_x <- pred_x[, preds_use, drop = FALSE]
  
  # -------------------------------------------------
  # Median imputation (training-based)
  # -------------------------------------------------
  for (col in preds_use) {
    pred_x[[col]][is.na(pred_x[[col]])] <- model_result$train_medians[[col]]
  }
  
  # -------------------------------------------------
  # Predict
  # -------------------------------------------------
  if (model_result$model_type == "xgb") {
    dpred <- xgb.DMatrix(as.matrix(pred_x))
    preds_out <- predict(model_result$final_model, dpred)
  } else {
    preds_out <- predict(model_result$final_model, newdata = pred_x)
  }
  
  # -------------------------------------------------
  # Return with metadata
  # -------------------------------------------------
  pred_grid %>%
    mutate(
      compound        = compound,
      log10_conc_pred = preds_out,
      conc_pred_ngL   = 10^preds_out - 1
    )
}

#this takes about ~10 minutes i think
cape_fear_preds <- map2_dfr(
  compounds,
  results_rf_all[compounds],
  predict_cape_fear
)

cat("Prediction rows:", nrow(cape_fear_preds), "\n")

# =============================================================
# STEP 15: JOIN PREDICTIONS TO SPATIAL NETWORK AND SAVE
# =============================================================

# Only keep NHD reaches that map to a MERIT river
valid_nhd <- unique(nhd_to_merit$nhd_COMID)

cape_fear_prediction_flowlines <- get_nhdplus(
  AOI         = cape_fear_wbd_4326,
  realization = "flowline"
) %>%
  filter(comid %in% valid_nhd)

cat("NHDPlus flowlines (MERIT-matched):", nrow(cape_fear_prediction_flowlines), "\n")

cape_fear_pred_spatial <- cape_fear_prediction_flowlines %>%
  inner_join(cape_fear_preds, by = c("comid" = "nhd_COMID"))

saveRDS(cape_fear_preds,
        "/home/rvera_umass_edu/cape_fear_predictions.rds")
saveRDS(cape_fear_pred_spatial,
        "/home/rvera_umass_edu/cape_fear_pred_spatial.rds")
saveRDS(petre_with_flow,
        "/home/rvera_umass_edu/petre_with_flow.rds")


