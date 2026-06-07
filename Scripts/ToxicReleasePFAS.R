

library(tidyverse)
library(sf)
library(nhdplusTools)

# ── Read TRI (tab-delimited, the standard format for Basic Plus) ──────────────
# The file is usually tab-separated with a header row
tri_raw <- read_tsv(
  "CONUS_PFAS/ToxicReleaseInventoryBasicPlus_2020data/US_1a_2020.txt",          # adjust filename; may also be .csv with tabs
  col_types = cols(.default = "c"),   # read everything as character first
  locale = locale(encoding = "latin1")
)

# ── Clean and select relevant columns ─────────────────────────────────────────
tri_clean <- tri_raw %>%
  rename(
    reporting_year    = `2. REPORTING YEAR`,
    facility_name     = `10. FACILITY NAME`,
    facility_state    = `14. FACILITY STATE`,
    latitude          = `47. LATITUDE`,
    longitude         = `48. LONGITUDE`,
    chemical_name     = `81. CHEMICAL NAME`,
    pfas_ind          = `88. PFAS_IND`,
    unit_of_measure   = `85. UNIT OF MEASURE`,
    # Surface water: use the pre-summed total column
    total_sw_discharge = `174. TOTAL SURFACE WATER DISCHARGE`,
    # Individual streams if you want to verify or handle NAs
    stream_a_lbs      = `120. DISCHARGES TO STREAM A - RELEASE POUNDS`,
    stream_b_lbs      = `126. DISCHARGES TO STREAM B - RELEASE POUNDS`,
    stream_c_lbs      = `132. DISCHARGES TO STREAM C - RELEASE POUNDS`,
    total_onsite_release = `221. TOTAL ON-SITE RELEASES`
  ) %>%
  mutate(
    latitude           = as.numeric(latitude),
    longitude          = as.numeric(longitude),
    total_sw_discharge = as.numeric(total_sw_discharge),
    total_onsite_release = as.numeric(total_onsite_release)
  )

# ── Two separate TRI layers ───────────────────────────────────────────────────

# 1. PFAS-specific facilities (most relevant for your model)
tri_pfas <- tri_clean %>%
  filter(
    pfas_ind == "YES",
    facility_state == "NC",        # adjust if your basin crosses state lines
    !is.na(latitude), !is.na(longitude)
  ) %>%
  group_by(facility_name, latitude, longitude) %>%
  summarise(
    pfas_sw_lbs     = sum(total_sw_discharge,  na.rm = TRUE),
    pfas_total_lbs  = sum(total_onsite_release, na.rm = TRUE),
    n_pfas_chems    = n_distinct(chemical_name),
    .groups = "drop"
  )

# 2. All TRI facilities with ANY surface water discharge (broader predictor)
tri_sw <- tri_clean %>%
  filter(
    facility_state == "NC",
    !is.na(latitude), !is.na(longitude),
    !is.na(total_sw_discharge),
    total_sw_discharge > 0
  ) %>%
  group_by(facility_name, latitude, longitude) %>%
  summarise(
    sw_lbs_total    = sum(total_sw_discharge, na.rm = TRUE),
    .groups = "drop"
  )

# ── Convert to sf and snap to NHD ─────────────────────────────────────────────
snap_to_nhd <- function(df, lon_col = "longitude", lat_col = "latitude") {
  sf_obj <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  sf_obj %>%
    mutate(comid = map_int(seq_len(n()), function(i) {
      tryCatch(
        discover_nhdplus_id(point = sf_obj[i, ]),
        error = \(e) NA_integer_
      )
    })) %>%
    filter(!is.na(comid))
}

tri_pfas_sf <- snap_to_nhd(tri_pfas)
tri_sw_sf   <- snap_to_nhd(tri_sw)

# ── COMID vectors ─────────────────────────────────────────────────────────────
#this takes a while to run. 
tri_pfas_comids <- unique(as.integer(tri_pfas_sf$comid))
tri_sw_comids   <- unique(as.integer(tri_sw_sf$comid))

# ── Intensity tables (summed per COMID for multi-facility reaches) ────────────
tri_pfas_intensity <- tri_pfas_sf %>%
  st_drop_geometry() %>%
  group_by(comid) %>%
  summarise(
    intensity      = sum(pfas_sw_lbs,    na.rm = TRUE),  # SW discharge in lbs
    n_facilities   = n(),
    n_pfas_chems   = sum(n_pfas_chems,   na.rm = TRUE),
    .groups = "drop"
  )

tri_sw_intensity <- tri_sw_sf %>%
  st_drop_geometry() %>%
  group_by(comid) %>%
  summarise(
    intensity    = sum(sw_lbs_total, na.rm = TRUE),
    n_facilities = n(),
    .groups = "drop"
  )

# Save
saveRDS(tri_pfas_sf,        "tri_pfas_sf.rds")
saveRDS(tri_sw_sf,          "tri_sw_sf.rds")
saveRDS(tri_pfas_intensity, "tri_pfas_intensity.rds")
saveRDS(tri_sw_intensity,   "tri_sw_intensity.rds")

cat("PFAS TRI facilities snapped:", nrow(tri_pfas_sf), "\n")
cat("SW-discharging TRI facilities snapped:", nrow(tri_sw_sf), "\n")

#now onto waste water treatment plants
# CWNS 2022 - download from:
# https://www.epa.gov/cwns/clean-watersheds-needs-survey-cwns-2022-data-and-reports
# The facility-level CSV has columns: LATITUDE, LONGITUDE, EXIST_DESIGN_FLOW_MGD

PHYSICAL_LOCATION <- read_csv("CONUS_PFAS/2022CWNS_NATIONAL_APR2024/PHYSICAL_LOCATION.csv")
FLOW <- read_csv("CONUS_PFAS/2022CWNS_NATIONAL_APR2024/FLOW.csv")
DISCHARGES <- read_csv("CONUS_PFAS/2022CWNS_NATIONAL_APR2024/DISCHARGES.csv")
EFFLUENT <- read_csv("CONUS_PFAS/2022CWNS_NATIONAL_APR2024/EFFLUENT.csv")
POPULATION_WASTEWATER <- read_csv("CONUS_PFAS/2022CWNS_NATIONAL_APR2024/POPULATION_WASTEWATER.csv")
ASSET_MANAGEMENT <- read_csv("CONUS_PFAS/2022CWNS_NATIONAL_APR2024/ASSET_MANAGEMENT.csv")

library(tidyverse)
library(sf)
library(nhdplusTools)
# ── Step 1: Filter to wastewater facilities ───────────────────────────────────
wwtp_facilities <- FACILITIES %>%
  filter(INFRASTRUCTURE_TYPE == "Wastewater") %>%
  select(FACILITY_ID, CWNS_ID, STATE_CODE, FACILITY_NAME,
         INFRASTRUCTURE_TYPE, OWNER_TYPE, SUPERFUND_FLAG)

# ── Step 2: Design flow — use Total Flow ─────────────────────────────────────
# Total Flow = Municipal + Industrial + Infiltration combined
# This is the best single proxy for plant capacity
design_flow <- FLOW %>%
  filter(FLOW_TYPE == "Total Flow") %>%
  group_by(FACILITY_ID) %>%
  summarise(
    design_flow_mgd = sum(CURRENT_DESIGN_FLOW, na.rm = TRUE),
    .groups = "drop"
  )

# Optional: keep the breakdown too — municipal flow alone is a useful predictor
# since industrial contributors affect PFAS loading differently
municipal_flow <- FLOW %>%
  filter(FLOW_TYPE == "Municipal Flow") %>%
  select(FACILITY_ID, municipal_flow_mgd = CURRENT_DESIGN_FLOW)

# ── Step 3: Surface water dischargers only ────────────────────────────────────
sw_dischargers <- DISCHARGES %>%
  filter(DISCHARGE_TYPE == "Outfall To Surface Waters") %>%
  distinct(FACILITY_ID)

# CSO (combined sewer overflow) also goes to surface water — worth including
# as a flag variable since CSOs are episodic high-load events
cso_flag <- DISCHARGES %>%
  filter(DISCHARGE_TYPE == "CSO Discharge") %>%
  distinct(FACILITY_ID) %>%
  mutate(has_cso = TRUE)

# ── Step 4: Treatment level ───────────────────────────────────────────────────
treatment <- EFFLUENT %>%
  distinct(FACILITY_ID, CURRENT_EFFLUENT_TREATMENT_LEVEL)

# ── Step 5: Population served ─────────────────────────────────────────────────
pop_served <- POPULATION_WASTEWATER %>%
  group_by(FACILITY_ID) %>%
  summarise(
    pop_served = sum(RESIDENTIAL_POP_2022 + NONRESIDENTIAL_POP_2022,
                     na.rm = TRUE),
    .groups = "drop"
  )

# ── Step 6: Assemble ──────────────────────────────────────────────────────────
wwtp_master <- wwtp_facilities %>%
  inner_join(sw_dischargers, by = "FACILITY_ID") %>%
  left_join(PHYSICAL_LOCATION %>%
              select(FACILITY_ID, LATITUDE, LONGITUDE),
            by = "FACILITY_ID") %>%
  left_join(design_flow,    by = "FACILITY_ID") %>%
  left_join(municipal_flow, by = "FACILITY_ID") %>%
  left_join(pop_served,     by = "FACILITY_ID") %>%
  left_join(treatment,      by = "FACILITY_ID") %>%
  left_join(cso_flag,       by = "FACILITY_ID") %>%
  mutate(has_cso = replace_na(has_cso, FALSE)) %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))

cat("WWTPs with surface water discharge and coordinates:", nrow(wwtp_master), "\n")

# Quick sanity check
cat("With design flow data:", sum(!is.na(wwtp_master$design_flow_mgd)), "\n")
cat("With population data:",  sum(!is.na(wwtp_master$pop_served)), "\n")
cat("States represented:",    n_distinct(wwtp_master$STATE_CODE), "\n")

# ── Step 7: Snap to NHD ───────────────────────────────────────────────────────
# For CONUS this will take a while — run overnight or parallelize
# For now, filter to NC / Cape Fear basin for testing:

wwtp_nc <- wwtp_master %>%
  filter(STATE_CODE == "NC")       # drop for CONUS

wwtp_sf <- st_as_sf(wwtp_nc,
                    coords = c("LONGITUDE", "LATITUDE"),
                    crs = 4326)

wwtp_sf <- wwtp_sf %>%
  mutate(comid = map_int(seq_len(n()), function(i) {
    tryCatch(
      discover_nhdplus_id(point = wwtp_sf[i, ]),
      error = \(e) NA_integer_
    )
  }))

wwtp_sf <- wwtp_sf %>% filter(!is.na(comid))
cat("WWTPs snapped:", nrow(wwtp_sf), "\n")

# ── Step 8: Build intensity table ─────────────────────────────────────────────
# Sum design flow per COMID (handles multiple plants on same reach)
wwtp_intensity <- wwtp_sf %>%
  st_drop_geometry() %>%
  group_by(comid) %>%
  summarise(
    intensity = sum(design_flow_mgd, na.rm = TRUE),  # MGD — keep this
    n_wwtps   = n(),                                  # count can still be useful
    .groups   = "drop"
  )

wwtp_comids <- unique(as.integer(wwtp_sf$comid))

saveRDS(wwtp_sf,        "wwtp_sf.rds")
saveRDS(wwtp_intensity, "wwtp_intensity.rds")


# Quick check on TRI data quality
tri_sw_sf %>%
  st_drop_geometry() %>%
  summarise(
    n_total          = n(),
    n_nonzero_sw     = sum(sw_lbs_total > 0, na.rm = TRUE),
    n_zero_sw        = sum(sw_lbs_total == 0, na.rm = TRUE),
    n_na_sw          = sum(is.na(sw_lbs_total)),
    median_lbs       = median(sw_lbs_total[sw_lbs_total > 0], na.rm = TRUE),
    max_lbs          = max(sw_lbs_total, na.rm = TRUE)
  )


# ── Pre-flight check ──────────────────────────────────────────────────────────

# 1. Source COMID vectors
source_list <- list(
  wwtp   = unique(as.integer(wwtp_sf$comid)),
  tri_sw = unique(as.integer(tri_sw_sf$comid))
)

# 2. Intensity tables
intensity_list <- list(
  wwtp   = wwtp_intensity %>% select(comid, intensity),
  tri_sw = tri_sw_intensity %>% select(comid, intensity)
)

# 3. Prediction COMIDs from your NHD layer
pred_comids <- unique(as.integer(cape_fear_nhd_proj$featureid))

# Sanity checks
cat("WWTP COMIDs:", length(source_list$wwtp), "\n")
cat("TRI SW COMIDs:", length(source_list$tri_sw), "\n")
cat("Prediction reaches:", length(pred_comids), "\n")

# Check intensity tables have correct columns
cat("\nWWTP intensity columns:", names(intensity_list$wwtp), "\n")
cat("TRI intensity columns:", names(intensity_list$tri_sw), "\n")

# Check for any overlap between source COMIDs and pred COMIDs
# (good to know — sources on prediction reaches will have distance ~ 0)
cat("\nWWTP COMIDs on prediction reaches:",
    sum(source_list$wwtp %in% pred_comids), "\n")
cat("TRI COMIDs on prediction reaches:",
    sum(source_list$tri_sw %in% pred_comids), "\n")


# ── Generalized multi-source upstream distance function ──────────────────────

get_upstream_sources <- function(target_comid,
                                 source_list,
                                 intensity_list  = NULL,
                                 max_dist_km     = 200) {
  
  source_names <- names(source_list)
  
  # Helper: build a blank result row (all NA / 0) for early exits or errors
  make_empty <- function(flag_val = 0L, dist_val = NA_real_,
                         n_val = 0L, err = FALSE) {
    out <- tibble(nhd_COMID = target_comid)
    for (nm in source_names) {
      out[[paste0(nm, "_flag")]]     <- if (err) NA_integer_ else flag_val
      out[[paste0(nm, "_dist_km")]]  <- if (err) NA_real_    else dist_val
      out[[paste0(nm, "_n_upstream")]] <- if (err) NA_integer_ else n_val
      if (!is.null(intensity_list[[nm]])) {
        out[[paste0(nm, "_intensity_nearest")]] <- NA_real_
      }
    }
    out
  }
  
  tryCatch({
    
    # ── 1. Single upstream navigation (shared across all sources) ────────────
    upstream_raw <- navigate_nldi(
      nldi_feature = list(featureSource = "comid",
                          featureID     = as.character(target_comid)),
      mode         = "upstreamMain",
      distance_km  = max_dist_km
    )
    upstream <- upstream_raw$UM_flowlines
    
    if (is.null(upstream) || nrow(upstream) == 0) return(make_empty())
    
    upstream_comids <- as.integer(upstream$nhdplus_comid)
    
    # ── 2. Check which sources have any upstream hits ────────────────────────
    hits <- lapply(source_list, function(src_comids) {
      intersect(upstream_comids, src_comids)
    })
    
    any_hits <- any(lengths(hits) > 0)
    if (!any_hits) return(make_empty())
    
    # ── 3. Fetch pathlength for target reach (one call) ──────────────────────
    target_pl <- get_nhdplus(comid = target_comid,
                             realization = "flowline") %>%
      st_drop_geometry() %>%
      pull(pathlength)
    
    # ── 4. Fetch pathlengths for all hit COMIDs in one batch call ────────────
    all_hit_comids <- unique(unlist(hits))
    
    if (length(all_hit_comids) > 0) {
      upstream_pl <- get_nhdplus(comid = all_hit_comids,
                                 realization = "flowline") %>%
        st_drop_geometry() %>%
        select(comid, pathlength)
    }
    
    # ── 5. Build result row ──────────────────────────────────────────────────
    out <- tibble(nhd_COMID = target_comid)
    
    for (nm in source_names) {
      src_hits <- hits[[nm]]
      
      if (length(src_hits) == 0) {
        out[[paste0(nm, "_flag")]]          <- 0L
        out[[paste0(nm, "_dist_km")]]       <- NA_real_
        out[[paste0(nm, "_n_upstream")]]    <- 0L
        if (!is.null(intensity_list[[nm]])) {
          out[[paste0(nm, "_intensity_nearest")]] <- NA_real_
        }
      } else {
        src_pl  <- upstream_pl %>% filter(comid %in% src_hits)
        dists   <- abs(target_pl - src_pl$pathlength)
        nearest_idx <- which.min(dists)
        
        out[[paste0(nm, "_flag")]]       <- 1L
        out[[paste0(nm, "_dist_km")]]    <- round(min(dists, na.rm = TRUE), 2)
        out[[paste0(nm, "_n_upstream")]] <- length(src_hits)
        
        # Intensity: join by comid of the nearest hit
        if (!is.null(intensity_list[[nm]])) {
          nearest_comid <- src_pl$comid[nearest_idx]
          int_val <- intensity_list[[nm]] %>%
            filter(comid == nearest_comid) %>%
            pull(intensity) %>%
            first()
          out[[paste0(nm, "_intensity_nearest")]] <- int_val %||% NA_real_
        }
      }
    }
    
    out
    
  }, error = function(e) {
    message("Failed for COMID ", target_comid, ": ", e$message)
    make_empty(err = TRUE)
  })
}

output_file <- "multi_source_hydro_dist.rds"

if (file.exists(output_file)) {
  completed        <- readRDS(output_file)
  remaining_comids <- setdiff(pred_comids, completed$nhd_COMID)
  cat("Resuming:", length(remaining_comids), "reaches remaining\n")
} else {
  completed        <- tibble()
  remaining_comids <- pred_comids
}

results_list <- vector("list", length(remaining_comids))

#this part of the script will take hours
for (i in seq_along(remaining_comids)) {
  results_list[[i]] <- get_upstream_sources(
    target_comid   = remaining_comids[i],
    source_list    = source_list,
    intensity_list = intensity_list,
    max_dist_km    = 200
  )
  if (i %% 50 == 0) {
    completed <- bind_rows(completed, bind_rows(results_list[1:i]))
    saveRDS(completed, output_file)
    message("Checkpoint: ", i, " / ", length(remaining_comids))
  }
}

nhd_pointsource <- bind_rows(completed, bind_rows(results_list)) %>%
  distinct(nhd_COMID, .keep_all = TRUE)

saveRDS(nhd_pointsource, output_file)
cat("Complete:", nrow(nhd_pointsource), "reaches processed\n")