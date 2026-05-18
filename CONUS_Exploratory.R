#conus 2: back at it

# ============================================================================
# PHASE 1, STEP 1: UNIFIED DATA LOADER WITH METADATA TRACKING
# ============================================================================

library(readr)
library(dplyr)
library(tibble)

# Define all dataset URLs with metadata
dataset_catalog <- tribble(
  ~dataset_name,        ~url,                                                                                                                   ~expected_region,
  "Caravan_PFAS",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Caravan_PFAS_2026_standardized.csv",         "Global",
  "Camacho_2024",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Camacho_et_al_2024_Florida.csv",  "USA",
  "Sims_2025",          "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sims_et_al_2025_%20Western_United_States.csv", "USA",
  "NH_DES_2026",        "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/NewHampshire_DES_PFAS_Data_Dump.csv", "USA",
  "Breitmeyer_2023",    "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Breitmeyer_et_al_2023_Pennsylvania.csv", "USA",
  "Zhang_2016",         "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Zhang_et_al_2016_RI_NY.csv",     "USA",
  "Goodrow_2020",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Goodrow_et_al_2020_New_Jersey.csv", "USA",
  "Bai_Son_2021",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Bai_and_Son_2021_Renoe_LasVegas.csv", "USA",
  "Maine_DEP_2026",     "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/MaineDEP_2026_Datadump_cleaned.csv", "USA",
  "WQP_USA_2026",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/WQP_USA_Data_complete.csv",   "USA",
  "Viticoski_2022",      "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Hayworth_et_al_2022_Alabama_cleaned.csv", "USA",
  "Dunn_2023",          "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Dunn_et_al_2023_RhodeIsland_complete.csv", "USA",
  "Forster_2024",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Forster_et_al_2024_SouthCarolina_cleaned.csv", "USA",
  "Penland_2020",       "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Penland_2020_SC_NC_cleaned.csv", "USA",
  "Labad_2025",         "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Labad_et_al_2025_Georgia.csv",   "USA",
  "Webb_2026",          "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Webb_et_al_2026_Savannah.csv",  "USA",
  "Colorado_DPH_2026",  "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Colorado_DPH.csv",              "USA",
  "Scott_2009",         "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Scott_et_al_2009_Canada.csv",   "Canada",
  "Teymoorian_2021",    "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Teymoorian_2025_Montreal.csv",  "Canada",
  "Ahrens_2023",        "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Ahrens_et_al_2023_Arctic.csv",  "Arctic",
  "Sharma_2016",        "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Sharma_et_al_2016_Ganges_River.csv", "India",
  "AustraliaMap_2026",  "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Australia_Government_PFAS_CHEM_MAP_Clean.csv", "Australia",
  "Woodward_2026",     "https://raw.githubusercontent.com/rvera177/GlobalPFAS/refs/heads/main/data/complete/Woodward_et_al_California_2026.csv", "USA"
  )

# PFAS compounds to standardize across all datasets
all_pfas <- c(
  "PFOS",
  "PFOA",
  "ADONA",
  "FBSA",
  "FOSAA",
  "GenX",
  "N_EtFOSA",       # was N.EtFOSA
  "N_EtFOSE",       # was N.EtFOSE
  "N_MeFOSE",       # was N.MeFOSE
  "NEtFOSAA",
  "NFDHA",
  "NMeFOSA",
  "NMeFOSAA",
  "PFBA",
  "PFBS",
  "PFDA",
  "PFDoA",
  "PFDoS",
  "PFDS",
  "PFECA_G",        # was PFECA.G
  "PFEESA",
  "PFESA_BP_1",     # was PFESA.BP.1
  "PFESA_BP_2",     # was PFESA.BP.2
  "PFHpA",
  "PFHpS",
  "PFHxA",
  "PFHxDA",
  "PFHxS",
  "PFMBA",
  "PFMOAA",
  "PFMOBA",
  "PFMOPrA",
  "PFNA",
  "PFNS",
  "PFO2HxA",
  "PFO3OA",
  "PFO4DA",
  "PFODA",
  "PFOSA",
  "PFPA",
  "PFPeA",
  "PFPeS",
  "PFTeDA",
  "PFTrDA",
  "PFUnDA",
  "X10_2_FTS",      # was X10.2.FTS
  "X11_Cl_PF3OUdS", # was X11.Cl.PF3OUdS
  "X3_3_FTCA",      # was X3.3.FTCA
  "X4_2_FTS",       # was X4.2.FTS
  "X5_3_FTCA",      # was X5.3.FTCA
  "X6_2_FTS",       # was X6.2.FTS
  "X7_3_FTCA",      # was X7.3.FTCA
  "X8_2_FTS",       # was X8.2.FTS
  "X9_Cl_PF3ONS"    # was X9.Cl.PF3ONS
)
# Function to load and standardize each dataset
load_and_standardize <- function(dataset_name, url, expected_region) {
  
  cat("Loading:", dataset_name, "...\n")
  
  # Load CSV
  df <- read_csv(url, show_col_types = FALSE)
  
  # Extract year and month from date columns (handle variable naming)
  if ("Sample Date (MM/DD/YYY)" %in% names(df)) {
    parsed_date <- as.Date(df$`Sample Date (MM/DD/YYY)`, format = "%m/%d/%Y")
    df <- df %>%
      mutate(
        year  = as.integer(format(parsed_date, "%Y")),
        month = as.integer(format(parsed_date, "%m"))
      )
  } else if ("Sampling Year" %in% names(df)) {
    df <- df %>%
      mutate(year = as.integer(`Sampling Year`))
  }
  
  # Filter to Surface Water only
  if ("Sample Type" %in% names(df)) {
    df <- df %>% filter(`Sample Type` == "Surface Water")
  }
  
  # Select core columns: essential spatial + temporal + PFAS compounds + source tracking
  df_clean <- df %>%
    dplyr::select(
      any_of(c("Latitude", "Longitude", "year", "month", all_pfas))
    ) %>%
    # Add metadata columns
    mutate(
      dataset_source = dataset_name,
      expected_region = expected_region,
      .before = Latitude
    )
  
  cat("  → Loaded:", nrow(df_clean), "observations\n")
  
  return(df_clean)
}

# Load all datasets
cat("LOADING ALL DATASETS\n")

all_data_list <- mapply(
  load_and_standardize,
  dataset_name = dataset_catalog$dataset_name,
  url = dataset_catalog$url,
  expected_region = dataset_catalog$expected_region,
  SIMPLIFY = FALSE
)

# Combine into single dataframe
global_pfas_raw <- bind_rows(all_data_list)

cat("GLOBAL DATABASE SUMMARY\n")
cat("Total observations:", nrow(global_pfas_raw), "\n")
cat("Datasets loaded:", length(all_data_list), "\n")
cat("Year range:", min(global_pfas_raw$year, na.rm = TRUE), 
    "to", max(global_pfas_raw$year, na.rm = TRUE), "\n\n")

# Show breakdown by dataset and region
cat("Observations by dataset:\n")
print(global_pfas_raw %>%
        group_by(dataset_source, expected_region) %>%
        summarise(n_obs = n(), .groups = "drop") %>%
        arrange(desc(n_obs)))

cat("\n")
# Count unique sites by unique coordinate pairs
unique_sites <- global_pfas_raw %>%
  distinct(Latitude, Longitude) %>%
  nrow()

cat("Total unique sites (unique coordinate pairs):", unique_sites, "\n\n")

# Breakdown of unique sites per dataset
cat("Unique sites by dataset:\n")
print(global_pfas_raw %>%
        group_by(dataset_source, expected_region) %>%
        summarise(
          n_obs        = n(),
          n_unique_sites = n_distinct(paste(Latitude, Longitude)),
          .groups = "drop"
        ) %>%
        arrange(desc(n_unique_sites)))



# ============================================================================
# PHASE 1, STEP 2: SPATIAL CLIPPING & REGIONAL SUBSETTING
# ============================================================================

library(sf)
library(rnaturalearth)
library(rnaturalearthhires)

# Load country boundaries at high resolution
world <- ne_countries(scale = 10, returnclass = "sf")

# Convert global data to spatial object
global_pfas_sf <- st_as_sf(
  global_pfas_raw %>% filter(!is.na(Latitude), !is.na(Longitude)),
  coords = c("Longitude", "Latitude"),
  crs = 4326  # WGS84
)

cat("Global PFAS dataset converted to spatial object\n")
cat("Total valid locations:", nrow(global_pfas_sf), "\n\n")

# ============================================================================
# Define regional subsets
# ============================================================================

# Function to clip data to country/region
clip_to_region <- function(data_sf, region_name, countries) {
  
  # Filter world map to selected countries
  region_map <- world %>% filter(admin %in% countries)
  
  # Spatial join: keep only points inside region
  region_data <- st_join(data_sf, 
                         region_map %>% dplyr::select(admin), 
                         join = st_intersects,
                         left = FALSE)
  
  cat(region_name, ":", nrow(region_data), "observations\n")
  
  return(region_data)
}

# ============================================================================
# CONUS (Continental US)
# ============================================================================

USA_states <- ne_states(country = "united states of america", returnclass = "sf")

conus_bbox <- st_as_sfc(
  st_bbox(c(xmin = -125, xmax = -66, ymin = 22, ymax = 49.5),
          crs = st_crs(4326))
)

conus_pfas <- global_pfas_sf %>%
  st_join(USA_states %>% dplyr::select(name), join = st_intersects, left = FALSE) %>%
  st_crop(conus_bbox) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(-name) %>%
  mutate(region = "USA_CONUS")

cat("CONUS (USA, lower 48):", nrow(conus_pfas), "observations\n")

# ============================================================================
# EUROPE
# ============================================================================

europe_countries <- c(
  "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden", "Austria"
)

europe_pfas <- clip_to_region(global_pfas_sf, "Europe", europe_countries) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  mutate(region = "Europe")

# ============================================================================
# CANADA
# ============================================================================

canada_pfas <- clip_to_region(global_pfas_sf, "Canada", c("Canada")) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  mutate(region = "Canada")

# ============================================================================
# AUSTRALIA
# ============================================================================

australia_pfas <- clip_to_region(global_pfas_sf, "Australia", c("Australia")) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  mutate(region = "Australia")

# ============================================================================
# REST OF WORLD
# ============================================================================

all_clipped <- bind_rows(conus_pfas, europe_pfas, canada_pfas, australia_pfas)

global_coords <- global_pfas_sf %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# Now anti_join will work
row_world <- global_coords %>%
  anti_join(
    all_clipped %>% dplyr::select(dataset_source, Latitude, Longitude),
    by = c("dataset_source", "Latitude", "Longitude")
  ) %>%
  mutate(region = "Other_Global")

cat("Other Global:", nrow(row_world), "observations\n")

# ============================================================================
# COMBINE ALL WITH REGION TAGS
# ============================================================================

global_pfas_regional <- bind_rows(
  conus_pfas, europe_pfas, canada_pfas, australia_pfas, row_world
)

cat("\n--- REGIONAL SUMMARY ---\n")
print(global_pfas_regional %>%
        group_by(region) %>%
        summarise(
          n_obs = n(),
          n_datasets = n_distinct(dataset_source),
          year_min = min(year, na.rm = TRUE),
          year_max = max(year, na.rm = TRUE),
          .groups = "drop"
        ))

# ============================================================================
# SAVE FOR NEXT PHASE
# ============================================================================

saveRDS(global_pfas_regional, "global_pfas_regional.rds")

cat("\n✓ Saved: global_pfas_regional.rds\n")
cat("Ready for Phase 1 visualization!\n")

#filtering the data for NH outliers
conus_data <- global_pfas_regional %>%
  filter(region == "USA_CONUS", !is.na(Latitude), !is.na(Longitude))

# --- Outlier removal: z-score filter per compound ---
# Remove observations where any PFAS compound is > 3 SD from the mean
# Applied log-scale since PFAS concentrations are log-normally distributed

pfas_present <- intersect(all_pfas, names(conus_data))

conus_data_filtered <- conus_data %>%
  mutate(across(
    any_of(pfas_present),
    ~ifelse(. <= 0, NA_real_, .)          # zero/negative → NA before log
  )) %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(any_of(pfas_present), names_to = "compound", values_to = "conc") %>%
  group_by(compound) %>%
  mutate(
    log_conc  = log10(conc),
    log_mean  = mean(log_conc, na.rm = TRUE),
    log_sd    = sd(log_conc,   na.rm = TRUE),
    z_score   = (log_conc - log_mean) / log_sd,
    is_outlier = !is.na(z_score) & abs(z_score) > 3.5
  ) %>%
  ungroup() %>%
  mutate(conc = ifelse(is_outlier, NA_real_, conc)) %>%  # NA out outliers, keep the row
  dplyr::select(-log_conc, -log_mean, -log_sd, -z_score, -is_outlier) %>%
  pivot_wider(names_from = compound, values_from = conc) %>%
  dplyr::select(-row_id)

# Report what was removed
cat("Observations before filtering:", nrow(conus_data), "\n")
cat("Rows retained (rows aren't dropped, outlier values set to NA):", nrow(conus_data_filtered), "\n")

# Then use conus_data_filtered going forward
conus_data <- conus_data_filtered

conus_data %>%
  distinct(Latitude, Longitude) %>%
  nrow()

# ============================================================================
# PHASE 1, STEP 3: Cleaned DATA EXPLORATION & VISUALIZATION
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(dbscan)
library(wesanderson)

# ============================================================================
# 1. SPATIAL DENSITY HEATMAP - CONUS
# ============================================================================

p_conus_spatial <- ggplot(conus_data, aes(x = Longitude, y = Latitude)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "tile", contour = FALSE, bins = 50) +
  scale_fill_viridis_c(name = "Density") +
  labs(
    title = "CONUS: Spatial Distribution of PFAS Observations",
    subtitle = paste0("n = ", nrow(conus_data), " observations"),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
p_conus_spatial
ggsave("01_conus_spatial_density.png", p_conus_spatial, width = 12, height = 8, dpi = 300)
cat("✓ Saved: 01_conus_spatial_density.png\n")

# ============================================================================
# 2. TEMPORAL TRENDS - CONUS
# ============================================================================

conus_temporal <- conus_data %>%
  group_by(year) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  filter(!is.na(year))

p_conus_temporal <- ggplot(conus_temporal, aes(x = year, y = n_obs)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_line(color = "darkblue", linewidth = 1) +
  labs(
    title = "CONUS: Sampling Effort Over Time",
    x = "Year", y = "Number of Observations"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
p_conus_temporal
ggsave("02_conus_temporal_trend.png", p_conus_temporal, width = 10, height = 6, dpi = 300)
cat("✓ Saved: 02_conus_temporal_trend.png\n")

# ============================================================================
# 3. COMPOUND DETECTION SUMMARY - CONUS
# ============================================================================

# Calculate detection frequency for each compound in CONUS
compound_detection <- conus_data %>%
  dplyr::select(any_of(all_pfas)) %>%
  summarise(across(everything(), 
                   list(
                     detected = ~sum(!is.na(.) & . > 0),
                     median = ~median(., na.rm = TRUE),
                     max = ~max(., na.rm = TRUE)
                   ))) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("compound", "metric"), sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value)

compound_detection <- compound_detection %>%
  mutate(detection_pct = (detected / nrow(conus_data)) * 100) %>%
  arrange(desc(detection_pct))

p_compound_detection <- ggplot(compound_detection, aes(x = reorder(compound, detection_pct), y = detection_pct)) +
  geom_col(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "CONUS: PFAS Compound Detection Frequency",
    x = "Compound", y = "Detection Frequency (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
p_compound_detection
ggsave("03_conus_compound_detection.png", p_compound_detection, width = 10, height = 8, dpi = 300)
cat("✓ Saved: 03_conus_compound_detection.png\n")

# Print summary table
cat("\nTop 10 Detected Compounds in CONUS:\n")
print(compound_detection %>% head(10))

# ============================================================================
# 4. REPEAT OBSERVATIONS - TEMPORAL VARIABILITY (ADVISOR ITEM #1)
# ============================================================================

# Find sites with repeat observations (same Lat/Lon, multiple years/months)
repeat_sites <- conus_data %>%
  group_by(Latitude, Longitude, dataset_source) %>%
  summarise(
    n_obs      = n(),
    n_years    = n_distinct(year,  na.rm = TRUE),
    n_months   = n_distinct(month, na.rm = TRUE),
    year_range = ifelse(
      all(is.na(year)), 
      NA_real_, 
      max(year, na.rm = TRUE) - min(year, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  filter(n_obs > 1) %>%
  arrange(desc(n_obs))

cat("\nSites with Repeat Observations (n > 1):\n")
cat("Total unique sites with repeats:", nrow(repeat_sites), "\n")
cat("Mean observations per site:", round(mean(repeat_sites$n_obs), 2), "\n")
cat("Max observations at single site:", max(repeat_sites$n_obs), "\n\n")

print(repeat_sites %>% head(15))
# Pull the actual coordinates of the top site first
repeat_sites %>% slice(1) %>% dplyr::select(Latitude, Longitude)

# Then filter on exact match instead of approximate
top_site <- repeat_sites %>% slice(1)

conus_data %>%
  filter(
    Latitude  == top_site$Latitude,
    Longitude == top_site$Longitude,
    dataset_source == "Caravan_PFAS"
  ) %>%
  group_by(year, month) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(year, month)
# ============================================================================
# 5. TEMPORAL VARIABILITY AT REPEAT SITES (ADVISOR ITEM #1)
# ============================================================================

# Calculate standard deviation of PFAS concentrations at repeat sites
repeat_site_variability <- repeat_sites %>%
  filter(n_obs >= 4) %>%          # <-- add this line
  left_join(conus_data, by = c("Latitude", "Longitude", "dataset_source")) %>%
  group_by(Latitude, Longitude, dataset_source) %>%
  summarise(
    across(any_of(all_pfas),
           list(
             cv = ~ifelse(mean(., na.rm = TRUE) == 0, NA_real_,
                          sd(., na.rm = TRUE) / mean(., na.rm = TRUE))
           )),
    .groups = "drop"
  )

# Calculate mean CV (coefficient of variation) for compounds with data
site_variability_summary <- repeat_site_variability %>%
  dplyr::select(ends_with("_cv")) %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("compound", "metric"), sep = "_cv") %>%
  drop_na(value) %>%
  group_by(compound) %>%
  summarise(
    mean_cv = mean(value, na.rm = TRUE),
    median_cv = median(value, na.rm = TRUE),
    max_cv = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cv))

cat("\nTemporal Variability at Repeat Sites (Coefficient of Variation):\n")
print(site_variability_summary, n = Inf)

# ============================================================================
# 6. DB SCAN CLUSTERING - METADATA + CONCENTRATIONS (ADVISOR ITEM #4)
# ============================================================================

cat("DB SCAN Clustering (Metadata + Concentrations)\n")

# Prepare data for DB SCAN: metadata (year, month) + top 5 PFAS compounds
top_compounds <- compound_detection %>% head(5) %>% pull(compound)

dbscan_data <- conus_data %>%
  dplyr::select(year, month, Latitude, Longitude, all_of(top_compounds)) %>%
  drop_na() %>%
  mutate(
    year = ifelse(is.na(year), median(year, na.rm = TRUE), year),
    month = ifelse(is.na(month), 6, month)  # Impute missing months with 6
  )

# Standardize features for DB SCAN
dbscan_scaled <- scale(dbscan_data %>% dplyr::select(year, month, all_of(top_compounds)))

# Run DB SCAN
db <- dbscan(dbscan_scaled, eps = 0.5, minPts = 5)

cat("DB SCAN Results:\n")
cat("Number of clusters:", length(unique(db$cluster[db$cluster > 0])), "\n")
cat("Noise points:", sum(db$cluster == 0), "\n")
cat("Clustered points:", sum(db$cluster > 0), "\n\n")

# Add cluster assignments back
dbscan_data$cluster <- db$cluster

# Visualize clusters in space
p_dbscan_spatial <- ggplot(dbscan_data, aes(x = Longitude, y = Latitude, color = factor(cluster))) +
  geom_point(size = 2, alpha = 0.6) +
  scale_color_manual(
    values = c("0" = "orange", setNames(rainbow(length(unique(db$cluster))-1), 
                                     as.character(1:(length(unique(db$cluster))-1)))),
    name = "Cluster"
  ) +
  labs(
    title = "CONUS: DB SCAN Clustering (Metadata + Top 5 PFAS)",
    subtitle = "orange points = Noise/Outliers",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
p_dbscan_spatial
ggsave("04_dbscan_clusters_spatial.png", p_dbscan_spatial, width = 12, height = 8, dpi = 300)
cat("✓ Saved: 04_dbscan_clusters_spatial.png\n")

# Summary by cluster
cat("\nCluster Sizes:\n")
print(table(dbscan_data$cluster))

# ============================================================================
# 7. RAW DATA SUMMARY STATISTICS
# ============================================================================
cat("RAW DATA SUMMARY STATISTICS (CONUS)\n")

summary_stats <- conus_data %>%
  dplyr::select(any_of(all_pfas)) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    n_detected = sum(!is.na(value) & value > 0),
    n_total = n(),
    pct_detected = (n_detected / n_total) * 100,
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_detected))

print(summary_stats)

# Save summary as CSV for your report
write.csv(summary_stats, "05_raw_data_summary_stats.csv", row.names = FALSE)
cat("\n✓ Saved: 05_raw_data_summary_stats.csv\n")

cat("Phase 1 exploration complete!\n")
cat("Ready for Phase 2: Data Cleaning\n")


#SCRIPT 1: IMPROVED PHASE 1 VISUALIZATIONS
#(Spatial Density Heatmaps, Temporal Patterns, Metadata Distributions)

library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(viridis)
library(patchwork)
library(scales)

# Get US state boundaries for context
states <- map_data("state")
p_conus_spatial <- ggplot() +
 geom_polygon(data = states, aes(x = long, y = lat, group = group),
 fill = "grey95", color = "grey70", linewidth = 0.2) +
 stat_density_2d(data = conus_data,
 aes(x = Longitude, y = Latitude, fill = after_stat(density)),
 geom = "tile", contour = FALSE, alpha = 0.7) +
 scale_fill_viridis_c(option = "inferno", name = "Observation\nDensity",
 na.value = "transparent") +
 geom_point(data = conus_data, aes(x = Longitude, y = Latitude),
 size = 0.3, alpha = 0.1, color = "white") +
 coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
 labs(
 title = "CONUS: Spatial Density of PFAS Surface Water Observations",
 subtitle = paste0("n = ", format(nrow(conus_data), big.mark = ","), " observations | ",
 n_distinct(conus_data$dataset_source), " datasets"),
 x = "Longitude", y = "Latitude"
 ) +
 theme_minimal(base_size = 12) +
 theme(
 plot.title = element_text(face = "bold", size = 14),
 panel.grid = element_line(color = "grey90")
 )
ggsave("01_conus_spatial_density_improved.png", p_conus_spatial,
 width = 14, height = 8, dpi = 300)
# --- 1B. DATASET SOURCE MAP (color by dataset) ---
p_source_map <- ggplot() +
 geom_polygon(data = states, aes(x = long, y = lat, group = group),
 fill = "grey95", color = "grey70", linewidth = 0.2) +
 geom_point(data = conus_data,
 aes(x = Longitude, y = Latitude, color = dataset_source),
 size = 1, alpha = 0.5) +
 coord_sf(xlim = c(-125, -66), ylim = c(24, 50), crs = 4326) +
 labs(
 title = "CONUS: Observations Colored by Source Dataset",
 subtitle = "Identifying spatial coverage gaps between datasets",
 x = "Longitude", y = "Latitude", color = "Dataset"
 ) +
 theme_minimal(base_size = 12) +
 theme(
 plot.title = element_text(face = "bold", size = 14),
 legend.position = "bottom",
 legend.text = element_text(size = 7)
 ) +
 guides(color = guide_legend(ncol = 4, override.aes = list(size = 3, alpha = 1)))
ggsave("01b_conus_source_map.png", p_source_map, width = 14, height = 10, dpi = 300)
# --- 1C. TEMPORAL PATTERNS (year + month heatmap) ---
temporal_heatmap <- conus_data %>%
 filter(!is.na(year), !is.na(month)) %>%
 group_by(year, month) %>%
 summarise(n_obs = n(), .groups = "drop")
p_temporal_heat <- ggplot(temporal_heatmap, aes(x = year, y = factor(month), fill = n_obs)) +
 geom_tile(color = "white", linewidth = 0.3) +
 scale_fill_viridis_c(option = "plasma", name = "N Observations",
 trans = "log1p", labels = comma) +
 scale_y_discrete(labels = month.abb) +
 labs(
 title = "CONUS: Sampling Effort by Year and Month",
 subtitle = "Log-scaled color to show temporal sampling gaps",
 x = "Year", y = "Month"
 ) +
 theme_minimal(base_size = 12) +
 theme(plot.title = element_text(face = "bold", size = 14))
ggsave("02_temporal_heatmap.png", p_temporal_heat, width = 12, height = 6, dpi = 300)
# --- 1D. METADATA DISTRIBUTIONS (multi-panel) ---
# Dataset size distribution
dataset_sizes <- conus_data %>%
 group_by(dataset_source) %>%
 summarise(n_obs = n(), n_sites = n_distinct(paste(Latitude, Longitude)),
 .groups = "drop") %>%
 arrange(desc(n_obs))
p_meta1 <- ggplot(dataset_sizes, aes(x = reorder(dataset_source, n_obs), y = n_obs)) +
 geom_col(fill = "steelblue", alpha = 0.8) +
 geom_text(aes(label = format(n_obs, big.mark = ",")), hjust = -0.1, size = 3) +
 coord_flip() +
 scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
 labs(title = "Observations per Dataset", x = NULL, y = "N Observations") +
 theme_minimal(base_size = 11)
# Year distribution
p_meta2 <- ggplot(conus_data %>% filter(!is.na(year)),
 aes(x = year)) +
 geom_histogram(fill = "coral", alpha = 0.8, binwidth = 1, color = "white") +
 labs(title = "Distribution of Sampling Years", x = "Year", y = "Count") +
 theme_minimal(base_size = 11)
# Spatial coverage (lat/lon marginals)
p_meta3 <- ggplot(conus_data, aes(x = Longitude)) +
 geom_histogram(fill = "darkgreen", alpha = 0.7, bins = 50, color = "white") +
 labs(title = "Longitude Distribution", x = "Longitude", y = "Count") +
 theme_minimal(base_size = 11)
p_meta4 <- ggplot(conus_data, aes(x = Latitude)) +
 geom_histogram(fill = "purple", alpha = 0.7, bins = 50, color = "white") +
 labs(title = "Latitude Distribution", x = "Latitude", y = "Count") +
 theme_minimal(base_size = 11)
p_metadata_panel <- (p_meta1 | (p_meta2 / p_meta3 / p_meta4)) +
 plot_annotation(
 title = "CONUS PFAS Dataset: Metadata Distributions",
 theme = theme(plot.title = element_text(face = "bold", size = 16))
 )
ggsave("03_metadata_distributions.png", p_metadata_panel, width = 16, height = 10, dpi = 300)
# --- 1E. COMPOUND DETECTION FREQUENCY (improved) ---
compound_stats <- conus_data %>%
 select(any_of(all_pfas)) %>%
 pivot_longer(everything(), names_to = "compound", values_to = "concentration") %>%
 group_by(compound) %>%
 summarise(
 n_total = n(),
 n_detected = sum(!is.na(concentration) & concentration > 0),
 detection_pct = (n_detected / n_total) * 100,
 median_conc = median(concentration[concentration > 0], na.rm = TRUE),
 p95_conc = quantile(concentration[concentration > 0], 0.95, na.rm = TRUE),
 .groups = "drop"
 ) %>%
 arrange(desc(detection_pct))
p_detection <- ggplot(compound_stats %>% filter(detection_pct > 0),
                      aes(x = reorder(compound, detection_pct), y = detection_pct)) +
  geom_col(aes(fill = log10(median_conc + 1)), alpha = 0.9) +
  geom_text(aes(label = paste0("n=", n_detected)), hjust = -0.1, size = 3) +
  scale_fill_viridis_c(option = "magma", name = "log10(Median\nConcentration)") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.6) +
  annotate("text", x = 2, y = 6, label = "Less than 5%", color = "red", size = 3) +
  labs(
    title = "PFAS Compound Detection Frequency in CONUS",
    subtitle = "Color intensity = median concentration among detections",
    x = "Compound", y = "Detection Frequency (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))
p_detection
ggsave("04_compound_detection.png", p_detection, width = 11, height = 10, dpi =
300)

#combining a few plots. 
p_source_map <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_point(data = conus_data,
             aes(x = Longitude, y = Latitude, color = dataset_source),
             size = 1.2, alpha = 0.6) +
  coord_sf(xlim = c(-125, -66), ylim = c(22, 50), crs = 4326) +
  labs(
    title = "A) Spatial Distribution by Dataset",
    x = "Longitude", y = "Latitude", color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 3, alpha = 1)))

# Right panel: Filter to meaningful detection, fix annotation
p_detection <- ggplot(compound_stats %>% filter(detection_pct > 1),
                      aes(x = reorder(compound, detection_pct), y = detection_pct)) +
  geom_col(aes(fill = log10(median_conc + 1)), alpha = 0.9, width = 0.75) +
  geom_text(aes(label = format(n_detected, big.mark = ",")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_viridis_c(option = "magma", name = "log10(Median\nConc. ng/L)") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  geom_vline(xintercept = which(levels(reorder(
    (compound_stats %>% filter(detection_pct > 1))$compound,
    (compound_stats %>% filter(detection_pct > 1))$detection_pct
  )) == 
    (compound_stats %>% filter(detection_pct > 1) %>% 
       filter(detection_pct <= 5) %>% 
       slice_max(detection_pct) %>% pull(compound))[1]) + 0.5,
  linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "B) Compound Detection Frequency",
    subtitle = "Compounds detected in >1% of samples",
    x = NULL, y = "Detection Frequency (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    legend.position = "right"
  )

# Combine
p_combined <- p_source_map + p_detection +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    title = "CONUS PFAS Surface Water Database Overview",
    subtitle = paste0("n = ", format(nrow(conus_data), big.mark = ","),
                      " observations | ", n_distinct(conus_data$dataset_source), " datasets"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 14, color = "grey30")
    )
  )

ggsave("05_combined_spatial_detection_v2.png", p_combined,
       width = 22, height = 11, dpi = 300)


# ============================================================================
# TEMPORAL VARIABILITY MAPS

# Define compound groups
short_chain <- c("PFBS", "PFHxA", "PFBA", "PFPeA", "PFHxS", "PFHpA")
long_chain  <- c("PFOS", "PFOA", "PFNA", "PFDA", "PFUnDA", "PFDoA")

# --- Step 1: Compute site-level temporal statistics ---

repeat_temporal <- conus_data %>%
  group_by(Latitude, Longitude) %>%
  filter(n() >= 3) %>%  # minimum 3 observations for meaningful CV
  summarise(
    n_obs = n(),
    n_years = n_distinct(year, na.rm = TRUE),
    year_span = max(year, na.rm = TRUE) - min(year, na.rm = TRUE),
    
    # CV for each compound (only where detected in 2+ observations)
    across(any_of(c(short_chain, long_chain)), list(
      cv = ~ifelse(sum(!is.na(.) & . > 0) >= 2,
                   sd(.[. > 0], na.rm = TRUE) / mean(.[. > 0], na.rm = TRUE),
                   NA_real_)
    ), .names = "{.col}_{.fn}"),
    
    .groups = "drop"
  )

# --- Step 2: Compute mean CV across all detected compounds ---

cv_cols <- names(repeat_temporal)[grepl("_cv$", names(repeat_temporal))]
short_cv_cols <- paste0(short_chain, "_cv")
long_cv_cols  <- paste0(long_chain, "_cv")

# Keep only columns that actually exist
short_cv_cols <- intersect(short_cv_cols, names(repeat_temporal))
long_cv_cols  <- intersect(long_cv_cols, names(repeat_temporal))

repeat_temporal <- repeat_temporal %>%
  rowwise() %>%
  mutate(
    # Overall mean CV
    mean_cv_all = mean(c_across(any_of(cv_cols)), na.rm = TRUE),
    
    # Short-chain mean CV
    mean_cv_short = mean(c_across(any_of(short_cv_cols)), na.rm = TRUE),
    
    # Long-chain mean CV
    mean_cv_long = mean(c_across(any_of(long_cv_cols)), na.rm = TRUE),
    
    # Ratio: short/long (>1 = short-chain more variable, <1 = long-chain more variable)
    cv_ratio = mean_cv_short / mean_cv_long
  ) %>%
  ungroup() %>%
  # Remove sites with no valid CVs
  
  filter(!is.na(mean_cv_all))

# Cap extreme CV ratio values for visualization
repeat_temporal <- repeat_temporal %>%
  mutate(
    cv_ratio_capped = case_when(
      cv_ratio > 5  ~ 5,
      cv_ratio < 0.2 ~ 0.2,
      TRUE ~ cv_ratio
    ),
    log_cv_ratio = log2(cv_ratio_capped)  # log2 so 0 = equal, +1 = 2x short, -1 = 2x long
  )

cat("Sites with temporal data (n >= 3 obs):", nrow(repeat_temporal), "\n")
cat("Mean CV range:", round(min(repeat_temporal$mean_cv_all, na.rm = TRUE), 2), 
    "to", round(max(repeat_temporal$mean_cv_all, na.rm = TRUE), 2), "\n")
cat("CV ratio (short/long) range:", round(min(repeat_temporal$cv_ratio, na.rm = TRUE), 2),
    "to", round(max(repeat_temporal$cv_ratio, na.rm = TRUE), 2), "\n\n")

# --- Step 3: Map A — Overall Temporal Variability ---

states <- map_data("state")

p_cv_map <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey60", linewidth = 0.2) +
  # All CONUS sites as background (faint)
  geom_point(data = conus_data %>% distinct(Latitude, Longitude),
             aes(x = Longitude, y = Latitude),
             size = 0.3, alpha = 0.15, color = "grey50") +
  # Repeat sites: size = n_obs, color = mean CV
  geom_point(data = repeat_temporal,
             aes(x = Longitude, y = Latitude, size = n_obs, fill = mean_cv_all),
             shape = 21, color = "black", stroke = 0.3, alpha = 0.8) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Mean CV\n(all compounds)",
    limits = c(0, quantile(repeat_temporal$mean_cv_all, 0.95, na.rm = TRUE)),
    oob = scales::squish
  ) +
  scale_size_continuous(
    name = "N Repeat\nObservations",
    range = c(2, 12),
    breaks = c(3, 5, 10, 20, 37)  # 37 = your actual max from repeat_sites
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(22, 50), crs = 4326) +
  labs(
    title = "A) Temporal Variability at Repeat-Sampled Sites",
    subtitle = "Size = sampling intensity | Color = coefficient of variation across all PFAS",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    legend.position = "right"
  ) +
  guides(
    fill = guide_colorbar(order = 1, barheight = 8),
    size = guide_legend(order = 2)
  )


# --- Step 4: Map B — Short-chain vs Long-chain CV Ratio ---

# Only include sites with both short and long chain data
ratio_data <- repeat_temporal %>%
  filter(!is.na(log_cv_ratio), is.finite(log_cv_ratio))

p_ratio_map <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_point(data = conus_data %>% distinct(Latitude, Longitude),
             aes(x = Longitude, y = Latitude),
             size = 0.3, alpha = 0.15, color = "grey50") +
  geom_point(data = ratio_data,
             aes(x = Longitude, y = Latitude, size = n_obs, fill = log_cv_ratio),
             shape = 21, color = "black", stroke = 0.3, alpha = 0.8) +
  scale_fill_gradient2(
    low = "#2166AC",      # blue = long-chain more variable (legacy remobilization)
    mid = "white",
    high = "#B2182B",     # red = short-chain more variable (active sources)
    midpoint = 0,
    name = "log2(Short-chain CV\n/ Long-chain CV)",
    limits = c(-2.5, 2.5),
    oob = scales::squish,
    labels = c("Long-chain\ndominant", "", "Equal", "", "Short-chain\ndominant"),
    breaks = c(-2, -1, 0, 1, 2)
  ) +
  scale_size_continuous(
    name = "N Repeat\nObservations",
    range = c(2, 12),
    breaks = c(3, 5, 10, 20, 37),
    limits = c(3, 37)
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(22, 50), crs = 4326) +
  labs(
    title = "B) Source Signature: Short-chain vs Long-chain Variability",
    subtitle = "Red = active/replacement PFAS sources | Blue = legacy contamination remobilization",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    legend.position = "right"
  ) +
  guides(
    fill = guide_colorbar(order = 1, barheight = 8),
    size = guide_legend(order = 2)
  )


# --- Step 5: Combine as patchwork ---

p_temporal_combined <- p_cv_map / p_ratio_map +
  plot_annotation(
    title = "Temporal Variability as a Predictive Feature",
    subtitle = paste0(nrow(repeat_temporal), " sites with 3+ repeat observations | ",
                      "Source persistence matters more than concentration"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 13, color = "grey30")
    )
  )

ggsave("06_temporal_variability_maps.png", p_temporal_combined,
       width = 18, height = 16, dpi = 300)

# --- Step 6: Summary stats for your methods section ---

cat("\n--- TEMPORAL VARIABILITY SUMMARY ---\n")
cat("Sites with 3+ observations:", nrow(repeat_temporal), "\n")
cat("Median repeat observations per site:", median(repeat_temporal$n_obs), "\n")
cat("Mean CV (overall):", round(mean(repeat_temporal$mean_cv_all, na.rm = TRUE), 3), "\n")
cat("Mean CV (short-chain):", round(mean(repeat_temporal$mean_cv_short, na.rm = TRUE), 3), "\n")
cat("Mean CV (long-chain):", round(mean(repeat_temporal$mean_cv_long, na.rm = TRUE), 3), "\n\n")

# Classification counts
cat("Temporal Classification:\n")
repeat_temporal %>%
  mutate(temporal_class = case_when(
    mean_cv_all < 0.5 ~ "Persistent (CV < 0.5)",
    mean_cv_all > 1.5 ~ "Episodic (CV > 1.5)",
    TRUE ~ "Mixed/Seasonal"
  )) %>%
  count(temporal_class) %>%
  print()

cat("\nCV Ratio Interpretation:\n")
ratio_data %>%
  mutate(source_type = case_when(
    log_cv_ratio > 0.5  ~ "Active sources (short-chain dominant)",
    log_cv_ratio < -0.5 ~ "Legacy remobilization (long-chain dominant)",
    TRUE ~ "Mixed/indeterminate"
  )) %>%
  count(source_type) %>%
  print()

library(gt)

cv_table <- conus_data %>%
  dplyr::select(any_of(all_pfas)) %>%
  pivot_longer(everything(), names_to = "compound", values_to = "conc") %>%
  filter(!is.na(conc), conc > 0) %>%
  group_by(compound) %>%
  summarise(
    n_obs        = n(),
    mean_conc    = mean(conc, na.rm = TRUE),
    median_conc  = median(conc, na.rm = TRUE),
    sd_conc      = sd(conc, na.rm = TRUE),
    cv           = sd_conc / mean_conc,
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs)) %>%
  slice(1:10) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# Print to console
print(cv_table)

# Nice formatted table with gt
cv_table %>%
  gt() %>%
  tab_header(
    title    = "Top 10 PFAS Compounds by Detection Frequency",
    subtitle = "CONUS surface water observations (outliers removed)"
  ) %>%
  cols_label(
    compound    = "Compound",
    n_obs       = "N Detections",
    mean_conc   = "Mean (ng/L)",
    median_conc = "Median (ng/L)",
    sd_conc     = "SD (ng/L)",
    cv          = "CV"
  ) %>%
  fmt_number(columns = c(mean_conc, median_conc, sd_conc), decimals = 2) %>%
  fmt_number(columns = cv, decimals = 3) %>%
  data_color(
    columns = cv,
    palette = "viridis"
  ) %>%
  tab_source_note("CV = coefficient of variation (SD/mean); higher values indicate greater temporal variability")















#SCRIPT 2: DBSCAN CLUSTERING - METADATA vs. COMPOUNDS ONLY (No Predictors)

library(dbscan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(patchwork)

cat("DBSCAN CLUSTERING: METADATA vs COMPOUNDS (No Predictors)\n")
# Define top detected compounds (adjust based on your detection analysis)
top_compounds <- c("PFOS", "PFOA", "PFHxS", "PFBS", "PFNA", "PFHxA", "PFHpA", "PFDA",
"PFPeA", "PFBA")

# CLUSTERING APPROACH A: METADATA ONLY (Geography + Time)
# Question: Do observations cluster by WHERE and WHEN, regardless of chemistry?
# =======================================================================

cat("--- APPROACH A: METADATA ONLY (Lat, Lon, Year, Month) ---\n\n")
meta_data <- conus_data %>%
 select(Latitude, Longitude, year, month, dataset_source) %>%
 filter(!is.na(year), !is.na(month)) %>%
 mutate(
 site_id = paste(round(Latitude, 4), round(Longitude, 4), sep = "_")
 )
# Scale metadata features
meta_scaled <- scale(meta_data %>% select(Latitude, Longitude, year, month))
# Determine optimal eps using k-nearest neighbor distance plot
kNNdist_plot <- kNNdistplot(meta_scaled, k = 5)
# Visual inspection: look for "elbow" in the distance curve
# Run DBSCAN with multiple eps values for sensitivity analysis
eps_values <- c(0.3, 0.5, 0.7, 1.0, 1.5)
minPts_values <- c(5, 10, 20)
dbscan_results_meta <- list()
cat("Sensitivity Analysis - Metadata Clustering:\n")
cat(sprintf("%-6s %-8s %-10s %-12s %-10s\n", "eps", "minPts", "Clusters", "Noise_Pct",
"Largest"))
for (eps in eps_values) {
 for (mp in minPts_values) {
 db <- dbscan(meta_scaled, eps = eps, minPts = mp)
 n_clusters <- length(unique(db$cluster[db$cluster > 0]))
 noise_pct <- round(sum(db$cluster == 0) / length(db$cluster) * 100, 1)
 largest <- if (n_clusters > 0) max(table(db$cluster[db$cluster > 0])) else 0

 cat(sprintf("%-6.1f %-8d %-10d %-12.1f %-10d\n", eps, mp, n_clusters, noise_pct, largest))

 dbscan_results_meta[[paste(eps, mp, sep = "_")]] <- list(
 eps = eps, minPts = mp, db = db,
 n_clusters = n_clusters, noise_pct = noise_pct
 )
 }
}
# Select best result (balance between too many clusters and too much noise)
# Heuristic: choose result with noise between 10-30% and reasonable cluster count
best_meta <- dbscan_results_meta[["0.5_10"]] # Adjust based on your output
db_meta <- best_meta$db
meta_data$cluster_meta <- db_meta$cluster
cat("\n\nSelected Metadata Clustering (eps=0.5, minPts=10):\n")
cat("Clusters:", best_meta$n_clusters, "\n")
cat("Noise points:", sum(db_meta$cluster == 0), "\n")
cat("Clustered points:", sum(db_meta$cluster > 0), "\n\n")
# Characterize metadata clusters
cat("Cluster Characterization (Metadata):\n")
meta_cluster_summary <- meta_data %>%
 filter(cluster_meta > 0) %>%
 group_by(cluster_meta) %>%
 summarise(
 n = n(),
 mean_lat = round(mean(Latitude), 2),
 mean_lon = round(mean(Longitude), 2),
 mean_year = round(mean(year), 1),
 year_range = paste(min(year), max(year), sep = "-"),
 n_datasets = n_distinct(dataset_source),
 primary_dataset = names(sort(table(dataset_source), decreasing = TRUE))[1],
 .groups = "drop"
 ) %>%
 arrange(desc(n))
print(meta_cluster_summary)
# =======================================================================

# CLUSTERING APPROACH B: COMPOUNDS ONLY (Chemical Fingerprint)
# Question: Do observations cluster by WHAT chemicals are present?
# =======================================================================

cat("\n\n--- APPROACH B: COMPOUNDS ONLY (Chemical Fingerprints) ---\n\n")
# Prepare compound data - use log-transformed concentrations
compound_data <- conus_data %>%
 select(Latitude, Longitude, year, dataset_source, all_of(top_compounds)) %>%
 filter(if_any(all_of(top_compounds), ~ !is.na(.) & . > 0))
# Log-transform and impute zeros with half-minimum detection
compound_matrix <- compound_data %>%
 select(all_of(top_compounds)) %>%
 mutate(across(everything(), ~ {
 half_min <- min(.[. > 0], na.rm = TRUE) / 2
 log10(ifelse(is.na(.) | . <= 0, half_min, .) + 1)
 }))
# Remove rows with all NAs
valid_rows <- rowSums(!is.na(compound_matrix)) > 0
compound_matrix <- compound_matrix[valid_rows, ]
compound_data <- compound_data[valid_rows, ]
# Scale compound features
compound_scaled <- scale(compound_matrix)
# Handle any remaining NAs (replace with 0 = mean after scaling)
compound_scaled[is.na(compound_scaled)] <- 0
# kNN distance plot for eps selection
kNNdistplot(compound_scaled, k = 5)
# Sensitivity analysis for compound clustering
cat("Sensitivity Analysis - Compound Clustering:\n")
cat(sprintf("%-6s %-8s %-10s %-12s %-10s\n", "eps", "minPts", "Clusters", "Noise_Pct",
"Largest"))
dbscan_results_chem <- list()
for (eps in c(0.5, 0.8, 1.0, 1.5, 2.0)) {
 for (mp in c(5, 10, 20)) {
 db <- dbscan(compound_scaled, eps = eps, minPts = mp)
 n_clusters <- length(unique(db$cluster[db$cluster > 0]))
 noise_pct <- round(sum(db$cluster == 0) / length(db$cluster) * 100, 1)
 largest <- if (n_clusters > 0) max(table(db$cluster[db$cluster > 0])) else 0

 cat(sprintf("%-6.1f %-8d %-10d %-12.1f %-10d\n", eps, mp, n_clusters, noise_pct, largest))

 dbscan_results_chem[[paste(eps, mp, sep = "_")]] <- list(
 eps = eps, minPts = mp, db = db,
 n_clusters = n_clusters, noise_pct = noise_pct
 )
 }
}
# Select best compound clustering
best_chem <- dbscan_results_chem[["1.0_10"]] # Adjust based on output
db_chem <- best_chem$db
compound_data$cluster_chem <- db_chem$cluster
cat("\n\nSelected Compound Clustering (eps=1.0, minPts=10):\n")
cat("Clusters:", best_chem$n_clusters, "\n")
cat("Noise points:", sum(db_chem$cluster == 0), "\n\n")
# Characterize chemical clusters
cat("Cluster Chemical Profiles:\n")
chem_cluster_profiles <- compound_data %>%
 filter(cluster_chem > 0) %>%
 group_by(cluster_chem) %>%
 summarise(
 n = n(),
 across(all_of(top_compounds), ~ median(., na.rm = TRUE), .names = "med_{.col}"),
 .groups = "drop"
 ) %>%
 arrange(desc(n))
print(chem_cluster_profiles)
# =======================================================================

# COMPARISON: Do metadata clusters align with chemical clusters?
# =======================================================================

cat("\n\n--- CROSS-COMPARISON: Metadata vs Chemical Clusters ---\n\n")
# Merge both cluster assignments (for overlapping observations)
comparison_data <- compound_data %>%
 left_join(
 meta_data %>% select(Latitude, Longitude, year, cluster_meta),
 by = c("Latitude", "Longitude", "year")
 ) %>%
 filter(!is.na(cluster_meta), cluster_meta > 0, cluster_chem > 0)
if (nrow(comparison_data) > 0) {
 cross_tab <- table(
 Meta = comparison_data$cluster_meta,
 Chem = comparison_data$cluster_chem
 )
 cat("Cross-tabulation (Metadata Cluster vs Chemical Cluster):\n")
 print(cross_tab)

 # Adjusted Rand Index for cluster agreement
 if (requireNamespace("mclust", quietly = TRUE)) {
 ari <- mclust::adjustedRandIndex(comparison_data$cluster_meta,
comparison_data$cluster_chem)
 cat("\nAdjusted Rand Index:", round(ari, 4), "\n")
 cat("Interpretation: 0 = random, 1 = perfect agreement\n")
 cat("If low: Geography/time and chemistry tell DIFFERENT stories\n")
 cat("If high: Chemical fingerprints are spatially/temporally determined\n")
 }
}
# =======================================================================

# VISUALIZATION: Dual-panel cluster maps
# =======================================================================

states <- map_data("state")
p_cluster_meta <- ggplot() +
 geom_polygon(data = states, aes(x = long, y = lat, group = group),
 fill = "grey95", color = "grey70", linewidth = 0.2) +
 geom_point(data = meta_data %>% filter(cluster_meta > 0),
 aes(x = Longitude, y = Latitude, color = factor(cluster_meta)),
 size = 1.5, alpha = 0.6) +
 geom_point(data = meta_data %>% filter(cluster_meta == 0),
 aes(x = Longitude, y = Latitude),
 size = 0.5, alpha = 0.2, color = "grey50") +
 coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
 labs(title = "A) Metadata Clusters (Geography + Time)",
 subtitle = "Grey = noise points", color = "Cluster") +
 theme_minimal(base_size = 11) +
 theme(legend.position = "bottom")
p_cluster_chem <- ggplot() +
 geom_polygon(data = states, aes(x = long, y = lat, group = group),
 fill = "grey95", color = "grey70", linewidth = 0.2) +
 geom_point(data = compound_data %>% filter(cluster_chem > 0),
 aes(x = Longitude, y = Latitude, color = factor(cluster_chem)),
 size = 1.5, alpha = 0.6) +
 geom_point(data = compound_data %>% filter(cluster_chem == 0),
 aes(x = Longitude, y = Latitude),
 size = 0.5, alpha = 0.2, color = "grey50") +
 coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
 labs(title = "B) Chemical Fingerprint Clusters (Compounds Only)",
 subtitle = "Grey = noise points", color = "Cluster") +
 theme_minimal(base_size = 11) +
 theme(legend.position = "bottom")
p_dual_cluster <- p_cluster_meta / p_cluster_chem +
 plot_annotation(
 title = "DBSCAN Clustering: Metadata vs. Chemical Fingerprints",
 subtitle = "Key Question: Does observation geometry drive chemical patterns?",
 theme = theme(plot.title = element_text(face = "bold", size = 14))
 )
ggsave("05_dbscan_dual_comparison.png", p_dual_cluster, width = 14, height = 14, dpi = 300)
# Save cluster assignments
saveRDS(list(
 meta_clusters = meta_data,
 chem_clusters = compound_data,
 meta_params = list(eps = best_meta$eps, minPts = 10),
 chem_params = list(eps = best_chem$eps, minPts = 10)
), "dbscan_cluster_results.rds")
cat("\nSaved: dbscan_cluster_results.rds\n")
cat("Saved: 05_dbscan_dual_comparison.png\n")

#------------GEt COMIDS-----------------------------

library(sf)
library(dplyr)
library(future.apply)
library(nhdplusTools)
setwd("C:/Users/Marston User/Documents/CONUS_PFAS")
# reuse saved COMIDs
if (file.exists("conus_datasets_streamcat.rds")) {
  
  saved <- readRDS("conus_datasets_streamcat.rds") %>%
    select(Latitude, Longitude, COMID) %>%
    filter(!is.na(COMID)) %>%
    distinct(Latitude, Longitude, .keep_all = TRUE)
  
  conus_data <- conus_data %>%
    left_join(saved, by = c("Latitude", "Longitude"))
  
  cat("COMIDs from saved file:",
      sum(!is.na(conus_data$COMID)),
      "/", nrow(conus_data), "\n")
  
} else {
  
  conus_data$COMID <- NA_integer_
}

# indices needing COMIDs
missing_idx <- which(is.na(conus_data$COMID))

cat("Fetching COMIDs for",
    length(missing_idx),
    "new points\n")


valid_coords <- which(
  !is.na(conus_data$Longitude) &
    !is.na(conus_data$Latitude)
)

# make sf object only for valid rows
conus_data_sf <- st_as_sf(
  conus_data[valid_coords, ],
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

# rows still needing COMIDs AND valid coords
missing_idx <- which(
  is.na(conus_data$COMID) &
    !is.na(conus_data$Longitude) &
    !is.na(conus_data$Latitude)
)

# map original row indices -> sf row indices
sf_match <- match(missing_idx, valid_coords)

pts_missing <- conus_data_sf[sf_match, ]

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
save_file <- "conus_data_partial_comids.rds"

# rows still missing COMIDs
missing_idx <- which(
  is.na(conus_data$COMID) &
    !is.na(conus_data$Longitude) &
    !is.na(conus_data$Latitude)
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
    
    pts_chunk <- conus_data_sf[sf_match, ]
    
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
    
    # write back to conus_data
    conus_data$COMID[idx_chunk] <- comids
    
    # autosave after every chunk
    saveRDS(conus_data, save_file)
    
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
saveRDS(conus_data, "conus_datasets_COMID.rds")
#------------Gather StreamCat information-------------------------

unique_comids <- unique(na.omit(conus_data$COMID))
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

conus_data <- conus_data %>% left_join(as.data.frame(streamcat_df), by = c("COMID" = "comid"))
conus_data_nozero <- conus_data_nozero %>% left_join(as.data.frame(streamcat_df), by = c("COMID" = "comid"))

sc_preds_found <- intersect(streamcat_metrics, names(conus_data))
cat("StreamCat metrics joined:", length(sc_preds_found), "/", length(streamcat_metrics), "\n\n")

saveRDS(conus_data,        "conus_datasets_streamcat_all.rds")
saveRDS(conus_data_nozero, "conus_datasets_streamcat_nozero.rds")

#setting up the rf and xgboost model functions
library(doParallel)
library(foreach)

run_model <- function(conus_data_input,
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
  
  conus_data_input <- conus_data_input %>%
    group_by(source) %>%
    mutate(weight = 1 / sqrt(n())) %>%
    ungroup()
  
  conus_data_input <- conus_data_input %>%
    mutate(across(all_of(all_pfas), ~ log10(.x + 1)))
  
  preds_all <- c(sc_preds_found)
  preds_df <- water_input %>%
    dplyr::select(all_of(preds_all))
  
  set.seed(seed)
  folds <- sample(rep(1:nfolds, length.out = nrow(conus_data_input)))
  
  cat("\n", model_type, "|", nfolds, "-fold CV [", suffix, ", n =", nrow(water_input), "]\n")
  
  # PARALLEL COMPOUND LOOP
  results_list <- foreach(
    compound = all_pfas,
    .packages = c("dplyr", "randomForest", "xgboost"),
    .errorhandling = "pass"  # if one compound fails, others continue
  ) %dopar% {
    
    outcome <- conus_data_input[[compound]]
    
    if (sum(!is.na(outcome)) < 150) {
      return(list(compound = compound, skipped = TRUE))
    }
    
    weights <- conus_data_input$weight
    
    if (hotspot_weight) {
      thresh <- quantile(outcome, hotspot_quantile, na.rm = TRUE)
      hotspot_mult <- ifelse(outcome >= thresh, hotspot_boost, 1)
      weights <- weights * hotspot_mult
    }
    
    cv_preds <- rep(NA_real_, nrow(conus_data_input))
    
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
      source = conus_data_input$source,
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
    conus_data_input = conus_data_input,
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
conus_data_above_BDL <- conus_data %>%
  mutate(across(all_of(all_pfas), ~ ifelse(.x < 0.2, NA, .x)))

results_all_rf_BDL <- run_model(
  conus_data_above_BDL,
  suffix = "all",
  model_type = "rf"
)

results_all_xgb_BDL <- run_model(
  conus_data_above_BDL,
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

#-----------


















library(dplyr)
library(tidyr)
library(ggplot2)
# =======================================================================

# CORE FUNCTION: detect_outliers()
# Flexible outlier detection with multiple methods and configurable SD thresholds
# =======================================================================

detect_outliers <- function(data,
 compounds = NULL,
 method = c("sd", "iqr", "mad", "percentile"),
 sd_threshold = 3,
 iqr_multiplier = 1.5,
 mad_threshold = 3,
 percentile_upper = 0.99,
 percentile_lower = 0.01,
 log_transform = TRUE,
 group_by_vars = NULL,
 flag_only = FALSE) {

# ARGUMENTS:
# data - dataframe with PFAS concentration columns
# compounds - character vector of compound column names (NULL = auto-detect)
# method - outlier detection method(s). Can specify multiple.
# sd_threshold - number of SDs from mean to flag (default 3)
# iqr_multiplier - IQR fence multiplier (default 1.5, use 3 for extreme)
# mad_threshold - MAD-based threshold (robust alternative to SD)
# percentile_upper/lower - percentile cutoffs
# log_transform - whether to log-transform before detection (recommended for PFAS)
# group_by_vars - optional grouping (e.g., "dataset_source", "year")
# flag_only - if TRUE, only adds flag columns; if FALSE, also removes outliers
#
# RETURNS:
# List with: $data_flagged, $data_clean, $outlier_summary, $thresholds_used

 method <- match.arg(method, several.ok = TRUE)

 # Auto-detect compound columns if not specified
 if (is.null(compounds)) {
 compounds <- names(data)[sapply(data, is.numeric)]
 compounds <- setdiff(compounds, c("Latitude", "Longitude", "year", "month"))
 }

 cat("Outlier Detection Configuration:\n")
 cat(" Methods:", paste(method, collapse = ", "), "\n")
 cat(" SD threshold:", sd_threshold, "\n")
 cat(" IQR multiplier:", iqr_multiplier, "\n")
 cat(" Log transform:", log_transform, "\n")
 cat(" Compounds:", length(compounds), "\n")
 cat(" Grouping:", ifelse(is.null(group_by_vars), "None", paste(group_by_vars, collapse = ", ")),
"\n\n")

 # Initialize flag columns
 data_flagged <- data

 # Track outlier counts
 outlier_summary <- tibble(
 compound = character(),
 method = character(),
 n_outliers = integer(),
 pct_outliers = numeric(),
 threshold_lower = numeric(),
 threshold_upper = numeric()
 )

 # Process each compound
 for (comp in compounds) {
 if (!comp %in% names(data)) next

 values <- data[[comp]]

 # Skip if too few valid observations
 valid_values <- values[!is.na(values) & values > 0]
 if (length(valid_values) < 10) next

 # Optional log transformation
 if (log_transform) {
 work_values <- log10(valid_values + 1)
 } else {
 work_values <- valid_values
 }

 # Initialize outlier flag for this compound
 is_outlier <- rep(FALSE, nrow(data))

 # --- METHOD: Standard Deviation ---
 if ("sd" %in% method) {
 mu <- mean(work_values, na.rm = TRUE)
 sigma <- sd(work_values, na.rm = TRUE)
 upper <- mu + sd_threshold * sigma
 lower <- mu - sd_threshold * sigma

 if (log_transform) {
 upper_orig <- 10^upper - 1
 lower_orig <- max(0, 10^lower - 1)
 } else {
 upper_orig <- upper
 lower_orig <- lower
 }

 is_outlier_sd <- !is.na(values) & (values > upper_orig | values < lower_orig)
 is_outlier <- is_outlier | is_outlier_sd

 outlier_summary <- bind_rows(outlier_summary, tibble(
 compound = comp, method = paste0("SD_", sd_threshold),
 n_outliers = sum(is_outlier_sd),
 pct_outliers = round(sum(is_outlier_sd) / sum(!is.na(values)) * 100, 2),
 threshold_lower = lower_orig,
 threshold_upper = upper_orig
 ))
 }

 # --- METHOD: IQR (Interquartile Range) ---
 if ("iqr" %in% method) {
 Q1 <- quantile(work_values, 0.25, na.rm = TRUE)
 Q3 <- quantile(work_values, 0.75, na.rm = TRUE)
 IQR_val <- Q3 - Q1
 upper <- Q3 + iqr_multiplier * IQR_val
 lower <- Q1 - iqr_multiplier * IQR_val

 if (log_transform) {
 upper_orig <- 10^upper - 1
 lower_orig <- max(0, 10^lower - 1)
 } else {
 upper_orig <- upper
 lower_orig <- lower
 }

 is_outlier_iqr <- !is.na(values) & (values > upper_orig | values < lower_orig)
 is_outlier <- is_outlier | is_outlier_iqr

 outlier_summary <- bind_rows(outlier_summary, tibble(
 compound = comp, method = paste0("IQR_", iqr_multiplier),
 n_outliers = sum(is_outlier_iqr),
 pct_outliers = round(sum(is_outlier_iqr) / sum(!is.na(values)) * 100, 2),
 threshold_lower = lower_orig,
 threshold_upper = upper_orig
 ))
 }

 # --- METHOD: MAD (Median Absolute Deviation) ---
 if ("mad" %in% method) {
 med <- median(work_values, na.rm = TRUE)
 mad_val <- mad(work_values, na.rm = TRUE)
 upper <- med + mad_threshold * mad_val
 lower <- med - mad_threshold * mad_val

 if (log_transform) {
 upper_orig <- 10^upper - 1
 lower_orig <- max(0, 10^lower - 1)
 } else {
 upper_orig <- upper
 lower_orig <- lower
 }

 is_outlier_mad <- !is.na(values) & (values > upper_orig | values < lower_orig)
 is_outlier <- is_outlier | is_outlier_mad

 outlier_summary <- bind_rows(outlier_summary, tibble(
 compound = comp, method = paste0("MAD_", mad_threshold),
 n_outliers = sum(is_outlier_mad),
 pct_outliers = round(sum(is_outlier_mad) / sum(!is.na(values)) * 100, 2),
 threshold_lower = lower_orig,
 threshold_upper = upper_orig
 ))
 }

 # --- METHOD: Percentile ---
 if ("percentile" %in% method) {
 upper_orig <- quantile(valid_values, percentile_upper, na.rm = TRUE)
 lower_orig <- quantile(valid_values, percentile_lower, na.rm = TRUE)

 is_outlier_pct <- !is.na(values) & (values > upper_orig | values < lower_orig)
 is_outlier <- is_outlier | is_outlier_pct

 outlier_summary <- bind_rows(outlier_summary, tibble(
 compound = comp, method = paste0("P", percentile_lower*100, "_P",
percentile_upper*100),
 n_outliers = sum(is_outlier_pct),
 pct_outliers = round(sum(is_outlier_pct) / sum(!is.na(values)) * 100, 2),
 threshold_lower = lower_orig,
 threshold_upper = upper_orig
 ))
 }

 # Add flag column
 data_flagged[[paste0(comp, "_outlier")]] <- is_outlier
 }

 # Create clean dataset (outliers removed or set to NA)
 data_clean <- data_flagged
 if (!flag_only) {
 for (comp in compounds) {
 flag_col <- paste0(comp, "_outlier")
 if (flag_col %in% names(data_clean)) {
 data_clean[[comp]][data_clean[[flag_col]]] <- NA
 }
 }
 }

 # Print summary
 cat("OUTLIER DETECTION SUMMARY:\n")
 cat("Total observations:", nrow(data), "\n")

 total_flags <- data_flagged %>%
 select(ends_with("_outlier")) %>%
 rowSums(na.rm = TRUE)

 cat("Observations with at least one outlier flag:", sum(total_flags > 0), "\n")
 cat("Percentage flagged:", round(sum(total_flags > 0) / nrow(data) * 100, 2), "%\n\n")

 cat("Per-compound outlier counts:\n")
 print(outlier_summary %>% arrange(desc(n_outliers)))

 return(list(
 data_flagged = data_flagged,
 data_clean = data_clean,
 outlier_summary = outlier_summary,
 config = list(
 method = method,
 sd_threshold = sd_threshold,
 iqr_multiplier = iqr_multiplier,
 mad_threshold = mad_threshold,
 log_transform = log_transform
 )
 ))
}
# =======================================================================
# CORE FUNCTION: aggregate_observations()
# Flexible site-level aggregation with multiple statistics
# =======================================================================

aggregate_observations <- function(data,
 group_vars = c("Latitude", "Longitude"),
 compounds = NULL,
 agg_method = c("median", "mean", "max", "cv"),
 min_obs = 1,
 include_metadata = TRUE) {

# ARGUMENTS:
# data - dataframe (ideally post-outlier-removal)
# group_vars - columns to group by for aggregation
# compounds - compound columns to aggregate
# agg_method - which statistics to compute
# min_obs - minimum observations required per group
# include_metadata - whether to include n_obs, temporal coverage etc.
#
# RETURNS:
# Aggregated dataframe at site level

 agg_method <- match.arg(agg_method, several.ok = TRUE)

 if (is.null(compounds)) {
 compounds <- names(data)[sapply(data, is.numeric)]
 compounds <- setdiff(compounds, c("Latitude", "Longitude", "year", "month"))
 # Remove outlier flag columns
 compounds <- compounds[!grepl("_outlier$", compounds)]
 }

 cat("Aggregation Configuration:\n")
 cat(" Group by:", paste(group_vars, collapse = ", "), "\n")
 cat(" Methods:", paste(agg_method, collapse = ", "), "\n")
 cat(" Min observations:", min_obs, "\n")
 cat(" Compounds:", length(compounds), "\n\n")

 # Build aggregation expressions
 agg_exprs <- list()

 for (method in agg_method) {
 for (comp in compounds) {
 col_name <- paste0(comp, "_", method)
 if (method == "median") {
 agg_exprs[[col_name]] <- rlang::expr(median(!!rlang::sym(comp), na.rm = TRUE))
 } else if (method == "mean") {
 agg_exprs[[col_name]] <- rlang::expr(mean(!!rlang::sym(comp), na.rm = TRUE))
 } else if (method == "max") {
 agg_exprs[[col_name]] <- rlang::expr(max(!!rlang::sym(comp), na.rm = TRUE))
 } else if (method == "cv") {
 agg_exprs[[col_name]] <- rlang::expr(
 sd(!!rlang::sym(comp), na.rm = TRUE) / mean(!!rlang::sym(comp), na.rm = TRUE)
 )
 }
 }
 }

 # Metadata expressions
 if (include_metadata) {
 agg_exprs[["n_obs"]] <- rlang::expr(n())
 agg_exprs[["n_years"]] <- rlang::expr(n_distinct(year, na.rm = TRUE))
 agg_exprs[["year_min"]] <- rlang::expr(min(year, na.rm = TRUE))
 agg_exprs[["year_max"]] <- rlang::expr(max(year, na.rm = TRUE))
 agg_exprs[["year_span"]] <- rlang::expr(max(year, na.rm = TRUE) - min(year, na.rm = TRUE))
 agg_exprs[["n_datasets"]] <- rlang::expr(n_distinct(dataset_source))
 }

 # Perform aggregation
 aggregated <- data %>%
 group_by(across(all_of(group_vars))) %>%
 summarise(!!!agg_exprs, .groups = "drop") %>%
 filter(n_obs >= min_obs)

 # Replace Inf/-Inf with NA
 aggregated <- aggregated %>%
 mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA, .)))

 cat("Aggregation Results:\n")
 cat(" Input observations:", nrow(data), "\n")
 cat(" Output sites:", nrow(aggregated), "\n")
 cat(" Sites filtered (< ", min_obs, " obs):",
 nrow(data %>% group_by(across(all_of(group_vars))) %>%
 summarise(n = n(), .groups = "drop") %>% filter(n < min_obs)), "\n\n")

 return(aggregated)
}
# =======================================================================

# USAGE EXAMPLE: Full Pipeline
# =======================================================================

cat("RUNNING FULL OUTLIER + AGGREGATION PIPELINE\n")
# Step 1: Detect outliers with multiple thresholds for comparison
results_3sd <- detect_outliers(conus_data, compounds = top_compounds,
 method = c("sd", "iqr"),
 sd_threshold = 3, log_transform = TRUE)
results_2sd <- detect_outliers(conus_data, compounds = top_compounds,
 method = "sd",
 sd_threshold = 2, log_transform = TRUE)
results_4sd <- detect_outliers(conus_data, compounds = top_compounds,
 method = "sd",
 sd_threshold = 4, log_transform = TRUE)
# Compare thresholds
cat("\n\n--- THRESHOLD COMPARISON ---\n")
cat("2 SD: ", sum(rowSums(results_2sd$data_flagged %>%
 select(ends_with("_outlier")), na.rm = TRUE) > 0), "observations flagged\n")
cat("3 SD: ", sum(rowSums(results_3sd$data_flagged %>%
 select(ends_with("_outlier")), na.rm = TRUE) > 0), "observations flagged\n")
cat("4 SD: ", sum(rowSums(results_4sd$data_flagged %>%
 select(ends_with("_outlier")), na.rm = TRUE) > 0), "observations flagged\n")
# Step 2: Aggregate to site level using 3SD-cleaned data
site_aggregated <- aggregate_observations(
 data = results_3sd$data_clean,
 group_vars = c("Latitude", "Longitude"),
 compounds = top_compounds,
 agg_method = c("median", "cv"),
 min_obs = 1,
 include_metadata = TRUE
)
# Step 3: Save
saveRDS(results_3sd, "outlier_detection_results.rds")
saveRDS(site_aggregated, "site_aggregated_data.rds")
write.csv(results_3sd$outlier_summary, "outlier_summary_table.csv", row.names = FALSE)
# =======================================================================

# VISUALIZATION: Outlier detection diagnostic plots

# Before/after distributions for top compounds
plot_outlier_diagnostics <- function(original, cleaned, compound) {

 orig_vals <- original[[compound]][!is.na(original[[compound]]) & original[[compound]] > 0]
 clean_vals <- cleaned[[compound]][!is.na(cleaned[[compound]]) & cleaned[[compound]] > 0]

 df_plot <- bind_rows(
 tibble(value = orig_vals, stage = "Before"),
 tibble(value = clean_vals, stage = "After")
 )

 ggplot(df_plot, aes(x = log10(value + 1), fill = stage)) +
 geom_histogram(alpha = 0.6, position = "identity", bins = 50, color = "white") +
 scale_fill_manual(values = c("Before" = "red", "After" = "steelblue")) +
 labs(
 title = paste0(compound, ": Distribution Before/After Outlier Removal"),
 subtitle = paste0("Removed: ", length(orig_vals) - length(clean_vals), " observations"),
 x = "log10(Concentration + 1)", y = "Count", fill = "Stage"
 ) +
 theme_minimal(base_size = 12)
}
# Generate diagnostic plots for top 4 compounds
for (comp in top_compounds[1:4]) {
 p <- plot_outlier_diagnostics(conus_data, results_3sd$data_clean, comp)
 ggsave(paste0("06_outlier_diagnostic_", comp, ".png"), p, width = 10, height = 6, dpi = 300)
}
cat("Saved: Outlier diagnostic plots\n")

#SCRIPT 4: FEATURE ABLATION FUNCTION (Replacing SHAP)
#(Advisor Item #5: Mechanistic Ablation)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(ranger)
# =======================================================================
# CORE FUNCTION: feature_ablation()
# Remove one predictor (or group) at a time, measure performance drop
# =======================================================================
feature_ablation <- function(data,
 target_col,
 predictor_cols,
 predictor_groups = NULL,
 model_type = "ranger",
 metric = "RMSE",
 n_folds = 5,
 n_repeats = 3,
 seed = 42,
 verbose = TRUE) {

# ARGUMENTS:
# data - dataframe with target and predictor columns
# target_col - name of target variable (e.g., "PFOS_median")
# predictor_cols - character vector of all predictor names
# predictor_groups - named list of predictor groups for group ablation
# e.g., list("Industrial" = c("TRI_count", "NPDES_dist"),
# "Land_Cover" = c("wetland_pct", "urban_pct"))
# model_type - "ranger" (random forest), "xgb" (xgboost), "lm" (linear)
# metric - "RMSE", "MAE", "R2"
# n_folds - cross-validation folds
# n_repeats - number of CV repeats
# seed - random seed for reproducibility
# verbose - print progress
#
# RETURNS:
# List with: $baseline, $single_ablation, $group_ablation, $importance_ranking

 set.seed(seed)

 if (verbose) {
 cat("============================================================\n")
 cat("FEATURE ABLATION EXPERIMENT\n")
 cat("============================================================\n")
 cat("Target:", target_col, "\n")
 cat("Predictors:", length(predictor_cols), "\n")
 cat("Model:", model_type, "\n")
 cat("Metric:", metric, "\n")
 cat("CV:", n_folds, "folds x", n_repeats, "repeats\n\n")
 }

 # Prepare data: remove NAs in target
 model_data <- data %>%
 select(all_of(c(target_col, predictor_cols))) %>%
 filter(!is.na(!!sym(target_col)))

 # Impute missing predictors with median (or use complete cases)
 model_data <- model_data %>%
 mutate(across(all_of(predictor_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

 if (verbose) cat("Modeling data:", nrow(model_data), "observations\n\n")

 # Define cross-validation control
 cv_control <- trainControl(
 method = "repeatedcv",
 number = n_folds,
 repeats = n_repeats,
 savePredictions = "final"
 )

 # --- HELPER: Train model and extract metric ---
 train_and_evaluate <- function(predictors, label = "baseline") {

 if (length(predictors) == 0) {
 return(list(metric_value = NA, model = NULL))
 }

 formula_str <- paste(target_col, "~", paste(predictors, collapse = " + "))

 tryCatch({
 if (model_type == "ranger") {
 model <- train(
 as.formula(formula_str),
 data = model_data,
 method = "ranger",
 trControl = cv_control,
 tuneLength = 3,
 importance = "impurity"
 )
 } else if (model_type == "lm") {
 model <- train(
 as.formula(formula_str),
 data = model_data,
 method = "lm",
 trControl = cv_control
 )
 }

 # Extract metric
 if (metric == "RMSE") {
 metric_value <- min(model$results$RMSE)
 } else if (metric == "MAE") {
 metric_value <- min(model$results$MAE)
 } else if (metric == "R2") {
 metric_value <- max(model$results$Rsquared)
 }

 return(list(metric_value = metric_value, model = model))

 }, error = function(e) {
 if (verbose) cat(" ERROR for", label, ":", e$message, "\n")
 return(list(metric_value = NA, model = NULL))
 })
 }

 # ======================================================================
 # STEP 1: BASELINE (all predictors)
 # ======================================================================

 if (verbose) cat("Training BASELINE model (all predictors)...\n")
 baseline <- train_and_evaluate(predictor_cols, "baseline")

 if (verbose) cat(" Baseline", metric, ":", round(baseline$metric_value, 4), "\n\n")

 # ======================================================================
 # STEP 2: SINGLE-FEATURE ABLATION
 # ======================================================================

 if (verbose) cat("SINGLE-FEATURE ABLATION:\n")

 single_results <- tibble(
 predictor_removed = character(),
 metric_without = numeric(),
 metric_baseline = numeric(),
 performance_drop = numeric(),
 pct_drop = numeric()
 )

 for (pred in predictor_cols) {
 remaining <- setdiff(predictor_cols, pred)

 if (verbose) cat(" Removing:", pred, "...")
 result <- train_and_evaluate(remaining, pred)

 drop <- result$metric_value - baseline$metric_value
 pct_drop <- (drop / baseline$metric_value) * 100

 if (verbose) cat(" ", metric, "=", round(result$metric_value, 4),
 " (drop:", round(pct_drop, 2), "%)\n")

 single_results <- bind_rows(single_results, tibble(
 predictor_removed = pred,
 metric_without = result$metric_value,
 metric_baseline = baseline$metric_value,
 performance_drop = drop,
 pct_drop = pct_drop
 ))
 }

 # Rank by importance (larger drop = more important)
 single_results <- single_results %>%
 arrange(desc(performance_drop)) %>%
 mutate(importance_rank = row_number())

 # ======================================================================
 # STEP 3: GROUP ABLATION (if groups specified)
 # ======================================================================

 group_results <- NULL

 if (!is.null(predictor_groups)) {
 if (verbose) cat("\nGROUP ABLATION:\n")

 group_results <- tibble(
 group_removed = character(),
 predictors_in_group = character(),
 n_predictors = integer(),
 metric_without = numeric(),
 metric_baseline = numeric(),
 performance_drop = numeric(),
 pct_drop = numeric()
 )

 for (group_name in names(predictor_groups)) {
 group_preds <- predictor_groups[[group_name]]
 remaining <- setdiff(predictor_cols, group_preds)

 if (verbose) cat(" Removing group:", group_name,
 "(", length(group_preds), "predictors)...")

 result <- train_and_evaluate(remaining, group_name)

 drop <- result$metric_value - baseline$metric_value
 pct_drop <- (drop / baseline$metric_value) * 100

 if (verbose) cat(" ", metric, "=", round(result$metric_value, 4),
 " (drop:", round(pct_drop, 2), "%)\n")

 group_results <- bind_rows(group_results, tibble(
 group_removed = group_name,
 predictors_in_group = paste(group_preds, collapse = ", "),
 n_predictors = length(group_preds),
 metric_without = result$metric_value,
 metric_baseline = baseline$metric_value,
 performance_drop = drop,
 pct_drop = pct_drop
 ))
 }

 group_results <- group_results %>%
 arrange(desc(performance_drop)) %>%
 mutate(importance_rank = row_number())
 }

 # ======================================================================

 # RESULTS
 # ======================================================================

 if (verbose) {
 cat("\n============================================================\n")
 cat("ABLATION RESULTS SUMMARY\n")
 cat("============================================================\n\n")
 cat("Top 10 Most Important Features (Single Ablation):\n")
 print(single_results %>% head(10) %>% select(importance_rank, predictor_removed,
pct_drop))

 if (!is.null(group_results)) {
 cat("\nGroup Ablation Results:\n")
 print(group_results %>% select(importance_rank, group_removed, n_predictors, pct_drop))
 }
 }

 return(list(
 baseline_metric = baseline$metric_value,
 baseline_model = baseline$model,
 single_ablation = single_results,
 group_ablation = group_results,
 config = list(
 target = target_col,
 predictors = predictor_cols,
 model_type = model_type,
 metric = metric,
 n_folds = n_folds,
 n_repeats = n_repeats
 )
 ))
}
# =======================================================================

# VISUALIZATION FUNCTION: plot_ablation_results()
# =======================================================================

plot_ablation_results <- function(ablation_results, top_n = 15, title_suffix = "") {

 single <- ablation_results$single_ablation %>%
 head(top_n)

 p_single <- ggplot(single, aes(x = reorder(predictor_removed, pct_drop), y = pct_drop)) +
 geom_col(aes(fill = pct_drop), alpha = 0.8) +
 geom_hline(yintercept = 0, color = "grey50") +
 scale_fill_gradient2(low = "steelblue", mid = "yellow", high = "red",
 midpoint = median(single$pct_drop)) +
 coord_flip() +
 labs(
 title = paste0("Feature Ablation: Single Predictor Importance ", title_suffix),
 subtitle = paste0("Baseline ", ablation_results$config$metric, " = ",
 round(ablation_results$baseline_metric, 4),
 " | Higher % drop = more important"),
 x = "Predictor Removed",
 y = paste0("% Increase in ", ablation_results$config$metric, " (Performance Drop)"),
 fill = "% Drop"
 ) +
 theme_minimal(base_size = 12) +
 theme(plot.title = element_text(face = "bold", size = 13),
 legend.position = "none")

 # Group ablation plot if available
 if (!is.null(ablation_results$group_ablation)) {
 group <- ablation_results$group_ablation

 p_group <- ggplot(group, aes(x = reorder(group_removed, pct_drop), y = pct_drop)) +
 geom_col(aes(fill = pct_drop), alpha = 0.8, width = 0.6) +
 geom_text(aes(label = paste0("n=", n_predictors)), hjust = -0.2, size = 3.5) +
 scale_fill_gradient2(low = "steelblue", mid = "yellow", high = "red",
 midpoint = median(group$pct_drop)) +
 coord_flip() +
 labs(
 title = "Group Ablation: Predictor Category Importance",
 subtitle = "Which thematic group of predictors matters most?",
 x = "Predictor Group Removed",
 y = paste0("% Increase in ", ablation_results$config$metric),
 fill = "% Drop"
 ) +
 theme_minimal(base_size = 12) +
 theme(plot.title = element_text(face = "bold", size = 13),
 legend.position = "none")

 return(list(single = p_single, group = p_group))
 }

 return(list(single = p_single))
}
# =======================================================================

# USAGE EXAMPLE WITH MECHANISTIC GROUPS
# =======================================================================
# Define mechanistic predictor groups (customize to your actual predictors)
predictor_groups_mechanistic <- list(
 "Industrial_Sources" = c("TRI_count_5km", "TRI_dist_nearest", "NPDES_count_5km",
 "NPDES_dist_nearest", "airports_5km"),
 "Land_Cover" = c("wetland_pct", "urban_pct", "agriculture_pct",
 "forest_pct", "impervious_pct"),
 "Hydrology" = c("watershed_area", "stream_order", "flow_accumulation",
 "drainage_density", "TWI"),
 "Soil_Properties" = c("organic_carbon", "clay_pct", "sand_pct",
 "soil_pH", "CEC"),
 "Climate" = c("annual_precip", "mean_temp", "PET", "aridity_index"),
 "Population" = c("population_density", "housing_density", "septic_density"),
 "Temporal" = c("year", "month", "temporal_cv", "n_prior_observations")
)
# Example call (uncomment when you have predictor data ready):
# ablation_pfos <- feature_ablation(
# data = modeling_data,
# target_col = "PFOS_median",
# predictor_cols = all_predictor_names,
# predictor_groups = predictor_groups_mechanistic,
# model_type = "ranger",
# metric = "RMSE",
# n_folds = 5,
# n_repeats = 3
# )
#
# plots <- plot_ablation_results(ablation_pfos, top_n = 20, title_suffix = "(PFOS)")
# ggsave("07_ablation_single_PFOS.png", plots$single, width = 12, height = 8, dpi = 300)
# ggsave("07_ablation_group_PFOS.png", plots$group, width = 10, height = 6, dpi = 300)
# =======================================================================

# MULTI-COMPOUND ABLATION WRAPPER
# =======================================================================

run_multi_compound_ablation <- function(data, target_compounds, predictor_cols,
 predictor_groups = NULL, ...) {
# Run ablation for multiple PFAS compounds and compare which predictors matter
# for which compounds. This reveals compound-specific drivers.

 all_results <- list()
 importance_comparison <- tibble()

 for (compound in target_compounds) {
 target_col <- paste0(compound, "_median") # adjust naming convention

 if (!target_col %in% names(data)) {
 cat("Skipping", compound, "- column not found\n")
 next
 }

 cat("\n--- Running ablation for:", compound, "---\n")

 result <- feature_ablation(
 data = data,
 target_col = target_col,
 predictor_cols = predictor_cols,
 predictor_groups = predictor_groups,
 verbose = FALSE,
 ...
 )

 all_results[[compound]] <- result

 # Add to comparison table
 importance_comparison <- bind_rows(
 importance_comparison,
 result$single_ablation %>%
 mutate(compound = compound) %>%
 select(compound, predictor_removed, pct_drop, importance_rank)
 )
 }

 # Create heatmap of compound-specific importance
 if (nrow(importance_comparison) > 0) {
 # Top predictors across all compounds
 top_preds <- importance_comparison %>%
 group_by(predictor_removed) %>%
 summarise(mean_drop = mean(pct_drop, na.rm = TRUE), .groups = "drop") %>%
 arrange(desc(mean_drop)) %>%
 head(20) %>%
 pull(predictor_removed)

 p_heatmap <- ggplot(
 importance_comparison %>% filter(predictor_removed %in% top_preds),
 aes(x = compound, y = reorder(predictor_removed, pct_drop), fill = pct_drop)
 ) +
 geom_tile(color = "white") +
 scale_fill_viridis_c(option = "plasma", name = "% Performance\nDrop") +
 labs(
 title = "Compound-Specific Feature Importance (Ablation)",
 subtitle = "Which predictors matter differently for different PFAS?",
 x = "Target Compound", y = "Predictor Removed"
 ) +
 theme_minimal(base_size = 11) +
 theme(
 plot.title = element_text(face = "bold"),
 axis.text.x = element_text(angle = 45, hjust = 1)
 )

 ggsave("08_ablation_compound_comparison.png", p_heatmap,
 width = 12, height = 10, dpi = 300)
 }

 return(list(
 compound_results = all_results,
 comparison = importance_comparison
 ))
}
=========================================================================
==============
SCRIPT 5: DISTANCE DECAY FRAMEWORK
(Advisor Item #6: Mechanistic Distance Decay)
=========================================================================
==============
library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)
# =======================================================================
=====
# CORE FRAMEWORK: Distance Decay Analysis for PFAS
# =======================================================================
=====
# --- 5A. COMPUTE DISTANCES TO POTENTIAL SOURCES ---
compute_source_distances <- function(pfas_sites_sf, source_data_list) {

# ARGUMENTS:
# pfas_sites_sf - sf object of PFAS sampling sites
# source_data_list - named list of sf objects for each source type
# e.g., list(TRI = tri_sf, WWTP = wwtp_sf, airports = airports_sf)
#
# RETURNS:
# dataframe with distance to nearest source of each type

 cat("Computing distances to potential PFAS sources...\n")

 distance_results <- pfas_sites_sf %>%
 st_drop_geometry() %>%
 select(Latitude, Longitude)

 for (source_name in names(source_data_list)) {
 cat(" Processing:", source_name, "...")

 source_sf <- source_data_list[[source_name]]

 # Ensure same CRS
 source_sf <- st_transform(source_sf, st_crs(pfas_sites_sf))

 # Compute nearest distance (in meters)
 nearest_idx <- st_nearest_feature(pfas_sites_sf, source_sf)
 distances <- st_distance(pfas_sites_sf, source_sf[nearest_idx, ], by_element = TRUE)

 # Convert to km
 distance_results[[paste0("dist_", source_name, "_km")]] <- as.numeric(distances) / 1000

 # Also count sources within buffers
 for (buffer_km in c(1, 5, 10, 25)) {
 buffer_m <- buffer_km * 1000
 buffered <- st_buffer(pfas_sites_sf, buffer_m)
 counts <- lengths(st_intersects(buffered, source_sf))
 distance_results[[paste0("count_", source_name, "_", buffer_km, "km")]] <- counts
 }

 cat(" done\n")
 }

 return(distance_results)
}
# --- 5B. DISTANCE DECAY MODEL FITTING ---
fit_distance_decay <- function(data,
 concentration_col,
 distance_col,
 decay_models = c("exponential", "power", "gaussian", "linear"),
 min_conc = 0,
 max_dist_km = 50,
 log_conc = TRUE) {

# Fits multiple distance-decay functional forms and compares fit
#
# ARGUMENTS:
# data - dataframe with concentration and distance columns
# concentration_col - name of concentration column
# distance_col - name of distance column (in km)
# decay_models - which functional forms to fit
# min_conc - minimum concentration to include
# max_dist_km - maximum distance to consider
# log_conc - whether to log-transform concentrations
#
# RETURNS:
# List with model fits, parameters, and comparison statistics

 # Prepare data
 df <- data %>%
 select(conc = !!sym(concentration_col), dist = !!sym(distance_col)) %>%
 filter(!is.na(conc), !is.na(dist), conc > min_conc, dist <= max_dist_km, dist >= 0)

 if (log_conc) {
 df$conc_work <- log10(df$conc + 1)
 } else {
 df$conc_work <- df$conc
 }

 cat("Distance decay fitting:\n")
 cat(" Compound:", concentration_col, "\n")
 cat(" Source:", distance_col, "\n")
 cat(" Observations:", nrow(df), "\n")
 cat(" Distance range:", round(min(df$dist), 1), "-", round(max(df$dist), 1), "km\n\n")

 results <- list()
 comparison <- tibble()

 # --- Exponential Decay: C = a * exp(-b * d) ---
 if ("exponential" %in% decay_models) {
 tryCatch({
 fit <- nls(conc_work ~ a * exp(-b * dist), data = df,
 start = list(a = max(df$conc_work), b = 0.1),
 control = nls.control(maxiter = 200))

 results$exponential <- fit
 r2 <- 1 - sum(residuals(fit)^2) / sum((df$conc_work - mean(df$conc_work))^2)
 aic <- AIC(fit)

 params <- coef(fit)
 half_life_km <- log(2) / params["b"] # Distance where concentration halves

 comparison <- bind_rows(comparison, tibble(
 model = "Exponential", formula = "C = a * exp(-b * d)",
 R2 = round(r2, 4), AIC = round(aic, 1),
 half_life_km = round(half_life_km, 2),
 param_a = round(params["a"], 4), param_b = round(params["b"], 4)
 ))

 cat(" Exponential: R2 =", round(r2, 4), "| Half-life =", round(half_life_km, 2), "km\n")
 }, error = function(e) cat(" Exponential: FAILED -", e$message, "\n"))
 }

 # --- Power Decay: C = a * d^(-b) ---
 if ("power" %in% decay_models) {
 tryCatch({
 df_power <- df %>% filter(dist > 0.01) # Avoid division by zero
 fit <- nls(conc_work ~ a * dist^(-b), data = df_power,
 start = list(a = max(df_power$conc_work), b = 0.5),
 control = nls.control(maxiter = 200))

 results$power <- fit
 r2 <- 1 - sum(residuals(fit)^2) / sum((df_power$conc_work - mean(df_power$conc_work))^2)
 aic <- AIC(fit)
 params <- coef(fit)

 comparison <- bind_rows(comparison, tibble(
 model = "Power", formula = "C = a * d^(-b)",
 R2 = round(r2, 4), AIC = round(aic, 1),
 half_life_km = NA_real_,
 param_a = round(params["a"], 4), param_b = round(params["b"], 4)
 ))

 cat(" Power: R2 =", round(r2, 4), "\n")
 }, error = function(e) cat(" Power: FAILED -", e$message, "\n"))
 }

 # --- Gaussian Decay: C = a * exp(-(d^2) / (2*sigma^2)) ---
 if ("gaussian" %in% decay_models) {
 tryCatch({
 fit <- nls(conc_work ~ a * exp(-(dist^2) / (2 * sigma^2)), data = df,
 start = list(a = max(df$conc_work), sigma = 10),
 control = nls.control(maxiter = 200))

 results$gaussian <- fit
 r2 <- 1 - sum(residuals(fit)^2) / sum((df$conc_work - mean(df$conc_work))^2)
 aic <- AIC(fit)
 params <- coef(fit)

 comparison <- bind_rows(comparison, tibble(
 model = "Gaussian", formula = "C = a * exp(-d^2 / 2*sigma^2)",
 R2 = round(r2, 4), AIC = round(aic, 1),
 half_life_km = round(params["sigma"] * sqrt(2 * log(2)), 2),
 param_a = round(params["a"], 4), param_b = round(params["sigma"], 4)
 ))

 cat(" Gaussian: R2 =", round(r2, 4), "| sigma =", round(params["sigma"], 2), "km\n")
 }, error = function(e) cat(" Gaussian: FAILED -", e$message, "\n"))
 }

 # --- Linear (for comparison): C = a - b*d ---
 if ("linear" %in% decay_models) {
 fit <- lm(conc_work ~ dist, data = df)
 r2 <- summary(fit)$r.squared
 aic <- AIC(fit)
 params <- coef(fit)

 results$linear <- fit
 comparison <- bind_rows(comparison, tibble(
 model = "Linear", formula = "C = a - b*d",
 R2 = round(r2, 4), AIC = round(aic, 1),
 half_life_km = NA_real_,
 param_a = round(params[1], 4), param_b = round(params[2], 4)
 ))

 cat(" Linear: R2 =", round(r2, 4), "\n")
 }

 cat("\nModel Comparison:\n")
 print(comparison %>% arrange(AIC))

 return(list(
 models = results,
 comparison = comparison,
 data = df,
 best_model = comparison %>% arrange(AIC) %>% slice(1) %>% pull(model)
 ))
}
# --- 5C. COMPOUND-SPECIFIC DISTANCE DECAY COMPARISON ---
compare_compound_decay <- function(data, compounds, distance_col, max_dist_km = 50) {

# Compare distance-decay behavior across different PFAS compounds
# Key hypothesis: Short-chain PFAS (PFBS, PFHxA) decay differently than
# long-chain (PFOS, PFOA) due to different environmental persistence

 cat("============================================================\n")
 cat("COMPOUND-SPECIFIC DISTANCE DECAY COMPARISON\n")
 cat("============================================================\n\n")

 all_comparisons <- tibble()
 all_fits <- list()

 for (compound in compounds) {
 if (!compound %in% names(data)) next

 # Check if enough data
 valid_n <- sum(!is.na(data[[compound]]) & data[[compound]] > 0 &
 !is.na(data[[distance_col]]))
 if (valid_n < 30) {
 cat("Skipping", compound, "- only", valid_n, "valid observations\n")
 next
 }

 cat("\n--- ", compound, " ---\n")

 result <- fit_distance_decay(
 data = data,
 concentration_col = compound,
 distance_col = distance_col,
 max_dist_km = max_dist_km
 )

 all_fits[[compound]] <- result

 # Add compound identifier to comparison
 if (nrow(result$comparison) > 0) {
 best <- result$comparison %>% arrange(AIC) %>% slice(1)
 all_comparisons <- bind_rows(all_comparisons, best %>% mutate(compound = compound))
 }
 }

 # Summary comparison
 cat("\n\n============================================================\n")
 cat("CROSS-COMPOUND COMPARISON (Best-fit models):\n")
 cat("============================================================\n")
 print(all_comparisons %>% arrange(desc(R2)))

 return(list(
 compound_fits = all_fits,
 summary = all_comparisons
 ))
}
# --- 5D. ENVIRONMENTAL MODIFIER ANALYSIS ---
distance_decay_with_modifiers <- function(data, concentration_col, distance_col,
 modifier_cols,
 n_quantile_bins = 3) {

# Stratify distance-decay by environmental modifiers
# Key question: Does wetland %, soil type, or land cover MODIFY the decay rate?
# This distinguishes atmospheric deposition vs hydrologic transport

 cat("DISTANCE DECAY WITH ENVIRONMENTAL MODIFIERS\n")
 cat("Modifier variables:", paste(modifier_cols, collapse = ", "), "\n\n")

 stratified_results <- list()

 for (modifier in modifier_cols) {
 if (!modifier %in% names(data)) next

 # Bin modifier into quantiles
 data_mod <- data %>%
 filter(!is.na(!!sym(modifier))) %>%
 mutate(
 modifier_bin = ntile(!!sym(modifier), n_quantile_bins),
 modifier_label = paste0(modifier, "_Q", modifier_bin)
 )

 cat("--- Modifier:", modifier, "---\n")

 bin_results <- tibble()

 for (bin in 1:n_quantile_bins) {
 bin_data <- data_mod %>% filter(modifier_bin == bin)
 bin_range <- range(bin_data[[modifier]], na.rm = TRUE)

 cat(" Bin", bin, "(", round(bin_range[1], 2), "-", round(bin_range[2], 2), "):")

 tryCatch({
 fit_result <- fit_distance_decay(
 data = bin_data,
 concentration_col = concentration_col,
 distance_col = distance_col,
 decay_models = "exponential"
 )

 best <- fit_result$comparison %>% slice(1)
 bin_results <- bind_rows(bin_results, best %>%
 mutate(modifier = modifier, bin = bin,
 bin_range = paste0(round(bin_range[1], 2), "-", round(bin_range[2], 2))))

 }, error = function(e) {
 cat(" FAILED\n")
 })
 }

 stratified_results[[modifier]] <- bin_results

 if (nrow(bin_results) > 1) {
 cat("\n Decay rate comparison across", modifier, "bins:\n")
 print(bin_results %>% select(bin, bin_range, R2, half_life_km, param_b))
 cat("\n")
 }
 }

 return(stratified_results)
}
# --- 5E. VISUALIZATION: Distance Decay Plots ---
plot_distance_decay <- function(fit_result, compound_name, source_name) {

 df <- fit_result$data

 # Generate prediction curves
 dist_seq <- seq(min(df$dist), max(df$dist), length.out = 200)
 predictions <- tibble(dist = dist_seq)

 for (model_name in names(fit_result$models)) {
 model <- fit_result$models[[model_name]]
 tryCatch({
 predictions[[model_name]] <- predict(model, newdata = data.frame(dist = dist_seq))
 }, error = function(e) {})
 }

 # Pivot for plotting
 pred_long <- predictions %>%
 pivot_longer(-dist, names_to = "model", values_to = "predicted")

 p <- ggplot() +
 geom_point(data = df, aes(x = dist, y = conc_work),
 alpha = 0.3, size = 1, color = "grey40") +
 geom_line(data = pred_long, aes(x = dist, y = predicted, color = model),
 linewidth = 1.2) +
 scale_color_brewer(palette = "Set1", name = "Model") +
 labs(
 title = paste0("Distance Decay: ", compound_name, " from ", source_name),
 subtitle = paste0("Best fit: ", fit_result$best_model,
 " | n = ", nrow(df), " observations"),
 x = "Distance to Nearest Source (km)",
 y = "log10(Concentration + 1)"
 ) +
 theme_minimal(base_size = 12) +
 theme(plot.title = element_text(face = "bold", size = 13))

 return(p)
}
# =======================================================================
=====
# USAGE EXAMPLE: Distance Decay Pipeline
# =======================================================================
=====
# STEP 1: After you have source data loaded (TRI, WWTP, airports, etc.)
# Compute distances:
# source_list <- list(
# TRI = tri_facilities_sf,
# WWTP = wwtp_sf,
# airports = airports_sf,
# landfills = landfills_sf,
# military = military_sf
# )
#
# distances <- compute_source_distances(pfas_sites_sf, source_list)
# STEP 2: Fit decay for each compound x source combination
#
# decay_PFOS_TRI <- fit_distance_decay(
# data = site_data,
# concentration_col = "PFOS_median",
# distance_col = "dist_TRI_km",
# max_dist_km = 50
# )
# STEP 3: Compare compounds
#
# compound_comparison <- compare_compound_decay(
# data = site_data,
# compounds = c("PFOS", "PFOA", "PFBS", "PFHxS", "PFNA", "PFHxA"),
# distance_col = "dist_TRI_km",
# max_dist_km = 50
# )
# STEP 4: Environmental modifiers
#
# modifier_analysis <- distance_decay_with_modifiers(
# data = site_data,
# concentration_col = "PFOS_median",
# distance_col = "dist_TRI_km",
# modifier_cols = c("wetland_pct", "urban_pct", "soil_organic_carbon",
# "annual_precip", "stream_order")
# )
cat("\nDistance Decay Framework ready for use.\n")
cat("Key outputs will include:\n")
cat(" - Compound-specific decay rates\n")
cat(" - Half-life distances by source type\n")
cat(" - Environmental modifier effects on decay\n")
cat(" - Atmospheric vs hydrologic transport signatures\n")
=========================================================================
==============
SCRIPT 6: TEMPORAL VARIABILITY AS FEATURE (Advisor Item #1)
(Source Persistence Classification)
=========================================================================
==============
library(dplyr)
library(ggplot2)
library(tidyr)
# =======================================================================
=====
# Classify sites by temporal behavior: Persistent Source vs Episodic
# =======================================================================
=====
classify_temporal_behavior <- function(data,
 compounds,
 min_observations = 3,
 cv_threshold_persistent = 0.5,
 cv_threshold_episodic = 1.5) {

# Sites with LOW temporal CV = persistent source (constant contamination)
# Sites with HIGH temporal CV = episodic contamination (spills, events)
# Sites with MODERATE CV = mixed/seasonal
#
# ARGUMENTS:
# data - full observation-level data with repeats
# compounds - PFAS compound columns
# min_observations - minimum repeat obs needed for classification
# cv_threshold_persistent - CV below this = persistent
# cv_threshold_episodic - CV above this = episodic

 cat("TEMPORAL BEHAVIOR CLASSIFICATION\n")
 cat("CV < ", cv_threshold_persistent, " = Persistent source\n")
 cat("CV > ", cv_threshold_episodic, " = Episodic contamination\n")
 cat("Between = Mixed/Seasonal\n\n")

 # Calculate temporal statistics per site
 site_temporal <- data %>%
 group_by(Latitude, Longitude) %>%
 filter(n() >= min_observations) %>%
 summarise(
 n_obs = n(),
 n_years = n_distinct(year, na.rm = TRUE),
 year_span = max(year, na.rm = TRUE) - min(year, na.rm = TRUE),

 # For each compound: mean, SD, CV, trend
 across(all_of(compounds), list(
 mean = ~mean(., na.rm = TRUE),
 sd = ~sd(., na.rm = TRUE),
 cv = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE),
 min = ~min(., na.rm = TRUE),
 max = ~max(., na.rm = TRUE),
 range_ratio = ~max(., na.rm = TRUE) / max(min(.[. > 0], na.rm = TRUE), 0.001)
 ), .names = "{.col}_{.fn}"),

 .groups = "drop"
 )

 # Compute overall temporal signature (mean CV across detectable compounds)
 cv_cols <- names(site_temporal)[grepl("_cv$", names(site_temporal))]

 site_temporal <- site_temporal %>%
 rowwise() %>%
 mutate(
 mean_cv = mean(c_across(all_of(cv_cols)), na.rm = TRUE),
 median_cv = median(c_across(all_of(cv_cols)), na.rm = TRUE)
 ) %>%
 ungroup() %>%
 mutate(
 temporal_class = case_when(
 mean_cv < cv_threshold_persistent ~ "Persistent",
 mean_cv > cv_threshold_episodic ~ "Episodic",
 TRUE ~ "Mixed/Seasonal"
 )
 )

 # Summary
 cat("Classification Results:\n")
 class_summary <- site_temporal %>%
 group_by(temporal_class) %>%
 summarise(
 n_sites = n(),
 mean_n_obs = round(mean(n_obs), 1),
 mean_year_span = round(mean(year_span, na.rm = TRUE), 1),
 mean_cv_overall = round(mean(mean_cv, na.rm = TRUE), 3),
 .groups = "drop"
 )
 print(class_summary)

 return(site_temporal)
}
# Usage:
# temporal_features <- classify_temporal_behavior(
# data = conus_data,
# compounds = c("PFOS", "PFOA", "PFBS", "PFHxS", "PFNA"),
# min_observations = 3,
# cv_threshold_persistent = 0.5,
# cv_threshold_episodic = 1.5
# )
# These temporal features become PREDICTORS in your model:
# - mean_cv (continuous feature)
# - temporal_class (categorical feature)
# - year_span (how long the site has been monitored)
# - range_ratio (max/min concentration ratio)
