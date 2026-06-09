

library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggspatial)
library(wesanderson)
library(scales)

#this is my dataset with PFOA predicitons from ML
load("~/CONUS_PFAS/pfoa_anim.RData")
#this has petre_data and shp file for watershed
load("~/CONUS_PFAS/sites_watershedboundary.RData")

pal <- colorRampPalette(
  c("#012A4A",wes_palette("Zissou1", type = "continuous"),"#8B0000"))(256)


library(lubridate)

# ── 1. CLEAN GEOMETRY LOOKUP (FAST + SAFE) ──
geom_lookup <- pfoa_anim %>%
  group_by(comid) %>%
  summarise(geometry = first(geometry), .groups = "drop")

# ── 2. AGGREGATE TIME SERIES (NO SF OVERHEAD) 
pfoa_anim_monthly <- pfoa_anim %>%
  st_drop_geometry() %>%
  mutate(anim_date = floor_date(date, "month")) %>%
  group_by(comid, anim_date) %>%
  summarise(
    conc_pred_ngL = mean(conc_pred_ngL, na.rm = TRUE),
    Flow_cms      = mean(Flow_cms, na.rm = TRUE),
    .groups = "drop"
  )

# ── 3. REATTACH GEOMETRY (ONE JOIN ONLY)
pfoa_anim_monthly <- pfoa_anim_monthly %>%
  left_join(geom_lookup, by = "comid") %>%
  st_as_sf()

flow_q <- quantile(log10(pfoa_anim_monthly$Flow_cms + 1),
                   probs = c(0.05, 0.95), na.rm = TRUE)

pfoa_anim_monthly <- pfoa_anim_monthly %>%
  mutate(
    flow_scaled = rescale(
      log10(Flow_cms + 1),
      to = c(0.2, 1.8),
      from = flow_q
    )
  )


date_column <- "anim_date"
class(pfoa_anim_monthly$anim_date)

petre_sites_sf <- petre_data %>%
  distinct(Longitude, Latitude) %>%
  mutate(site_idx = row_number()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

petre_sites_sf <- petre_sites_sf[petre_sites_sf$site_idx != 1, ]

#check range of PFOA concentrations
summary(pfoa_anim_monthly$conc_pred_ngL)
quantile(
  pfoa_anim_monthly$conc_pred_ngL,
  c(.01,.05,.25,.5,.75,.95,.99),
  na.rm = TRUE
)

lims <- quantile(
  pfoa_anim_monthly$conc_pred_ngL,
  c(0.02, 0.98),
  na.rm = TRUE)

pfoa_anim_obj <- ggplot() +
  geom_sf(data = cape_fear_wbd_4326,
    fill = NA, color = "black",
    linewidth = 0.8) +
  geom_sf(
    data = pfoa_anim_monthly,
    aes(
      color = conc_pred_ngL,
      linewidth = flow_scaled)) +
  scale_linewidth(
    range = c(0.8, 3),
    guide = "none")+
  geom_sf(
    data = dplyr::filter(petre_sites_sf, site_idx != 3),
    shape = 21,
    fill = "white",
    color = "black",
    size = 3) +
  geom_sf(
    data = dplyr::filter(petre_sites_sf, site_idx == 3),
    shape = 24,
    fill = "red",
    color = "black",
    size = 3) +
scale_color_gradientn(
  colours = pal,
  trans   = "log10",
  limits  = lims,
  oob     = scales::squish,
  name    = "PFOA (ng/L)")+
  annotation_north_arrow(
    location = "tr",
    which_north = "true") +
  annotation_scale(
    location = "bl") +
  labs(
    title =
      "Predicted Surface Water PFOA Concentration\nCape Fear Basin",
    subtitle = "Month: {format(frame_time, '%b %Y')}",
    caption =
      "Monthly mean PFOA concentrations predicted using a Random Forest model trained with leave-one-site-out cross-validation. Site 2 excluded from model development. R2 0.612.") +
  coord_sf() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(
      face = "bold",
      size = 16),
    plot.subtitle = element_text(size = 13)) +
  transition_time(anim_date)+
  ease_aes("linear")

print(pfoa_anim_obj)

library(av)
animate(
  pfoa_anim_obj,
  renderer = av_renderer("PFOA_CapeFear_2020.mp4"),
  width = 1400,
  height = 1200,
  fps = 8,
  nframes = 80
)



# How many unique COMIDs at each stage?
cat("In pfoa_anim:         ", n_distinct(pfoa_anim$merit_COMID), "\n")
cat("In pfoa_anim_monthly: ", n_distinct(pfoa_anim_monthly$merit_COMID), "\n")
cat("In geom_lookup:       ", n_distinct(geom_lookup$merit_COMID), "\n")

# Are any COMIDs lost in the monthly aggregation?
missing_after_agg <- setdiff(
  unique(pfoa_anim$merit_COMID),
  unique(pfoa_anim_monthly$merit_COMID)
)
cat("COMIDs lost in aggregation:", length(missing_after_agg), "\n")

# Are any COMIDs lost in the geometry join?
missing_after_join <- setdiff(
  unique(pfoa_anim_monthly$merit_COMID),
  unique(geom_lookup$merit_COMID)
)
cat("COMIDs lost in geometry join:", length(missing_after_join), "\n")

# How many COMIDs are in cape_fear_pred_spatial_time vs pfoa_anim?
cat("In cape_fear_pred_spatial_time:", 
    n_distinct(cape_fear_pred_spatial_time$merit_COMID), "\n")
cat("In pfoa_anim (PFOA filter):    ", 
    n_distinct(pfoa_anim$merit_COMID), "\n")



library(nhdplusTools)

# Get all flowlines in Cape Fear basin
cape_fear_network <- get_nhdplus(
  AOI  = cape_fear_wbd_4326,
  realization = "flowline"
)

cat("Total NHD flowlines in basin:", nrow(cape_fear_network), "\n")
cat("Your MERIT COMIDs:           ", 106, "\n")








library(FedData)
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)

# ── 1. DOWNLOAD NLCD ──────────────────────────────────────────────────────
# cape_fear_wbd_4326 is your watershed boundary
nlcd <- get_nlcd(
  template  = as(cape_fear_wbd_4326, "SpatVector"),
  label     = "cape_fear",
  year      = 2019,
  dataset   = "landcover"
)

# Crop and mask to watershed boundary
nlcd_crop <- crop(nlcd, vect(st_transform(cape_fear_wbd_4326, crs(nlcd))))
nlcd_mask <- mask(nlcd_crop, vect(st_transform(cape_fear_wbd_4326, crs(nlcd))))

# ── 2. NLCD COLOR PALETTE ─────────────────────────────────────────────────
# Official NLCD colors and labels
# Step 1: create nlcd_df from the raster (do this first)
nlcd_df <- as.data.frame(nlcd_factor, xy = TRUE) %>%
  rename(landcover = Class) %>%
  filter(!is.na(landcover))

# Step 2: aggregate classes
nlcd_df <- nlcd_df %>%
  mutate(landcover_agg = case_when(
    landcover %in% c("21", "22", "23", "24") ~ "Developed",
    landcover %in% c("41", "42", "43")       ~ "Forest",
    landcover %in% c("81", "82")             ~ "Agriculture",
    landcover %in% c("90", "95")             ~ "Wetlands",
    landcover == "11"                         ~ "Open Water",
    landcover %in% c("52", "71")             ~ "Shrub/Grassland",
    landcover == "31"                         ~ "Barren",
    TRUE                                      ~ NA_character_
  ))

# Quick check
table(nlcd_df$landcover_agg, useNA = "always")
agg_colors <- c(
  "Developed"       = "#C1272D",
  "Forest"          = "#3A7D44",
  "Agriculture"     = "#F4D03F",
  "Wetlands"        = "#5B8DB8",
  "Open Water"      = "#2980B9",
  "Shrub/Grassland" = "#D4AC6E",
  "Barren"          = "#B3AC9F"
)
# Get classes actually present in your raster
present_vals <- unique(values(nlcd_mask, na.rm = TRUE))
legend_use   <- nlcd_legend %>% filter(value %in% present_vals)

# ── 3. PLOT ───────────────────────────────────────────────────────────────
nlcd_4326 <- project(nlcd_mask, "EPSG:4326", method = "near")  
# "near" = nearest neighbor, important for categorical data

nlcd_factor <- as.factor(nlcd_4326)
present_vals <- as.numeric(levels(nlcd_factor)[[1]]$value)
legend_use   <- nlcd_legend %>% filter(value %in% present_vals)

# Pull levels directly to build the scale
legend_use <- nlcd_legend %>% 
  filter(value %in% as.numeric(nlcd_levels$Class))

ggplot() +
  geom_raster(data = nlcd_df, 
              aes(x = x, y = y, fill = landcover_agg)) +
  geom_sf(data = cape_fear_wbd_4326,
          fill = NA, color = "black", linewidth = 0.9) +
  geom_sf(data = st_transform(petre_sites_sf, 4326),
          shape = 21, fill = "white", color = "black", 
          size = 3, stroke = 0.8) +
  scale_fill_manual(
    values   = agg_colors,
    na.value = "white",
    name     = "Land Cover"
  ) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(location = "bl") +
  labs(
    title   = "Land Cover — Cape Fear Basin",
    subtitle = "NLCD 2019",
    caption = "Source: USGS National Land Cover Database"
  ) +
  coord_sf(crs = 4326) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid  = element_blank(),
    plot.title  = element_text(face = "bold"),
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10),
    legend.key.size = unit(0.5, "cm")
  )
ggsave("cape_fear_landcover.png", width = 10, height = 8, dpi = 300)