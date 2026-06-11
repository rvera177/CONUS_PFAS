

library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggspatial)
library(wesanderson)
library(scales)

#this is my dataset with PFOA predicitons from ML
cape_fear_pred_spatial <- readRDS("~/cape_fear_pred_spatial.rds")

pfoa_anim <- cape_fear_pred_spatial %>%
  filter(compound == "PFOA")

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

# ── 4. SCALE LINE WIDTH BY FLOW (sqrt for better visual contrast)
pfoa_anim_monthly <- pfoa_anim_monthly %>%
  mutate(
    flow_lw = scales::rescale(
      sqrt(Flow_cms),
      to   = c(0.3, 3.5),
      from = range(sqrt(Flow_cms), na.rm = TRUE)
    )
  )

# ── 5. SITES
petre_sites_sf <- petre_with_flow %>%
  distinct(Longitude, Latitude) %>%
  mutate(site_idx = row_number()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# ── 6. COLOR LIMITS
lims <- quantile(
  pfoa_anim_monthly$conc_pred_ngL,
  c(0.02, 0.98),
  na.rm = TRUE)

# Color by flow to see which reaches are "big" vs "small"
pfoa_jan <- pfoa_anim_monthly %>% filter(anim_date == min(anim_date))

ggplot() +
  geom_sf(data = cape_fear_wbd_4326, fill = NA, color = "black") +
  geom_sf(data = pfoa_jan, aes(color = Flow_cms), linewidth = 1) +
  scale_color_viridis_c(trans = "log10") +
  labs(title = "Flow_cms by NHD reach (Jan 2020)") +
  theme_minimal()

# Pull metrics directly from model results
pfoa_res <- results_rf_all[["PFOA"]]

pfoa_caption <- sprintf(
  
  paste0(
    
    "Monthly mean PFOA predicted by Random Forest (leave-one-site-out CV).\n",
    "R\u00b2 = %.3f | RMSE = %.3f log\u2081\u2080(ng/L) | Spearman \u03c1 = %.3f | n = %d\n",
    "Line width scaled to mean monthly discharge. One headwater site excluded (no modeled flow)."
  ),
  pfoa_res$r2, pfoa_res$rmse, pfoa_res$spearman, pfoa_res$n
)

pfoa_anim_obj <- ggplot() +
  geom_sf(data = cape_fear_wbd_4326,
          fill = NA, color = "black",
          linewidth = 0.8) +
  geom_sf(
    data = pfoa_anim_monthly,
    aes(color = conc_pred_ngL,
        linewidth = flow_lw)) +
  scale_linewidth_identity() +
  geom_sf(
    data = petre_sites_sf,
    shape = 21,
    fill = "white",
    color = "black",
    size = 3) +
  scale_color_gradientn(
    colours = pal,
    trans   = "log10",
    limits  = lims,
    oob     = scales::squish,
    name    = "PFOA (ng/L)") +
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(location = "bl") +
  labs(
    title    = "Predicted Surface Water PFOA Concentration\nCape Fear Basin",
    subtitle = "Month: {format(frame_time, '%b %Y')}",
    caption  = pfoa_caption) +
  coord_sf() +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    plot.caption  = element_text(size = 9, hjust = 0, lineheight = 1.2),
    plot.margin   = ggplot2::margin(t = 10, r = 10, b = 20, l = 10)) +
  transition_time(anim_date) +
  ease_aes("linear")

print(pfoa_anim_obj)


library(gifski)
animate(
  pfoa_anim_obj,
  renderer = gifski_renderer("PFOA_CapeFear_2020_2021.gif"),
  width    = 1400,
  height   = 1400,
  fps      = 2,
  nframes  = n_distinct(pfoa_anim_monthly$anim_date)
)

#why doesn't this av package work anymore?!?
library(av)
animate(
  pfoa_anim_obj,
  renderer = av_renderer("PFOA_CapeFear_2020.mp4"),
  width = 1400,
  height = 1200,
  fps = 8,
  nframes = 80
)

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
nlcd_4326 <- project(nlcd_mask, "EPSG:4326", method = "near")  
# "near" = nearest neighbor, important for categorical data

nlcd_factor <- as.factor(nlcd_4326)
present_vals <- as.numeric(levels(nlcd_factor)[[1]]$value)

# ── 2. NLCD COLOR PALETTE ─────────────────────────────────────────────────
# Official NLCD colors and labels
# Step 1: create nlcd_df from the raster 
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

# ── 3. PLOT ───────────────────────────────────────────────────────────────

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
    legend.key.size = unit(0.5, "cm")
  )

ggsave("cape_fear_landcover.png", width = 10, height = 8, dpi = 300)