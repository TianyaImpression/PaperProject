# Load required packages
library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)
library(sf)

# 1. Read 43-year mean climatology rasters
mean_tmp_1km <- rast("H:/WorldDeltaClim/mean_tmp_1982_2024.tif")
mean_pre_1km <- rast("H:/WorldDeltaClim/mean_pre_1982_2024.tif")

# Aggregate to coarser resolution for faster rendering
tmp_small <- aggregate(mean_tmp_1km, fact = 10, fun = "mean")
pre_small <- aggregate(mean_pre_1km, fact = 10, fun = "mean")

# Extend to full global extent [-180, 180, -90, 90]
global_ext <- ext(-180, 180, -90, 90)
tmp_full <- extend(tmp_small, global_ext)
pre_full <- extend(pre_small, global_ext)

# Define Robinson projection
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Project to Robinson
tmp_robin <- project(tmp_full, robin_crs)
pre_robin <- project(pre_full, robin_crs)

# Construct a closed Robinson globe outline polygon
lons <- c(seq(-180, 180, length.out = 360), rep(180, 180),
          seq(180, -180, length.out = 360), rep(-180, 180))
lats <- c(rep(90, 360), seq(90, -90, length.out = 180),
          rep(-90, 360), seq(-90, 90, length.out = 180))
globe_poly <- st_polygon(list(cbind(lons, lats)))
globe_sf <- st_sfc(globe_poly, crs = 4326) %>% st_transform(robin_crs)

# Mask rasters to the Robinson outline
tmp_robin <- terra::mask(tmp_robin, vect(globe_sf))
pre_robin <- terra::mask(pre_robin, vect(globe_sf))

# Create graticules (30° latitude, 60° longitude)
graticules <- st_graticule(lat = seq(-90, 90, 30), lon = seq(-180, 180, 60)) %>%
  st_transform(robin_crs)

# ------------------------------------------------------------------------------
# Panel (a): Mean annual temperature
# ------------------------------------------------------------------------------
p_tmp_map <- ggplot() +
  geom_sf(data = globe_sf, fill = "#f8fafc", color = "black", linewidth = 0.6) +
  geom_spatraster(data = tmp_robin) +
  geom_sf(data = graticules, color = "white", linewidth = 0.25,
          linetype = "dotted", alpha = 0.7) +
  geom_sf(data = globe_sf, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Mean TMP (°C)",
    na.value = "transparent"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "(a) Global 43-year Mean Annual Temperature (1982–2024)") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid  = element_blank(),
    axis.text   = element_blank(),
    axis.title  = element_blank(),
    axis.ticks  = element_blank(),
    legend.position   = "bottom",
    legend.key.width  = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title      = element_text(size = 11, face = "bold", vjust = 0.8),
    legend.text       = element_text(size = 10),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5,
                                     margin = margin(b = 10)),
    plot.margin       = margin(10, 10, 15, 10)
  )

# ------------------------------------------------------------------------------
# Panel (b): Mean annual precipitation
# ------------------------------------------------------------------------------
p_pre_map <- ggplot() +
  geom_sf(data = globe_sf, fill = "#f8fafc", color = "black", linewidth = 0.6) +
  geom_spatraster(data = pre_robin) +
  geom_sf(data = graticules, color = "white", linewidth = 0.25,
          linetype = "dotted", alpha = 0.7) +
  geom_sf(data = globe_sf, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_viridis_c(
    option = "mako", direction = -1,
    name = "Annual PRE (mm)",
    na.value = "transparent",
    trans = "pseudo_log",
    breaks = c(0, 100, 500, 1500, 3000, 5000)
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "(b) Global 43-year Mean Annual Precipitation (1982–2024)") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid  = element_blank(),
    axis.text   = element_blank(),
    axis.title  = element_blank(),
    axis.ticks  = element_blank(),
    legend.position   = "bottom",
    legend.key.width  = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title      = element_text(size = 11, face = "bold", vjust = 0.8),
    legend.text       = element_text(size = 10),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5,
                                     margin = margin(b = 10)),
    plot.margin       = margin(10, 10, 15, 10)
  )

# ------------------------------------------------------------------------------
# Combine and save
# ------------------------------------------------------------------------------
fig_6 <- p_tmp_map / p_pre_map +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "white", color = NA)))

ggsave("D:/R/DeltaClim/Figure_6_Global_Climatology_Robinson.pdf",
       plot = fig_6, width = 11, height = 13, device = "pdf")
ggsave("D:/R/DeltaClim/Figure_6_Global_Climatology_Robinson.tif",
       plot = fig_6, width = 11, height = 13, dpi = 300, compression = "lzw")