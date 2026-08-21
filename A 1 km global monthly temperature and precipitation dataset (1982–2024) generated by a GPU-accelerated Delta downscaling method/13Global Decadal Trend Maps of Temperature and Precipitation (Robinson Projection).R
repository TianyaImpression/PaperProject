# Load required packages
library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)
library(sf)
library(scales)

# ------------------------------------------------------------------------------
# 1. Read trend data and extend to global extent
# ------------------------------------------------------------------------------

# Read decadal trend rasters
trend_tmp_raw <- rast("H:/WorldDeltaClim/trend_tmp_decade.tif")
trend_pre_raw <- rast("H:/WorldDeltaClim/trend_pre_decade.tif")

# Aggregate to reduce rendering load
tmp_small <- aggregate(trend_tmp_raw, fact = 10, fun = "mean")
pre_small <- aggregate(trend_pre_raw, fact = 10, fun = "mean")

# Extend to full global extent [-180, 180, -90, 90]
global_ext <- ext(-180, 180, -90, 90)
tmp_full <- extend(tmp_small, global_ext)
pre_full <- extend(pre_small, global_ext)

# Robinson projection definition
robin_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Project to Robinson
tmp_robin <- project(tmp_full, robin_crs)
pre_robin <- project(pre_full, robin_crs)

# ------------------------------------------------------------------------------
# 2. Build closed Robinson ellipse boundary and clip rasters
# ------------------------------------------------------------------------------

# Construct a closed polygon approximating the Robinson globe outline
lons <- c(seq(-180, 180, length.out = 360), rep(180, 180),
          seq(180, -180, length.out = 360), rep(-180, 180))
lats <- c(rep(90, 360), seq(90, -90, length.out = 180),
          rep(-90, 360), seq(-90, 90, length.out = 180))

globe_poly <- st_polygon(list(cbind(lons, lats)))
globe_sf <- st_sfc(globe_poly, crs = 4326) %>% st_transform(robin_crs)

# Mask rasters to the Robinson outline
tmp_robin <- terra::mask(tmp_robin, vect(globe_sf))
pre_robin <- terra::mask(pre_robin, vect(globe_sf))

# Create graticules
graticules <- st_graticule(lat = seq(-90, 90, 30), lon = seq(-180, 180, 60)) %>%
  st_transform(robin_crs)

# ------------------------------------------------------------------------------
# 3. Panel (a): Temperature trend
# ------------------------------------------------------------------------------
p_tmp_trend <- ggplot() +
  geom_sf(data = globe_sf, fill = "#f1f5f9", color = "black", linewidth = 0.6) +
  geom_spatraster(data = tmp_robin) +
  geom_sf(data = graticules, color = "white", linewidth = 0.25,
          linetype = "dotted", alpha = 0.7) +
  geom_sf(data = globe_sf, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_gradient2(
    low = "#0571b0",
    mid = "#ffffff",
    high = "#ca0020",
    midpoint = 0,
    limits = c(-1.0, 1.0),
    oob = scales::squish,
    name = expression("Decadal Trend ("*degree*"C decade"^-1*")"),
    na.value = "transparent"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "(a) Decadal Trend of Mean Temperature (1982–2024)") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 11, face = "bold", vjust = 0.8),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                              margin = margin(b = 10)),
    plot.margin = margin(10, 10, 15, 10)
  )

# ------------------------------------------------------------------------------
# 4. Panel (b): Precipitation trend
# ------------------------------------------------------------------------------
p_pre_trend <- ggplot() +
  geom_sf(data = globe_sf, fill = "#f1f5f9", color = "black", linewidth = 0.6) +
  geom_spatraster(data = pre_robin) +
  geom_sf(data = graticules, color = "white", linewidth = 0.25,
          linetype = "dotted", alpha = 0.7) +
  geom_sf(data = globe_sf, fill = NA, color = "black", linewidth = 0.6) +
  scale_fill_gradient2(
    low = "#543005",
    mid = "#f5f5f5",
    high = "#003c30",
    midpoint = 0,
    limits = c(-15, 15),
    oob = scales::squish,
    name = expression("Decadal Trend (mm decade"^-1*")"),
    na.value = "transparent"
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "(b) Decadal Trend of Annual Precipitation (1982–2024)") +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 11, face = "bold", vjust = 0.8),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5,
                              margin = margin(b = 10)),
    plot.margin = margin(10, 10, 15, 10)
  )

# ------------------------------------------------------------------------------
# 5. Combine and save
# ------------------------------------------------------------------------------
fig_8 <- p_tmp_trend / p_pre_trend +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "white", color = NA)))

ggsave("D:/R/DeltaClim/Figure_8_Global_Trends_FinalDeep.pdf",
       plot = fig_8, width = 11, height = 13, device = "pdf")
ggsave("D:/R/DeltaClim/Figure_8_Global_Trends_FinalDeep.tif",
       plot = fig_8, width = 11, height = 13, dpi = 300, compression = "lzw")