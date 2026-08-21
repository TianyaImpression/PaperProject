# Load required packages
library(terra)
library(sf)
library(elevatr)
library(tidyverse)

# 1. Define study area (core Qinghai-Tibet Plateau)
# Extent: 75E - 100E, 28N - 38N
qtp_bbox <- ext(75, 100, 28, 38)

# Data frame for elevatr input
qtp_df <- data.frame(x = c(75, 100), y = c(28, 38))

# 2. Read mean temperature raster and crop to study area
temp_1km_raw <- rast("H:/WorldDeltaClim/mean_tmp_1982_2024.tif")
temp_1km_qtp <- crop(temp_1km_raw, qtp_bbox)

# Generate coarse-resolution control (reverse smoothing)
temp_05_qtp <- aggregate(temp_1km_qtp, fact = 30, fun = "mean")

# 3. Download high-resolution DEM using elevatr (requires internet)
dem_raw <- get_elev_raster(locations = qtp_df, prj = "EPSG:4326", z = 7, clip = "bbox")
dem_qtp <- rast(dem_raw)

# 4. Align resolutions and extract random topographic samples
temp_05_res <- resample(temp_05_qtp, temp_1km_qtp, method = "near")
dem_res <- resample(dem_qtp, temp_1km_qtp, method = "bilinear")

# Combine into a multi-band raster
combined_rast <- c(temp_1km_qtp, temp_05_res, dem_res)
names(combined_rast) <- c("TMP_1km", "TMP_05deg", "Elevation")

# Randomly sample 10,000 pixels
set.seed(42)
sample_pts <- spatSample(combined_rast, size = 10000, xy = TRUE, na.rm = TRUE)

# Clean data: keep only high elevations (>= 2000 m)
sample_df <- as_tibble(sample_pts) %>%
  filter(Elevation >= 2000)

# 5. Compute lapse rates using linear regression
lm_1km <- lm(TMP_1km ~ Elevation, data = sample_df)
lm_05 <- lm(TMP_05deg ~ Elevation, data = sample_df)

# Convert slope to °C per km
lr_1km <- coef(lm_1km)[2] * 1000
lr_05 <- coef(lm_05)[2] * 1000
r2_1km <- summary(lm_1km)$r.squared
r2_05 <- summary(lm_05)$r.squared

cat(sprintf("1-km downscaled lapse rate: %.2f °C/km (R² = %.3f)\n", lr_1km, r2_1km))
cat(sprintf("0.5° original lapse rate: %.2f °C/km (R² = %.3f)\n", lr_05, r2_05))

# 6. Reshape data for plotting
plot_df <- sample_df %>%
  pivot_longer(cols = c(TMP_1km, TMP_05deg),
               names_to = "Model",
               values_to = "Temperature") %>%
  mutate(Model = if_else(Model == "TMP_1km",
                         "1-km Downscaled",
                         "0.5° Original CRU"))

# Create comparison scatter plot
p_lapse <- ggplot(plot_df, aes(x = Elevation, y = Temperature, color = Model)) +
  geom_point(alpha = 0.15, size = 0.8, shape = 16) +
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.2,
              se = FALSE, aes(linetype = Model)) +
  scale_color_manual(values = c("1-km Downscaled" = "#2980B9",
                                "0.5° Original CRU" = "#E74C3C")) +
  scale_linetype_manual(values = c("1-km Downscaled" = "solid",
                                   "0.5° Original CRU" = "dashed")) +
  annotate("text", x = max(sample_df$Elevation), y = max(sample_df$TMP_1km),
           label = sprintf("1-km Rate: %.2f °C/km (R² = %.2f)", lr_1km, r2_1km),
           hjust = 1, vjust = 1, color = "#2980B9", fontface = "bold", size = 4) +
  annotate("text", x = max(sample_df$Elevation), y = max(sample_df$TMP_1km) - 2.5,
           label = sprintf("0.5° Rate: %.2f °C/km (R² = %.2f)", lr_05, r2_05),
           hjust = 1, vjust = 1, color = "#E74C3C", fontface = "bold", size = 4) +
  labs(
    title = "Topographical Fidelity Validation (Qinghai-Tibet Plateau)",
    x = "Elevation (m)",
    y = "Mean Annual Temperature (°C)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# 7. Save figure as PDF and TIFF
ggsave("D:/R/DeltaClim/Figure_S1_Lapse_Rate_Validation.pdf",
       p_lapse, width = 8, height = 6)
ggsave("D:/R/DeltaClim/Figure_S1_Lapse_Rate_Validation.tif",
       p_lapse, width = 8, height = 6, dpi = 300, compression = "lzw")
