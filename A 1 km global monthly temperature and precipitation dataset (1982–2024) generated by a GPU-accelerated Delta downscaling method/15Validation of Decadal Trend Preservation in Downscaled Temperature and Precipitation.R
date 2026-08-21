# Load required packages
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(patchwork)

# 1. Process GSOD observations and compute station-level decadal trends

# Read raw monthly observations
gsod_raw <- read_csv("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv",
                     show_col_types = FALSE)

# Aggregate monthly data to annual values, keeping only stations with >=10 valid months per year
gsod_annual <- gsod_raw %>%
  mutate(Year = as.integer(substr(年月, 1, 4))) %>%
  filter(Year >= 1982, Year <= 2024, 当月有效天数 >= 25) %>%
  group_by(气象站代码, Year, 经度, 纬度) %>%
  summarise(
    valid_months = n(),
    Annual_TMP = mean(月平均气温, na.rm = TRUE),
    Annual_PRE = sum(月降水总量, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(valid_months >= 10)

# Compute decadal trend (slope * 10) for each station with at least 20 years of data
obs_trends <- gsod_annual %>%
  group_by(气象站代码, 经度, 纬度) %>%
  summarise(
    n_years = n(),
    Obs_Trend_TMP = ifelse(n_years >= 20, coef(lm(Annual_TMP ~ Year))[2] * 10, NA),
    Obs_Trend_PRE = ifelse(n_years >= 20, coef(lm(Annual_PRE ~ Year))[2] * 10, NA),
    .groups = "drop"
  ) %>%
  drop_na(Obs_Trend_TMP, Obs_Trend_PRE)

cat("Station-based observed trends computed. Valid stations:", nrow(obs_trends), "\n")

# 2. Extract downscaled trend values at station locations from precomputed 1-km rasters

trend_tmp_rast <- rast("H:/WorldDeltaClim/trend_tmp_decade.tif")
trend_pre_rast <- rast("H:/WorldDeltaClim/trend_pre_decade.tif")

stations_vect <- vect(obs_trends, geom = c("经度", "纬度"), crs = "EPSG:4326")

obs_trends$Pred_Trend_TMP <- terra::extract(trend_tmp_rast, stations_vect)[, 2]
obs_trends$Pred_Trend_PRE <- terra::extract(trend_pre_rast, stations_vect)[, 2]

# Remove stations with missing extracted values
val_trends <- obs_trends %>%
  drop_na(Pred_Trend_TMP, Pred_Trend_PRE)

# 3. Calculate validation metrics (R², slope, N)

calc_metrics <- function(obs, pred) {
  lm_fit <- lm(pred ~ obs)
  r2 <- summary(lm_fit)$r.squared
  slope <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]
  n <- length(obs)
  
  label_text <- sprintf("y = %.2fx %s %.2f\nR² = %.3f\nN = %s",
                        slope,
                        ifelse(intercept >= 0, "+", "-"),
                        abs(intercept),
                        r2,
                        format(n, big.mark = ","))
  
  list(label = label_text, r2 = r2, slope = slope, n = n)
}

met_tmp <- calc_metrics(val_trends$Obs_Trend_TMP, val_trends$Pred_Trend_TMP)
met_pre <- calc_metrics(val_trends$Obs_Trend_PRE, val_trends$Pred_Trend_PRE)

cat("Temperature trend validation: Slope =", met_tmp$slope,
    " R² =", met_tmp$r2, " N =", met_tmp$n, "\n")
cat("Precipitation trend validation: Slope =", met_pre$slope,
    " R² =", met_pre$r2, " N =", met_pre$n, "\n")

# 4. Plot density scatter plots for observed vs. downscaled decadal trends

plot_hex <- function(df, x_col, y_col, metrics, title, xlab, ylab, viridis_opt) {
  all_vals <- c(df[[x_col]], df[[y_col]])
  lims <- c(min(all_vals, na.rm = TRUE), max(all_vals, na.rm = TRUE))
  
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_hex(bins = 65) +
    scale_fill_viridis_c(option = viridis_opt, trans = "log10",
                         name = "Count (log10)") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "black", linewidth = 0.8) +
    geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1) +
    coord_fixed(xlim = lims, ylim = lims) +
    annotate("text", x = lims[1] + (lims[2] - lims[1]) * 0.05,
             y = lims[2] - (lims[2] - lims[1]) * 0.05,
             label = metrics$label,
             hjust = 0, vjust = 1, fontface = "bold", size = 3.8) +
    labs(title = title, x = xlab, y = ylab) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.key.height = unit(0.8, "cm")
    )
}

p9a <- plot_hex(val_trends, "Obs_Trend_TMP", "Pred_Trend_TMP", met_tmp,
                "(a) Decadal Trend: Temperature",
                expression("Observed Trend ("*degree*"C decade"^-1*")"),
                expression("1-km Downscaled Trend ("*degree*"C decade"^-1*")"),
                "plasma")

p9b <- plot_hex(val_trends, "Obs_Trend_PRE", "Pred_Trend_PRE", met_pre,
                "(b) Decadal Trend: Precipitation",
                expression("Observed Trend (mm decade"^-1*")"),
                expression("1-km Downscaled Trend (mm decade"^-1*")"),
                "viridis")

# Combine panels side by side
fig_9 <- p9a + p9b + plot_layout(ncol = 2)

# Save as PDF and TIFF
ggsave("D:/R/DeltaClim/Figure_9_Trend_Validation.pdf",
       plot = fig_9, width = 11, height = 5.0, device = "pdf")
ggsave("D:/R/DeltaClim/Figure_9_Trend_Validation.tif",
       plot = fig_9, width = 11, height = 5.0, dpi = 300, compression = "lzw")

