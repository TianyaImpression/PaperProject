# Load required packages
library(tidyverse)
library(viridis)
library(patchwork)
library(scales)

# ------------------------------------------------------------------------------
# 1. Read and prepare observation data
# ------------------------------------------------------------------------------
gsod_raw <- read_csv("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv",
                     show_col_types = FALSE)

obs_df <- gsod_raw %>%
  mutate(
    Year = as.integer(substr(年月, 1, 4)),
    Month = as.integer(substr(年月, 6, 7))
  ) %>%
  filter(Year >= 1982, Year <= 2024, 当月有效天数 >= 25) %>%
  select(气象站代码, Year, Month,
         Obs_TMP = 月平均气温,
         Obs_PRE = 月降水总量)

# ------------------------------------------------------------------------------
# 2. Merge with predictions
# ------------------------------------------------------------------------------
# Temperature
preds_tmp <- readRDS("H:/WorldDeltaClim/all_preds_tmp_43years_with30m.rds")
val_tmp <- obs_df %>%
  inner_join(preds_tmp, by = c("气象站代码", "Year", "Month")) %>%
  drop_na(Obs_TMP, `30'`, `0.5'`)

# Precipitation
preds_pre <- readRDS("H:/WorldDeltaClim/all_preds_pre_43years_with30m.rds")
val_pre <- obs_df %>%
  inner_join(preds_pre, by = c("气象站代码", "Year", "Month")) %>%
  drop_na(Obs_PRE, `30'`, `0.5'`)

# ------------------------------------------------------------------------------
# 3. Function to compute performance metrics
# ------------------------------------------------------------------------------
calc_metrics <- function(obs, pred) {
  n <- length(obs)
  lm_fit <- lm(pred ~ obs)
  coefs <- coef(lm_fit)
  slope <- coefs[2]
  intercept <- coefs[1]

  r2 <- summary(lm_fit)$r.squared
  rmse <- sqrt(mean((pred - obs)^2))
  mae <- mean(abs(pred - obs))

  ss_res <- sum((obs - pred)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  nse <- 1 - (ss_res / ss_tot)

  label_text <- sprintf(
    "y = %.2fx %s %.2f\nR² = %.3f\nRMSE = %.2f\nMAE = %.2f\nNSE = %.3f\nN = %s",
    slope,
    ifelse(intercept >= 0, "+", "-"),
    abs(intercept),
    r2, rmse, mae, nse,
    format(n, big.mark = ",")
  )

  list(
    label = label_text,
    r2 = r2,
    rmse = rmse,
    mae = mae,
    nse = nse,
    slope = slope,
    intercept = intercept,
    n = n
  )
}

# Compute metrics for all four combinations
m_tmp_05 <- calc_metrics(val_tmp$Obs_TMP, val_tmp$`30'`)
m_tmp_1k <- calc_metrics(val_tmp$Obs_TMP, val_tmp$`0.5'`)
m_pre_05 <- calc_metrics(val_pre$Obs_PRE, val_pre$`30'`)
m_pre_1k <- calc_metrics(val_pre$Obs_PRE, val_pre$`0.5'`)

# Print key metrics for the manuscript
cat("Temperature  0.5° RMSE:", round(m_tmp_05$rmse, 2),
    " MAE:", round(m_tmp_05$mae, 2), "\n")
cat("Temperature  1km  RMSE:", round(m_tmp_1k$rmse, 2),
    " MAE:", round(m_tmp_1k$mae, 2), "\n")
cat("Precipitation 0.5° RMSE:", round(m_pre_05$rmse, 2),
    " MAE:", round(m_pre_05$mae, 2), "\n")
cat("Precipitation 1km  RMSE:", round(m_pre_1k$rmse, 2),
    " MAE:", round(m_pre_1k$mae, 2), "\n")

# ------------------------------------------------------------------------------
# 4. Plotting function for hexagonal density scatter
# ------------------------------------------------------------------------------
plot_scatter_hex <- function(df, x_col, y_col, metrics, title_str,
                             x_label, y_label, viridis_opt = "plasma") {
  all_vals <- c(df[[x_col]], df[[y_col]])
  axis_min <- min(all_vals, na.rm = TRUE)
  axis_max <- max(all_vals, na.rm = TRUE)
  axis_lim <- c(axis_min, axis_max)

  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_hex(bins = 70) +
    scale_fill_viridis_c(option = viridis_opt, trans = "log10",
                         name = "Count (log10)") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "black", linewidth = 0.8) +
    geom_smooth(method = "lm", color = "#D32F2F", se = FALSE, linewidth = 1.0) +
    coord_fixed(xlim = axis_lim, ylim = axis_lim) +
    annotate("text",
             x = axis_min + (axis_max - axis_min) * 0.04,
             y = axis_max - (axis_max - axis_min) * 0.04,
             label = metrics$label,
             hjust = 0, vjust = 1,
             size = 3.6, fontface = "bold", color = "black") +
    labs(title = title_str, x = x_label, y = y_label) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.key.height = unit(0.8, "cm")
    )
}

# ------------------------------------------------------------------------------
# 5. Create four panels
# ------------------------------------------------------------------------------
p_a <- plot_scatter_hex(val_tmp, "Obs_TMP", "30'", m_tmp_05,
                        "(a) Temperature: Original 0.5°",
                        "Observed Mean TMP (°C)", "Predicted TMP (°C)", "plasma")

p_b <- plot_scatter_hex(val_tmp, "Obs_TMP", "0.5'", m_tmp_1k,
                        "(b) Temperature: Downscaled 1-km",
                        "Observed Mean TMP (°C)", "Predicted TMP (°C)", "plasma")

p_c <- plot_scatter_hex(val_pre, "Obs_PRE", "30'", m_pre_05,
                        "(c) Precipitation: Original 0.5°",
                        "Observed Annual PRE (mm)", "Predicted PRE (mm)", "viridis")

p_d <- plot_scatter_hex(val_pre, "Obs_PRE", "0.5'", m_pre_1k,
                        "(d) Precipitation: Downscaled 1-km",
                        "Observed Annual PRE (mm)", "Predicted PRE (mm)", "viridis")

# ------------------------------------------------------------------------------
# 6. Combine and save
# ------------------------------------------------------------------------------
final_fig7 <- (p_a + p_b) / (p_c + p_d) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave("D:/R/DeltaClim/Figure_7_Scatter_Validation_2x2.pdf",
       plot = final_fig7, width = 11, height = 10, device = "pdf")
ggsave("D:/R/DeltaClim/Figure_7_Scatter_Validation_2x2.tif",
       plot = final_fig7, width = 11, height = 10, dpi = 300, compression = "lzw")