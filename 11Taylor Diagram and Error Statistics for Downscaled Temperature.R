# Load required packages
library(tidyverse)
library(plotrix)

# 1. Read and prepare data
gsod_raw <- read_csv("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv",
                     show_col_types = FALSE)

obs_data <- gsod_raw %>%
  mutate(Year = as.integer(substr(年月, 1, 4)),
         Month = as.integer(substr(年月, 6, 7))) %>%
  filter(Year >= 1982, Year <= 2024, 当月有效天数 >= 25) %>%
  rename(Obs = 月平均气温) %>%
  select(气象站代码, Year, Month, Obs)

preds_data <- readRDS("H:/WorldDeltaClim/all_preds_tmp_43years_with30m.rds")

taylor_data <- obs_data %>%
  inner_join(preds_data, by = c("气象站代码", "Year", "Month")) %>%
  drop_na(Obs, `30'`, `0.5'`)

# Extract vectors for Taylor diagram
val_obs <- taylor_data$Obs
val_cru <- taylor_data$`30'`       # Original CRU (30 arc-minute)
val_ds  <- taylor_data$`0.5'`      # Downscaled (1 km)

# 2. Export Taylor diagram as PDF
pdf_file_path <- "D:/R/DeltaClim/Figure_5_Taylor_Diagram_TMP_InsideLegend.pdf"
pdf(file = pdf_file_path, width = 7, height = 7)

# Set square plot area with standard margins
par(pty = "s", mar = c(5, 5, 2, 2))

# Draw Taylor diagram base (reference = observations)
taylor.diagram(val_obs, val_obs,
               add = FALSE,
               col = "#222222", pch = 15, pcex = 2.0, cex.axis = 1.1,
               main = "", xlab = "Standard Deviation", ylab = "Standard Deviation",
               pos.cor = TRUE, show.gamma = TRUE, ngamma = 4,
               gamma.col = "#27AE60", sd.arcs = 1, ref.sd = TRUE)

# Add CRU and downscaled points
taylor.diagram(val_obs, val_cru, add = TRUE, col = "#D35400", pch = 16, pcex = 2.2)
taylor.diagram(val_obs, val_ds,  add = TRUE, col = "#2980B9", pch = 17, pcex = 2.2)

# Add inset legend
legend(x = "topright",
       inset = c(0.02, 0.05),
       legend = c("Ground Truth (GSOD)", "Original CRU (30')", "Downscaled (1-km)"),
       col = c("#222222", "#D35400", "#2980B9"),
       pch = c(15, 16, 17),
       pt.cex = 1.8,
       cex = 1.0,
       bty = "n",
       text.font = 2,
       y.intersp = 1.3)

dev.off()

# 3. Calculate Taylor diagram statistics
calc_stats <- function(obs, pred, name) {
  bias   <- mean(pred) - mean(obs)
  sd_obs <- sd(obs)
  sd_pred <- sd(pred)
  r      <- cor(obs, pred)
  rmse   <- sqrt(mean((pred - obs)^2))
  crmse  <- sqrt(sd_pred^2 + sd_obs^2 - 2 * sd_pred * sd_obs * r)
  
  tibble(
    Model    = name,
    SD_obs   = sd_obs,
    SD_pred  = sd_pred,
    Cor      = r,
    Bias     = bias,
    RMSE     = rmse,
    CRMSE    = crmse,
    SD_ratio = sd_pred / sd_obs
  )
}

stats_obs <- calc_stats(val_obs, val_obs, "GSOD (Reference)")
stats_cru <- calc_stats(val_obs, val_cru, "CRU 30'")
stats_ds  <- calc_stats(val_obs, val_ds,  "Downscaled 1km")

taylor_table <- bind_rows(stats_obs, stats_cru, stats_ds)
print(taylor_table, digits = 4)

# Improvement metrics
improvement <- (stats_cru$CRMSE - stats_ds$CRMSE) / stats_cru$CRMSE * 100
cat(sprintf("\nDownscaled CRMSE improvement over CRU: %.1f%%\n", improvement))
cat(sprintf("Correlation increase: +%.3f\n", stats_ds$Cor - stats_cru$Cor))

# Save statistics table
write_csv(taylor_table, "D:/R/DeltaClim/Figure5_Taylor_Stats_TMP.csv")