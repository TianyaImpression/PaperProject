# Load required packages
library(tidyverse)

# ==========================================
# 1. Data loading and cleaning function
# ==========================================
prepare_eval_data <- function(gsod_path, rds_path, var_type) {
  # Read data
  gsod_raw <- read_csv(gsod_path, show_col_types = FALSE)
  preds <- readRDS(rds_path)

  # Clean observations
  obs <- gsod_raw |>
    mutate(Year = as.integer(substr(年月, 1, 4)),
           Month = as.integer(substr(年月, 6, 7))) |>
    filter(Year >= 1982, Year <= 2024, 当月有效天数 >= 25)

  # Select variable and rename
  if (var_type == "TMP") {
    obs <- obs |> rename(Obs = 月平均气温)
  } else {
    obs <- obs |> rename(Obs = 月降水总量)
  }

  obs <- obs |> select(气象站代码, Year, Month, Obs)

  # Merge predictions and remove rows with NA in key columns
  df_merged <- obs |>
    inner_join(preds, by = c("气象站代码", "Year", "Month")) |>
    drop_na(`0.5'`, `30'`, Obs)

  return(df_merged)
}

# Load temperature and precipitation data
df_tmp <- prepare_eval_data("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv",
                            "H:/WorldDeltaClim/all_preds_tmp_43years_with30m.rds", "TMP")

df_pre <- prepare_eval_data("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv",
                            "H:/WorldDeltaClim/all_preds_pre_43years_with30m.rds", "PRE")

# ==========================================
# 2. Compute metrics across all resolutions
# ==========================================
evaluate_resolutions <- function(df, var_name) {
  resolutions <- c("30'", "10'", "5'", "2.5'", "0.5'")

  results <- map_dfr(resolutions, function(res) {
    obs <- df$Obs
    pred <- df[[res]]

    tibble(
      Variable = var_name,
      Resolution = res,
      MAE = mean(abs(pred - obs), na.rm = TRUE),
      RMSE = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
      COR = cor(pred, obs, use = "complete.obs"),
      NSE = 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
    )
  })

  # Set factor levels for coarse-to-fine ordering
  results <- results |>
    mutate(Resolution = factor(Resolution, levels = c("30'", "10'", "5'", "2.5'", "0.5'")))

  return(results)
}

# ==========================================
# 3. Run evaluation and create summary table
# ==========================================
res_tmp <- evaluate_resolutions(df_tmp, "TMP (°C)")
res_pre <- evaluate_resolutions(df_pre, "PRE (mm)")

final_table <- bind_rows(res_tmp, res_pre) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(final_table)

# Save table to CSV
write_csv(final_table, "H:/WorldDeltaClim/Multi_Resolution_Evaluation_Table.csv")

# ==========================================
# 4. Print relative improvement from 30' to 0.5'
# ==========================================
print_improvements <- function(res_df, var_label) {
  orig <- res_df |> filter(Resolution == "30'")
  down <- res_df |> filter(Resolution == "0.5'")

  cat(sprintf("\n=== %s improvement (30' vs 0.5') ===\n", var_label))
  cat(sprintf("MAE relative reduction: %.2f%%\n", (orig$MAE - down$MAE) / orig$MAE * 100))
  cat(sprintf("RMSE relative reduction: %.2f%%\n", (orig$RMSE - down$RMSE) / orig$RMSE * 100))
  cat(sprintf("COR absolute increase: %.3f\n", down$COR - orig$COR))
  cat(sprintf("NSE absolute increase: %.3f\n", down$NSE - orig$NSE))
}

cat("\n--- Relative improvement metrics for manuscript ---\n")
print_improvements(res_tmp, "Temperature")
print_improvements(res_pre, "Precipitation")