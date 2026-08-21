# Load required packages
library(tidyverse)
library(terra)

# 1. Prepare station points and ground truth observations
# Ensure 'unique_stations' is defined with columns "经度" and "纬度"
stations_vect <- vect(unique_stations, geom = c("经度", "纬度"), crs = "EPSG:4326")

# Read cleaned GSOD monthly observations (43 years)
gsod_raw <- read_csv("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv")
gsod_obs <- gsod_raw |> 
  mutate(
    Year = as.integer(substr(年月, 1, 4)),
    Month = as.integer(substr(年月, 6, 7))
  ) |> 
  filter(Year >= 1982, Year <= 2024, 当月有效天数 >= 25) |> 
  rename(Obs = 月降水总量) |> 
  select(气象站代码, Year, Month, Obs)

# 2. Define processing function for a single raster file
folder_1km <- "H:/WorldDeltaClim/pre"
tif_files <- list.files(folder_1km, pattern = "\\.tif$", full.names = TRUE)

all_preds <- map_dfr(tif_files, function(file_path) {
  # Extract year and month from filename (e.g., "1982-01-01.tif")
  b_name <- basename(file_path)
  year_val <- as.integer(substr(b_name, 1, 4))
  month_val <- as.integer(substr(b_name, 6, 7))
  
  # Load 1km raster and aggregate to coarser resolutions
  r_1km <- rast(file_path)
  r_2_5m <- aggregate(r_1km, fact = 5, fun = "mean")
  r_5m   <- aggregate(r_1km, fact = 10, fun = "mean")
  r_10m  <- aggregate(r_1km, fact = 20, fun = "mean")
  
  # Extract values at station locations
  ext_1km  <- extract(r_1km, stations_vect)
  ext_2_5m <- extract(r_2_5m, stations_vect)
  ext_5m   <- extract(r_5m, stations_vect)
  ext_10m  <- extract(r_10m, stations_vect)
  
  # Build tidy data frame for this month
  tibble(
    气象站代码 = unique_stations$气象站代码,
    Year = year_val,
    Month = month_val,
    `0.5'` = ext_1km[, 2],
    `2.5'` = ext_2_5m[, 2],
    `5'`   = ext_5m[, 2],
    `10'`  = ext_10m[, 2]
  )
})

# 3. Merge with observations and compute metrics
val_data <- gsod_obs |> 
  inner_join(all_preds, by = c("气象站代码", "Year", "Month")) |> 
  drop_na(`0.5'`, `10'`)

# Reshape to long format for grouped computation
val_long <- val_data |> 
  pivot_longer(
    cols = c(`0.5'`, `2.5'`, `5'`, `10'`),
    names_to = "Resolution",
    values_to = "Prediction"
  )

# Calculate MAE and correlation by resolution and month
metrics_summary <- val_long |> 
  group_by(Resolution, Month) |> 
  summarize(
    MAE = mean(abs(Prediction - Obs), na.rm = TRUE),
    Cor = cor(Prediction, Obs, use = "complete.obs"),
    .groups = "drop"
  )

# 4. Format as Table 1 (monthly evaluation metrics)
table_1 <- metrics_summary |> 
  pivot_longer(cols = c(MAE, Cor), names_to = "Metric", values_to = "Value") |> 
  mutate(Month = factor(Month, levels = 1:12, labels = month.abb)) |> 
  pivot_wider(names_from = Month, values_from = Value) |> 
  mutate(
    Resolution = factor(Resolution, levels = c("10'", "5'", "2.5'", "0.5'")),
    Metric = factor(Metric, levels = c("MAE", "Cor")),
    Variable = "PRE (mm)"
  ) |> 
  arrange(Resolution, Metric) |> 
  mutate(across(Jan:Dec, ~ round(.x, 3))) |> 
  select(Variable, Resolution, Metric, Jan:Dec)

# Save outputs
write_csv(table_1, "Table_1_Precipitation.csv")
saveRDS(all_preds, "H:/WorldDeltaClim/all_preds_pre_43years.rds")
write_csv(all_preds, "H:/WorldDeltaClim/all_preds_pre_43years.csv")