# Load required packages
library(terra)
library(tidyverse)

# Read global station coordinates and remove invalid points
stn_coords <- read_csv("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv", 
                       show_col_types = FALSE) %>%
  select(气象站代码, Lon = 经度, Lat = 纬度) %>%
  distinct(气象站代码, .keep_all = TRUE)

# ==============================================================================
# Temperature (TMP) 30' extraction
# ==============================================================================

# Load downscaled predictions (without 30' original data)
all_preds_tmp <- readRDS("H:/WorldDeltaClim/all_preds_tmp_43years.rds")

# Prepare unique station coordinates
stn_unique_tmp <- all_preds_tmp %>%
  distinct(气象站代码) %>%
  left_join(stn_coords, by = "气象站代码") %>%
  filter(Lon >= -180, Lon <= 180, Lat > -89.9, Lat < 89.9)

coords_mat_tmp <- as.matrix(stn_unique_tmp[, c("Lon", "Lat")])

# Read CRU temperature NetCDF and explicitly extract the "tmp" variable
cru_sds_tmp <- sds("G:/Geodata/Weather/Raster/CRUTS4.09/cru_ts4.09.1901.2024.tmp.dat.nc")
cru_raw_tmp <- cru_sds_tmp["tmp"]

# Build layer mapping from time dimension
cru_times_tmp <- terra::time(cru_raw_tmp)
layer_mapping_tmp <- tibble(
  layer_idx = 1:nlyr(cru_raw_tmp),
  Year = as.integer(format(cru_times_tmp, "%Y")),
  Month = as.integer(format(cru_times_tmp, "%m"))
)
target_layers_tmp <- layer_mapping_tmp %>%
  filter(Year >= 1982, Year <= 2024) %>%
  mutate(safe_name = paste0("T_", 1:n()))

# Subset to target years
cru_subset_tmp <- subset(cru_raw_tmp, target_layers_tmp$layer_idx)

# Extract values at station locations
full_matrix_tmp <- terra::values(cru_subset_tmp)
cell_ids_tmp <- cellFromXY(cru_subset_tmp, coords_mat_tmp)
raw_ext_tmp <- full_matrix_tmp[cell_ids_tmp, ]

# Convert to tidy data frame
ext_df_tmp <- as_tibble(raw_ext_tmp, .name_repair = "minimal")
colnames(ext_df_tmp) <- target_layers_tmp$safe_name
ext_df_tmp$气象站代码 <- stn_unique_tmp$气象站代码

df_30m_clean_tmp <- ext_df_tmp %>%
  pivot_longer(cols = -气象站代码, names_to = "safe_name", values_to = "30'") %>%
  left_join(target_layers_tmp, by = "safe_name") %>%
  select(气象站代码, Year, Month, `30'`) %>%
  distinct(气象站代码, Year, Month, .keep_all = TRUE)

# Merge with downscaled predictions and save
all_preds_tmp_new <- all_preds_tmp %>%
  left_join(df_30m_clean_tmp, by = c("气象站代码", "Year", "Month"))

saveRDS(all_preds_tmp_new, "H:/WorldDeltaClim/all_preds_tmp_43years_with30m.rds")

# ==============================================================================
# Precipitation (PRE) 30' extraction
# ==============================================================================

# Load downscaled predictions (without 30' original data)
all_preds_pre <- readRDS("H:/WorldDeltaClim/all_preds_pre_43years.rds")

# Prepare unique station coordinates
stn_unique_pre <- all_preds_pre %>%
  distinct(气象站代码) %>%
  left_join(stn_coords, by = "气象站代码") %>%
  filter(Lon >= -180, Lon <= 180, Lat > -89.9, Lat < 89.9)

coords_mat_pre <- as.matrix(stn_unique_pre[, c("Lon", "Lat")])

# Read CRU precipitation NetCDF and explicitly extract the "pre" variable
cru_sds_pre <- sds("G:/Geodata/Weather/Raster/CRUTS4.09/cru_ts4.09.1901.2024.pre.dat.nc")
cru_raw_pre <- cru_sds_pre["pre"]

# Build layer mapping from time dimension
cru_times_pre <- terra::time(cru_raw_pre)
layer_mapping_pre <- tibble(
  layer_idx = 1:nlyr(cru_raw_pre),
  Year = as.integer(format(cru_times_pre, "%Y")),
  Month = as.integer(format(cru_times_pre, "%m"))
)
target_layers_pre <- layer_mapping_pre %>%
  filter(Year >= 1982, Year <= 2024) %>%
  mutate(safe_name = paste0("P_", 1:n()))

# Subset to target years
cru_subset_pre <- subset(cru_raw_pre, target_layers_pre$layer_idx)

# Extract values at station locations
full_matrix_pre <- terra::values(cru_subset_pre)
cell_ids_pre <- cellFromXY(cru_subset_pre, coords_mat_pre)
raw_ext_pre <- full_matrix_pre[cell_ids_pre, ]

# Convert to tidy data frame
ext_df_pre <- as_tibble(raw_ext_pre, .name_repair = "minimal")
colnames(ext_df_pre) <- target_layers_pre$safe_name
ext_df_pre$气象站代码 <- stn_unique_pre$气象站代码

df_30m_clean_pre <- ext_df_pre %>%
  pivot_longer(cols = -气象站代码, names_to = "safe_name", values_to = "30'") %>%
  left_join(target_layers_pre, by = "safe_name") %>%
  select(气象站代码, Year, Month, `30'`) %>%
  distinct(气象站代码, Year, Month, .keep_all = TRUE)

# Merge with downscaled predictions and save
all_preds_pre_new <- all_preds_pre %>%
  left_join(df_30m_clean_pre, by = c("气象站代码", "Year", "Month"))

saveRDS(all_preds_pre_new, "H:/WorldDeltaClim/all_preds_pre_43years_with30m.rds")