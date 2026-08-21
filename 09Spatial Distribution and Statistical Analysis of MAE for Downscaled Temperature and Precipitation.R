# Load required packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(patchwork)

# Read raw GSOD monthly summary and prepare station coordinates
gsod_raw <- read_csv("D:/R/DeltaClim/GSOD_monthly/GSOD_Monthly_Summary.csv",
                     show_col_types = FALSE)

station_coords <- gsod_raw %>%
  distinct(气象站代码, 经度, 纬度) %>%
  drop_na()

gsod_obs_base <- gsod_raw %>%
  mutate(Year = as.integer(substr(年月, 1, 4)),
         Month = as.integer(substr(年月, 6, 7))) %>%
  filter(Year >= 1982, Year <= 2024, 当月有效天数 >= 25)

# Load world country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# -----------------------------------------------------------------------------
# Panel (a): Mean temperature MAE
# -----------------------------------------------------------------------------
obs_tmp <- gsod_obs_base %>%
  rename(Obs = 月平均气温) %>%
  select(气象站代码, Year, Month, Obs)

preds_tmp <- readRDS("H:/WorldDeltaClim/all_preds_tmp_43years.rds")

mae_tmp <- obs_tmp %>%
  inner_join(preds_tmp, by = c("气象站代码", "Year", "Month")) %>%
  drop_na(`0.5'`) %>%
  group_by(气象站代码) %>%
  summarize(MAE = mean(abs(`0.5'` - Obs), na.rm = TRUE), .groups = "drop") %>%
  inner_join(station_coords, by = "气象站代码") %>%
  mutate(MAE_plot = ifelse(MAE > 3, 3, MAE))

sf_tmp <- st_as_sf(mae_tmp, coords = c("经度", "纬度"), crs = 4326)

p3a <- ggplot() +
  geom_sf(data = world, fill = "gray92", color = "white", size = 0.15) +
  geom_sf(data = sf_tmp, aes(color = MAE_plot), size = 0.6, alpha = 0.7) +
  scale_color_viridis_c(option = "inferno", direction = -1, name = "MAE (°C)",
                        limits = c(0, 3), breaks = c(0, 1, 2, 3),
                        labels = c("0.0", "1.0", "2.0", "> 3.0")) +
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 11, hjust = 0),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "(a) Spatial distribution of Mean Temperature MAE (0.5' Resolution)")

# -----------------------------------------------------------------------------
# Panel (b): Precipitation MAE
# -----------------------------------------------------------------------------
obs_pre <- gsod_obs_base %>%
  rename(Obs = 月降水总量) %>%
  select(气象站代码, Year, Month, Obs)

preds_pre <- readRDS("H:/WorldDeltaClim/all_preds_pre_43years.rds")

mae_pre <- obs_pre %>%
  inner_join(preds_pre, by = c("气象站代码", "Year", "Month")) %>%
  drop_na(`0.5'`) %>%
  group_by(气象站代码) %>%
  summarize(MAE = mean(abs(`0.5'` - Obs), na.rm = TRUE), .groups = "drop") %>%
  inner_join(station_coords, by = "气象站代码") %>%
  mutate(MAE_plot = ifelse(MAE > 150, 150, MAE))

sf_pre <- st_as_sf(mae_pre, coords = c("经度", "纬度"), crs = 4326)

p3b <- ggplot() +
  geom_sf(data = world, fill = "gray92", color = "white", size = 0.15) +
  geom_sf(data = sf_pre, aes(color = MAE_plot), size = 0.6, alpha = 0.7) +
  scale_color_viridis_c(option = "viridis", direction = -1, name = "MAE (mm)",
                        limits = c(0, 150), breaks = c(0, 50, 100, 150),
                        labels = c("0", "50", "100", "> 150")) +
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 11, hjust = 0),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "(b) Spatial distribution of Precipitation MAE (0.5' Resolution)")

# Combine panels and save as PDF
fig3_pdf <- p3a / p3b + plot_layout(heights = c(1, 1))
ggsave("Figure_3_Spatial_MAE.pdf", plot = fig3_pdf, width = 10, height = 9,
       device = cairo_pdf)

# -----------------------------------------------------------------------------
# Supplementary statistical analyses
# -----------------------------------------------------------------------------

# Global MAE summary statistics
summary_global_tmp <- summary(mae_tmp$MAE)
sd_global_tmp <- sd(mae_tmp$MAE, na.rm = TRUE)
summary_global_pre <- summary(mae_pre$MAE)
sd_global_pre <- sd(mae_pre$MAE, na.rm = TRUE)

cat("\nTemperature MAE global statistics:\n")
print(summary_global_tmp)
cat("SD:", sd_global_tmp, "\n")

cat("\nPrecipitation MAE global statistics:\n")
print(summary_global_pre)
cat("SD:", sd_global_pre, "\n")

# Continent-level statistics
world_cont <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(continent, region_un, iso_a3)

coords_sf <- st_as_sf(mae_tmp, coords = c("经度", "纬度"), crs = 4326)
coords_with_region <- st_join(coords_sf, world_cont) %>%
  st_drop_geometry() %>%
  select(气象站代码, MAE, continent, region_un) %>%
  distinct()

stats_cont_tmp <- mae_tmp %>%
  inner_join(coords_with_region %>% select(气象站代码, continent),
             by = "气象站代码", relationship = "many-to-many") %>%
  group_by(continent) %>%
  summarise(Mean_MAE = mean(MAE), SD_MAE = sd(MAE), n_stations = n()) %>%
  arrange(desc(Mean_MAE))

stats_cont_pre <- mae_pre %>%
  inner_join(coords_with_region %>% select(气象站代码, continent),
             by = "气象站代码", relationship = "many-to-many") %>%
  group_by(continent) %>%
  summarise(Mean_MAE = mean(MAE), SD_MAE = sd(MAE), n_stations = n()) %>%
  arrange(desc(Mean_MAE))

cat("\nContinent-level temperature MAE:\n")
print(stats_cont_tmp)

cat("\nContinent-level precipitation MAE:\n")
print(stats_cont_pre)

# Climate zone statistics
mae_tmp_climate <- mae_tmp %>%
  mutate(Climate_Zone = case_when(
    abs(纬度) <= 23.5 ~ "Tropical",
    abs(纬度) <= 66.5 ~ "Temperate",
    TRUE ~ "Polar"
  ))

stats_climate_tmp <- mae_tmp_climate %>%
  group_by(Climate_Zone) %>%
  summarise(Mean_MAE = mean(MAE), SD_MAE = sd(MAE), n_stations = n())

cat("\nClimate zone temperature MAE:\n")
print(stats_climate_tmp)

mae_pre_climate <- mae_pre %>%
  mutate(Climate_Zone = case_when(
    abs(纬度) <= 23.5 ~ "Tropical",
    abs(纬度) <= 66.5 ~ "Temperate",
    TRUE ~ "Polar"
  )) %>%
  group_by(Climate_Zone) %>%
  summarise(Mean_MAE = mean(MAE), SD_MAE = sd(MAE), n_stations = n())

cat("\nClimate zone precipitation MAE:\n")
print(mae_pre_climate)

# Additional error metrics (RMSE, Bias, Correlation)
metrics_tmp <- obs_tmp %>%
  inner_join(preds_tmp, by = c("气象站代码", "Year", "Month")) %>%
  drop_na(`0.5'`) %>%
  group_by(气象站代码) %>%
  summarise(
    MAE = mean(abs(`0.5'` - Obs), na.rm = TRUE),
    RMSE = sqrt(mean((`0.5'` - Obs)^2, na.rm = TRUE)),
    Bias = mean(`0.5'` - Obs, na.rm = TRUE),
    Cor = ifelse(sd(`0.5'`, na.rm = TRUE) == 0 | sd(Obs, na.rm = TRUE) == 0,
                 NA,
                 cor(`0.5'`, Obs, use = "complete.obs"))
  ) %>%
  inner_join(station_coords, by = "气象站代码")

cat("\nGlobal average additional metrics (temperature):\n")
cat("RMSE mean:", mean(metrics_tmp$RMSE, na.rm = TRUE), "\n")
cat("Bias mean:", mean(metrics_tmp$Bias, na.rm = TRUE), "\n")
cat("Cor mean:", mean(metrics_tmp$Cor, na.rm = TRUE), "\n")

# Station density by continent
station_density <- station_coords %>%
  st_as_sf(coords = c("经度", "纬度"), crs = 4326) %>%
  st_join(world_cont) %>%
  st_drop_geometry() %>%
  distinct(气象站代码, continent) %>%
  group_by(continent) %>%
  summarise(Total_Stations = n())

cat("\nStation density by continent:\n")
print(station_density)

# Focus region: Tibetan Plateau (25N-40N, 75E-105E)
region_mae <- mae_tmp %>%
  filter(纬度 >= 25 & 纬度 <= 40 & 经度 >= 75 & 经度 <= 105)

cat("\nFocus region (Tibetan Plateau) statistics:\n")
cat("Number of stations:", nrow(region_mae), "\n")
cat("Mean MAE:", mean(region_mae$MAE), "\n")
cat("Max MAE:", max(region_mae$MAE), "\n")