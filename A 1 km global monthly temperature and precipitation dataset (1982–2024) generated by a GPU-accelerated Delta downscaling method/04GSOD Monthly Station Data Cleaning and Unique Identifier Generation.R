# Load required packages
library(tidyverse)

# Read GSOD monthly summary data
gsod_monthly <- read.csv("./GSOD_monthly/GSOD_Monthly_Summary.csv", header = TRUE)

# Clean and build a high-quality, spatiotemporally unique station-month dataset
gsod_ts <- gsod_monthly %>%
  filter(纬度 != 0, 经度 != 0, 当月有效天数 >= 25) %>%
  mutate(
    year = as.integer(substr(年月, 1, 4)),
    month = as.integer(substr(年月, 6, 7)),
    UID = paste(气象站代码, year, month, sep = "_")
  ) %>%
  filter(year >= 1982, year <= 2024) %>%
  distinct(UID, .keep_all = TRUE)

# Optional: save cleaned dataset
# write.csv(gsod_ts, "./GSOD_monthly/GSOD_Monthly_Cleaned.csv", row.names = FALSE)