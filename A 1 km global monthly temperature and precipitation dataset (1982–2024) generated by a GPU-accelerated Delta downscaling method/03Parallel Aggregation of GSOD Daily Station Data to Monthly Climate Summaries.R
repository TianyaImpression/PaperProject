# Load required packages
library(tidyverse)
library(furrr)       # Parallel map functions
library(data.table)  # Fast file reader (fread)

# 1. List GSOD yearly CSV files (subset of years)
gsodlist <- list.files(
  path = "G:/Geodata/Weather/Station/1929～2024 年 GSOD 气象站点数据/分年数据",
  pattern = ".csv$",
  full.names = TRUE
)[54:96]

# 2. Configure parallel backend
# Use all cores except 10 to keep system responsive
n_cores <- parallel::detectCores() - 10
plan(multisession, workers = n_cores)
message(sprintf("Parallel processing started using %d cores...", n_cores))

# 3. Read and merge all yearly files in parallel
gsod_all_days <- future_map_dfr(gsodlist, function(file) {
  df <- fread(
    file,
    select = c("气象站代码", "日期", "纬度", "经度", "平均气温", "降水量"),
    colClasses = list(character = c("气象站代码", "日期")),
    encoding = "UTF-8"
  )
  return(df)
}, .progress = TRUE)

message("File reading and merging completed. Generating monthly summaries...")

# 4. Aggregate to monthly values by station and month
gsod_monthly <- gsod_all_days |>
  mutate(
    日期 = as.Date(日期),
    年月 = format(日期, "%Y-%m")
  ) |>
  group_by(气象站代码, 纬度, 经度, 年月) |>
  summarise(
    月平均气温 = mean(平均气温, na.rm = TRUE),
    月降水总量 = sum(降水量, na.rm = TRUE),
    当月有效天数 = n(),
    .groups = "drop"
  )

# 5. Stop parallel backend and return to sequential mode
plan(sequential)

# 6. Save monthly summary to CSV
write.csv(gsod_monthly, "./GSOD_monthly/GSOD_Monthly_Summary.csv", row.names = FALSE)