# 加载必要包
library(readxl)
library(dplyr)
library(writexl)
library(bibliometrix)
library(ggplot2)
library(tidyr)

# 读取数据
file_path <- "高原湿地遥感2024年.xlsx"
wosdata <- read_excel(file_path, sheet = 1)

# 提取第一作者国家信息
wosdata = metaTagExtraction(wosdata, Field = "AU1_CO", sep = ";")

# 选择需要的列
wosdata2 = wosdata %>% 
  select(AU1_CO, PY) %>%
  filter(!is.na(PY))  # 移除PY为NA的记录

# 首先计算年度总发文量（ArtYears），并补齐所有年份
min_year <- min(wosdata2$PY, na.rm = TRUE)
max_year <- max(wosdata2$PY, na.rm = TRUE)
all_years <- seq(min_year, max_year)

ArtYears <- wosdata2 %>%
  group_by(PY) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  complete(PY = all_years, fill = list(total = 0))  # 补齐所有年份，空值为0

# 国家发文分年统计
CO_PY <- wosdata2 %>%
  group_by(CO = AU1_CO, PY) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(!is.na(CO), !is.na(PY))

# 筛选出中国逐年的发文量，并补齐所有年份
CN_CO_PY <- CO_PY %>%   
  filter(CO == "CHINA") %>%
  complete(PY = all_years, fill = list(CO = "CHINA", count = 0))

# 合并数据并计算百分比
CN_PY_Percent <- left_join(CN_CO_PY, ArtYears, by = "PY") %>%
  mutate(percent = ifelse(total > 0, count/total*100, 0)) %>%
  filter(!is.na(PY))

# 确保所有年份都显示
cat("数据时间范围:", min_year, "-", max_year, "\n")

# 中国发文百分比绘图
p6 = ggplot(CN_PY_Percent, aes(PY, percent))+
  geom_point(size = 1.2, color = '#5398D6')+
  geom_line(color = '#5398D6')+
  labs(x = "Years", y = "Percent(%)")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 18, family = "serif")) +
  scale_x_continuous(
    breaks = seq(min_year, max_year, by = 5),
    limits = c(min_year, max_year)
  ) +
  scale_y_continuous(limits = c(0, 80))  # 百分比范围0-100%

p6

ggsave("./中国2.PDF", p6, width = 13, height = 7, units = "cm", dpi = 600)

# 输出一些统计信息
cat("分析完成！\n")
cat("时间范围:", min_year, "-", max_year, "\n")
cat("中国最高发文比例年份:", CN_PY_Percent$PY[which.max(CN_PY_Percent$percent)], 
    "(", round(max(CN_PY_Percent$percent), 1), "%)\n")

# 查看所有年份的数据
cat("\n所有年份数据:\n")
print(CN_PY_Percent)

# 检查是否有NA值
cat("\n数据完整性检查:\n")
cat("CN列NA值数量:", sum(is.na(CN_PY_Percent$count)), "\n")
cat("Total列NA值数量:", sum(is.na(CN_PY_Percent$total)), "\n")
cat("Percent列NA值数量:", sum(is.na(CN_PY_Percent$percent)), "\n")