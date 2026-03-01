# 加载必要包
library(readxl)
library(dplyr)
library(writexl)
library(bibliometrix)
library(ggplot2)

# 读取数据
file_path <- "高原湿地遥感2024年.xlsx"
wosdata <- read_excel(file_path, sheet = 1)

# 提取第一作者国家信息
wosdata = metaTagExtraction(wosdata, Field = "AU1_CO", sep = ";")

# 选择需要的列
wosdata2 = wosdata %>% 
  select(AU1_CO, PY)

# 提取国家发文分年统计
CO = wosdata2$AU1_CO
PY = wosdata2$PY

# 国家发文分年统计
CO_PY = as.data.frame(cbind(CO, PY)) %>%
  group_by(CO, PY) %>%
  summarise(annual_count = n())

# 国家发文统计
CO_PY2 = as.data.frame(cbind(CO, PY)) %>%
  group_by(CO) %>%
  summarise(total_count = n())

# 获取TOP5发文国家
TOP_CO = CO_PY2[order(CO_PY2$total_count, decreasing = T),]  # 国家按发文数量排序
TOP5_CO = TOP_CO[1:5, 1]

# 筛选TOP5国家的数据
TOP5_CO_PY = inner_join(TOP5_CO, CO_PY, by = "CO")

# 按国家和年份排序，然后计算累计发文量
TOP5_CO_PY_cumulative <- TOP5_CO_PY %>%
  arrange(CO, PY) %>%
  group_by(CO) %>%
  mutate(cumulative_count = cumsum(annual_count))

# 生成时间序列
TOP5_CO_PY_cumulative$PY_date = paste0(TOP5_CO_PY_cumulative$PY, "-01-01") %>%
  as.Date(format = "%Y-%m-%d")

# ggplot2绘图 - 使用累计发文量
p5 <- ggplot(TOP5_CO_PY_cumulative, aes(PY_date, cumulative_count, color = CO)) +
  labs(x = "Year", y = "Cumulative Number of Articles") +
  guides(color = guide_legend(title = "文献作者国籍")) +
  theme_bw() +
  theme(
    title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.75),
    legend.background = element_blank(),  # 图例背景完全透明
    legend.key = element_blank(),        # 图例标识块背景透明
    text = element_text(size = 12, family = "serif")
  ) +
  geom_line() +
  geom_point(size = 1.2)

p5

# 输出图片
ggsave("./国家累计2.PDF", p5, width = 13, height = 7, units = "cm", dpi = 600)

# 可选：查看累计数据
print(TOP5_CO_PY_cumulative)