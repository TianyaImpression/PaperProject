# 加载必要的包
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

# 读取Excel数据
data <- read_excel("Countries_Production_Over_Time.xlsx", sheet = "Sheet1")

# 清理列名
colnames(data) <- c("Country", "Year", "Articles")

# 检查数据结构
str(data)

# 筛选主要国家（根据数据中实际存在的国家）
selected_countries <- c("USA", "China", "France", "Germany", "United kingdom")
filtered_data <- data %>%
  filter(Country %in% selected_countries) %>%
  mutate(Country = factor(Country, levels = selected_countries))

# 自定义颜色
country_colors <- c(
  "USA" = "#1F77B4",           # 蓝色
  "CHINA" = "#FF7F0E",         # 橙色
  "FRANCE" = "#2CA02C",        # 绿色
  "GERMANY" = "#D62728",       # 红色
  "UNITED KINGDOM" = "#9467BD" # 紫色
)

# 绘制折线图
p <- ggplot(filtered_data, aes(x = Year, y = Articles, color = Country, group = Country)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(
    limits = c(0, 4500),
    breaks = seq(0, 4500, by = 500),
    labels = comma,
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(1980, 2025),
    breaks = seq(1980, 2025, by = 5),
    minor_breaks = seq(1980, 2025, by = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "Country Production over Time",
    x = "Year",
    y = "Number of Articles",
    color = "Country"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10, family = "serif", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, family = "serif"),
    axis.title.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(size = 12, family = "serif"),
    text = element_text(size = 18, family = "serif"),
    plot.title = element_text(hjust = 0.5, face = "bold", family = "serif", size = 12),
    
    # 图例左上角 + 背景透明
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    
    legend.title = element_text(size = 12, family = "serif"),
    legend.text = element_text(size = 10, family = "serif"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.1)
  )

# 显示图形
print(p)

# 保存高清图片
ggsave("country_production8.JPG", p, width = 13, height = 8, units = "cm", dpi = 600)

# 查看数据摘要
cat("数据摘要:\n")
summary(filtered_data)

# 查看每个国家的年份范围
cat("\n每个国家的年份范围:\n")
filtered_data %>%
  group_by(Country) %>%
  summarise(
    Min_Year = min(Year),
    Max_Year = max(Year),
    Total_Articles = sum(Articles)
  )