# 加载必要的包
library(readxl)
library(ggplot2)
library(dplyr)
library(scales) # 用于坐标轴标签格式化

file_path <- "Countries_Production_Over_Time.xlsx"
data <- read_excel(file_path)

# 清理列名（移除可能存在的空格或特殊字符）
colnames(data) <- make.names(colnames(data))  # 处理特殊字符
colnames(data) <- c("Country", "Year", "Articles")  # 确保列名正确

# 筛选主要国家（使用Excel中实际存在的国家）
selected_countries <- c("USA", "CHINA", "FRANCE", "GERMANY", "UNITED KINGDOM")
filtered_data <- data %>%
  filter(Country %in% selected_countries) %>%
  mutate(Country = factor(Country, levels = selected_countries)) # 固定国家顺序

# 自定义颜色
country_colors <- c(
  "USA" = "#1F77B4",
  "CHINA" = "#FF7F0E", 
  "FRANCE" = "#2CA02C",
  "GERMANY" = "#D62728",
  "UNITED KINGDOM" = "#9467BD"
)

# 绘制折线图
p <- ggplot(filtered_data, aes(x = Year, y = Articles, color = Country, group = Country)) +
  geom_line(linewidth = 0.5) +  # 减小线宽以适应小图尺寸
  geom_point(size = 1) +        # 减小点大小
  scale_color_manual(values = country_colors) +
  scale_y_continuous(
    limits = c(0, 4200),
    breaks = seq(0, 4200, by = 400),
    labels = comma # 千位分隔符
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2024, by = 5),
    minor_breaks = seq(1980, 2024, by = 1)
  ) +
  labs(
    x = "Year",
    y = "Number of Authors",
    color = "Country"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 8, family = "serif"),
    axis.text.y = element_text(size = 8, family = "serif"),
    axis.title.x = element_text(size = 10, family = "serif"),
    axis.title.y = element_text(size = 10, family = "serif"),
    text = element_text(size = 16, family = "serif"),  # 全局字体设置
    plot.title = element_text(hjust = 0.5, face = "bold", family = "serif"),  # 标题加粗
    legend.position = c(0.05, 0.95),  # 图例放在左上角
    legend.justification = c(0, 1),   # 左上角对齐
    legend.title = element_text(size = 10, family = "serif"),  # 图例标题
    legend.text = element_text(size = 8, family = "serif"),    # 图例文字大小
    legend.background = element_blank(),  # 图例背景透明
    legend.key = element_blank(),         # 图例键背景透明
    panel.grid.major.y = element_line(color = "grey90")
  ) +
  guides(color = guide_legend(nrow = 5))  # 垂直排列图例

# 显示图形
print(p)

# 保存高清图片（调整尺寸与参考图比例相似）
ggsave("country_production.PDF", p,
       width = 13, height = 8, units = "cm", dpi = 600)