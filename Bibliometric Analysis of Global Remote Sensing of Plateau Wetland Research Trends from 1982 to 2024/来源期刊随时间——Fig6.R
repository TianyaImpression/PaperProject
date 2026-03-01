library(tidyverse)
library(ggsci)
library(readxl)

# 读取Excel数据
data <- read_excel("./来源随时间变化.xlsx", sheet = "Sheet1")

# 查看数据结构
head(data)

# 重命名列名（去掉中文部分）
colnames(data) <- c("Year", "REMOTE_SENSING", "JOURNAL_OF_HYDROLOGY", 
                    "REMOTE_SENSING_OF_ENVIRONMENT", "WATER", 
                    "SCIENCE_OF_THE_TOTAL_ENVIRONMENT")

# 数据转换
SourceDynamics <- data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Sources",
    values_to = "Number"
  )

# 创建期刊名称的友好显示名称
SourceDynamics <- SourceDynamics %>%
  mutate(Sources = recode(Sources,
                          "REMOTE_SENSING" = "REMOTE SENSING",
                          "JOURNAL_OF_HYDROLOGY" = "JOURNAL OF HYDROLOGY",
                          "REMOTE_SENSING_OF_ENVIRONMENT" = "REMOTE SENSING OF ENVIRONMENT",
                          "WATER" = "WATER",
                          "SCIENCE_OF_THE_TOTAL_ENVIRONMENT" = "SCIENCE OF THE TOTAL ENVIRONMENT"
  ))

# 创建图表
p5 <- ggplot(SourceDynamics, aes(Year, Number, color = Sources)) +
  labs(x = "Year", y = "Annual Number of Articles") +
  theme_bw() +
  theme(
    title = element_text(size = 10),
    axis.title = element_text(size = 10),    # 调整标题大小
    axis.text.x = element_text(size = 8),    # x轴标签大小
    axis.text.y = element_text(size = 8),    # y轴标签大小 
    legend.title = element_blank(),          # 去除图例标题
    legend.text = element_text(size = 5),    # 图例文字字号
    legend.key.size = unit(3, "mm"),         # 图例大小
    legend.spacing = unit(2, "mm"),          # 图例间距
    legend.position = c(0.15, 0.75),         # 图例位置（内部左上）
    legend.background = element_rect(fill = NA),
    text = element_text(size = 10, family = "serif")
  ) +
  guides(color = guide_legend(ncol = 1, bycol = TRUE)) +   # 1列图例
  geom_smooth(size = 0.5, se = TRUE, method = "loess", span = 0.3) +  # 平滑曲线
  scale_color_npg() +
  scale_fill_npg() +
  
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200), limits = c(0, 200))

# 显示图表
print(p5)

# 保存图表
ggsave("SourceDynamics.pdf", p5, width = 10.29, height = 7, units = "cm", dpi = 600)


