# 加载必要的包
library(readxl)
library(dplyr)
library(ggplot2)

# 读取 Excel 文件
df_raw <- read_excel("高原湿地遥感2024年.xlsx", sheet = 1)

# 只提取 PY 列，并统计每个年份出现的次数，然后计算累计数量
df_py_count <- df_raw %>%
  select(PY) %>%
  group_by(PY) %>%
  summarise(Count = n()) %>%
  arrange(PY) %>%  # 按年份排序
  mutate(Cumulative_Count = cumsum(Count))  # 计算累计数量

# 绘制折线统计图（显示累计数量）
p2 <- ggplot(df_py_count, aes(x = PY, y = Cumulative_Count)) +
  geom_point(size = 1.2, color = '#5398D6') +
  geom_line(color = '#5398D6') +
  labs(x = "Years", y = "Number of Articles") +   # 设置坐标轴标注
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 18, family = "serif")
  )

# 保存图形
ggsave("./p41.PDF", p2, width = 13, height = 6, units = "cm", dpi = 600)

# 显示图形
print(p2)

# 可选：查看数据框，确认累计数量计算正确
print(df_py_count)