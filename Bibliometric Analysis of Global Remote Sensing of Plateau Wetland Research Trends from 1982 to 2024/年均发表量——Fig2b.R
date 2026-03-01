# 加载必要的包
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)  # 用于输出Excel文件

# 读取 Excel 文件
df_raw <- read_excel("高原湿地遥感2024年.xlsx", sheet = 1)

# 只提取 PY 列，并统计每个年份出现的次数
df_py_count <- df_raw %>%
  select(PY) %>%
  group_by(PY) %>%
  summarise(Count = n()) %>%
  arrange(PY)  # 按年份排序

# 输出到Excel文件
write.xlsx(df_py_count, "年份统计结果.xlsx", row.names = FALSE)

# 绘制折线统计图（移除虚线）
p2 <- ggplot(df_py_count, aes(x = PY, y = Count)) +
  geom_point(size = 1.2, color = '#5398D6') +
  geom_line(color = '#5398D6') +
  labs(x = "Years", y = "Publication Count") +   # 设置坐标轴标注
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    text = element_text(size = 16, family = "serif")
  )

# 保存图形
ggsave("./p2.PDF", p2, width = 13, height = 6, units = "cm", dpi = 600)

# 显示图形
print(p2)

# 显示统计结果
print(df_py_count)