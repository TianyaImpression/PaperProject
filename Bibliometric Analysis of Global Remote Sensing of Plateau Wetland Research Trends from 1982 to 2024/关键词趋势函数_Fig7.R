library(tidyverse)
library(ggsci)
library(readxl)

# 读取 Excel 文件
TrendTopic_DE <- read_excel("关键词数据.xlsx", sheet = 1, col_names = TRUE) %>%
  mutate(size = log(Frequency)) %>%   # 添加新列 size，其值等于 Frequency 列的对数值
  arrange(desc(`Year (Median)`)) %>%  # 按 `Year (Median)` 列降序排列
  mutate(Term = forcats::fct_reorder(Term, `Year (Median)`)) %>%  # 根据 `Year (Median)` 重新排序 Term 列
  mutate(q3q1 = `Year (Q3)` - `Year (Q1)`)  # 添加新列 q3q1，其值等于 `Year (Q3)` 列减去 `Year (Q1)` 列

# 绘制哑铃图
p6 <- ggplot(TrendTopic_DE) +
  geom_segment(aes(x = `Year (Q1)`, xend = `Year (Q3)`, y = Term, yend = Term), 
               color = "grey", linewidth = 1.2) +
  geom_point(aes(x = `Year (Q1)`, y = Term), color = "#70C1B3", size = 3) +  # 起点
  geom_point(aes(x = `Year (Q3)`, y = Term), color = "#FF1654", size = 3) +  # 终点
  geom_point(aes(x = `Year (Median)`, y = Term, size = size), color = "#247BA0") +  # 中位数点，大小映射到size
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  xlab("Year") +
  ylab("Author's Keywords") +
  scale_size_identity()  # 使用实际大小值而不是图例

# 显示图形
print(p6)

# 保存图形 - 修正了文件路径和引号问题
ggsave("./关键词趋势11111.JPG", p6, width = 15, height = 18, units = "cm", dpi = 600)