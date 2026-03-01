library(readxl)
library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggpmisc)
library(ggsci)

# 读取数据
file_path <- "高原湿地遥感2024年.xlsx"
wosdata <- read_excel(file_path, sheet = 1)

# 创建新的数据框 PYSC
PYSC <- wosdata %>% 
  select(PY, SC)

# 分拆多行
tidySC <- PYSC %>% 
  separate(SC, into = c("SC1", "SC2", "SC3", "SC4", "SC5"), sep = ";") %>%   # 分割带；的SC
  pivot_longer(-PY, values_to = "SC") %>%                                    # 宽转长数据
  na.omit() %>%                                                              # 删除空值
  mutate(SC = str_trim(SC, side = "both"))                                   # 去除SC字段多余空格

# 不同研究领域发文数量统计
tidySC <- tidySC %>% 
  group_by(SC, PY) %>% 
  summarise(NUM = n()) %>%                                                   # 获得逐年不同研究领域的发文数量
  ungroup()

# 获取TOP10研究领域
TOPSC <- tidySC %>% 
  group_by(SC) %>% 
  summarise(total = sum(NUM)) %>%                                            # 统计研究领域发文总量
  arrange(desc(total))                                                       # 按发文数量降序排列

# 获取TOP10研究领域
TOP10_SC <- TOPSC %>% 
  slice_head(n = 10)                                                         # 取前10个

# 获取TOP10研究领域占总发文量的百分比
TISC <- wosdata[, c("TI", "SC")]

# 创建过滤条件
filter_condition <- map_lgl(1:nrow(TISC), function(i) {
  any(map_lgl(TOP10_SC$SC, ~ str_detect(TISC$SC[i], .x)))
})

TISC_TOP10 <- TISC[filter_condition, ]

print(paste0("TOP10研究领域包含的论文数", nrow(TISC_TOP10), "占", "总论文数", nrow(TISC), "的", round(nrow(TISC_TOP10)/nrow(TISC)*100, 2), "%"))

# 提取TOP10研究领域文章时间序列
TOP10_SC_PY <- tidySC %>% 
  filter(SC %in% TOP10_SC$SC) %>%
  mutate(PY = as.Date(paste0(PY, "-01-01"), format = "%Y-%m-%d"))            # 生成时间序列

# ggplot2绘图 - 设置所有字体为衬线字体（Times New Roman）
p1 <- ggplot(TOP10_SC_PY, aes(PY, NUM, color = SC)) +
  labs(x = "Year", y = "Number of Articles") +
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 12),  # 设置所有文字基础字体和大小
    plot.title = element_text(family = "serif", size = 12),
    axis.title = element_text(family = "serif", size = 12),
    axis.title.x = element_text(family = "serif", size = 12),
    axis.title.y = element_text(family = "serif", size = 12),
    axis.text.x = element_text(family = "serif", size = 10),
    axis.text.y = element_text(family = "serif", size = 10),
    legend.title = element_blank(),  # 移除图例标题"SC"
    legend.text = element_text(family = "serif", size = 8),
    legend.key.size = unit(4, "mm"),
    legend.spacing = unit(2, "mm"),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.box.background = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1, bycol = TRUE)) +
  geom_line() +
  geom_point(size = 1.2) +
  scale_color_npg() +
  scale_fill_npg()

p1   # 发文数量前10的研究领域时间序列

ggsave("./TOP10SC.PDF", p1, width = 13, height = 8, units = "cm", dpi = 600)