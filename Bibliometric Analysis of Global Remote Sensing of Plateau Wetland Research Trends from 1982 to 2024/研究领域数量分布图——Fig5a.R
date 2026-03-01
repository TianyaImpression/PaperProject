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

# 方法1：创建新的数据框 PYSC（推荐）
PYSC <- wosdata %>% 
  select(PY, SC)

# 方法2：基础R方法
PYSC <- wosdata[, c("PY", "SC")]

# 分拆多行
tidySC = PYSC %>% 
  separate(SC, into = c("SC1", "SC2", "SC3", "SC4", "SC5") ,sep = ";") %>%   #分割带；的SC
  pivot_longer(-PY, values_to = "SC") %>%                                           #宽转长数据
  na.omit() #删除空值
#去除SC字段多余空格
tidySC$SC =  str_trim(string = tidySC$SC, side = "both") 
#不同研究领域发文数量统计
tidySC = tidySC %>% 
  group_by(SC, PY) %>% 
  summarise(n()) #获得逐年不同研究领域的发文数量

colnames(tidySC) = c("SC", "PY", "NUM")

#计算历年研究领域数量
YearSC = tidySC %>% 
  group_by(PY) %>% 
  summarise(NUM=n())

#研究领域数量分布图
p9 <- ggplot(YearSC, aes(PY, NUM))+
  labs(x="Year", y="Number of Research Areas")+
  theme_bw()+
  theme(
    text = element_text(family = "serif"),  # 设置所有文字为衬线字体（Times New Roman）
    title = element_text(size = 12),
    axis.title = element_text(size = 12),    #调整标题大小
    axis.text.x = element_text(size = 10),    #x轴标签大小
    axis.text.y = element_text(size = 10)     #y轴标签大小 
  ) +
  geom_line()+
  geom_point(size = 1.2)+
  geom_smooth(method = "lm", formula = y~x, size=0.3, linetype =2)+
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(rr.label), stat(p.value.label),sep = "*\", \"*")),
               formula = y~x, parse = TRUE, 
               size=4,
               family = "serif")+  
  scale_color_npg()+
  scale_fill_npg()
p9 
ggsave("./YearSC.JPG", p9, width = 13, height = 8, units = "cm", dpi = 600)