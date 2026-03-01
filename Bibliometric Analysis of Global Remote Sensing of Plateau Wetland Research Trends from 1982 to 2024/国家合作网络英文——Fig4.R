# 加载必要包
library(readxl)
library(dplyr)
library(bibliometrix)
library(igraph)
library(ggplot2)
library(maps)
library(ggrepel)
library(viridis)
library(purrr)

# 读取数据
file_path <- "高原湿地遥感2024年.xlsx"
M <- read_excel(file_path, sheet = 1)
M <- as.data.frame(M)

# 提取国家字段
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# 构建国家合作网络矩阵
CO_CollabrationMap <- biblioNetwork(
  M, 
  analysis = "collaboration", 
  network = "countries",
  sep = ";"
)

# 将矩阵转换为igraph对象
net_graph <- graph_from_adjacency_matrix(CO_CollabrationMap, 
                                         mode = "undirected", 
                                         weighted = TRUE, 
                                         diag = FALSE)

# 创建节点数据框
nodes <- data.frame(
  id = V(net_graph)$name,
  name = V(net_graph)$name,
  stringsAsFactors = FALSE
)

# 计算节点权重（度中心性）
nodes$weight <- degree(net_graph)

# 设置主要国家坐标（扩展版，包含更多国家）
country_coords <- data.frame(
  country = tolower(c("China", "USA", "Canada", "Mexico", "Brazil", 
                      "Netherlands", "France", "Spain", "Germany", 
                      "Switzerland", "Italy", "Russia", "Japan", 
                      "India", "Singapore", "Australia", "UK", "South Korea",
                      "Pakistan", "Bangladesh", "Nepal", "Bhutan", "Myanmar",
                      "Thailand", "Vietnam", "Malaysia", "Indonesia")),
  lon = c(104.0, -95.0, -106.0, -102.0, -53.0, 
          5.3, 2.2, -3.7, 10.5, 
          8.2, 12.5, 105.0, 138.0, 
          78.0, 103.8, 133.0, -2.0, 127.5,
          73.0, 90.0, 84.0, 90.4, 96.0,
          100.5, 105.8, 112.5, 113.0),
  lat = c(35.0, 37.0, 56.0, 23.0, -14.0, 
          52.0, 46.0, 40.0, 51.0, 
          47.0, 42.0, 61.0, 36.0, 
          20.0, 1.3, -25.0, 54.0, 36.0,
          30.0, 24.0, 28.0, 27.5, 22.0,
          15.0, 16.0, 4.2, -5.0)
)

# 匹配节点坐标
nodes <- nodes %>%
  mutate(country_lower = tolower(id)) %>%
  left_join(country_coords, by = c("country_lower" = "country")) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  select(id, name, weight, lon, lat)

# 创建边数据框
edges <- as.data.frame(as_edgelist(net_graph)) %>%
  rename(from = V1, to = V2) %>%
  mutate(weight = E(net_graph)$weight)

# 网络边和地图经纬度整合 - 过滤掉坐标相同的边
edges_for_plot <- edges %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodes %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat) %>%
  # 过滤掉起点和终点相同的边（避免错误）
  filter(!(x == xend & y == yend))

# 获取世界地图数据并添加区域分类
world_map <- map_data('world') %>%
  mutate(region_lower = tolower(region))

# 根据大洲对国家进行分类
classify_continent <- function(country) {
  case_when(
    country %in% c('china', 'japan', 'south korea', 'india', 'pakistan', 
                   'bangladesh', 'nepal', 'bhutan', 'myanmar', 'thailand',
                   'vietnam', 'malaysia', 'indonesia', 'singapore', 'taiwan',
                   'north korea', 'mongolia', 'philippines', 'sri lanka') ~ 'Asia',
    country %in% c('usa', 'canada', 'mexico') ~ 'North America',
    country %in% c('brazil', 'argentina', 'chile', 'peru', 'colombia',
                   'venezuela', 'ecuador', 'bolivia') ~ 'South America',
    country %in% c('france', 'germany', 'uk', 'spain', 'italy', 
                   'netherlands', 'switzerland', 'russia', 'poland',
                   'sweden', 'norway', 'finland', 'denmark', 'belgium',
                   'austria', 'portugal', 'greece', 'ireland', 'ukraine') ~ 'Europe',
    country %in% c('australia', 'new zealand') ~ 'Oceania',
    country %in% c('south africa', 'egypt', 'nigeria', 'kenya', 'ethiopia',
                   'morocco', 'tanzania') ~ 'Africa',
    TRUE ~ 'Other Regions'
  )
}

# 为地图数据添加大洲分类
world_map <- world_map %>%
  mutate(continent = classify_continent(region_lower))

# 为网络中的国家节点添加颜色分类
nodes <- nodes %>%
  mutate(continent = classify_continent(tolower(id)))

# 创建颜色调色板
continent_colors <- c(
  'Asia' = '#FF6B6B',      # 红色
  'North America' = '#4ECDC4',    # 青绿色
  'South America' = '#45B7D1',    # 蓝色
  'Europe' = '#96CEB4',      # 绿色
  'Oceania' = '#FECA57',    # 黄色
  'Africa' = '#FF9FF3',      # 粉色
  'Other Regions' = '#C8D6E5'   # 浅蓝色
)

# 设置新罗马字体
windowsFonts(Times = windowsFont("Times New Roman"))

# 绘制地图网络图 - 国家按大洲填充颜色
p8 <- ggplot() +
  # 世界地图背景 - 按大洲填充不同颜色
  geom_polygon(aes(x = long, y = lat, group = group, fill = continent),
               data = world_map,
               color = "white", size = 0.1, alpha = 0.8) +
  
  # 绘制合作连线 - 使用细弧线
  geom_curve(data = edges_for_plot,
             aes(x = x, y = y, xend = xend, yend = yend,
                 linewidth = weight),
             color = 'black', curvature = 0.2, alpha = 0.6) +
  
  # 绘制国家节点 - 根据大洲填充颜色
  geom_point(data = nodes,
             aes(x = lon, y = lat, size = weight, fill = continent),
             shape = 21, color = 'white', stroke = 1) +
  
  # 设置大洲填充颜色
  scale_fill_manual(
    name = "Continent",
    values = continent_colors,
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  
  # 设置连线宽度尺度
  scale_linewidth_continuous(
    name = "Collaboration Strength",
    range = c(0.1, 0.5),
    guide = guide_legend(title.position = "top")
  ) +
  
  # 设置节点大小尺度
  scale_size_continuous(
    name = "Node Centrality",
    range = c(2, 10),
    guide = guide_legend(title.position = "top")
  ) +
  
  # 添加国家标签 - 只显示重要的国家
  geom_text_repel(data = nodes %>% filter(weight >= quantile(weight, 0.3)),
                  aes(x = lon, y = lat, label = name),
                  size = 2, color = "black", fontface = "bold",
                  box.padding = 0.3, min.segment.length = 0.1,
                  max.overlaps = 15, bg.color = "white", bg.r = 0.15,
                  family = "Times") +  # 设置标签字体
  
  # 坐标轴范围和比例
  coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80), ratio = 1.3) +
  
  # 主题设置 - 使用新罗马字体，并移除所有图例
  theme_void() +
  theme(
    text = element_text(family = "serif"),  # 全局字体设置
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, 
                              margin = margin(b = 8)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, 
                                 color = "gray40", margin = margin(b = 15)),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",  # 移除所有图例
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm')
  )

# 输出图片
ggsave("./国家合作2.pdf", p8, width = 13, height = 7, units = "cm", dpi = 600)

# 统计信息
cat("=== Network Statistics ===\n")
cat("Number of nodes (countries):", nrow(nodes), "\n")
cat("Number of edges (collaborations):", nrow(edges_for_plot), "\n")
cat("Number of continents involved:", length(unique(nodes$continent)), "\n")

# 按大洲统计国家数量
continent_stats <- nodes %>%
  group_by(continent) %>%
  summarise(Number_of_Countries = n(), 
            Average_Centrality = round(mean(weight), 2)) %>%
  arrange(desc(Number_of_Countries))

cat("\n=== Statistics by Continent ===\n")
print(continent_stats)

print("国家合作网络地图绘制完成！字体已设置为新罗马字体，图例已隐藏，图片已保存。")