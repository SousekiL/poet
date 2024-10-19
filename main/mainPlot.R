library(tmap)
library(sf)
library(ggplot2)
library(ggrepel)
library(grid)
library(risingCoord)
# a <- 6378245.0
# f <- 1 / 298.3
# b <- a * (1 - f)
# ee <- 1 - (b * b) / (a * a)
library(showtext)
## Add the font with the corresponding font faces
font_add("Canger", "/Library/Fonts/仓耳今楷01-W04.ttf")
font_families()

showtext_auto() # 全局自动使用
# 读取 shapefile 文件
shapefile_path <- "/Users/sousekilyu/Documents/Meta Data/MAP/2023年10月14日版（CTAmap 1.12）/2023年/2023年省级/2023年省级.shp"
provincial_shape <- st_read(shapefile_path, crs = 4326) %>%
  st_transform(crs = 2333)
sea_path <- "/Users/sousekilyu/Documents/Meta Data/MAP/2023年10月14日版（CTAmap 1.12）/2023年/九段线/九段线.shp"
sea_line <- st_read(sea_path, crs = 4326) %>%
  st_transform(crs = 2333)
# 读取 bbox shapefile
bbox_path <- "data/bbox/bbox2.shp"
bbox_shape <- st_read(bbox_path, crs = 4326) %>%
  st_transform(crs = 2333)

# 输出bbox的边界信息
bbox_bounds <- st_bbox(bbox_shape)

# 检查 shapefile 数据
print(provincial_shape)
# geom_type <- st_geometry_type(provincial_shape)
#
# # 确定如何绘制
# if ("POLYGON" %in% geom_type | "MULTIPOLYGON" %in% geom_type) {
#   # 如果是多边形
#   plot_command <- tm_polygons()
# } else if ("LINESTRING" %in% geom_type | "MULTILINESTRING" %in% geom_type) {
#   # 如果是线
#   plot_command <- tm_lines()
# } else {
#   stop("不支持的几何类型")
# }
#
# # 设定 tmap 模式为绘图模式
# tmap_mode("plot")
# tmap_options(check.and.fix = TRUE)
#
# # 绘制地图
# tm_shape(bbox_shape) +
#   tm_borders(col = "white", lwd = 0) +
#   tm_shape(provincial_shape) +
#   tm_polygons(fill = "white", col = "gray") +  # 绘制省级区域
#   tm_shape(sea_line) +
#   tm_lines(col = "black", lwd = 1) +  # 用蓝色线绘制九段线
#   tm_layout(main.title = "2023 Provincial Map",
#             legend.title.size = 1.2,
#             legend.text.size = 0.8)

# 创建南海九段线的小图
south_china_sea_plot <- ggplot() +
  geom_sf(data = provincial_shape, fill = "#EDDBC9", color = "gray20") + # 绘制省级区域
  geom_sf(data = sea_line, color = "black", size = 1) +
  coord_sf(
    xlim = st_bbox(sea_line)[c("xmin", "xmax")],
    ylim = st_bbox(sea_line)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  theme_void()

# 将小图转换为可用的 grob 对象
inset_grob <- ggplotGrob(south_china_sea_plot)
# 创建一个黑色框的 grob，用于包裹插图
border_grob <- rectGrob(
  gp = gpar(col = "black", fill = NA, lwd = 1) # 黑色边框，无填充，线宽1
)
# 将边框 grob 和 插图 grob 合并，确保边框在内图周围
inset_with_border <- grobTree(border_grob, inset_grob)


# 创建 sf 对象
poem_addr_df_sf <- st_as_sf(poem_addr_df, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(provincial_shape))
capital_addr <- poem_addr_df_sf %>%
  dplyr::filter(addr %in% c("西安", "洛阳")) %>%
  select(addr, geometry) %>%
  mutate(addr = ifelse(addr == "西安", "长安", addr)) %>%
  unique()


# for

for (poetry_name in unique(poem_addr_df_sf$id)) {
  case_proj <- poem_addr_df_sf %>%
    dplyr::filter(id == poetry_name)

  # 使用 ggplot2 绘制点和曲线
  # 获取每个组的第一个和最后一个点
  first_last_points <- case_proj %>%
    arrange(order) %>%
    group_by(name) %>%
    slice(c(1, n())) %>%
    ungroup()
  # 作图
  ggplot() +
    geom_sf(data = provincial_shape, fill = "#EDDBC9", color = "gray20", alpha = .7) + # 绘制省级区域
    geom_sf(data = sea_line, color = "black", size = 1) + # 绘制九段线
    geom_sf(data = case_proj, aes(color = name), size = 2, alpha = .5, color = "#F97A71") + # 绘制点
    geom_curve(
      data = case_proj %>%
        arrange(order) %>% # 按照 order 字段排序
        group_by(name) %>%
        mutate(
          x = st_coordinates(geometry)[, 1],
          y = st_coordinates(geometry)[, 2],
          xend = lead(x),
          yend = lead(y)
        ) %>%
        filter(!is.na(xend) & !is.na(yend) & (x != xend | y != yend)),
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "darkred", # 设置曲线颜色
      alpha = 0.5, # 设置透明度
      curvature = 0.2, # 控制弧度
      arrow = arrow(type = "open", length = unit(0.2, "cm"))
    ) +
    ## 都城
    geom_point(
      data = capital_addr,
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2]
      ), # 通过order判断颜色
      size = 3,
      shape = 17
    ) + # 都城
    geom_label_repel(
      data = capital_addr,
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        label = addr
      ),
      color = "black",
      box.padding = 0.3,
      point.padding = 0.5,
      segment.color = "grey50",
      family = "Canger",
      fontface = "bold",
      alpha = .7,
      size = 10
    ) + 
    ## 生卒地
    geom_point(
      data = first_last_points,
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        color = factor(order == min(order), labels = c("去世", "出生"))
      ), # 通过order判断颜色
      size = 3
    ) + # 增加点的大小以突出显示
    geom_label_repel(
      data = first_last_points,
      aes(
        x = st_coordinates(geometry)[, 1],
        y = st_coordinates(geometry)[, 2],
        label = cm_addr,
        color = factor(order == min(order), labels = c("去世", "出生"))
      ),
      box.padding = 0.3,
      point.padding = 0.5,
      segment.color = "grey50",
      family = "Canger",
      fontface = "bold",
      alpha = .9,
      size = 12
    ) + # 设置标签字体大小
    coord_sf(
      xlim = st_bbox(bbox_shape)[c("xmin", "xmax")] - c(-500000, 500000),
      ylim = st_bbox(bbox_shape)[c("ymin", "ymax")] - c(0, 500000),
      expand = FALSE
    ) +
    theme_bw() +
    labs(
      title = paste0(unique(case_proj$name), " 编年地图"),
      x = "", y = ""
    ) +
    theme(
      text = element_text(family = "Canger", size = 35),
      title = element_text(family = "Canger", size = 60),
      legend.position = c(0.2, 0.2),
      # old map
      panel.background = element_rect(fill = "#f7f4e9", color = NA), # antique paper color
      plot.background = element_rect(fill = "#f7f4e9", color = NA),
      panel.border = element_rect(color = "#c1a184", size = 1.2), # soft brown border
      panel.grid.major = element_line(color = "#d3c2a3", size = 0.2), # light grid lines
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "#c1a184"),
      axis.text = element_text(color = "#4a3f2f"), # brown axis labels
      axis.title = element_text(color = "#4a3f2f"),
      plot.title = element_text(color = "#4a3f2f"),
      plot.subtitle = element_text(color = "#4a3f2f"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text = element_text(color = "#4a3f2f", family = "Canger", size = 35),
      legend.title = element_text(color = "#4a3f2f", family = "Canger", size = 45),
      legend.key = element_rect(fill = "transparent", color = NA)
    ) +
    scale_color_manual(
      values = c("出生" = "#E6700B", "去世" = "#1A3DA3"),
      labels = c("出生" = "出生(或最早见于)", "去世" = "去世(或最后见于)"),
      name = "生卒地",
      limits = c("出生", "去世")
    ) + # 设置图例和颜色
    guides(color = guide_legend(override.aes = list(size = 10))) +
    annotation_custom(
      inset_with_border,
      xmin = st_bbox(bbox_shape)["xmax"] - 500000 - 700000, # 调整位置
      xmax = st_bbox(bbox_shape)["xmax"] - 500000 - 50000,
      ymin = st_bbox(bbox_shape)["ymin"] + 100000,
      ymax = st_bbox(bbox_shape)["ymin"] + 950000
    )

  ggsave(paste0("plot/", poetry_name, "_plot.png"),
    width = 10, height = 8,
    dpi = 300
  )

  print(poetry_name)
}
