## 排错

case_proj <- poem_addr_df_sf %>%
  dplyr::filter(id == "韩愈")

tmap_mode("view")

# 创建交互式地图
tm_shape(provincial_shape) +
  tm_polygons(col = "white", border.col = "gray20") + # 绘制省级区域
  tm_shape(sea_line) +
  tm_lines(col = "black", lwd = 1) + # 绘制九段线
  tm_shape(case_proj) +
  tm_dots(col = "#F97A71", size = 0.5, alpha = 0.5, id = "addr") + # 绘制点，设置成可交互显示标签
  tm_text(text = "addr", size = 0.7, shadow = TRUE, fontface = "bold") + # 添加交互标签
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE
  )
