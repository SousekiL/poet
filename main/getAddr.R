# 安装必要的包，如果未安装
# install.packages("osmdata")
# install.packages("httr")
library(osmdata)
library(httr)
library(dplyr)
library(magrittr)
# 地名需要用URL编码
# 使用 Nominatim 进行地理编码
geocode_nominatim <- function(place) {
  place_encoded <- URLencode(place)
  base_url <- "https://nominatim.openstreetmap.org/search"
  query <- paste0(base_url, "?q=", place_encoded, "&format=json&limit=1")
  response <- GET(query, user_agent("R"))
  
  if (status_code(response) == 200) {
    result <- content(response, "parsed")
    if (length(result) > 0) {
      lat <- as.numeric(result[[1]]$lat)
      lon <- as.numeric(result[[1]]$lon)
      return(c(lat = lat, lon = lon))
    }
  }
  return(NULL)
}

# 查询地名
poem_df2 <- poem_df %>%
  mutate(search_addr = case_when(
           addr == "德阳" ~ "四川德阳市",
           addr == "彬县" ~ "陕西彬州市",
           addr == "玉华宫" ~ "铜川市",
           addr == "蓝田" ~ "陕西蓝田县",
           addr == "故县" ~ "河南灵宝市",
           addr == "新昌" ~ "浙江新昌县",
           addr == "浙江" ~ "杭州市",
           addr == "山东" ~ "济南市",
           addr == "宝鸡" ~ "陕西宝鸡市",
           addr == "丹凤" ~ "陕西商洛市",
           addr == "华阴" ~ "陕西华阴市",
           addr == "天山" ~ "乌鲁木齐",
           addr == "江陵" ~ "湖北江陵县",
           addr == "周至" ~ "陕西周至县",
           addr == "巴东" ~ "湖北巴东县",
           addr == "渭南" ~ "陕西渭南市",
           TRUE ~ addr
         )) 
  
addr_list <- unique(poem_df2$search_addr)
location <- sapply(addr_list, geocode_nominatim)
location_df <- as.data.frame(t(location))
location_df$search_addr <- rownames(location_df)

# 合并数据
poem_addr_df <- left_join(poem_df2, location_df) %>%
  dplyr::mutate(id = str_trim(str_replace(name, "\\(.*?\\)", "")))
