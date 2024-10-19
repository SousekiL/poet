# 安装必要的包，如果未安装
# install.packages("osmdata")
# install.packages("httr")

library(osmdata)
library(httr)
library(dplyr)

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
addr_list <- unique(poem_df$addr)
location <- sapply(addr_list, geocode_nominatim)
location_df <- as.data.frame(t(location))
location_df$addr <- rownames(location_df)
poem_addr_df <- left_join(poem_df, location_df)
