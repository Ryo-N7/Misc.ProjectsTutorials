library(ggmap)

iskandar <- c(left = 22.15, bottom = 25.01, right = 70.31, top = 41.90)

iskandar_map <- get_stamenmap(bbox = iskandar, zoom = 5, maptype = "watercolor", where = "cache")


ggmap(iskandar_map)
