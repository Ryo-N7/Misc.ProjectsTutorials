# http://uribo.hatenablog.com/entry/2017/11/08/213751

devtools::install_github("mikkelkrogsholm/hexamapmaker")

library(magrittr)
library(tidyverse)
library(ggthemes)

df.jp.prefs <- tibble::frame_data(
  ~x, ~y, ~id,
  15, 14, "HKD",
  14, 12, "AOM",
  15, 11, "IWT",
  14, 11, "AKT",
  14, 10, "MYG",
  13, 10, "YGT",
  14, 9, "FKS",
  13, 9, "IBR",
  12, 9, "NGT",
  14, 8, "GNM",
  13, 8, "SIT",
  12, 8, "TCG",
  11, 8, "TYM",
  10, 8, "ISK",
  14, 7, "CHB",
  13, 7, "TKY",
  12, 7, "YMN",
  11, 7, "NGN",
  10, 7, "FKI",
  9, 7, "KYT",
  8, 7, "HYO",
  7, 7, "TTR",
  6, 7, "SMN",
  13, 6, "KNG",
  12, 6, "SZO",
  11, 6, "AIC",
  10, 6, "GIF",
  9, 6, "SIG",
  8, 6, "OSK",
  7, 6, "OKY",
  6, 6, "HRS",
  5, 6, "YMG",
  10, 5, "MIE",
  9, 5, "NAR",
  9, 4, "WKY",
  7, 4, "KGW",
  6, 4, "EHM",
  7, 3, "TKS",
  6, 3, "KUC",
  4, 5, "FKO",
  3, 5, "SAG",
  2, 5, "NGS",
  3, 4, "OIT",
  2, 4, "KUM",
  3, 3, "MYZ",
  2, 3, "KGS",
  1, 1, "OKN"
)

ggplot(df.jp.prefs, aes(x = x, y = y, group = id)) +
  geom_point() +
  coord_fixed(ratio = 1) +
  theme_map()



library(hexamapmaker)

df.jp.prefs <- fix_shape(df.jp.prefs)

df.jp.prefs.hex <- make_polygons(df.jp.prefs)

(p <- ggplot(df.jp.prefs.hex, aes(x, y, group = id)) +
    geom_polygon(fill = "white", colour = "black", show.legend = FALSE) +
    coord_fixed(ratio = 1) +
    theme_map())

p


# use purrrlyr to create polygon for each prefecture:

library(sf)

make_hex <- function(d) {
  res <- d %>% 
    mutate(geom = sf::st_polygon(list(rbind(c(min(d$x), min(d$y) + 0.577),
                                            c(min(d$x), min(d$y) + 0.577 + 1),
                                            c(mean(d$x), max(d$y)),
                                            c(max(d$x), min(d$y) + 0.577 + 1),
                                            c(max(d$x), min(d$y) + 0.577),
                                            c(mean(d$x), min(d$y)),
                                            c(min(d$x), min(d$y) + 0.577)
    )))) %>% 
    magrittr::use_series(geom)  
  return(res)
}


sfdf.jp.hex <- sf::st_sf(id = sort(df.jp.prefs$id),
                         geometry = df.jp.prefs.hex %>% 
                           purrrlyr::slice_rows("id") %>% 
                           purrrlyr::by_slice(make_hex) %>% magrittr::use_series(.out) %>% 
                           sf::st_sfc())
sfdf.jp.hex %>% head()


sfdf.jp.hex.centroid <- sfdf.jp.hex %>% 
  mutate(x = map_dbl(geometry, ~st_centroid(.x)[[1]]), 
         y = map_dbl(geometry, ~st_centroid(.x)[[2]]))

st_centroid(sfdf.jp.hex$geometry[[1]])[[1]]
st_centroid(sfdf.jp.hex$geometry[[2]])[[2]]
st_centroid(sfdf.jp.hex$geometry[[3]])

library(ggplot2)
library(extrafont)
fonts()

ggplot() + 
  geom_sf(data = sfdf.jp.hex) +
  geom_text(data = sfdf.jp.hex.centroid, 
            aes(label = id, 
                x = x, y = y, alpha = 0.5), family = "Garamond", 
            size = 3, color = "darkred",
            show.legend = FALSE) +
  theme_map()


### http://uribo.hatenablog.com/entry/2017/10/12/083110

latlon2tile <- function(lon, lat, z) {
  x = trunc((lon/180 + 1) * 2^z/2)
  y = trunc(((-log(tan((45 + lat/2) * pi/180)) + 
                pi) * 2^z/(2 * pi)))
  return(list(x = x, y = y, zoom = z))
}


# https://maps.gsi.go.jp/development/tileCoordCheck.html#15/35.6188/139.6192


xyz <- latlon2tile(139.6192, 35.6188, 15)
xyz

library(sf)

df_nrpt <- read_sf(paste0("https://cyberjapandata.gsi.go.jp/xyz/experimental_nrpt/",
                          paste(xyz[3], xyz[1], xyz[2], sep = "/"),
                          ".geojson"))
df_nrpt

df.nffpt <- read_sf(paste0("https://cyberjapandata.gsi.go.jp/xyz/experimental_pfpt/",
                           paste(xyz[3], xyz[1], xyz[2], sep = "/"), 
                           ".geojson"))
df.nffpt



df_nrpt %<>% select(name, kana) %>% mutate(lon = map_dbl(geometry, 
                                                         ~st_centroid(.x)[[1]]), lat = map_dbl(geometry, 
                                                                                               ~st_centroid(.x)[[2]]))
df.nffpt %<>% select(type, name = pfName) %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]), 
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))


plot(df.nffpt)






# http://uribo.hatenablog.com/entry/2017/12/08/144549

library(magrittr)
library(tidyverse)
library(ggimage)
library(jpndistrict)
library(sf)

sf_ja <- 1:47 %>% magrittr::extract(-13) %>% 
  map(~jpndistrict::jpn_pref(pref_code = ., 
                             district = FALSE)) %>% reduce(rbind) %>% 
  st_simplify(dTolerance = 0.01)

sf_pref13 <- jpn_pref(pref_code = 13, district = TRUE) %>% 
  st_simplify(dTolerance = 0.01) %>% mutate(city_code = as.numeric(city_code)) %>% 
  filter(city_code != 13421) %>% st_union() %>% 
  as.data.frame() %>% mutate(jis_code = "13", 
                             prefecture = "?????????") %>% magrittr::set_names(c("geometry", 
                                                                           "jis_code", "prefecture")) %>% st_as_sf()
sf_ja_omit47 <- sf_ja %>% filter(jis_code != "47")

sf_ja_pref47 <- sf_ja %>% filter(jis_code == "47")

sf_ja_pref47$geometry %<>% magrittr::add(c(5.6, 17.5))

sf_ja_pref47 %<>% st_set_crs(value = 4326)

p <- ggplot(sf_ja_omit47) + 
  geom_sf() + 
  geom_sf(data = sf_pref13, inherit.aes = TRUE) + 
  geom_sf(data = sf_ja_pref47, inherit.aes = TRUE) + 
  geom_segment(aes(x = round(st_bbox(sf_ja_omit47)[1], 0), xend = 132.5, y = 40, yend = 40)) + 
  geom_segment(aes(x = 132.5, xend = 138,y = 40, yend = 42)) + 
  geom_segment(aes(x = 138, xend = 138, y = 42, yend = round(st_bbox(sf_ja_omit47)[4], 0))) + 
  xlab(NULL) + ylab(NULL) + 
  theme(plot.caption = element_text(size = 6)) + 
  labs(caption = "?????????????????????????????????????????????????????????\n???????????????????????????(??????????????????)?????????????????????(????????????)\n?????????????????????????????? (??????????????????28????????????603???)")

p












