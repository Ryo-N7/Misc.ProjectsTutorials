library(ggplot2)
library(ggpomological)
library(scales)
library(sf)
library(readr)
library(dplyr)
library(grid)
library(ggsn)
library(units)
library(extrafont)


# https://statnmap.com/2018-04-18-draw-maps-like-paintings/#

font_import()

# France pop by region

Region_L93 <- read_rds("Region_L93.rds")

# pop under-20s in 2015

Region_L93 %>% 
  ggplot() +
  geom_sf(aes(fill = Age_0.19.ans), alpha = 0.75, color = NA) +
  geom_sf(aes(color = Age_0.19.ans), alpha = 1, fill = NA) +
  scale_fill_gradient(
    "Dem youthss (< 20 yrs)",
    low = muted(ggpomological:::pomological_palette[2], l = 90, c = 70),
    high = ggpomological:::pomological_palette[2]
  ) +
  scale_color_gradient(
    low = muted(ggpomological:::pomological_palette[2], l = 90, c = 70),
    high = ggpomological:::pomological_palette[2],
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Homemade Apple")


## pop by proportion

# set_units() instead of `units <- ` 
# drop_units()
# st_area(): compute area of given geometries

Region_Prop_L93 <- Region_L93 %>%
  mutate(area_km2 = st_area(.) %>% set_units(km^2) %>% drop_units()) %>%
  mutate(Prop_20_km2 = `Age_0.19.ans` / area_km2)

ggplot() +
  geom_sf(data = Region_Prop_L93,
          aes(fill = Prop_20_km2), alpha = 0.9, colour = NA
  ) +
  geom_sf(data = Region_Prop_L93,
          aes(colour = Prop_20_km2), alpha = 1, fill = NA
  ) +
  scale_fill_gradient(
    "Youngs / km²",
    low = muted(ggpomological:::pomological_palette[2], l = 100, c = 70),
    high = muted(ggpomological:::pomological_palette[2], l = 50, c = 70),
    trans = "log"
  ) +
  scale_colour_gradient(
    low = muted(ggpomological:::pomological_palette[2], l = 80, c = 70),
    high = muted(ggpomological:::pomological_palette[2], l = 50, c = 70),
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Nanum Pen") +
  labs(
    title = "Number of people aged below 20 by km² in 2015",
    caption = "source: http://www.ecosante.fr/"
  )




## add cities

Region_Prop_wgs84 <- Region_Prop_L93 %>% 
  st_transform(4326)

cities_wgs84 <- tibble(
  
  city = c("Paris", "Rennes", "Lille", "Strasbourg", "Brest", "Bordeaux",
           "Montpellier", "Nice", "Lyon"),
  lat = c(48.857256, 48.110867, 50.625291, 48.576816, 48.384679, 44.843019,
          43.609519, 43.694233, 45.749206),
  
  long = c(2.344655, -1.678327, 3.057288, 7.754883, -4.498229, -0.581680,
           3.877594, 7.245262, 4.847652)
 ) %>% 
  st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4326) %>% 
  bind_cols(st_coordinates(.) %>% as.data.frame())

glimpse(cities_wgs84)

Region_Prop_wgs84 %>% 
  ggplot() +
  geom_sf(data = Region_Prop_wgs84, 
          aes(fill = Prop_20_km2), alpha = 0.9, color = NA) +
  geom_sf(data = Region_Prop_wgs84, 
          aes(color = Prop_20_km2), alpha = 1, fill = NA) +
  geom_sf(data = cities_wgs84, color = "grey30") +
  geom_text(
    data = cities_wgs84, aes(X, Y, label = city), nudge_y = 0.3,
    family = "Nanum Pen", color = "grey20", size = 6
  ) +
  scale_fill_gradient("Youngs / km^2",
                      low = muted(ggpomological:::pomological_palette[2], l = 100, c = 70),
                      high = muted(ggpomological:::pomological_palette[2],l = 50, c = 70),
                      trans = "log") +
  scale_colour_gradient(
    low = muted(ggpomological:::pomological_palette[2],l = 80, c = 70),
    high = muted(ggpomological:::pomological_palette[2],l = 50, c = 70),
    guide = FALSE) +
  theme_pomological(base_family = "Nanum Pen") +
  labs(
    title = "Number of people aged below 20 by km² in 2015",
    caption = "source: http://www.ecosante.fr/\nproj: wgs84, epsg: 4326. aut.: S. Rochette, ThinkR.",
    x = "", y = ""
  ) +
  north(Region_Prop_wgs84, symbol = 4, scale = 0.1) +
  scalebar(Region_Prop_wgs84, location = "bottomleft", dist = 200, dd2km = TRUE,
           model = "WGS84", box.fill = c("grey30", "white"),
           box.color = "grey30", st.color = "grey30", family = "Nanum Pen") +
  theme(text = element_text(family = "Nanum Pen"))

# ggsn::north(): NORTH map symbol!
# ggsn::scalebar(): map scalebar thingy





# color for region

glimpse(Region_Prop_wgs84)

cols <- rep(ggpomological:::pomological_palette,
            length.out = length(Region_Prop_wgs84$NOM_REG))

ggpomological:::check_font("Nanum Pen")


ggplot(Region_Prop_wgs84) +
  geom_sf(
    data = Region_Prop_wgs84,
    aes(fill = NOM_REG),
    alpha = 0.9, colour = NA
  ) +
  geom_sf(
    data = Region_Prop_wgs84,
    aes(colour = NOM_REG),
    alpha = 1, fill = NA
  ) +
  geom_sf(data = cities_wgs84, colour = "grey30") +
  geom_text(
    data = cities_wgs84, aes(X, Y, label = city),
    nudge_y = 0.3, family = "Nanum Pen",
    colour = "grey20", size = 6
  ) +
  scale_fill_manual(
    "Regions",
    values = muted(cols, l = 60, c = 70)
  ) +
  scale_colour_manual(
    values = muted(cols, l = 40, c = 70),
    guide = FALSE
  ) +
  theme_pomological_fancy(base_family = "Nanum Pen") +
  labs(
    title = "French regions",
    caption = "source: https://www.ign.fr/\nproj: wgs84, epsg: 4326. aut.: S. Rochette, ThinkR."
  ) +
  xlab("") + ylab("") +
  north(Region_Prop_wgs84, symbol = 4, scale = 0.1) +
  scalebar(
    Region_Prop_wgs84,
    location = "bottomleft", dist = 200,
    dd2km = TRUE, model = "WGS84",
    box.fill = c("grey30", "white"), box.color = "grey30",
    st.color = "grey30", family = "Nanum Pen"
  ) +
  theme(text = element_text(family = "Nanum Pen"))






