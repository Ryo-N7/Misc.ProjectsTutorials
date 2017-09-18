# Amazon HQ  http://sharpsightlabs.com/blog/new-amazon-hq/
library(tidyverse)
library(rvest)
library(stringr)
library(ggmap)

url <- "https://www.cbsnews.com/news/amazons-hq2-cities-second-headquarters-these-cities-are-contenders/"

df_amz_cities <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()

df_amz_cities %>% head(6)

colnames(df_amz_cities)   #    X1   X2   X3   X4

# manually rename
colnames(df_amz_cities) <- c("metro_area", "state", "population", "bachelor_degree_pct")
colnames(df_amz_cities)

?row_number
df_amz_cities <- df_amz_cities %>% filter(row_number() != 1)
row_number(df_amz_cities)

str(df_amz_cities)
# pop and bachelors both 'chr' variables, change to numeric!
# df_amz_cities <- df_amz_cities %>% mutate(population = as.numeric(population))
# if as.numeric() >>>  NAs introduced by coercion...
# use parse_number() instead!
?parse_number
df_amz_cities <- df_amz_cities %>% mutate(population = parse_number(population))
str(df_amz_cities)

df_amz_cities <- df_amz_cities %>% mutate(bachelor_degree_pct = as.numeric(bachelor_degree_pct))

# metro names too broad! ex. "New York-Newark-Jersey City"   lol wtf would do it like this??
# won't work well esp with geocoding...
# create new city variable by extract (the first) indiv. cities from the metro_area name!
df_amz_cities <- df_amz_cities %>% mutate(city = str_extract(metro_area, "^[^-]*"))
str(df_amz_cities)

# extract lat/lon of cities in df
data_geo <- geocode(df_amz_cities$city)
data_geo %>% head(6)

# merge 2 datasets
df_amz_cities <- cbind(df_amz_cities, data_geo)
str(df_amz_cities)

df_amz_cities <- df_amz_cities %>% rename(long = lon)
glimpse(df_amz_cities)

df_amz_cities %>% names()

# reorder columns in df with select 
df_amz_cities <- df_amz_cities %>% select(city, state, metro_area, long, lat, population, bachelor_degree_pct)
glimpse(df_amz_cities)

# Get map
map_states <- map_data("state")


# first iteration
ggplot() +
  geom_polygon(data = map_states, aes(x = long, y = lat, 
                                      group = group)) +
  geom_point(data = df_amz_cities, aes(x = long, y = lat, 
                                       size = population, color = bachelor_degree_pct))


theme_amzn <- 
  theme(text = element_text(color = "#444444", family = "Comic Sans MS")) +   # cuz lol
  theme(panel.background = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(plot.title = element_text(size = 25)) +
  theme(plot.subtitle = element_text(size = 16)) +
  theme(legend.key = element_rect(fill = "lightblue"))

library(extrafont)
fonts()
loadfonts(device = "win")

ggplot() +
  geom_polygon(data = map_states, aes(x = long, y = lat, group = group)) +
  geom_point(data = df_amz_cities, aes(x = long, y = lat, size = population, color = bachelor_degree_pct*.01), alpha = .5) +
  geom_point(data = df_amz_cities, aes(x = long, y = lat, size = population, color = bachelor_degree_pct*.01), shape = 1) +
  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121,-73), ylim = c(25,51)) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = .41, labels = scales::percent_format()) +
  scale_size_continuous(range = c(.9, 11),  breaks = c(2000000, 10000000, 20000000),labels = scales::comma_format()) +
  guides(color = guide_legend(reverse = T, override.aes = list(alpha = 1, size = 4) )) +
  labs(color = "Bachelor's Degree\nPercent"
       ,size = "Total Population\n(metro area)"
       ,title = "Possible cities for new Amazon Headquarters"
       ,subtitle = "Based on population & percent of people with college degrees") +
  theme_amzn

























































































































































































































































































