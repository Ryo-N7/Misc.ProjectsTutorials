# Background data for groups >>> ggplot2


# visualize data for different groups in facetted plot with ALL data in background

library(ggplot2)

ggplot(iris, aes(Sepal.Width)) +
  geom_histogram()

# but for EACH flower `species`?

# Method 1: colored stacking 

ggplot(iris, aes(Sepal.Width, fill = Species)) +
  geom_histogram()

# Method 2: Facettting

ggplot(iris, aes(Sepal.Width)) +
  geom_histogram() +
  facet_wrap(~ Species)

# Complete data in background >>> ^interpretability for any group relative to REST of groups 

# Example 1:

d <- iris         # FULL data set
d_bg <- d[, -5]   # Background data without `Species` grouping data

ggplot(d, aes(Sepal.Width)) +
  geom_histogram(data = d_bg, fill = "grey") +
  geom_histogram() +
  facet_wrap(~ Species)

# with pipes so NOT have to make subsetted data frame as separate
ggplot(iris, aes(Sepal.Width)) +
  geom_histogram(data = iris %>% select(-Species), fill = "grey") +
  geom_histogram() +
  facet_wrap(~ Species)

# NICE! easy way to see how distribution of each group "species" fits with rest of the data!

# Example 1.1:
# Give color to each specific group in facet

ggplot(d, aes(Sepal.Width, fill = Species)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = 0.5) +
  geom_histogram(color = "black") +
  facet_wrap(~Species) +
  guides(fill = FALSE) +
  theme_bw()


# Example 2: Points

ggplot(d, aes(Sepal.Width, Sepal.Length)) +
  geom_point(data = d_bg, color = "grey") +
  geom_point() +
  facet_wrap(~ Species)

# Example 2.1: Points 
# Give color to each specific group in facet

ggplot(d, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(data = d_bg, color = "grey", alpha = 0.2) +
  geom_point() +
  facet_wrap(~ Species) +
  guides(color = FALSE) +
  theme_bw()


# Example 3: Mapping

library(nycflights13)
library(dplyr)

usa_map <- map_data("usa")

airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", 
                     stringsAsFactors = FALSE, header = FALSE)

airports <- airports[, c(5, 7, 8)]

names(airports) <- c("code", "lat", "long")

orig <- airports %>% 
  dplyr::rename(origin = code, long_o = long, lat_o = lat)

dest <- airports %>% 
  dplyr::rename(dest = code, long_d = long, lat_d = lat)


d <- flights %>% 
  left_join(origin) %>% 
  left_join(destination) %>% 
  filter(carrier %in% c("AS", "F9", "OO", "YV", "VX", "FL"))

glimpse(d)

d_bg <- d %>% select(-carrier)

ggplot(d) +
  geom_polygon(data = usa_map, aes(long, lat, group = region)) +
  geom_segment(data = d_bg, color = "grey", alpha = 0.7,
               aes(x = long_o, y = lat_o,
                   xend = long_d, yend = lat_d)) +
  geom_segment(aes(x = long_o, y = lat_o,
                   xend = long_d, yend = lat_d,
                   color = carrier)) +
  facet_wrap(~ carrier) +
  guides(color = FALSE) +
  theme_bw()

# for 6 different carriers as the grouping var
# ALL flight paths appear in background >>> flight path of facetted airline = highlight COLOR












