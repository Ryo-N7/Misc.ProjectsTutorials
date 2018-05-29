library(dplyr)
library(readxl)
library(sf)

# read-in data
starbucks_raw <- read_excel("../tidy_tuesday/may_7_week_6/week6_coffee_chains.xlsx", sheet = 1)

# clean, select cols, filter USA, summarize
starbucks_usa <- starbucks_raw %>% 
  janitor::clean_names() %>% 
  select(brand, city, state_province, country, longitude, latitude) %>% 
  filter(country == "US") %>% 
  group_by(state_province) %>% 
  summarize(count = n()) %>% 
  ungroup()

# grab geometries of USA from tigris pkg, turn into sf
states_sf <- tigris::states(cb = TRUE) %>% 
  st_as_sf() %>% 
  select(STUSPS, NAME, geometry) %>% 
  filter(!STUSPS %in% c("VI", "MP", "GU", "PR", "AS")) # filter out territories and Puerto Rico

# join with starbucks data
starbucks_sf <- states_sf %>% left_join(starbucks_usa, by = c("STUSPS" = "state_province"))

# remove Alaska and Hawaii... try to rescale and put them back in another time
starbucks_sf2 <- starbucks_sf %>% filter(!NAME %in% c("Alaska", "Hawaii"))

library(units)
starbucks_sf_norm2 <- starbucks_sf2 %>% 
  mutate(area_km2 = st_area(geometry) %>% 
           set_units(km^2) %>% 
           as.numeric(),
         area_m2 = st_area(geometry) %>% 
           as.numeric(),
         count_norm_m2 = count / area_m2,
         count_norm_km2 = count / area_km2,
         area_sq2 = st_area(geometry) %>% 
           set_units(mi^2) %>% 
           as.numeric(),
         count_norm_sq2 = count / area_sq2) %>% 
  filter(!NAME == "District of Columbia")


library(spData) # us_states has pop for each state

states_pop <- us_states %>% 
  select(GEOID, NAME, total_pop_15) %>% 
  filter(!NAME == "District of Columbia") %>% 
  st_set_geometry(NULL)

starbucks_population <- starbucks_sf_norm2 %>% 
  left_join(states_pop, by = "NAME")

starbucks_population <- starbucks_population %>% 
  mutate(pop_norm = (count / total_pop_15 * 100000) %>% ceiling()) # try per 100,000 people?

library(cartogram)
starb_cartogram <- st_transform(starbucks_population, crs = 2163)

starb_sp <- as(starb_cartogram, "Spatial")

# weigh size by state pop
starb_cartogram <- nc_cartogram(starb_sp, weight = "total_pop_15", k = 0.5)

library(tmap)

# Sequential single hue color palette :: http://colorbrewer2.org/#type=sequential&scheme=Greens&n=5
greenpal <- c('#edf8e9','#bae4b3','#74c476','#31a354','#006d2c')

# legend.reverse for HIGH values on TOP, slight sepia to offset white glare?
# fiddle with margins to fit legend and small title
# plot!
starbucks_cartogram <- tm_shape(starb_cartogram) + 
  tm_borders("grey10") +
  tm_fill(title = "", "pop_norm", 
          palette = greenpal, 
          legend.reverse = TRUE) +
  tm_layout(inner.margins = c(.04,.02, .08, .02),
            main.title = "Number of Starbucks per 100,000 people",
            title = "(Source: https://www.kaggle.com/starbucks/store-locations)\nState size by state population",
            title.position = c("center", "top"), title.size = 0.7,
            fontfamily = "Garamond", fontface = "bold",
            legend.text.size = 0.85, 
            sepia.intensity = 0.1)

starbucks_cartogram

save_tmap(starbucks_cartogram, "starbucks_cartogram_pop_fix.png")















