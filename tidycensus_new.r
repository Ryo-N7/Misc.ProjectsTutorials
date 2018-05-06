# new version of tidy census with new US census data: 
# https://github.com/slu-soc1120/US_DEMOS_medianIncome/blob/v0.1.0/2016/source/createMap.R

library(ggplot2)
library(albersusa)
library(classInt)
library(sf)
library(tidycensus)
library(prener) # themes


#US state outlines: 
us_sf <- usa_sf("laea")


# median income by county 
library(acs)

api.key.install(key = "7ab09e940b5289a9a6ef7fc45f5e652572d7e363")

census_api_key("7ab09e940b5289a9a6ef7fc45f5e652572d7e363")

us_county_income <- get_acs(geography = "county", 
                            variables = "B19013_001", 
                            shift_geo = TRUE,
                            geometry = TRUE)


# jenks natural breaks: takes a while!!
jenks <- classIntervals(us_county_income$estimate, n = 5, style = "jenks")

income <- cut(us_county_income$estimate, breaks = c(jenks$brks))



# map

base <- ggplot() +
  geom_sf(data = us_county_income, aes(fill = income), color = NA) +
  geom_sf(data = us_sf, fill = NA, color = "#000000", size = 0.25) +
  coord_sf(datum = NA) +
  scale_fill_brewer(palette = "BuPu", name = "Median Income", 
                    labels = c("$19.0k - $35.5k", 
                               "$38.5k - $47.9k", 
                               "$47.9k - $58.9k",
                               "$58.9k - $76.4k",
                               "$76.4k - $126k")) +
  labs(title = "Median income by county (2016)",
       subtitle = "2016 - 5-year ACS Estimates",
       caption = "Data via US Census Bureau") +
  theme_minimal()



??crs
??st_geometry
??st_centroid
??coord_sf







