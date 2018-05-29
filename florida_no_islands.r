library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(tigris)


states_sf <- tigris::states(cb = TRUE) %>% 
  st_as_sf() %>% 
  select(STUSPS, NAME, geometry) %>% 
  filter(!STUSPS %in% c("VI", "MP", "GU", "PR", "AS")) 



str(states_sf, list.len = 2)

fl <- tracts("FL", cb = TRUE)
fl <- fl %>% st_as_sf()
monroe_cty <- fl %>% filter(COUNTYFP == "087")
fl_keys <- fl %>% filter(TRACTCE == "990000")

core_based_statistical_areas()


# 
fl %>% 
  filter(COUNTYFP != "087") %>% 
  ggplot() + 
  geom_sf()

# 
fl %>% 
  filter(TRACTCE != "990000") %>% 
  ggplot() + 
  geom_sf()

monroe_cty %>% 
  ggplot() +
  geom_sf()

fl_keys %>% 
  ggplot() +
  geom_sf()
