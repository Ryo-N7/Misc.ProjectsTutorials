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
us_county_income <- get_acs(geography = "county", 
                            variables = "B19013_001", 
                            shift_geo = TRUE,
                            geometry = TRUE)


