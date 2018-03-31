library(ggplot2)
library(scales)
library(tidyverse)
library(highcharter)

data(mpg)
str(mpg)


mpgman2 <- count(mpg, manufacturer, year)

str(mpgman2)

hchart(mpgman2, "bar", hcaes(x = manufacturer, y = n, group = year),
       color = c("#FCA50A", "#FCFFA4"),
       name = c("year 1999", "Year 2008"))


species <- starwars %>% count(species, homeworld, sort = TRUE)
species
species %>% count(species, sort = TRUE)

str(iris)
Lengthyy <- iris %>% count(Sepal.Width, Species)

unique(iris$Petal.Width)
unique(iris$Sepal.Length)
unique(iris$Sepal.Width)

length(unique(iris$Petal.Length)) # 43
library(data.table)
uniqueN(iris$Petal.Length)
uniqueN(iris$Petal.Width)
length(unique(iris$Petal.Width))

hchart(Lengthyy, "bar", hcaes(x = Species, y = n, group = Sepal.Width),
       color = "blue")


seg.df <- read.csv("http://goo.gl/qw303p")

seg.df %>% group_by(ownHome) %>% summarise(meankids = mean(kids))
seg.df %>% group_by(ownHome, Segment) %>% mutate(meankids = mean(kids)) %>% hchart("bar", hcaes(x = ownHome, y = meankids, group = Segment))

seg.df <- seg.df %>% count(Segment, kids, ownHome)

hchart(seg.df, "bar", hcaes(x = Segment, y = mean(kids), group = ownHome))

seggy <- seg.df %>% filter(Segment == "Moving up")




## Creating interactive charts/maps with html_widgets and highcharter
# http://paldhous.github.io/ucb/2016/dataviz/week13.html

library(htmlwidgets)

# connects to JS highcharts and highstock visualizaiton libs.

library(highcharter)
library(RColorBrewer)
library(readr)
library(dplyr)


nations <- read_csv("nations.csv") %>% 
  mutate(gdp_tn = gdp_percap*population/1000000000000)

glimpse(nations)


big4 <- nations %>% 
  filter(iso3c == "CHN" | iso3c == "DEU" | iso3c == "JPN" | iso3c == "USA") %>% 
  arrange(year)

glimpse(big4)

# arrange() == important, as highcharter needs proper ordering for time series!

highchart() %>% 
  hc_add_series_df(data = big4,
                   type = "line", 
                   x = year, y = gdp_tn, group = country)

# customize colors
cols <- brewer.pal(4, "Set1")

highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols)

# add axis labels
highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)"))

# custom fonts
library(extrafont)

highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)")) %>% 
  hc_chart(style = list(fontFamily = "Comic Sans MS",
                        fontWeight = "bold"))

# set symbols used
highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)")) %>% 
  hc_chart(style = list(fontFamily = "Trebuchet MS")) %>% 
  hc_plotOptions(series = list(marker = list(symbol = "circle")))

# change legend position
highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)")) %>% 
  hc_chart(style = list(fontFamily = "Trebuchet MS")) %>% 
  hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>% 
  hc_legend(align = "right", verticalAlign = "top")

# customize tooltips
highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)")) %>% 
  hc_chart(style = list(fontFamily = "Trebuchet MS")) %>% 
  hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>% 
  hc_legend(align = "right", verticalAlign = "top") %>% 
  hc_tooltip(shared = TRUE,
             borderColor = "black", 
             pointFormat = "{point.country}: {point.gdp_tn:.2f}<br>")
# tooltip shared across all groups on that point.
# set to black for all
# have decimal place for values to 2. >>> .2f

# save as R object:
big4_chart <- highchart() %>% 
  hc_add_series_df(data = big4, type = "line",
                   x = year, y = gdp_tn, group = country) %>% 
  hc_colors(cols) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)")) %>% 
  hc_chart(style = list(fontFamily = "Trebuchet MS")) %>% 
  hc_plotOptions(series = list(marker = list(symbol = "circle"))) %>% 
  hc_legend(align = "right", verticalAlign = "top") %>% 
  hc_tooltip(shared = TRUE,
             borderColor = "black", 
             pointFormat = "{point.country}: {point.gdp_tn:.2f}<br>")

# line chart with no symbols
big4_chart %>% 
  hc_plotOptions(series = list(marker = list(enable = FALSE), lineWidth = 8))


# export as WEB PAGE!
saveWidget(big4_chart, "big4.html", selfcontained = TRUE, libdir = NULL, background = "white")


# GDP by region

regions <- nations %>% 
  group_by(year, region) %>% 
  summarize(gdp_tn = sum(gdp_tn, na.rm = TRUE)) %>% 
  arrange(year, region)

highchart() %>% 
  hc_add_series_df(data = regions, type = "area", 
                   x = year, y = gdp_tn, group = region)


cols <- brewer.pal(7, "Set2")

highchart() %>% 
  hc_add_series_df(data = regions, type = "area", 
                   x = year, y = gdp_tn, group = region) %>% 
  hc_colors(cols) %>% 
  hc_chart(style = list(fontFamily = "Verdana", 
                        fontWeight = "bold")) %>% 
  hc_plotOptions(series = list(stacking = "normal", 
                               marker = list(enabled = FALSE, 
                                             states = list(hover = list(enabled = FALSE))),
                               lineWidth = 0.5, 
                               lineColor = "white")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_yAxis(title = list(text = "GDP ($, trillions)")) %>% 
  hc_legend(align = "right", verticalAlign = "bottom", layout = "horizontal") %>% 
  hc_tooltip(enabled = FALSE)


library(forcats)

regions <- regions %>% 
  mutate(region = as.factor(region), 
         region = fct_relevel(region,
                              "Sub-Saharan Africa",
                              "South Asia", 
                              "North America",
                              "Middle East & North Africa",
                              "Latin America & Caribbean", 
                              "Europe & Central Asia",
                              "East Asia & Pacific"))

cols <- rev(cols)

highchart () %>%
  hc_add_series_df(data = regions,
                   type = "area",
                   x = year,
                   y = gdp_tn, 
                   group = region) %>%
  hc_colors(cols) %>% 
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold")) %>%
  hc_plotOptions(series = list(stacking = "normal",
                               marker = list(enabled = FALSE,
                                             states = list(hover = list(enabled = FALSE))),
                               lineWidth = 0.5,
                               lineColor = "white")) %>%
  hc_xAxis(title = list(text="Year")) %>%
  hc_yAxis(title = list(text="GDP ($ trillion)")) %>%
  hc_legend(align = "right", 
            verticalAlign = "bottom",
            layout = "horizontal",
            reversed = TRUE) %>%
  hc_tooltip(enabled = FALSE)


# Combine multiple chart types into one

food_stamps <- read_csv("food_stamps.csv")

cols <- c("red", "black")


highchart() %>% 
  hc_add_series(data = food_stamps$participants, 
                name = "Participants (millions)", 
                type = "column") %>% 
  hc_add_series(data = food_stamps$costs,
                name = "Costs ($, billions",
                type = "line") %>% 
  hc_xAxis(categories = food_stamps$year,
           tickInterval = 5) %>% 
  hc_colors(cols) %>% 
  hc_chart(style = list(fontFamily = "Trebuchet MS", 
                        fontWeight = "bold"))

# each variable added separately with hc_add_series(), set type separately in each

# hc_xAxis(categories = unique(food_stamps$year), tickInterval = 5)
# >>> tick label for every 5 years


# separate y-axis for each series

highchart() %>%
  hc_yAxis_multiples(
    list(title = list(text = "Participants (millions)")),
    list(title = list(text = "Costs ($ billions)"), 
         opposite = TRUE)
  ) %>%
  hc_add_series(data = food_stamps$participants,
                name = "Participants (millions)",
                type = "column",
                yAxis = 0) %>%
  hc_add_series(data = food_stamps$costs,
                name = "Costs ($ billions)",
                type = "line",
                yAxis = 1) %>%
  hc_xAxis(categories = food_stamps$year,
           tickInterval = 5) %>%
  hc_colors(cols) %>%
  hc_chart(style = list(fontFamily = "Georgia",
                        fontWeight = "bold")) %>% 
  hc_tooltip(shared = TRUE)
# opposite = TRUE to place 2nd axis on RIGHT SIDE, if not both on same side...
# map each series to correct axis with yAxis = 0, 1...



## Financial charts with highstock

library(quantmod)

google <- getSymbols("GOOG", src = "yahoo", auto.assign = FALSE)
facebook <- getSymbols("FB", src = "yahoo", auto.assign = FALSE)
amazon <- getSymbols("AMZN", src = "yahoo", auto.assign = FALSE)

cols <- brewer.pal(3, "Set1")

highchart(type = "stock") %>% 
  hc_colors(cols) %>% 
  hc_add_series_xts(google$GOOG.Adjusted, name = "Google") %>% 
  hc_add_series_xts(facebook$FB.Adjusted, name = "Facebook") %>% 
  hc_add_series_xts(amazon$AMZN.Adjusted, name = "Amazon") %>% 
  hc_legend(enabled = TRUE, 
            verticalAlign = "top") %>% 
  hc_chart(style = list(fontFamily = "Verdana", 
                        fontWeight = "bold")) %>% 
  hc_tooltip(borderColor = "black")

## Financial charts with dygraphs

library(dygraphs)

companies <- cbind(google$GOOG.Adjusted, facebook$FB.Adjusted, amazon$AMZN.Adjusted)

names(companies) <- c("Google", "Facebook", "Amazon")

dygraph(companies, ylab = "Adjusted @ close") %>% 
  dyOptions(colors = brewer.pal(3, "Set1")) %>% 
  dyRangeSelector() %>%                           # date range select slider
  dyAxis("x", drawGrid = FALSE)        # erase vertical grid lines


# Leaflet maps!

library(leaflet)
library(rgdal)

leaflet() %>% 
  setView(lng = -122.2705383, lat = 37.8698807, zoom = 11)

leaflet() %>% 
  setView(lng = -122.2705383, lat = 37.8698807, zoom = 11) %>% 
  addProviderTiles("CartoDB.Positron")

# shapefile
seismic_risk <- readOGR("seismic_risk_clip", "seismic_risk_clip")

# earthquakes since 1965, Magnitude == +6
quakes <- read_csv("http://earthquake.usgs.gov/fdsnws/event/1/query?starttime=1965-01-01T00:00:00&minmagnitude=6&format=csv&latitude=39.828175&longitude=-98.5795&maxradiuskm=6000&orderby=magnitude")


summary(seismic_risk)

seismic <- leaflet(data = seismic_risk)

breaks <- c(0, 19, 39, 59, 79, 200)

binpal <- colorBin("Reds", seismic_risk$ACC_VAL, breaks)

seismic %>% 
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7, smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  )

# add circles for each earthquake

seismic %>% 
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7, smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  ) %>% 
  addCircles(
    data = quakes, 
    radius = sqrt(10^quakes$mag)*50,
    color = "#000000", weight = 0.2,
    fillColor = "#ffffff", fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b, %d, %Y"))
  )

# add legend:
seismic %>% 
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7, smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  ) %>% 
  addCircles(
    data = quakes, 
    radius = sqrt(10^quakes$mag)*50,
    color = "#000000", weight = 0.2,
    fillColor = "#ffffff", fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b, %d, %Y"))
  ) %>% 
  addLegend(
    "bottomleft", pal = binpal, values = ~ACC_VAL, 
    title = "Seismic risk", opacity = 0.7
  )


# add second tileset, set layer controls to switch between maps + layers


seismic <- seismic %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
  addProviderTiles("Stamen.TonerLite", group = "Toner") %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.7, 
    smoothFactor = 0.1,
    color = ~binpal(ACC_VAL)
  ) %>%
  # add historical earthquakes
  addCircles(
    data=quakes, 
    radius = sqrt(10^quakes$mag)*50, 
    weight = 0.2, 
    color = "#000000", 
    fillColor ="#ffffff",
    fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y")),
    group = "Quakes"
  ) %>%
  # add legend
  addLegend(
    "bottomleft", pal = binpal, values = ~ACC_VAL,
    title = "Seismic risk",
    opacity = 0.7
  ) %>%
  # add layers control
  addLayersControl(
    baseGroups = c("CartoDB", "Toner"),
    overlayGroups = "Quakes",
    options = layersControlOptions(collapsed = FALSE)
  )


seismic

# save as html!

saveWidget(seismic, "seismic.html", selfcontained = TRUE, libdir = NULL, background = "white")

















