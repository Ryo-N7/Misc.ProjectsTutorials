library(rnaturalearth)
library(sf)
library(plotly) # automatically loads ggplot2
library(crosstalk)
library(viridis)

# https://gist.github.com/walkerke/5fe9a198a30270e2fcb8120a7fc8242a

ng <- ne_states(country = "Nigeria", returnclass = "sf") %>% 
  select(Name = name)

ng$TFR <- c(4.7, 4.2, 3.9, 4.8, 3.8, 4.5, 
            5.2, 5.4, 6.0, 5.1, 4.1, 6.1, 
            5.4, 5.2, 4.3, 4.1, 4.5, 4.2, 
            8.1, 7.0, 4.1, 4.4, 4.8, 5.3, 
            4.1, 4.2, 5.4, 5.4, 7.6, 6.8, 
            7.4, 7.0, 8.4, 6.6, 6.7, 5.8, 4.5)
dplyr::glimpse(ng)


ng <- SharedData$new(ng, key = ~Name)

bar <- ng %>% 
  ggplot(aes(x = reorder(Name, TFR), y = TFR, fill = TFR)) +
  geom_col() +
  coord_flip() + 
  scale_fill_viridis(guide = FALSE) +
  labs(x = "", 
       y = "Total fertility rate, 2013.",
       title = "States in Nigeria",
       subtitle = "Source: USAID Demographic & Health Surveys.")

# devtools::install_github('hadley/ggplot2')

map <- ng %>% 
  ggplot(aes(fill = TFR)) +
  geom_sf() +
  coord_sf() +
  scale_fill_viridis()


barly <- bar %>% 
  ggplotly(tooltip = NA, height = 600) %>% 
  highlight("plotly_click", color = "red")

maply <- map %>% 
  ggplotly(tooltip = "Name", height = 600) %>% 
  highlight("plotly_click", color = "red")







