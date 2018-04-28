library(dplyr)
library(rvest)
library(stringr)
library(ggplot2)


public_debt_link <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_public_debt")

df_public_debt_country <- public_debt_link %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()


names(df_public_debt_country)

colnames(df_public_debt_country) <- c("country", "debt_as_perc_gdp", "measure_year", 
                                      "gross_debt_perc_gdp_IMF",
                                      "net_debt_perc_gdp_IMF",
                                      "measure_year_IMF",
                                      "region")

df_public_debt_country <- df_public_debt_country %>% 
  select(-gross_debt_perc_gdp_IMF, 
         -net_debt_perc_gdp_IMF,
         -measure_year_IMF)

df_public_debt_country <- df_public_debt_country %>% filter(country != "World") 

df_public_debt_country %>% as_tibble()


df_map <- map_data("world")
df_map %>% glimpse()
df_map %>% names()

df_map %>% 
  rename(country = region) -> df_map

df_public_debt_country %>% 
  anti_join(df_map, by = "country")


df_map %>% 
  group_by(country) %>% 
  summarise() %>% 
  print(n = Inf)

df_public_debt_country %>% 
  mutate(country = recode(country
                          ,`Antigua and Barbuda` = 'Antigua'
                          ,`Burma` = 'Myanmar'
                          ,`People's Republic of China` = 'China'
                          ,`Congo, Democratic Republic of the` = 'Democratic Republic of the Congo'
                          ,`Congo, Republic of the` = 'Republic of Congo'
                          ,`Cote d'Ivoire` = 'Ivory Coast'
                          ,`Gambia, The` = 'Gambia'
                          #,`Gibraltar` = ''
                          #,`Hong Kong` = ''
                          ,`Korea, North` = 'North Korea'
                          ,`Korea, South` = 'South Korea'
                          ,`Saint Kitts and Nevis` = 'Saint Kitts'
                          ,`Saint Vincent and the Grenadines` = 'Saint Vincent'
                          ,`Trinidad and Tobago` = 'Trinidad'
                          #,`Tuvalu` = ''
                          ,`United Kingdom` = 'UK'
                          ,`United States` = 'USA'
  )
  ) ->
  df_public_debt_country


df_public_debt_country %>% 
  anti_join(df_map, by = "country")


df_map_public_debt <- df_map %>% 
  left_join(df_public_debt_country, by = "country")

#### plot

df_map_public_debt %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = debt_as_perc_gdp))

library(extrafont)

theme_map <- theme(
  text = element_text(family = 'Arial Narrow', color = '#444444')
  ,panel.background = element_rect(fill = '#CCCCCC')
  ,plot.background = element_rect(fill = '#CCCCCC')
  ,legend.background = element_rect(fill = '#CCCCCC')
  ,panel.grid = element_blank()
  ,plot.title = element_text(size = 18, face = 'bold')
  ,plot.subtitle = element_text(size = 12)
  ,legend.key = element_blank()
  ,axis.text = element_blank()
  ,axis.ticks = element_blank()
  ,axis.title = element_blank()
)


plot_debt_gdp_map <- df_map_public_debt %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = debt_as_perc_gdp)) +
  theme_map +
  labs(title = str_c('Countries in the developed world are'
                     ,'\ncarrying high levels of debt compared to GDP'
  )
  ,fill = str_c('Net public debt','\nas a % of GDP')
  ) +
  scale_fill_gradientn(colors = c('#009933', '#ffff00', 'orange', '#e60000')
                       ,values = scales::rescale(c(30, 50, 70, 100, 200))
  )


# world
plot_debt_gdp_map

# europe

plot_debt_gdp_map +
  coord_cartesian(xlim = c(-15, 50), ylim = c(30, 75)) +
  labs(title = "European countries have high levels of public debt"
       ,subtitle = str_c('In particular, countries in southern Europe - including Portugal, Italy,'
                         ,'\nGreece, and Spain - have high levels of public debt.'
       )
  )



















