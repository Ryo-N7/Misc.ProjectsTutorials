library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)


link <- "https://en.wikipedia.org/wiki/List_of_countries_by_oil_production"
df_oil <- link %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()

colnames(df_oil) <- c("rank", "country", "oil_bbl_per_day")


df_oil <- df_oil %>% mutate(rank = as.integer(rank))

df_oil <- df_oil %>% mutate(oil_bbl_per_day = oil_bbl_per_day %>% 
                              str_replace_all(",", "") %>% 
                              as.integer())

glimpse(df_oil)

df_oil <- df_oil %>% mutate(opec_ind = if_else(str_detect(country, "OPEC"), 1, 0))

df_oil <- df_oil %>% mutate(opec_ind = if_else(str_detect(country, 'OPEC'), 1, 0))

df_oil <- df_oil %>% 
  mutate(country = country %>% str_replace(" \\(OPEC\\)", "") %>% str_replace("\\s{2,}", ""))



df_oil %>% filter(opec_ind == 1)

df_oil <- df_oil %>% select(rank, country, opec_ind, oil_bbl_per_day)

glimpse(df_oil)



map_world <- map_data("world")


df_oil %>% 
  anti_join(map_world, by = c("country" = "region"))

map_world %>% 
  group_by(region) %>% 
  summarize() %>% 
  print(n = Inf)


df_oil <- df_oil %>%  
  mutate(country = recode(country, 
                          `United States` = 'USA',
                          `United Kingdom` = 'UK', 
                          `Congo, Democratic Republic of the` = 'Democratic Republic of the Congo',
                          `Trinidad and Tobago` = 'Trinidad',
                          `Sudan and South Sudan` = 'Sudan',
                          #, `Sudan and  South Sudan` = 'South Sudan'
                          `Congo, Republic of the` = 'Republic of Congo')
        )

map_oil <- map_world %>% left_join(df_oil, by = c("region" = "country"))


ggplot(map_oil, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = oil_bbl_per_day))

map_oil %>% glimpse()



df.oil %>% filter(oil_bbl_per_day > 822675) %>% summarise(mean(oil_bbl_per_day))
# 3190373

df.oil %>% filter(oil_bbl_per_day < 822675) %>% summarise(mean(oil_bbl_per_day))
# 96581.08

glimpse(map_oil)


ggplot(map_oil, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = oil_bbl_per_day)) +
  scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F')
                       ,values = scales::rescale(c(100,96581,822675,3190373,10000000))
                       ,labels = comma
                       ,breaks = c(100,96581,822675,3190373,10000000)
  ) +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'bbl/day'
       ,title = 'Oil Production by Country'
       ,subtitle = 'Barrels per day, 2016'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Source: U.S. Energy Information Administration\nhttps://en.wikipedia.org/wiki/List_of_countries_by_oil_production'
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Gill Sans'
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )


map_oil %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = oil_bbl_per_day))


glimpse(map_oil)

## Highlight  OPEC countries

map_oil %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(color = as.factor(opec_ind))) +
  scale_color_manual(values = c("1" = "red", "0" = NA))


map_oil %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(color = as.factor(opec_ind), fill = oil_bbl_per_day)) +
  scale_color_manual(values = c("1" = "red", "0" = NA))


quantile(map_oil$oil_bbl_per_day, na.rm = TRUE)

ggplot(map_oil, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = oil_bbl_per_day, color = as.factor(opec_ind))) +
  scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F')
                       ,values = scales::rescale(c(100,96581,822675,3190373,10000000))
                       ,labels = comma
                       ,breaks = c(100,96581,822675,3190373,10000000)
  ) +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'Barrels per day\n2016'
       ,color = 'OPEC Countries'
       ,title = 'OPEC countries produce roughly 44% of world oil'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Source: U.S. Energy Information Administration\nhttps://en.wikipedia.org/wiki/List_of_countries_by_oil_production\nhttps://en.wikipedia.org/wiki/OPEC'
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Gill Sans'
           ,color = '#CCCCCC'
           ,hjust = 'left'
  ) +
  scale_color_manual(values = c('1' = 'orange', '0' = NA), labels = c('1' = 'OPEC'), breaks = c('1'))









