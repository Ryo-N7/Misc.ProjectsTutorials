# Data wrangling: World Shipping data
rm(list = ls())

install.packages("tidyverse")
library(tidyverse)
library(forcats)
library(stringr)
install.packages("ggmap")
install.packages("rvest")
library(ggplot2)
library(ggmap)
library(rvest)
library(ggthemes)

html.world_ports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_container_ports")

df.world_ports <- html_table(html_nodes(html.world_ports, "table")[[2]], fill = TRUE)

?glimpse
glimpse(df.world_ports)
# Variables: Rank, Port, Economy (Country), Year 2014 downwards. 

# Remove capital letters from variable names, use pipe operators. 
colnames(df.world_ports) <- colnames(df.world_ports) %>% tolower()
colnames(df.world_ports)

# Use ggmaps::geocode() to find lat/lon values of each port. 
geocodes.world_ports <- geocode(df.world_ports$port)

# MERGE lat/lon values back onto df.world_ports data frame
df.world_ports <- cbind(df.world_ports, geocodes.world_ports)
glimpse(df.world_ports)
head(df.world_ports)

# Obtain missing lat/lon value NOT found by geocode(), manually input lat/lon data. 
# Missing port data:
# Tanjung Pelepas, Johor Bahru: lon = 103.551035, lat = 1.362374
# Yingkou:..................... lon = 122.108231, lat = 40.266062
# Valencia, Spain:............. lon = -0.3762881, lat = 39.46991
# Malta Freeport:.............. lon = 14.537637 , lat = 35.816287
?mutate
?is.na
apply(is.na(df.world_ports),2,sum)
rowSums(is.na(df.world_ports))
anyNA(df.world_ports)
sum(is.na(df.world_ports))

?case_when
??"%%"

df.world_ports <- df.world_ports %>%
  mutate( lon = case_when(.$port == "Tanjung Pelepas" ~ 103.551035
                          ,.$port == "Yingkou"        ~ 122.108231
                          ,.$port == "Valencia"       ~ -0.3762881
                          ,.$port == "Malta Freeport" ~ 14.537637
                          ,TRUE ~ .$lon
  )
  ,lat = case_when(.$port == "Tanjung Pelepas" ~ 1.362374
                   ,.$port == "Yingkou"        ~ 40.266062
                   ,.$port == "Valencia"       ~ 39.46991
                   ,.$port == "Malta Freeport" ~ 35.816287
                   ,TRUE ~ .$lat
  )
  )

df.world_ports %>% filter(port == "Tanjung Pelepas") %>% select(lat,lon)
df.world_ports %>% filter(port == "Yingkou") %>% select(lat,lon)
df.world_ports %>% filter(port == "Valencia") %>% select(lat,lon)
df.world_ports %>% filter(port == "Malta Freeport") %>% select(lat,lon)


# Convert character variables as factor variables.
df.world_ports <- mutate(df.world_ports, economy = as.factor(str_trim(economy)), 
                         port = as.factor(port))
?str_trim
?as.factor

# Create new 'label' variables for long name ports.
# First, identify long name ports. 
# Then, recode into shorter form as secondary variable, 'label'.
levels(df.world_ports$port)
# Ho Chi Minh City (Saigon), New York and New Jersey, Tanjung Priok (Jakarta), 
# Bremen/Bremerhaven, Tanger-Med (Tangiers), Ningbo-Zhoushan, Jebel Ali (Dubai), 
# Ambarli (Istanbul). 

df.world_ports <-  mutate(df.world_ports
                          , port_label = fct_recode(port
                                                    ,"Saigon" = "Ho Chi Minh City (Saigon)"
                                                    ,"New York" = "New York and New Jersey"
                                                    ,"Jakarta" = "Tanjung Priok (Jakarta)"
                                                    ,"Bremen" = "Bremen/Bremerhaven"
                                                    ,"Istanbul" = "Ambarli (Istanbul)"
                                                    ,"Tangiers" = "Tanger-Med (Tangiers)"
                                                    ,"Dubai" = "Jebel Ali (Dubai)"
                                                    ,"Ningbo/Z-shan" = "Ningbo-Zhoushan"
                          )
)

levels(df.world_ports$port_label)

# Tidy data
head(df.world_ports)
# Fix year as 'year' variable instead of untidy individual column. 
# Also create 'volume' variable. 
?gather

df.world_ports <- df.world_ports %>%
  gather(year,volume,4:14)

# INSPECT
head(df.world_ports)
levels(as.factor(df.world_ports$year))
levels(df.world_ports$economy)
levels(df.world_ports$port)
names(df.world_ports)

# Change 'year' character variable into factor variable
df.world_ports <- mutate(df.world_ports, year = as.factor(year))
str(df.world_ports$year)

# Remove extraneous [#] from 'year' variable.
levels(df.world_ports$year)
?fct_recode()

df.world_ports <- mutate(df.world_ports
                         , year = fct_recode(year
                                             ,"2014" = "2014[1]"
                                             ,"2013" = "2013[2]"
                                             ,"2012" = "2012[3]"
                                             ,"2011" = "2011[4]"
                                             ,"2010" = "2010[5]"
                                             ,"2009" = "2009[6]"
                                             ,"2008" = "2008[7]"
                                             ,"2007" = "2007[8]"
                                             ,"2006" = "2006[9]"
                                             ,"2005" = "2005[10]"
                                             ,"2004" = "2004[11]"
                         )
)

levels(df.world_ports$year)
head(df.world_ports)

# Change volume character variable to numeric variable
?str_replace()

df.world_ports <- mutate(df.world_ports, volume = as.numeric(str_replace(volume, ",", "")))
head(df.world_ports)
glimpse(df.world_ports)

# Ranking data 
# Only done in terms of 2014 data, necessary to reshape. 
# Drop existing rank variable.
# Calculate new rank variable.

df.world_ports <- select(df.world_ports, -rank)
head(df.world_ports)

df.world_ports <- df.world_ports %>% 
  group_by(year) %>%
  mutate(rank = min_rank(desc(volume))) %>%
  ungroup()

glimpse(df.world_ports)
head(df.world_ports)

# Create 'continent' variable
levels(df.world_ports$economy)
df.world_ports <- df.world_ports %>% 
  mutate(continent = fct_collapse(economy, South_America = c("Brazil", "Panama"),
                                  North_America = c("United States", "Canada"),
                                  Asia = c("China", "India", "Japan", "Singapore",
                                           "Sri Lanka", "Indonesia", "Malaysia",
                                           "Philippines", "South Korea", "Taiwan",
                                           "United Arab Emirates", "Vietnam",
                                           "Oman", "Saudi Arabia", "Thailand"),
                                  Europe = c("Belgium", "Turkey", "Netherlands", "Germany",
                                             "Italy", "Malta", "Spain", "United Kingdom"),
                                  Africa = c("Morocco", "Egypt")
                                  )
         )
glimpse(df.world_ports)
head(df.world_ports)
levels(df.world_ports$continent)

# Check
df.world_ports %>%
  group_by(continent, economy) %>%
  summarize(1) %>%
  print.data.frame()

# Re-order variables in data frame
df.world_ports %>% ncol()
df.world_ports %>% names()

# Select in preferred order
df.world_ports <- select(df.world_ports, rank, year, continent, 
                         economy, port, port_label, lon, lat, 
                         volume) 
glimpse(df.world_ports)
head(df.world_ports)
df.world_ports %>% ncol()

# Check data to Wikipedia original.

df.world_ports %>% filter(year == '2012', port == 'Guangzhou') %>% select(volume)       # 14744 OK
df.world_ports %>% filter(year == '2007', port == 'Guangzhou') %>% select(volume)       # 9200 OK
df.world_ports %>% filter(year == '2005', port == 'Rotterdam') %>% select(volume)       # 9287 OK
df.world_ports %>% filter(year == '2005', port == 'Yingkou') %>% select(volume)         # 634 OK
df.world_ports %>% filter(year == '2004', port == 'Yingkou') %>% select(volume)         # NA OK
df.world_ports %>% filter(year == '2007', port == 'Keelung') %>% select(volume)         # NA OK
df.world_ports %>% filter(year == '2014', port == 'Seattle/Tacoma') %>% select(volume)  # 3456 OK
df.world_ports %>% filter(year == '2009', port == 'Nagoya') %>% select(volume)          # 2113 OK

# ALL CLEAR 


# Create custom themes

theme.porttheme <- 
  theme(text = element_text(family = "Comic Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank())

theme.widebar <-
  theme.porttheme +
  theme(plot.title = element_text(size = 30)) + 
  theme(plot.subtitle = element_text(size = 20)) +
  theme(legend.title = element_blank(), legend.background = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = c(0.9, 0.55)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
  
theme.smallmult <-
  theme.porttheme +
  theme(axis.text = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90))
  
  
  
# Bar graph for Year = 2014
df.world_ports %>% 
  filter(year == 2014) %>%
  ggplot(aes(x = reorder(port_label, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", fill = "dark red") +
  labs(title = "Busiest Ports in the World") +
  labs(subtitle = "2014, In order of shipping volume") +
  labs(x = "Port", y = "Volume") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme.widebar
  
# Flip bar chart for better interpretation/reading. 

df.world_ports %>% 
  filter(year == 2014, rank <= 10) %>%
  ggplot(aes(x = reorder(port, volume), y = volume)) +
  geom_bar(stat = "identity", fill = "dark red") +
  labs(title = "TOP 10 Busiest Ports en el Mundo") +
  labs(subtitle = "2014, In order of Shipping Volume") +
  labs(x = "Port", y = "Shippign Volume\n(1000 TEUs)") +
  geom_text(aes(label = volume), hjust = 1.1, color = "#FFFFFF") +
  scale_y_continuous(labels = scales::comma_format()) +
  coord_flip() +
  theme.porttheme
  
  
    graphics.off()
  

# Highlight Chinese ports
?ifelse  

df.world_ports %>%
  mutate(china_flag = ifelse(economy == "China", "China", "Not China")) %>%
  filter(year == 2014) %>%
  ggplot(aes(x = reorder(port_label, desc(volume)), y = volume)) + 
  geom_bar(stat = "identity", aes(fill = china_flag)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("dark red", "#999999")) +
  labs(title = "China, Greatest Shipping Country") +
  labs(subtitle = "20% of WOrld's Shipping Volume!") +
  labs(x = "Port", y = "Shipping Volume \n(1000 TEUs)") +
  theme.widebar
    
    
    
# How highlight certain ports?
# At the very top of the dplyr pipeline, we're piping df.world_ports into mutate(). 
# Inside of mutate(), we're using an ifelse() statement to create a "flag." Essentially, 
# we're creating a new (temporary) variable that we can use as we pipe this result into ggplot:
# df.world_ports %>%
#  mutate(china_flag = ifelse(economy == "China","China","Not China"))

# We then pipe that result into ggplot(), and there's a piece of code inside of geom_bar() 
# that allows us to highlight the Chinese ports: aes(fill = china_flag). Essentially, 
# we're using the "flag" variable that we created in mutate() to specify how we want to 
# color our bars. If the port is a Chinese port, it will get one color, and if it's not 
# a Chinese port, it will get another.
# Where do we specify the colors? We specify which colors to use for the "China" bars and 
# which to use for the "Not China" bars in the following line of code: 
# scale_fill_manual(values = c("dark red","#999999???)).

# Highlight ports in Asia
df.world_ports %>%
  mutate(asia_flag = ifelse(continent == "Asia", "Asia", "Other")) %>%
  filter(year == 2014) %>%
  ggplot(aes(x = reorder(port_label, desc(volume)), y = volume)) +
  geom_bar(stat = "identity", aes(fill = asia_flag)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("dark blue", "#999999")) +
  labs(title = "Asia, Greatest Shipping Continent") +
  labs(subtitle = "Exporter of Potassium!") +
  labs(x = "Port", y = "Shipping Volume \n(1000 TEUs)") +
  theme.widebar

# Shipping volume over TIME. (Trellis/Panel chart)
df.world_ports %>%
  ggplot(aes(x = year, y = volume, group = port_label)) +
  geom_line(color = "dark red", size = 1, na.rm = TRUE) +
  facet_wrap(~ port_label) +
  labs(title = "Shipping Volume Growth \n (2004-2014)") +
  labs(x = "Port", y = "Shipping \n Volume \n (1000 TEUs)") +
  theme.smallmult
graphics.off()

# Focus on Shanghai over TIME
df.world_ports %>%
  mutate(port_highlight = ifelse(port == "Shanghai", "Shanghai", "Other")) %>%
  ggplot(aes(x = year, y = volume, group = port)) +
  geom_line(aes(color = port_highlight, alpha = port_highlight), size = 1.5, na.rm = TRUE) +
  scale_color_manual(values = c("#999999", "dark green")) + 
  scale_alpha_manual(values = c(.3, 1)) +
  labs(title = "Shanghai Shipping Volume \n increased substantially 2004-2014") +
  labs(x = "Year", y = "Shipping\nVolume\n(1000 TEUs)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme.porttheme
  
?scale_alpha_manual()

# Singapore

df.world_ports %>%
  filter(port == "Singapore") %>%
  ggplot(aes(x = year, y = volume, group = 1)) +
  geom_line(color = "dark blue", size = 2) +
  labs(title = "Singapore Shipping Volume increased \n substantially from 2004-2014") +
  labs(x = "Year", y = "Shipping Volume \n (1000 TEUs)") +
  scale_y_continuous(limits = c(0, NA)) +
  theme.porttheme
# 
# Rank changes over TIME

df.world_ports %>%
  ggplot(aes(x = year, y = rank, group = port_label)) +
  geom_line(size = 1, color = "dark red", na.rm = T) +
  scale_y_reverse() +
  facet_wrap(~ port_label) +
  labs(title = "Ranking over time of world's busiest ports") +
  labs(subtitle = "2004 to 2014") +
  labs(x = "Year", y = "Rank") +
  theme.smallmult


# Bump chart: track changes over TIME

param.rank_n <- 15

df.world_ports %>%
  filter(rank <= param.rank_n) %>%
  mutate(china_flag = ifelse(economy == "China", T,F)) %>%
  mutate(china_labels = ifelse(china_flag == T, port,"other")) %>%
  ggplot(aes(x = year, y = rank, group = port_label)) +
  geom_line(aes(color = china_labels, alpha = china_flag), size = 2) +
  geom_point(aes(color = china_labels, alpha = china_flag), size = 2.3) +
  geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
  geom_text(data = df.world_ports %>% filter(year == "2014", rank <= param.rank_n), 
            aes(label = port_label, x = '2014') , hjust = -.05, color = "#888888", size = 4) +
  geom_text(data = df.world_ports %>% filter(year == "2004", rank <= param.rank_n), 
            aes(label = port_label, x = '2004') , hjust = 1.05, color = "#888888", size = 4) +
  scale_x_discrete(expand = c(.3, .3)) +
  scale_y_reverse(breaks = c(1,5,10,15)) +
  scale_alpha_discrete(range = c(.4,.9)) +
  labs(title = "Top Chinese ports increased rank\nsubstantially from 2004 to 2014") +
  labs(subtitle = "(Port ranks, by volume)") +
  labs(x = "Year", y = "Rank") +
  theme.porttheme +
  theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +  
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#4e79a5","#f18f3b","#af7a0a","#e0585b","#5aa155","#edc958",
                                "#77b7b2","dark red", "#BBBBBB"))

graphics.off()

# Dot distribution map
map.world_polygon <- map_data("world")
head(map.world_polygon)

# Dot for all ports in dataset
df.world_ports %>%
  filter(year == "2014") %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
  geom_point(color = "dark red")

# Bubble distribution map theme:
theme.maptheeme <-
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 30)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(panel.background = element_rect(fill = "#FCFCFF")) +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = c(.17,.35)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 10))

# Relate volume to size of dots/bubbles on ports around worldmap.
df.world_ports %>%
  filter(year == "2014") %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
  geom_point(color = "dark red")

install.packages("ggalt")
library(ggalt)

df.world_ports %>% 
  filter(year == "2014") %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group),
               fill = "#AAAAAA",colour = "#818181", size = .15) +
  geom_point(aes(size = volume), color = "dark blue", alpha = 0.15) +                    
  geom_point(aes(size = volume), color = "dark blue", alpha = 0.7, shape = 1) +
  scale_size_continuous(range = c(.2,10), breaks = c(5000, 10000, 30000), 
                        name = "Shipping Volume\n(1000 TEUs)") +
  labs(title = "High volume ports were highly clustered in\nChina and Asia in 2014") +
  theme.maptheeme

graphics.off()


