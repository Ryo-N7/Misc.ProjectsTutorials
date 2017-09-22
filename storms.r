# Tropical storms 

library(tidyverse)
library(ggthemes)
library(ggmap)
library(htmlwidgets)

data <- read.csv("~/R_materials/Misc.ProjectsTutorials/Historical_Tropical_Storm_Tracks.csv")
names(data)
head(data)

data <- data %>% rename(ID = ï..FID)
colnames(data) <- data %>% colnames %>% tolower()
names(data)

glimpse(data)

summary(data %>% select(year,
                        month, 
                        day,
                        wind_kts,
                        pressure))

library(lubridate)
data %>% mutate(date = make_date(year, month, day))
data <- data %>% mutate(date = make_date(year, month, day))

df <- data %>% 
  filter(name != 'NOTNAMED' & name != 'SUBTROP1') %>% 
  group_by(year) %>% 
  summarise(Distinct_Storms = n_distinct(name)) # n_distinct() == better version of length(unique(x))!

plot1 <- df %>% ggplot(aes(year, Distinct_Storms)) + theme_bw()

plot1 + 
  geom_line(size = 1.1) +
  ggtitle("Numero de los Stormos en cada Ano") +
  geom_smooth(method = 'lm', se = FALSE) +
  ylab("Stormos")
# appears storms do increase over time >>> dig deeper for more details
# unique storms NOT named until 1950 onwards
# utilize IDs for unique names for storms before 1950...?

?round()
# function for percentage difference (in storms):
percent_diff <- function(x) {
  round((x - lag(x))/lag(x), 2)
}
  
# function for actual difference (in storms):
actual_diff <- function(x) {
  round((x - lag(x)), 2)
}
  
df <- data %>% 
  arrange(year) %>% 
  filter(name != 'NOTNAMED' & name != 'SUBTROP1') %>% 
  group_by(year) %>% 
  summarise(Distinct_Storms = n_distinct(name)) %>% 
  mutate(Distinct_Storm_Change = actual_diff(Distinct_Storms),
         Distinct_Storm_Percent_Change = percent_diff(Distinct_Storms)) %>% 
  na.omit() %>%    # na.omit() removes NA cases!
  arrange(year)

df$year <- as.factor(df$year)
  
head(df, 10)

summary(df %>% select(-year))
# aggregate measures may not tell whole story... group storms by categories instead
# such as only Hurricane levels, H(1-5) in dataset

df <- data %>% 
  filter(name != 'NOTNAMED' & name != 'SUBTROP1') %>% 
  filter(grepl("H", cat)) %>% 
  group_by(year, cat) %>% 
  summarise(Distinc_Storms = n_distinct(name))
df$cat <- as.factor(df$cat)
glimpse(df)

plot2 <- df %>% ggplot(aes(year, Distinc_Storms, col = cat)) + theme_bw()

plot2 +
  geom_line(size = 1.1) +
  geom_smooth(method = 'lm', se = FALSE, col = "#252525") +
  facet_wrap(~cat, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  ggtitle("Numero de los stormos cada ano (del categoria (H)") +
  labs(y = "Storms")
  
# 
df <- data %>% 
  arrange(year) %>% 
  filter(grepl("H", cat)) %>% 
  filter(name != 'NOTNAMED' & name != 'SUBTROP1') %>% 
  group_by(year) %>% 
  summarise(Distinct_Storms = n_distinct(name)) %>% 
  mutate(Distinct_Storm_Change = actual_diff(Distinct_Storms),
         Distinct_Storm_Percent_Change = percent_diff(Distinct_Storms)) %>% 
  na.omit() %>%    # na.omit() removes NA cases!
  arrange(year)

summary(df %>% select(-year))


#
df <- data %>% 
  filter(name != 'NOTNAMED' & name != 'SUBTROP1') %>% 
  filter(grepl("H", cat)) %>% 
  group_by(year) %>% 
  summarise(Distinct_Storms = n_distinct(name)) %>% 
  mutate(Distinct_Storm_Percent_Change = percent_diff(Distinct_Storms))
  
library(scales)
plot3 <- df %>% 
  ggplot(aes(Distinct_Storm_Percent_Change)) + theme_bw()

plot3 <- plot3 + 
  geom_histogram(bins = 20) +
  ggtitle("YoY % Change Density") +
  scale_x_continuous(labels = scales::percent, breaks = pretty_breaks()) +
  ylab('') + xlab('YoY % Change in Hurricanes')

plot3

plot4 <- df %>% 
  ggplot(aes(Distinct_Storm_Percent_Change)) + theme_bw() +
  geom_density(fill = "#262626", alpha = 0.5) +
  scale_x_continuous(labels = percent_format(), breaks = pretty_breaks()) +
  ggtitle("YoY % Change Density") +
  labs(x = "YoY % Change in Huracanes", y = "")

plot4

gridExtra::grid.arrange(plot3, plot4, ncol = 2)   # use gridExtra for arrange multiple plots...
# right-skew across n = 58 (years) >>> WHY???

big_map <- get_googlemap(c(lon = -95, lat = 30), zoom = 4, maptype = "terrain")
ggmap(big_map) +
  geom_point(data = data, aes(long, lat), col = "darkred", alpha = 0.1)
# unruly swarm of red locusts converging on atlantic coast...
# TOO MANY DATA POINTS, need to split and use different type of mapping >>> heat map?
df <- data %>% filter(grepl("H", cat))
ggmap(big_map) +
  geom_density2d(data = df, aes(long, lat), size = 0.5) +
  stat_density2d(data = df,
                 aes(long, lat, fill = ..level.., alpha = ..level..), size = 0.1,
                 bins = 20, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", 
                      guide = FALSE) +
  scale_alpha(range = c(0.1, 0.5), guide = FALSE)  +
  facet_wrap(~cat)
# H4 = west coast of Mexico, H5 = gulf of mexico


df <-  data %>% filter(!grepl("H", cat) & !grepl("W", cat))

ggmap(big_map) + 
  geom_density_2d(data = df, mapping = aes(x = long, y = lat), size = 0.5) + 
  stat_density2d(data = df, 
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.1, bins = 20, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
  scale_alpha(range = c(0.1, 0.5), guide = FALSE) + 
  facet_wrap(~cat)
# include TD, TS, L storms


# Relationship between wind_kts and pressure?

df <-  data %>% 
  filter(pressure > 0) %>%
  filter(grepl("H", cat)) %>%
  group_by(cat,year, month, day, lat, long) %>%
  summarise(mean_wind_kts = mean(wind_kts), mean_pressure = mean(pressure)) %>%
  arrange(mean_wind_kts)

df$cat <- as.factor(df$cat)

p <-  df %>% ggplot(aes(mean_wind_kts, mean_pressure, fill = cat)) + theme_bw()
p + 
  geom_hex(alpha = 0.8) +
  scale_fill_brewer(direction = -1, palette = "Spectral") + 
  scale_y_continuous(labels = scales::comma)+ 
  theme(legend.position = 'right') + 
  ggtitle("Wind KTS vs. Pressure by category (H)")

# negative correlation between mean_wind_kts and mean_pressure
# wind_kts is THE distinguishing element in how classify storms H1-H5


# Research:
# Most common name for a hurricane?
# Names follow alphabetical pattner over time?
# IF names = alphabetical_order, THEN num_times repeat IN year?
# merge data with FEMA, charitable donations, aid data?

top_names <- data %>%
  filter(name != 'NOTNAMED' & name != 'SUBTROP1') %>%
  group_by(name) %>%
  summarise(Years_Used = n_distinct(year)) %>%
  arrange(-Years_Used)

p <-  
  ggplot(top_names %>% top_n(10), 
         aes(x = reorder(name, Years_Used), y = Years_Used)) + 
  theme_bw()
p + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  xlab('') + 
  ggtitle('Most Used Tropical Storm Names')











