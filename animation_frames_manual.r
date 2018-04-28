library(tidyverse)
library(gganimate)

df <- data_frame(
  ID = factor(c(1,1,2,2,1,1,2,2,1,1,2,2), levels = 1:2),
  value = c(10,15,5,10,7,17,4,12,9,20,6,17),
  sim_category = factor(c(1,1,1,1,2,2,2,2,3,3,3,3), levels = 1:3)
) 


glimpse(df)

df2 <- df %>%
  pull(sim_category) %>% 
  levels() %>% 
  as.integer() %>%
  map_df(~ df %>% filter(sim_category %in% 1:.x) %>% mutate(sim_category = .x))






p <- df %>%
  pull(sim_category) %>% 
  levels() %>% 
  as.integer() %>%
  map_df(~ df %>% filter(sim_category %in% 1:.x) %>% mutate(sim_category = .x)) %>%
  ggplot(aes(ID, value, frame = factor(sim_category))) + 
  geom_boxplot(position = "identity")

gganimate(p)