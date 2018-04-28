library(fivethirtyeight)
library(tidyr)
library(tibble)
library(dplyr)

names(murder_2015_final)

# gather murder2014 and murder2015 into single column

murders_gathered <- murder_2015_final %>% 
  gather(key = murder_year, value = murders, murders_2014:murders_2015, na.rm = TRUE)

glimpse(murders_gathered)

# create sorted
murders_arranged <- murders_gathered %>% 
  arrange(state, city)

murders_arranged


# separate murder_year into text + year

murders_separate <- murders_arranged %>% 
  separate(murder_year, into = c("text", "year"), sep = "_")

# spread cols back to individual years
murders_spread <- murders_separate %>% 
  spread(key = year, value = murders) %>% 
  arrange(state, city)

# unite columns

murders_final <- murders_spread %>% 
  unite(city_state, city, state, sep = "_") %>% 
  arrange(city_state) %>% 
  select(-text)

# write.csv(murders_final, file = "murders_final.csv", row.names = FALSE, na = "")

# shorten state names....
murders_states <- murders_final %>% 
  separate(city_state, into = c("city", "state"), sep = "_") %>% 
  pull(state) %>% 
  unique()
  
# fill in abbreviations 

# inner_join back into murders_final



library(ggplot2)

murders_final %>% 
  ggplot(aes(x = reorder(city_state, change), y = change)) +
  geom_col() +
  ylim(c(-20, 120)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, size = 8))

















