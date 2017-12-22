library(readr)
library(dplyr)
library(readxl)

getwd()
jpn_sports <- read_xlsx("jpn_sports_participation.xlsx", skip = 14)


jpn_sports_df <- read_csv("jpn_sports_particip.csv")




glimpse(jpn_sports)

glimpse(jpn_sports_df)

# take out the columns in japanese (that are all gibberish anyways)
jpn_sports_df <- jpn_sports_df %>% select(-X1, -X2, -X3)

# take out the top 3 rows
jpn_sports_df <- jpn_sports_df %>% slice(-c(1, 2, 3, 4))


jpn_sports_df %>% remove_rownames()

# NOTE: all populations in thousands except for sample size!!!
colnames(jpn_sports_df) <- c("gender", "region", "densely_inhabited_district", "sample_size", 
                             "est_population", "total_all_sport", "baseball", "softball", 
                             "volleyball", "basketball", "soccer", "table_tennis", "tennis", 
                             "badminton", "golf", "judo", "kendo", "gateball", "bowling", 
                             "fishing", "swimming", "ski_snowboard", "hiking", "cycling", 
                             "jogging", "walking_light_exercise", "weight_training",
                             "Other")

colnames(jpn_sports_df)

# no NAs in data >>> NOICE
jpn_sports_df %>% is.na() %>% sum()


glimpse(jpn_sports_df)



jpn_sports_df %>% mutate(est_population = as.numeric(est_population))


jpn_sports_df %>% 
  select(est_population:Other) %>% 
  str_replace_all(",", "") 

jpn_sports_df %>% 
  select(est_population) %>% 
  gsub(",", "") %>% 
  as.numeric()

jpn_sports_df %>% gsub(est_population, ",", "") %>% as.numeric()

gsub(jpn_sports_df$est_population, ",", "")


class(jpn_sports_df)

jpn_sports_df %>% 
  mutate(est_population = est_population %>% str_replace(",", "") %>% as.numeric())

fix <- jpn_sports_df %>% 
  select(est_population:Other) %>% 
  mutate_all(funs(str_replace(., ",", ""))) %>% 
  mutate_all(funs(as.numeric)) %>% 
  mutate_all(funs(. * 1000))


fix %>% is.na() %>% sum()
fix %>% slice(432) %>% select(judo, kendo, golf, soccer)













