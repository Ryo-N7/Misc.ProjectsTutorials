library(dplyr)
library(tidyr)





data_raw %>% 
  group_by(`Area Code`, Date) %>% 
  summarize(HDX1sum = n()) %>% View()




data_raw %>% 
  tally(`Area Code`, Date) 



library(dplyr)
library(tidyr)
library(readxl)

data_raw <- readxl::read_excel(here::here("mo-help/Book2.xlsx"))
  
data_count <- data_raw %>% 
  mutate(across(contains("HDX"), ~ if_else(is.na(.), 0, 1)))
  
data_count %>% 
  group_by(`Area Code`, Date) %>% 
  summarize(HDX1sum = sum(HDX1),
            HDX2sum = sum(HDX2),
            HDX3sum = sum(HDX3),
            HDX4sum = sum(HDX4)) 

## or since you have a billion columns...
likethis <- data_count %>% 
  group_by(`Area Code`, Date) %>% 
  summarize(across(starts_with("HDX"), sum, .names = "{.col}_sum")) %>% 
  ungroup()



data_raw2 <- readxl::read_excel(here::here("mo-help/Book333333.xlsx"))


## one row == one patient
## la , dee, dah, wee are different health conditions
## event == some combination of conditions, defined as:
event1 <- c("la", "dee") ## for example


event1_df <- data_raw2 %>% 
  mutate(across(contains("HDX"), ~ if_else(. %in% event1, 1, 0))) # %>% 
  # group_by(`Area Code`, Date) %>% 
  # summarize(across(starts_with("HDX"), sum)) %>% 
  # ungroup()


event1_df %>% 
  rowwise() %>% 
  group_by(`Area Code`, Date) %>% 
  summarize(across(starts_with("HDX"), sum)) %>% 
  ungroup() %>% View()





## event1: la and/or dee
## event2: wee
## event3: wee and/or dah

data_raw2 <- readxl::read_excel(here::here("mo-help/Book333333.xlsx"))

eveve <- data_raw2 %>% 
  tidyr::unite("all_conditions", 3:ncol(data_raw2), sep = ", ")

check_events <- eveve %>% 
  group_by(`Area Code`, Date) %>% 
  summarize(
    event1 = if_else(stringr::str_detect(all_conditions, "la | dee"), 1, 0),
    event2 = if_else(stringr::str_detect(all_conditions, "wee"), 1, 0),
    event3 = if_else(stringr::str_detect(all_conditions, "wee | dah"), 1, 0))

View(check_events)

summed_events <- check_events %>% 
  group_by(`Area Code`, Date) %>% 
  summarize(
    event1_sum = sum(event1),
    event2_sum = sum(event2),
    event3_sum = sum(event3)
  ) %>% 
  ungroup()

View(summed_events)





