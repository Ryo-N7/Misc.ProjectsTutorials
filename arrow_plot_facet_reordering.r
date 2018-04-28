# arrows plots

# https://github.com/kjhealy/nces-degrees/blob/master/nces.r

library(tidyverse)
library(ggalt)
library(janitor)
library(stringr)


my_colors <- function(palette = "cb") {
  
  cb.palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  rcb.palette <- rev(cb.palette)
  bly.palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
                   "#009E73", "#F0E442", "#D55E00", "#CC79A7")
  if (palette == "cb")
    return(cb.palette)
  else if (palette == "rcb")
    return(rcb.palette)
  else if (palette == "bly")
    return(bly.palette)
  else stop("Choose cb, rcb, or bly only.")
  
}


# C:/Users/Ryo Nakagawara/Documents/R_materials/Misc.ProjectsTutorials/tabn322_10_clean.csv
data <- read_csv("../R_materials/Misc.ProjectsTutorials/tabn322_10_clean.csv")


names(data)

data <- clean_names(data)

glimpse(data)

data_1 <- data %>%
  gather(key = "year", value = "count", x1970_71:x2015_16) %>% 
  group_by(year) %>% 
  mutate(yr_pct = count / sum(count) * 100) %>% 
  ungroup()

yr <- as.integer(str_extract(data_1$year, "\\d{4}"))   # extract first 4 digits (correspond to year)

data_1 <- data_1 %>% 
  add_column(yr, .before = 2) %>%     # add col + specify where to place
  select(-year) %>% 
  group_by(yr)

?add_column

data_comp <- data_1 %>% 
  filter(yr == 1995 | yr == 2015) %>% 
  select(-count) %>% 
  tidyr::spread(yr, yr_pct) %>% 
  mutate(delta = `2015` - `1995`, 
         growth = delta > 0)


# only look at fields with significant changes
area_pcts <- data_1 %>% 
  group_by(field_of_study) %>% 
  summarize(mean_pct = mean(yr_pct)) %>% 
  mutate(cutoff = mean_pct < 2)

ind <- area_pcts$field_of_study[!area_pcts$cutoff]

data_1 <- data_1 %>% 
  mutate(cutoff = field_of_study %in% ind)

p <- data_comp %>% 
  ggplot(aes(x = `1995`, 
             xend = `2015`,
             y = reorder(field_of_study, `1995`),
             yend = reorder(field_of_study, `2015`), 
             color = growth))
p

p + geom_segment(size = 0.7, 
                 arrow = arrow(type = "closed", angle = 35, length = unit(0.01, "npc"))) +
  scale_color_manual(labels = c("Decline", "Growth"), 
                     values = my_colors()) +
  labs(title = "Change in Percentage of all Bachelor's Degrees Awarded\n  by Field of Study between 1995-1996 and 2015-16",
       x = "Percentage of all Bachelor's degrees",
       y = NULL,
       color = "Direction of Change",
       caption = "Data calculated from NCES Digest 2017, Table 322.10.") +
  theme_minimal() +
  theme(legend.position = "bottom")



data_1 %>% 
  ggplot(aes(x = yr, y = yr_pct, 
             group = field_of_study)) +
  geom_line() +
  facet_wrap(~ reorder(field_of_study, -yr_pct),
             labeller = label_wrap_gen(width = 35),
             ncol = 5) +
  labs(x = "Year", y = "Percent of all BAs conferred",
       caption = "Data from NCES Digest 2017, Table 322.10.") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 6, face = "bold"))




