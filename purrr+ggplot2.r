install.packages("pwt9")
library(pwt9)

data("pwt9.0")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(purrr)

country_list <- c("France", "Germany", "United States of America", 
                  "Luxembourg", "Switzerland", "Greece")

small_pwt <- pwt9.0 %>%
  filter(country %in% country_list)

glimpse(small_pwt)

small_pwt <- small_pwt %>%
  mutate(country = factor(country, levels = country_list, ordered = TRUE))

small_pwt %>% group_by(country)

plots <- small_pwt %>%
  group_by(country) %>%
  nest() %>%
  mutate(plot = map2(data, country, ~ggplot(data = .x) + theme_tufte() +
                       geom_line(aes(y = avh, x = year)) +
                       ggtitle(.y) +
                       ylab("Year") +
                       xlab("Average annual hours worked by persons engaged")))

glimpse(plots)





