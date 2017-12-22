library(dplyr)
library(ggplot2)
library(boot)
library(scales)
library(tidyr)
library(stringr)

getwd()
kaggle <- read.csv("C:/Users/Ryo Nakagawara/Desktop/Kaggle data/survey_results_public.csv")

glimpse(kaggle)

kaggle %>% filter(!is.na(Salary)) %>% glimpse()


kaggle_salary <- kaggle %>% 
  filter(!is.na(Salary)) %>% 
  filter(Salary > 0) %>% 
  mutate(Salary = str_replace_all(Salary, "[[:punct:]]", ""),
         Salary = as.numeric(Salary)) 

kaggle_salary %>% glimpse()

mod1 <- glm(Salary ~ Gender, data = kaggle_salary, family = poisson)

summary(mod1)

glm.diag.plots(mod1)

kaggle_salary %>% 
  filter(!is.na(Gender)) %>% 
  ggplot(aes(x = Gender, y = Salary)) +
  geom_point() +
  coord_flip() +
  geom_jitter()








