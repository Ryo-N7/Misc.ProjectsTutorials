library(ggplot2)
library(scales)
library(tidyverse)
library(highcharter)

data(mpg)
str(mpg)


mpgman2 <- count(mpg, manufacturer, year)

str(mpgman2)

hchart(mpgman2, "bar", hcaes(x = manufacturer, y = n, group = year),
       color = c("#FCA50A", "#FCFFA4"),
       name = c("year 1999", "Year 2008"))


species <- starwars %>% count(species, homeworld, sort = TRUE)
species
species %>% count(species, sort = TRUE)

str(iris)
Lengthyy <- iris %>% count(Sepal.Width, Species)

unique(iris$Petal.Width)
unique(iris$Sepal.Length)
unique(iris$Sepal.Width)

length(unique(iris$Petal.Length)) # 43
library(data.table)
uniqueN(iris$Petal.Length)
uniqueN(iris$Petal.Width)
length(unique(iris$Petal.Width))

hchart(Lengthyy, "bar", hcaes(x = Species, y = n, group = Sepal.Width),
       color = "blue")


seg.df <- read.csv("http://goo.gl/qw303p")

seg.df %>% group_by(ownHome) %>% summarise(meankids = mean(kids))
seg.df %>% group_by(ownHome, Segment) %>% mutate(meankids = mean(kids)) %>% hchart("bar", hcaes(x = ownHome, y = meankids, group = Segment))

seg.df <- seg.df %>% count(Segment, kids, ownHome)

hchart(seg.df, "bar", hcaes(x = Segment, y = mean(kids), group = ownHome))

seggy <- seg.df %>% filter(Segment == "Moving up")


