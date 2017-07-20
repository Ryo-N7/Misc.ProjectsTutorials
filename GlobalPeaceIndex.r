# Global Peace Index
library(rvest)
library(tidyverse)
library(calibrate)
library(stringi)
library(tidytext)
library(stringr)
library(dplyr)




url <- "https://en.wikipedia.org/wiki/Global_Peace_Index"

GPI <- url %>% read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  .[[1]] %>% 
  html_table(fill = T)

head(GPI)
as.data.frame(GPI) %>% glimpse()

str(GPI)

as.numeric(GPI$`2017 rank`)   # several are tied nth place == "=10" or etc..... just manually replace as not many NAs

namesGPI <- names(GPI)

GPI %>% GPI[grep("rank")]

GPI %>% GPI[grepl("rank", namesGPI)]

GPIrank <- select(GPI, ends_with("rank"))

GPI %>% select(`2017 rank`:`2009 rank`)

GPI %>% select(matches(".rank"))

GPI %>% select(-contains("score"))

GPI %>% select(-contains("score")) %>% gather(`2017 rank`:`2008 rank`, key = "year", value = "rank")


select(iris, ends_with("Length"))
select(iris, Sepal.Length)
??select()
glimpse(iris)
# need to spread COUNTRY on year and rank (rank and year as own columns)


gpi2 <- GPI %>% gather(`2017 rank`, `2016 rank`, `2015 rank`, key = "year", value = "rank")

# split rank and score, gather them on YEAR, recombine on YEAR!

url.world_ports <- url("http://sharpsightlabs.com/wp-content/datasets/world_ports.RData")

load(url.world_ports)

glimpse(df.world_ports)