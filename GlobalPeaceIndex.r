# Global Peace Index
library(rvest)
library(tidyverse)
library(stringi)
library(tidytext)
library(stringr)
library(dplyr)
library(forcats)

url <- "https://en.wikipedia.org/wiki/Global_Peace_Index"

GPI <- url %>% read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  .[[1]] %>% 
  html_table(fill = T)

head(GPI)
as.data.frame(GPI) %>% glimpse()

str(GPI)

colnames(GPI) <- colnames(GPI) %>% tolower()
colnames(GPI)

as.numeric(GPI$`2017 rank`)   # several are tied nth place == "=10" or etc..... just manually replace as not many NAs

namesGPI <- names(GPI)

GPIrank <- select(GPI, country, ends_with("rank"))

GPIrank <- GPIrank %>% gather(`2017 rank`:`2008 rank`, key = "year", value = "rank")

str(GPIrank)

levels(as.factor(GPIrank$year))  # need to take out "rank" for each.
GPIrank <- GPIrank %>% mutate(year = as.factor(year))
levels(GPIrank$year)

GPIrank <- GPIrank %>% mutate(year = fct_recode(year,
                                                           "2008" = "2008 rank", 
                                                           "2009" = "2009 rank", 
                                                           "2010" = "2010 rank",
                                                           "2011" = "2011 rank",
                                                           "2012" = "2012 rank",
                                                           "2013" = "2013 rank",
                                                           "2014" = "2014 rank",
                                                           "2015" = "2015 rank",
                                                           "2016" = "2016 rank",
                                                           "2017" = "2017 rank"))

levels(GPIrank$year)   # now as factor + no "rank" afterwards.

str(GPIrank)

# change 'rank' variable into numeric/int
GPIrank <- GPIrank %>% mutate(rank = as.integer(rank))

as.numeric(GPIrank$rank)


# fix tied ranking + as.numeric() or as.integer()???????
min_rank(GPI$rank$rank)

GPIrank <- GPIrank %>% gsub("\\=", "", GPIrank$rank)




GPI %>% select(`2017 rank`:`2008 rank`)

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
