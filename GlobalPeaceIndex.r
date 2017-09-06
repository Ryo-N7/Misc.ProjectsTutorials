# Global Peace Index
library(tidyverse)
library(stringi)
library(tidytext)
library(stringr)
library(forcats)
library(rvest)
library(xml2)

url <- "https://en.wikipedia.org/wiki/Global_Peace_Index"

GPI <- url %>% read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  .[[1]] %>% 
  html_table(fill = T)

head(GPI)
as.tibble(GPI) %>% glimpse()

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

GPIrank %>% mutate(rank = str_replace(rank, "\\=", ""))  # take out '=' in rank values...

GPIrank %>% head(15)

GPIrank[10:11, 3]
GPIrank[10:11, 3] <- c(10, 11)
GPIrank[10:11, 3]

GPIrank[19:20, 3]
GPIrank[19:20, 3] <- c(19, 20)
GPIrank[19:20, 3]

GPIrank[41:42, 3]
GPIrank[41:42, 3] <- c(41, 42)
GPIrank[41:42, 3]

GPIrank[84:85, 3]
GPIrank[84:85, 3] <- c(84, 85)
GPIrank[84:85, 3]

GPIrank[97:98, 3]
GPIrank[97:98, 3] <- c(97, 98)
GPIrank[97:98, 3]

GPIrank[146:147, 3]
GPIrank[146:147, 3] <- c(146, 147)
GPIrank[146:147, 3]

GPIrank[155:156, 3]
GPIrank[155:156, 3] <- c(155, 156)
GPIrank[155:156, 3]


GPIrank %>% glimpse()
GPIrank$rank <- as.numeric(GPIrank$rank)
GPIrank %>% glimpse()


GPIrank %>% 
  filter(rank <= 10) %>% 
  mutate(jpn = ifelse(country == "Japan", T, F)) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = jpn, alpha = jpn), size = 2) +
  geom_point(aes(color = jpn, alpha = jpn), size = 2.3) +
  geom_text(data = GPIrank %>% filter(year == "2008", rank <= 10), aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -0.5) +
  geom_text(data = GPIrank %>% filter(year == "2017", rank <= 10), aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 0.5) +
  scale_y_reverse(breaks = c(1, 3, 5, 7, 10)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index\n (2008-2017)") +
  theme.porttheme +
  theme(legend.position = "none")
  
theme.porttheme <-  
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank()) 


# create sub-region of East Asia


library(gapminder)
geo <- as.tibble(gapminder)

GPI_Asia <- GPIrank %>% filter(country %in% c("Japan", "China", "Korea Republic", "DPR Korea", 
                                "Philippines", "Taiwan")) %>% 
  mutate(region = "East Asia")
library(scales)
library(ggrepel)

GPI_Asia %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(data = GPI_Asia %>% filter(year == "2008"), aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -0.5) +
  geom_text(data = GPI_Asia %>% filter(year == "2017"), aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 0.5) +
  scale_y_reverse(breaks = pretty_breaks(n = 20)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (East Asia Region)\n (2008-2017)") +
  theme.porttheme +
  theme(legend.position = "none")





################
################
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
