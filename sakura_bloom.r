# Sakura data
library(tidyverse)
library(stringr)

sakura <- read.csv("~/R materials/Kyoto_Flowers.csv")
sakura <- sakura %>% filter(AD %in% 812:2015)
colnames(sakura)

colnames(sakura) <- sakura %>% 
  colnames() %>% 
  str_to_lower() %>% 
  str_replace_all("\\.", "_")
colnames(sakura)
colnames(sakura)[2] <- "full_flowering_day_of_year"
colnames(sakura)[1] <- "year"

sakura <- sakura %>% filter(!is.na(full_flowering_date))

date_sep <- as.character(sakura$full_flowering_date) %>% 
  str_replace_all("(.{1})(.*)", "\\1.\\2") %>% as.data.frame()
colnames(date_sep)[1] <- "date_fl"
colnames(date_sep)
date_sep <- date_sep %>% separate(date_fl, c("month", "day"), "\\.")
sakura <- bind_cols(date_sep, sakura)
sakura <- sakura %>% select(-full_flowering_date, -full_flowering_day_of_year, -x)

library(lubridate)
?make_date()

sakura <- sakura %>% 
  select(year, month, day) %>% 
  mutate(bloom = make_date(year, month, day))

sakura$DayOfYear <- as.numeric(format(sakura$bloom, "%j"))
sakura$Year <- format(sakura$bloom, "%Y")
sakura$Month <- format(sakura$bloom, "%b")
sakura$Day <- format(sakura$bloom, "%d")

ggplot(sakura, aes(x = year, y = DayOfYear)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))

ggplot(sakura, aes(x = year, y = DayOfYear)) +
  geom_point() +
  geom_smooth(span = 0.2, color = "black", fill = "red", size = 3) +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b-%d"))



#### With background image!
library(jpeg)
library(grid)
library(gridExtra)
library(cowplot)


sakura_r <- function(df = sakura, xvar = 'year', yvar = 'DayOfYear') {
  img_url <- 'http://ellarow.com/i/2017/01/sakura-tree-wallpaper-iphone.jpg'
  tmp_file <- tempfile()
  download.file(img_url, tmp_file, mode = "wb")
  img <- readJPEG(tmp_file)
  file.remove(tmp_file)
  
  rstr <- rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE)
  
  g <- ggplot(data = df)  + annotation_custom(rstr, -Inf, Inf, -Inf, Inf)
  g <- g + geom_point(aes_string(x = xvar, y = yvar))
  g <- g + geom_smooth(aes_string(x = xvar, y = yvar), span = 0.2)
  g <- g + scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))
  g <- g + theme(legend.position = "top", legend.background = element_rect(color = "blue"),
                 panel.grid = element_line(size = rel(4), color = "purple"),
                 axis.text.x = element_text(angle = 45, hjust = 1))
  return (g)
}
sakura_r()


##########################
# with just points

sakura_r <- function(df = sakura, xvar = 'year', yvar = 'DayOfYear') {
  img_url <- 'http://ellarow.com/i/2017/01/sakura-tree-wallpaper-iphone.jpg'
  tmp_file <- tempfile()
  download.file(img_url, tmp_file, mode = "wb")
  img <- readJPEG(tmp_file)
  file.remove(tmp_file)
  
  rstr <- rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE)
  
  g <- ggplot(data = df)  + annotation_custom(rstr, -Inf, Inf, -Inf, Inf)
  g <- g + geom_line(aes_string(x = xvar, y = yvar))
  g <- g + theme(legend.position = "top", legend.background = element_rect(color = "blue"),
                 panel.grid = element_line(size = rel(4), color = "purple"),
                 axis.text.x = element_text(angle = 45, hjust = 1))
  return (g)
}
sakura_r()

###############################################################

###############################################################
library(jpeg)
library(grid)
library(gridExtra)
library(cowplot)

sigh_colors <- c("#993366", "#FFFFCC", "#CCFFFF", "#660066", "#FF8080", "#0066CC", "#CCCCFF")

sighr <- function(df = mpg, xvar = 'class', fillvar = 'drv', sighmore = FALSE) {
  img_url <- 'http://ellarow.com/i/2017/01/sakura-tree-wallpaper-iphone.jpg'
  tmp_file <- tempfile()
  download.file(img_url, tmp_file, mode = "wb")
  img <- readJPEG(tmp_file)
  file.remove(tmp_file)
  
  rstr <- rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE)
  
  g <- ggplot(data = df)  + annotation_custom(rstr, -Inf, Inf, -Inf, Inf)
  g <- g + geom_bar(aes_string(x = xvar, fill = fillvar))
  g <- g + theme(legend.position = "top", legend.background = element_rect(color = "blue"),
                 panel.grid = element_line(size = rel(4), color = "purple"),
                 axis.text.x = element_text(angle = 45, hjust = 1))
  g <- g + scale_fill_manual(values = sigh_colors)
  
  if (sighmore){
    totals <- group_by_(df, xvar, fillvar) %>% count() %>% spread_(xvar, 'n')
    total_grob <- tableGrob(totals)
    g <- plot_grid(g, total_grob, nrow = 2, rel_heights = c(7/8, 1/8))
  }
  return (g)
}

sighr()
###############################################################

###############################################################

sakura %>% ggplot(aes(bloom)) + geom_freqpoly()

sakura %>% unite(date, c("month", "day"), sep = "-")
sakura %>% ggplot(aes(date, year)) + geom_line()

sak <- sakura$full_flowering_date %>% str_replace_all("(.{1})(.*)", "\\1.\\2") %>% as.data.frame()
colnames(sak)[1] <- "full_flower" 
colnames(sak)
sak <- sak %>% separate(full_flower, c("month", "day"), "\\.")




set.seed(12345)
Date <- seq(as.Date("2010/1/1"), as.Date("2014/1/1"), "week")
Y <- rnorm(n=length(Date), mean=100, sd=1)
df <- data.frame(Date, Y)
df$DayOfYear <- as.numeric(format(df$Date, "%j"))
df$Year <- format(df$Date, "%Y")
df$Month <- format(df$Date, "%b")
df$Day <- format(df$Date, "%d")

df$MonthDay <- format(df$Date, "%d-%b")



ggplot(data = df,
       mapping = aes(x = DayOfYear, y = Y, shape = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = Year ~ .) +
  theme_bw()

ggplot(data = df,
       mapping = aes(x = DayOfYear, y = Y, shape = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = Year ~ .) +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  theme_bw()






sakura %>% gsub("^([0-9]{2})([0-9]+)$", "\\1f\\2")

num <- c("24598", "34958",  "984")
sub("(.)(.)(.)", "\\1\\.\\2", num)
gsub("(.{1})(.*)", "\\1.\\2", num)

tex <- sakura %>% gsub("(.{1})(.*)", "\\1.\\2", sakura$full_flowering_date)




df <- data.frame(x = c(NA, "a b", "a. d", "b.c c"))
df %>% separate(x, c("A", "B"), sep = "1")
df %>% paste(sep = " ", collapse = "__")
             
             
str_split(sakura$full_flowering_date, pattern = "")

jed <- as.character(sakura$full_flowering_date) %>% 
  str_split(sakura$full_flowering_date, pattern = "") %>% 
  as.data.frame()

?separate()

sakura %>% as.Date(full_flowering_date, "%m/%d")
