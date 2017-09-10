# Japan PM and Unemployment Rate ------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(stringr)

# load data ---------------------------------------------------------------
japan_unemploy <- read.csv("~/R materials/Misc.ProjectsTutorials/japan_pm/LRUNTTTTJPM156S.csv", header = TRUE, stringsAsFactors = FALSE)
glimpse(japan_unemploy)

# tidy --------------------------------------------------------------------
# convert date into R date format:
japan_unemploy$DATE <- as.Date(japan_unemploy$DATE, format = "%Y-%m-%d")
colnames(japan_unemploy) <- japan_unemploy %>% 
  colnames() %>% 
  str_to_lower()

glimpse(japan_unemploy)

# change unemployment column name...
japan_unemploy <- japan_unemploy %>% rename(unemployed = lrunttttjpm156s)

glimpse(japan_unemploy)
glimpse(economics)
glimpse(presidential)

# create PM dataset -------------------------------------------------------
# wikipedia data table is too difficult to scrape
View(economics)
View(presidential)

prime_ministers <- data.frame(
  name = c("Tetsu Katayama", "Hitoshi Ashida", "Shigeru Yoshida", "Ichiro Hatoyama", "Tanzan Ishibashi",
           "Nobusuke Kishi", "Hayato Ikeda", "Eisaku Sato", "Kakuei Tanaka", "Takeo Miki",
           "Takeo Fukuda", "Masayoshi Ohira", "Zenko Suzuki", "Yasuhiro Nakasone", "Noboru Takeshita",
           "Sosuke Uno", "Toshiki Kaifu", "Kiichi Miyazawa", "Morihiro Hosokawa", "Tsutomu Hata",
           "Tomiichi Murayama", "Ryutaro Hashimoto", "Keizo Obuchi", "Yoshiro Mori", "Junichiro Koizumi",
           "Shinzo Abe (1)", "Yasuo Fukuda", "Taro Aso", "Yukio Hatoyama", "Naoto Kan",
           "Yoshihiko Noda", "Shinzo Abe (2)"),
  
  start = as.Date(c("1947-05-24", "1948-03-10", "1948-10-15", "1954-12-10", "1956-12-23",
                    "1957-02-25", "1960-07-19", "1964-11-09", "1972-07-07", "1974-12-09",
                    "1976-12-24", "1978-12-07", "1980-07-17", "1982-11-27", "1987-11-06",
                    "1989-06-03", "1989-08-10", "1991-11-05", "1993-08-09", "1994-04-28",
                    "1994-06-30", "1996-01-11", "1998-06-30", "2000-04-05", "2001-04-26",
                    "2006-09-26", "2007-09-26", "2008-09-24", "2009-09-16", "2010-06-08",
                    "2011-09-02", "2012-12-26")),
  
  end = as.Date(c("1948-03-10", "1948-10-15", "1954-12-10", "1956-12-23", "1957-02-25",
                  "1960-07-19", "1964-11-09", "1972-07-07", "1974-12-09", "1976-12-24",
                  "1978-12-07", "1980-07-17", "1982-11-27", "1987-11-06", "1989-06-03",
                  "1989-08-10", "1991-11-05", "1993-08-09", "1994-04-28", "1994-06-30",
                  "1996-01-11", "1998-06-30", "2000-04-05", "2001-04-26", "2006-09-26",
                  "2007-09-26", "2008-09-24", "2009-09-16", "2010-06-08", "2011-09-02",
                  "2012-12-26", "2017-01-01"))
)

# done!
glimpse(prime_ministers)


# Plotting ----------------------------------------------------------------
library(ggrepel)

japan_unemploy %>% 
  ggplot() +
  geom_line(aes(date, unemployed/100)) +
  geom_vline(data = prime_ministers, 
             aes(xintercept = as.numeric(start)),
             color = "blue") +
  geom_text_repel(
    data = prime_ministers %>% filter(start > japan_unemploy$date[1]),
    aes(x = start, y = 0.015, label = name)
  ) +
  scale_y_continuous(labels = percent_format()) +
  geom_hline(yintercept = mean(japan_unemploy$unemployed/100), color = "red") +
  labs(x = "Year", y = "Unemployment Rate (%)") +
  theme_bw()

# too many PM >>> REVOLVING DOOR!
# calculate total days and NOT include those with x < y ?

(prime_ministers$end - prime_ministers$start) %>% head(8)
prime_ministers <- prime_ministers %>% mutate(pm_term = (end - start))

# vertical line for each 5th year in decade as scale_x_date() is being uncooperative...

japan_unemploy %>% 
  ggplot() +
  geom_line(
    aes(date, unemployed/100)) +
  geom_vline(
    data = prime_ministers, 
    aes(xintercept = as.numeric(start)),
    color = "blue", alpha = 0.5) +
  scale_x_date(
    limits = as.Date(c("1960-01-01", "2020-01-01")),
    date_labels = "%Y"
  ) +
  geom_text_repel(
    data = prime_ministers %>% filter(start > japan_unemploy$date[1], pm_term > 730),
    aes(x = start, y = 0.06, label = name), 
    force = 15, arrow = arrow(length = unit(0.01, 'npc'))
  ) +
  scale_y_continuous(
    labels = percent_format(), 
    limits = c(0.0, 0.07),
    expand = c(0, 0)) +
  geom_hline(
    yintercept = median(japan_unemploy$unemployed/100), color = "red") +
  labs(
    x = "Year", y = "Unemployment Rate (%)") +
  theme_bw()


japan_unemploy$date[121]
japan_unemploy$date[361]
japan_unemploy$date[481]
japan_unemploy$date[601]
# limits = c(japan_unemploy$date[1], japan_unemploy$date[689]),
# breaks = c(japan_unemploy$date[1], japan_unemploy$date[121], japan_unemploy$date[361], japan_unemploy$date[481], japan_unemploy$date[601])


??glimpse
??percent_format
