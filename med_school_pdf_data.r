# pdf text extraction and tidying
library(pdftools)
library(tidyverse)
library(stringr)

txt <- pdf_text("http://goo.gl/wUXvjk")

txt %>% head(n = 1)


pattern <- "([0-9]{4} [M\\.|Mme|Mlle]{1}.*?, [né|neé]{1}.*?)\\."
#           [digits]{matches exactly n = 4 times}
#                   escape '.'    {matches exactly n = 1 times}
#                               any chr, match at least 0 times and at most one time
#                                                 né OR neé {matches exactly n = 1 times}
#                               any chr, match at least 0 times and at most one time
#                                                                            escape '.'

# gsubfn package:
library(gsubfn)       # similar to gsub, instead - usage function > replacement string
                      # uses matched text as input, emits replacement text from function run on it
?strapply             # apply function over string(s), treutnrs output of the function()
                      # pattern = ____    chr string of regex to be matched in any given chr vector

data <- unlist(gsubfn::strapply(txt, pattern = pattern))
?unlist()   # given list structure, simplify to produce vector with all atomic components in 'x'

head(data, 5)

# Stringr
?matrix()
data_parsed <- matrix(NA_character_, length(data), 7)
# create matrix with row = length(data), column = 7

?boundary()
data_words <- str_extract_all(data, boundary("word"))


words <- c("These are   some words.", "homina homina homina")
str_count(words, boundary("word"))   # 4 words
str_split(words, " ")[[1]]           # split
str_split(words, " ")[[2]]           # split
str_split(words, boundary("word"))[[1]]  # split only "word"
str_split(words, boundary("word"))[[2]]

# data_parsed[, 1:7] <- t(sapply(data_words, head, n = 7))
data_parsed[, 1:4] <- t(sapply(data_words, head, n = 4))     # ranking, gender prefix, last name, first name
data_parsed[, 5:7] <- t(sapply(data_words, tail, n = 3))     # day, month, year
# or else include the ne and nee!
?t()  # trasnpose of 'x', need to transpose or each word go into subsequent row of same column!

head(data_parsed)
#       [,1]   [,2]  [,3]           [,4]        [,5] [,6]        [,7] 
# [1,] "0001" "Mme" "Beaumont"     "Anne"      "1"  "septembre" "1993"
# [2,] "0002" "M"   "Petitdemange" "Arthur"    "15" "septembre" "1993"
# ~~VOILA~~
as.tibble(data_parsed)   # for column vars names

library(purrr)

data_parsed %>% as_tibble() %>% mutate(birth_date = pmap(list(V5, V6, V7), function(d, m, y) {
  paste(d, m, y, collapse = "")
}) %>% lubridate::dmy()
)
# NOT WORK because "months" in french...  ??
data_parsed %>% as.tibble %>% dplyr::select(V6)
data_parsed <- data_parsed %>% as_tibble()
data_parsed %>% select(V6)
data_parsed %>% select(V6) %>% n_distinct()  # 12 distinct for 12 months duh
data_parsed %>% select(V6) %>% distinct()

data_parsed$V6 <- as.factor(data_parsed$V6)
glimpse(data_parsed)

library(forcats)
levels(data_parsed$V6)

data_parsed$V6 <- data_parsed$V6 %>% 
  fct_recode("january" = "janvier",
             "february" = "février",
             "march" = "mars",
             "april" = "avril", 
             "may" = "mai",
             "june" = "juin",
             "july" = "juillet",
             "august" = "août",
             "september" = "septembre",
             "october" = "octobre",
             "november" = "novembre",
             "december" = "décembre"
)

?fct_recode

data_parsed_tidy <- as_tibble(data_parsed) %>% 
  transmute(
    ranking = as.integer(V1),
    is_male = (V2 == "M"),
    family_name = V3,
    first_name = V4,
    birth_date = pmap(list(V5, V6, V7), function(d, m, y) {
      paste(d, m, y, collapse = "")
    }) %>% lubridate::dmy()
  )

head(data_parsed_tidy)
sum(is.na(data_parsed_tidy$birth_date))

complete()

mean(data_parsed_tidy$is_male)
# 0.4345     43.5% is male!
library(scales)

data_parsed_tidy %>% 
  ggplot() +
  geom_histogram(aes(birth_date), bins = 100) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_date(breaks = pretty_breaks(n = 10))

# mutate(actual age?)
glimpse(data_parsed_tidy)
data_parsed_tidy$birth_date %>% as.character() %>% str_extract(pattern = "[0-9]{4}")

data_parsed_tidy <- data_parsed_tidy %>% mutate(birth_year = (birth_date %>% as.character() %>% str_extract(pattern = "[0-9]{4}")),
                            age = (2017 - as.numeric(birth_year)))
glimpse(data_parsed_tidy)
summary(data_parsed_tidy$age) # min. age = 20, max. age = 54   !

cummean(data_parsed_tidy$is_male) # proportion of males AT each new observation 0.00 as Rank 1 = Female!
mean(data_parsed_tidy$is_male)    #  0.43   as above...

data_parsed_tidy %>% 
  mutate(prop_male = cummean(is_male)) %>% 
  ggplot() +
  geom_line(aes(ranking, prop_male)) +
  geom_hline(yintercept = mean(data_parsed_tidy$is_male), col = "orange", size = 1.1)

(data_parsed_tidy %>% 
  ggplot() +
  geom_point(aes(ranking, birth_date, color = is_male)) + 
  aes(text = asPlotlyText(data_parsed_tidy))) %>% 
  plotly::ggplotly(tooltip = "text")

data_parsed_tidy %>% 
  ggplot(aes(ranking, birth_date)) +
  geom_point() +
  geom_smooth(method = 'gam', aes(color = is_male), lwd = 0.8)

?geom_smooth





