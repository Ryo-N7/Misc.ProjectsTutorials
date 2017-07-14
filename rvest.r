# rvest
library(rvest)
library(tidyverse)
library(calibrate)
library(stringi)
library(tidytext)
library(stringr)

url <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"

population <- url %>%
  read_html() %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()

head(population)
population %>% rownames_to_column(var = "hmm")


#############

#############

wiki <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_proven_oil_reserves")

oil <- wiki %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(header = NA)

oil[1:10,]    # select columns 1-10

oil <- oil[, c(1,3)]
oil <- oil[2:101, ]

names(oil) <- c("country", "reserves")   # rename first few columsn
oil[1:10, ]
oil[101, ]

oil %>% rownames_to_column(var = "pseudo")  # hmmmmmmmmmmmm.........
print(oil, row.names = FALSE)


# oil <- as_tibble(oil)  # dont unless you really want to for later.
# oil
# glimpse(oil)

# oil <- slice(oil, 2:102)
# ?slice()
# oil <- filter(2:102)

?gsub()
oil[ , 1]   # annoying (see: Oil reserves in "x") blurb next to country name...
oil[ , 1] <- gsub("\\((.*)", "", oil[ , 1])
oil[ , 1]

oil[, 2]    # let's remove commas
oil[, 2] <- gsub(",", "", oil[, 2])
oil[, 2]
# there's one pesky point that has (2016) next to the value for some reason...
oil[, 2] <- gsub("\\((.*)", "", oil[ , 2])
oil[, 2]


# only for if you include rank....
oil[, 3] <- gsub("\\((.*)", "", oil[, 3])  # remove parentheses from USA's row...
oil[, 3]




## IMDB data ####

lego_movie <- html("http://www.imdb.com/title/tt1490017")

rating <- lego_movie %>% 
  html_nodes("strong span") %>% 
  html_text() %>% 
  as.numeric()

rating    # 7.8


ratingOf <- lego_movie %>% 
  html_nodes(".ratingValue > span:nth-child(3)") %>% 
  html_text() %>% 
  as.numeric()

ratingOf   # rating out Of: 10!


cast <- lego_movie %>% 
  html_nodes("#titleCast .itemprop span") %>% 
  html_text()

cast   # Will Arnett    Elizabeht Banks     Craig Berry   .........


# Who are the Directors???
# div.credit_summary_item:nth-child(2) > span:nth-child(2) > a:nth-child(1) > span:nth-child(1)
# div.credit_summary_item:nth-child(2) > span:nth-child(3) > a:nth-child(1) > span:nth-child(1)

director1 <- lego_movie %>% 
  html_nodes("div.credit_summary_item:nth-child(2) > span:nth-child(2) > a:nth-child(1) > span:nth-child(1)") %>% 
  html_text()

director1   # Director: Phil Lord

director2 <- lego_movie %>% 
  html_nodes("div.credit_summary_item:nth-child(2) > span:nth-child(3) > a:nth-child(1) > span:nth-child(1)") %>% 
  html_text()

director2

# Need to clean text!
directors <- lego_movie %>% 
  html_nodes("div.credit_summary_item:nth-child(2)") %>% 
  html_text()

directors

directors %>% gsub(",", "", directors)
directors %>% grep(",", "")
directors %>% paste(collapse = "\n ")
directors_clean <- directors %>% 
  str_replace_all("\n", "") %>%
  str_replace_all("Directors:", "") %>% 
  str_replace_all("[^a-zA-Z\\s]", " ") %>% 
  str_replace_all("[\\s]+", " ") %>% # shrink all white spaces to one each.
  trimws("both")

directors_clean
#   str_replace_all("[^a-zA-Z\\s]", " ")  to replace ALL non-numbers/non-letters

# trimws(directors_clean, "both")    both for leading and ending spaces

# need to str split each director into separate " " 



# CSS: ".poster > a:nth-child(1) > img:nth-child(1)"
poster <- lego_movie %>% 
  html_nodes(".poster > a:nth-child(1) > img:nth-child(1)") %>% 
  html_attr("src")

poster   # link to The Lego Movie poster!


review <- lego_movie %>% 
  html_nodes("#titleUserReviewsTeaser p ") %>% 
  html_text()

review   # Extract first review!








