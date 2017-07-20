# rvest
library(rvest)
library(tidyverse)
library(calibrate)
library(stringi)
library(tidytext)
library(stringr)
library(dplyr)


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

#################

movie <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(movie, "#titleCast span.itemprop")
html_text(cast)
html_name(cast)
html_attrs(cast)
html_attr(cast, "class")


#########################




# THe Simpsons side characters: 
# from wikipedia page of all episodes, take side characters

url <- "https://en.wikipedia.org/wiki/List_of_The_Simpsons_episodes_(seasons_1-20)#Episodes"

episode_urls <- url %>% 
                read_html() %>%
                html_nodes(".summary a") %>% 
                html_attr("href")
episode_urls

synopsis <- read_html(paste("https://en.wikipedia.org", episode_urls, sep = "")) %>% 
            html_nodes("p") %>% 
            html_text()
synopsis

synopsis <- read_html("http://en.wikipedia.org/wiki/Simpsons_Roasting_on_an_Open_Fire") %>% 
  html_nodes("p") %>% 
  html_text()

synopsis <- synopsis[6:7]


synopsis <- gsub("[^a-zA-Z ]", " z ", synopsis)

synopsis <- paste(synopsis, collapse = " ")  %>%
  strsplit(split = " ") %>%
  .[[1]]



ind <- grep("[A-Z]", synopsis)
synopsis <- synopsis[ind]

synopsis

# Check if two adjacent words are capitalised
merge_ind <- ind[2:length(ind)] - ind[1:(length(ind) - 1)] == 1
# If two adjacent words are capitalised we'll merge them
for(i in length(synopsis):2) {
  if(merge_ind[i - 1]) {
    synopsis[i - 1] <- paste(synopsis[i - 1], synopsis[i], sep = " ")
    synopsis[i] <- ""
  }
}

synopsis

synopsis <- synopsis[synopsis != ""]   # erase blanks
synopsis <- unique(synopsis)           # erase duplicate names

synopsis


synopsis <- gsub(".*Bart*", "Bart", synopsis)
synopsis

# create list of character names then anti-join????






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






# Staaaar Waaaaarss
library(rvest)
library(stringr)
library(calibrate)
library(stringi)
library(tidytext)
library(tidyverse)

url <- ("http://imsdb.com/scripts/Star-Wars-A-New-Hope.html")

newhope <- url %>% read_html() %>% 
  html_nodes(xpath = '//pre') %>%
  .[[1]] %>% 
  html_text()

str(newhope)

newhope <- data.frame(newhope)

# .scrtext > pre:nth-child(1)
# //pre

newhopeTidy <- newhope %>% 
  str_replace_all("\n", "") %>% 
  str_replace_all("[^a-zA-Z\\s]", " ") %>% 
  str_replace_all("[\\s]+", " ") %>% 
  tolower() %>% 
  trimws("both")

str(newhopeTidy)
ls(newhopeTidy)
names(newhopeTidy)


newhopedf <- data.frame(script = newhopeTidy, stringsAsFactors = FALSE)   # STRINGASFACTORS = FALSE!!!!!

newhopeTokenized <- newhopedf %>% unnest_tokens(word, script)

str(newhopeTokenized)




.scrtext > pre:nth-child(1)

