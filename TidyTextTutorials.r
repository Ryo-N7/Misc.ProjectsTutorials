# TidyText Analysis tutorials:
# Emily Dickinson excerpt:
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text
library(tidyverse)

# Transform to dataframe
text_df <- data_frame(line = 1:4, text = text)
text_df


# Breakdown text into individual tokens
library(tidytext)
text_df %>% 
  unnest_tokens(word, text)   # (output column name define, from input column name)
# unnest_tokens auto-converts to lowercase, to_lower = F if not.

library(janeaustenr)
library(stringr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenum = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divlc]", 
                                                 ignore_case = T)))) %>% 
  ungroup()

tidy_books <-  original_books %>% 
  unnest_tokens(word, text)
tidy_books

data("stop_words")
tidy_books <- tidy_books %>% 
  anti_join(stop_words)

??stop_words

# most common words
tidy_books %>% 
  count(word, sort = T)    # pass 'sort' as argument inside count function

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 700) %>%
  mutate(word = reorder(word, n)) %>%    # reorder 'word' on 'n'
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity', show.legend = F) +    # or just use geom_col()
  xlab(NULL) +
  coord_flip()

# Gutenbergr package:
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = T)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

# spread() + gather() for re-shaping
frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),   # bind rows from tidy_bronte with new column head 
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%       # str_extract()
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

?bind_rows()
library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)    # ~76% correlation

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)    # ~42% correlation




















### early 19th century poetry ###
# Yeats, TS Eliot, R. Frost, Wallace Stevens, EE Cummings, DH Lawrence.
library(tidytext)
library(stringr)
library(lubridate)
library(purrr)
library(tidyverse)
library(rvest)

# scrape poem names:
poemNameUrl <- "http://www.poetry-archive.com/y/yeats_w_b.html"

poemName <- poemNameUrl %>% 
  read_html() %>% 
  html_nodes("a font") %>% 
  html_text()

poemName

# Clean poem names:
poemName <- poemName[1:50] %>% 
  str_replace_all(pattern = "\r", replacement = "") %>% 
  str_replace_all(pattern = "\n", replacement = " ") %>% 
  str_replace_all(pattern = "[  ]{2}", replacement = "") %>% 
  str_replace_all(pattern = "[[:punct:]]", replacement = "") %>% 
  tolower()

head(poemName)
poemName[9] <- "he mourns for the change"
poemName[24] <- "the old men admiring themselves"

head(poemName, 24)

# scrape poems and dates:
nameVec <- unlist(str_split(poemName, pattern = " "))
nameVec


url <- "http://www.poetry-archive.com/y/at_galway_races.html"
date <-  url %>%
  read_html() %>%
  html_nodes("td font") %>%
  html_text()

date[1]
date[2]
date[3]
date[3] %>% str_extract(pattern = "[0-9]+")
date[1] %>% str_replace_all(pattern = "\r", replacement = "") %>% 
            str_replace_all(pattern = "\n", replacement = " ") %>% 
            tolower()



GetPoemAndDate <- function(poemName) {
  # split string at empty space and unlist the result  
  nameVec <- unlist(str_split(poemName, pattern = " "))
  
  # use result in url 
  url <- str_c("http://www.poetry-archive.com/y/", 
               paste(nameVec, collapse = "_"),
               ".html") 
  
  # scrape for poem and clean
  poem <- url %>%
    read_html() %>%
    html_nodes("dl") %>%
    html_text() %>%
    str_replace_all(pattern = "\r", replacement = "") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[  ]{2}", replacement = "")
  
  # scrape for date 
  date <- url %>%
    read_html() %>%
    html_nodes("td font") %>%
    html_text() 
  
  # clean dates
  date <- date[3] %>%
    str_extract(pattern = "[0-9]+")
  
  # pause before function return
  Sys.sleep(runif(1, 0, 1))
  
  return(list(poem = poem, date = date))
}


# for loop GetPoemAndDate()
# loop each poem's name and scrape for that particular poem + date.
# for each loop poem df and date to list.
# bind all df together into single df.

poemDataFrame <- list()
count <- 1

for (name in poemName) {
  
  
  poemNameDate <- GetPoemAndDate(name)
  
  
  poemDataFrame[[count]] <- data.frame(poem = poemNameDate$poem,
                                       date = poemNameDate$date,
                                       stringsAsFactors = F)
  count <- count + 1
}

poemDataFrame <- do.call(rbind, poemDataFrame)

poemDataFrame

poemDataFrame <- cbind(poemName, poemDataFrame)

# poem #40 has '29' as date, obvs. some kinda error...
poemDataFrame$date[40] <- "1916"







poemDataFrame %>% unnest_tokens(word, poem) %>% head()

sentiments %>% 
  filter(lexicon == "bing") %>% 
  select(-score) %>% head(5)

sentiments %>% filter(lexicon == "bing") %>% head(5)

# using bing lexicon for sentiment scores:
bingAnalysePoems <- function(i) {
  
  
  # each poem in df
  poem <- data_frame(poem = poemDataFrame$poem[i])
  
  # tokenize into separate words
  textTokenized <- poem %>% 
                   unnest_tokens(word, poem)
  
  # stop words dataset
  data("stop_words")
  
  # anti-join stopwords dataset
  tidyPoem <- textTokenized %>% 
              anti_join(stop_words)
  
  # filter on bing lexicon
  bing <- sentiments %>% 
          filter(lexicon == "bing") %>% 
          select(-score)
  
  # join tidy with bing
  poemSentiment <- tidyPoem %>% 
                   inner_join(bing)
  
  # get score for poem
  poemSentiment <- poemSentiment %>% 
                   mutate(score = ifelse(poemSentiment$sentiment == "positive", 1, -1))
  
  # score as % of total words in poem
  finalScore <- (sum(poemSentiment$score)/length(poemSentiment$word))*10
  
  return(finalScore)
}

# scores
poemDataFrame$poem %>% head(3)

# apply bingAnalysePoems() over entire length of poemDataFrame$poem (each poem)
scores <- sapply(1:length(poemDataFrame$poem), bingAnalysePoems) 

dateAndScore <- data.frame(scores) %>% 
                mutate(date = year(ymd(str_c(poemDataFrame$date, "/01/01"))))

head(dateAndScore, 10)

# NRC lexicon: for sum of # words relating to certain NRC sentiment

nrcAnalysePoems <- function(sent) {
  
  poem <- data_frame(poem = poemDataFrame$poem[i])
  
  textTokenized <- poem %>% 
                   unnest_tokens(word, poem)
  
  data("stop_words")
  
  tidyPoem <- textTokenized %>% 
              anti_join(stop_words) 
  
  nrcSentiment <- sentiments %>% 
         filter(lexicon == "nrc", sentiment == sent)
  
  sentimentInPoem <- tidyPoem %>% 
                     semi_join(nrcSentiment) %>% 
                     count(word, sort = T)
  
  return(sum(sentimentInPoem$n))
  
  
  
}

nrcscore <- sapply(1:length(poemDataFrame$poem), nrcAnalysePoems)

sentimentsVec <- c("anger", "anticipation", "disgust", "fear", 
                   "joy", "sadness", "surprise", "trust")

nrcAnalysisDataFrame <- list()

for (i in 1:length(poemDataFrame$poem)) {
  
  nrcPoem <- data_frame(poem = poemDataFrame$poem[i])
  
  nrcDataFrame <- as.data.frame(sapply(sentimentsVec, nrcAnalysePoems)) %>% 
                  rownames_to_column() %>% 
                  select(sentiment = rowname, value = 2) %>% 
                  mutate(name = poemDataFrame$poemName[i],
                         date = poemDataFrame$date[i])
  
  nrcAnalysisDataFrame[[i]] <- nrcDataFrame
  
  
}


# rbind on all dataframes within list of nrc dataframes built.
nrcAnalysisDataFrame <- do.call(rbind, nrcAnalysisDataFrame)

head(nrcAnalysisDataFrame, 15)

## repeat for other poets ##


?map()




