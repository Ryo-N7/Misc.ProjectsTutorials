library(rvest)
library(dplyr)
library(tm)
library(tidytext)
library(DT)
library(ggplot2)
library(ggthemes)


swIV_url <-"http://www.imsdb.com/scripts/Star-Wars-A-New-Hope.html"
startrek_url <- "http://www.dailyscript.com/scripts/startrek01.html"


star_wars <- swIV_url %>% 
  read_html() %>% 
  html_nodes("td") %>% 
  html_text() %>% 
  .[[88]]

star_trek <- startrek_url %>% 
  read_html() %>% 
  html_nodes("pre") %>% 
  html_text()

# clean strings function
library(stringr)


clean_text <- function(x) {
  x <- str_replace(x, "\\\n", " ")
  x <- str_replace(x, "\\\r", " ")
  x <- str_replace(x, "\\\t", " ")
  x <- str_replace(x, "[[:punct:]]", " ")
  x <- x %>% 
    tolower() %>% 
    removeNumbers() %>% 
    stripWhitespace()
  
  x
}

# now as clean vectors
clean_star_trek <- clean_text(star_trek)
clean_star_wars <- clean_text(star_wars)



# stop words
data("stop_words")

swthingy <- clean_star_wars %>% 
  as_tibble()

sw_tokens <- clean_star_wars %>% 
  as_tibble() %>% 
  rename_(sw_text = names(.)[1]) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(swt = unlist(sw_text)) %>% 
  unique() %>% 
  unnest_tokens("word", sw_text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  rename(sw_n = n)


st_tokens <- clean_star_trek %>%
  as_tibble() %>% 
  rename_(st_text = names(.)[1]) %>%
  mutate_if(is.factor, as.character) %>% 
  mutate(stt = unlist(st_text)) %>% 
  unique() %>% 
  unnest_tokens("word", st_text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  rename(st_n = n)


# join common words between scripts

final_tokens <- sw_tokens %>% 
  inner_join(st_tokens) %>% 
  mutate(log_word_prop = (sw_n / st_n) %>% log(3) %>% round(),
         dominates_in = if_else(log_word_prop > 0, "star wars", "star trek") %>% as.factor())

# positive == ^frequency in Star wars 
# negative == ^frequency in Star trek
# zero == occur same frequency Star wars and star trek

set.seed(8)

final_tokens %>% 
  filter(abs(log_word_prop) == 0) %>% 
  arrange(desc(sw_n)) %>% 
  sample_n(30) %>% 
  ggplot(aes(reorder(word, log_word_prop), log_word_prop, fill = dominates_in)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  coord_flip() +
  xlab("") + ylab("log(word_prop)") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Sample of words occur ~similar frequency in SW and ST")


# largest differences between scripts

final_tokens %>% 
  filter(abs(log_word_prop) > 2.4) %>% 
  filter(!word %in% c("ext", "int")) %>% 
  ggplot(aes(reorder(word, log_word_prop), log_word_prop, fill = dominates_in)) +
  geom_col() +
  theme_minimal() +
  coord_flip() +
  xlab("") + ylab("log(word_prop)") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Words of opposing frequencies in SW and ST")

# SW: death, wing, tie, fighter, trench, target
# ST: viewer, cloud, angle, captain, carbon, console



# Word frequency in movie <><><> word proportion
library(ggrepel)

final_tokens %>% 
  mutate(dominant_freq = if_else(sw_n > st_n, sw_n, st_n) %>% as.numeric) %>% 
  filter(!word %in% c("ext", "int")) %>% 
  filter(dominant_freq > 50) %>% 
  ggplot(aes(log_word_prop, dominant_freq, label = word)) +
  geom_label_repel(aes(fill = dominates_in), color = "white", fontface = "bold") +
  theme_minimal() +
  xlab("log(word_prop)") + ylab("Word frequency in dominating movie") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle("Word frequency by word proportion in SW and ST")













