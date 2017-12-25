library(magrittr)
library(tidyverse)
library(rvest)
library(tidytext)
library(forcats)
library(wordcloud)
library(wordcloud2)
library(rworldmap)
library(stringr)
library(ggrepel)

# get speech:

url <- "http://www.politico.com/story/2017/09/19/trump-un-speech-2017-full-text-transcript-242879"


speech_excerpt <- 
  read_html(url) %>% 
  html_nodes("style~ p+ p , .lazy-load-slot+ p , .fixed-story-third-paragraph+ p , .story-related+ p , p~ p+ p") %>% 
  html_text() %>% 
  .[-2] %>% 
  gsub("Mr.", "Mr", ., fixed = T) %>% # Make sure that dots in the text will not signify sentences
  gsub("Latin America","latin_america", .) %>% # Not to confuse with USA
  gsub("United States of America", "usa", .) %>% # USA has to be preserved as one expression
  gsub("United States", "usa", .) %>% 
  gsub("America", "usa", .) %>% 
  gsub("Britain", "uk", .) %>% # UK is mentioned 
  gsub("North Korea", "north_korea", .) %>% # North Korea should be preserved as one word for now
  data_frame(paragraph = .)    # each list element as row in "paragraph" var column


speech_sentences <- 
  speech_excerpt %>% 
  unnest_tokens(sentence, paragraph, token = "sentences")


speech_words <- speech_excerpt %>% 
  unnest_tokens(word, paragraph, token = "words") %>% 
  mutate(word = gsub("_", " ", word)) %>% 
  mutate(word = word %>% 
           str_replace_all("'s$","") %>% # Cut 's
           if_else(. == "iranian", "iran", .) %>% 
           if_else(. %in% c("usans", "north koreans"), str_replace(., "ns$",""), .) %>% 
           if_else(. %in% c("usan","syrian","african","cuban","venezuelan"), str_replace(., "n$",""), .)
  )

# manual stem names >>> difficult!



# word cloud




# most common words:

speech_words %>% 
  count(word) %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  filter(n >= 3) %>% 
  mutate(n = if_else(sentiment == "negative", -n, n)) %>% 
  ggplot() +
  aes(y = n, x = fct_reorder(word, n), fill = sentiment) +
  geom_col() +
  coord_flip() +
  labs(x = "word", 
       y ="Occurance in speech",
       title = "Most common words in Trump's 17/09/19 UN speech by sentiment")


# pos-neg word cloud:
# without having to use dcast !!!
# >> use for Thrice lyrics!!!

speech_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0L) %>%
  as.data.frame() %>% 
  remove_rownames() %>% 
  column_to_rownames("word") %>% 
  comparison.cloud(colors = c("red", "blue"))




# Load map database
map_world <- 
  map_data(map="world") %>% 
  mutate(region = region %>% str_to_lower()) # Make country name lower case to match word

# Calculate mentions of a country, and join geodata
trump_countries <-
  speech_words %>% 
  count(word) %>% 
  right_join(map_world, by = c("word" = "region")) %>% # Match country coordinates to speech
  select(region = word, everything())

# use everything() to select all vars!!!


# Get country names with the middle of the country coordinates
country_names <- 
  trump_countries %>% 
  drop_na(n) %>%
  group_by(region) %>% 
  summarise(lat = mean(lat),
            long = mean(long))


trump_countries %>% 
  ggplot() +
  aes(map_id = region, 
      x = long, 
      y = lat, 
      label = paste0(region %>% str_to_title(),": ", n)) +
  geom_map(aes(fill = n %>% log10()), 
           map = trump_countries) +
  geom_label_repel(data = trump_countries %>% 
                     drop_na(n) %>% 
                     group_by(region) %>% 
                     slice(1), 
                   alpha = .75) +
  scale_fill_gradient(low = "lightblue", 
                      high = "darkblue", 
                      na.value = "grey90") +
  labs(title = "Number of mentions by country", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none")


# Speech sentiment over time!

# Sentiment of each sentence
sentence_sentiment <-
  speech_sentences %>% 
  mutate(sentence_num = row_number(),
         sentence_length = length(sentence)
  ) %>% 
  unnest_tokens(word, sentence, "words") %>% 
  mutate(word = gsub("_", " ", word)) %>% 
  # Here comes a nasty manual stemming of country names. Sadly, I failed to get satisfactory results  on country names with standard stemmers (I tried snowballC, hunspell, and textstem). I also tried to create a custom dictionary with added country names to no avail. What am I missing? Anyway, this works.        
  mutate(word = word %>% 
           str_replace_all("'s$","") %>% # Cut 's
           if_else(. == "iranian", "iran", .) %>% 
           if_else(. %in% c("usans", "north koreans"), str_replace(., "ns$",""),.) %>% 
           if_else(. %in% c("usan","syrian","african","cuban","venezuelan"), str_replace(., "n$",""),.)
  ) %>% 
  left_join(get_sentiments("bing"), by = "word") %>%
  mutate(sentiment_score = case_when(sentiment == "positive" ~ 1,
                                     sentiment == "negative" ~ -1,
                                     is.na(sentiment) ~ NA_real_)) %>%
  group_by(sentence_num) %>%
  summarise(sum_sentiment = sum(sentiment_score, na.rm = T),
            sentence = paste(word, collapse = " "))

# GOOD example for case_when() !!!!

# Which sentence has a country name
country_sentence <- 
  speech_sentences %>% 
  mutate(sentence_num = row_number()) %>% 
  unnest_tokens(word, sentence, "words") %>% 
  mutate(word = gsub("_", " ", word)) %>% 
  right_join(country_names %>% select(region), by = c("word" = "region")) %>% 
  arrange(sentence_num)

# Sentiment for each country
country_sentiment <-         
  sentence_sentiment %>% 
  full_join(country_sentence, by = "sentence_num") %>% 
  select(region = word, sum_sentiment) %>% 
  drop_na() %>% 
  group_by(region) %>% 
  summarise(country_sentiment = sum(sum_sentiment, na.rm = T))


# calculate ROLLING MEAN of sentence sentiments instead of summarizing for each sentence
# >>> each sentence has "spillover" sentiment from previous + following sentences!

# use of rollmean() function from zoo package, with window = 3
library(zoo)

roll_sentiment <- sentence_sentiment %>% 
  full_join(country_sentence) %>% 
  mutate(roll_sentiment = zoo::rollmean(sum_sentiment, 3, fill = 0, align = "center")) %>% # Calculate a rolling mean with a window of 3
  mutate(sentiment_type = case_when(roll_sentiment > .5 ~ "positive",
                                    roll_sentiment < (-.5) ~ "negative",
                                    (roll_sentiment > -.5 & roll_sentiment < .5) ~ "neutral") %>% # Label sentence sentiments based on rolling mean
           fct_rev()
  ) 

# plot rolling sentiment!
roll_sentiment %>% 
  ggplot() +
  aes(x = sentence_num, 
      y = roll_sentiment, 
      label = word %>% str_to_title()) +
  geom_hline(yintercept = 0, 
             color = "grey", 
             linetype = "dashed", 
             size = 1.2) +
  geom_line(size = 1.2, 
            color = "black") +
  geom_label_repel(aes(fill = sentiment_type), 
                   alpha = .8, 
                   segment.alpha = 0.5) +
  scale_fill_manual(values = c("green","grey","red")) +
  theme_minimal() +
  labs(x = "Sentence number", 
       y = "Sentence sentiment", 
       title = "The summarised sentiment of sentences, and the appearance of country names in the speech \nby sentiment in sentence order",
       subtitle = "The dashed line signifies neutral sentence sentiment. \nCountry label colors show the direction of the sentiment (positive/negative)") 


# on map:

sentiment_map_data <- 
  trump_countries %>% 
  left_join(country_sentiment, by = "region")

sentiment_map_data %>% 
  mutate(country_sentiment = if_else(region == "usa", NA_real_, country_sentiment)) %>% # Exclude US
  ggplot() +
  aes(    map_id = region, 
          x = long, 
          y = lat, 
          label = paste0(region %>% str_to_title(), ": ", country_sentiment)
  ) +
  geom_map(aes(fill = country_sentiment), 
           map = trump_countries) +
  scale_fill_gradient(high = "green", 
                      low = "red", 
                      na.value = "grey90") +
  geom_label_repel(data = sentiment_map_data %>%
                     drop_na(n) %>%
                     group_by(region) %>%
                     slice(1),              # take first row for each region/country!!
                   alpha = .5
  ) +
  theme_minimal() +
  labs(title = "Sentiment of the sentences where countries were mentioned (USA excluded)", 
       x = "Longitude", 
       y = "Latitude")




# Specific emotions with NRC lexicon
library(scales)

speech_words %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  group_by(sentiment) %>% 
  count(sentiment, sort = TRUE) %>% 
  ggplot(
    aes(x = reorder(sentiment %>% str_to_title, n), y = n, label = n)) +
  geom_col() +
  geom_label(hjust = 1) +
  coord_flip() +
  theme_bw() +
  labs(title = "The occurance of words linked to distinct emotions in the speech", 
       x = "Word", 
       y = "Frequency") +
  scale_y_continuous(expand = c(0.01, 0), breaks = pretty_breaks()) 


# Emotion terms associated with EACH country:

speech_emotions <- 
  speech_sentences %>% 
  mutate(sentence_num = row_number(),
         sentence_length = length(sentence)
  ) %>% 
  unnest_tokens(word, sentence, "words") %>% 
  mutate(word = gsub("_", " ", word)) %>% 
  # Here comes a nasty manual stemming of country names. Sadly, I failed to get satisfactory results  on country names with standard stemmers (I tried snowballC, hunspell, and textstem). I also tried to create a custom dictionary with added country names to no avail. What am I missing? Anyway, this works.        
  mutate(word = word %>% 
           str_replace_all("'s$","") %>% # Cut 's
           if_else(. == "iranian", "iran", .) %>% 
           if_else(. %in% c("usans", "north koreans"), str_replace(., "ns$",""), .) %>% 
           if_else(. %in% c("usan","syrian","african","cuban","venezuelan"), str_replace(., "n$",""), .)
  ) %>% 
  left_join(get_sentiments("nrc"), by = "word") %>% 
  filter(!sentiment %in% c("positive", "negative"))


country_emotions <-
  speech_emotions %>% 
  group_by(sentence_num) %>% 
  count(sentiment) %>% 
  drop_na(sentiment) %>% 
  full_join(country_sentence, by = "sentence_num") %>% 
  select(sentence_num, region = word, emotion = sentiment, n) %>% 
  group_by(region, emotion) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  drop_na(emotion, region) %>% 
  ungroup() %>% 
  right_join(modelr::data_grid(., region, emotion), by = c("region","emotion")) %>% 
  mutate(n = if_else(is.na(n), 0L, n)) 

# use right_join to fill out regions/emotion terms not in filtered/summarized data frame
# as each country needs to have all emotion terms in the graph regardless of if 0 or not.

country_emotions %>% 
  mutate(region = region %>% str_to_title()) %>% 
  ggplot() + 
  aes(x = fct_rev(emotion), y = n) +
  geom_col(position = "dodge") +
  facet_wrap(~region) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Number of words associated with a distinct emotion",
       y = "Number of words associated with distinct emotions",
       x = NULL)








