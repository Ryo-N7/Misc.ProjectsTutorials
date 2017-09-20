# HP tidytext tutorials

library(harrypotter)
library(tidyverse)
library(tidytext)
library(stringr)

philosophers_stone[1]
glimpse(philosophers_stone)

# convert into data frame/ tibble
text_tb <- tibble(chapter = seq_along(philosophers_stone),
                  text = philosophers_stone)
glimpse(text_tb)
seq_along(philosophers_stone)

# unnest text
text_tb <- text_tb %>% unnest_tokens(word, text)
# unnest_tokens(): splits txt into single words, strips punct, converts each word to lower_case

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

series %>% 
  filter(book == "Prisoner of Azkaban") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T)


glimpse(stop_words)
names <- data.frame(word = c("harry", "ron", "hermione", 
                                   "lupin", "hagrid", "snape", 
                                   "dumbledore", "mcgonagall", "sirius"))
glimpse(names)
series %>% 
  filter(book == "Prisoner of Azkaban") %>% 
  anti_join(names) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T) %>% 
  top_n(10)


































































































