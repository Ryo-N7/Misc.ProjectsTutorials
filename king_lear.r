library(gutenbergr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tibble)

g <- gutenberg_metadata %>% 
  filter(title == "King Lear", has_text == TRUE)
g
# 2266

gutenberg_works(author == "Shakespeare, William")


king_lear <- gutenberg_download(2266)

glimpse(king_lear)

# cut out foreword/project notes:
king_lear <- king_lear %>% 
  rownames_to_column() %>% 
  filter(!rowname %in% 1:92)

# cut out blank spaces, not necessary because unnest_tokens() anyways but...
king_lear <- king_lear %>% filter(text != "")

# remove old rownames, re-add in for non-blank
king_lear <- king_lear %>% 
  select(-rowname) %>% 
  rownames_to_column()

# actus == 5 for Act I-V, as it should!
king_lear %>% unnest_tokens(word, text) %>% filter(word == "actus")

# scaena, scena, scoena ... Early Modern typesetting ugh.
king_lear %>% unnest_tokens(word, text) %>% filter(word == "scaena")

king_lear %>% filter(rowname %in% 1265:1270)

# exeunt, exit
king_lear %>% unnest_tokens(word, text) %>% filter(word == "exeunt")
king_lear %>% unnest_tokens(word, text) %>% filter(word == "exit")



king_tokens <- king_lear %>% unnest_tokens(word, text)

king_tokens %>% count(word, sort = TRUE)

king_tokens %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>% 
  top_n(50) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip()


