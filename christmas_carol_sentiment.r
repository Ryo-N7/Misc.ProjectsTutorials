library(rprojroot)
library(gutenbergr)
library(hrbrthemes)
library(stringi)
library(tidytext)
library(tidyverse)

carol_df <- gutenberg_download("46")


carol_txt <- carol_df$text


carol_txt[stri_detect_fixed(carol_txt, "STAVE")]

str(carol_txt)

sprintf("Stave %s: %s", stave, carol_txt[stri_detect_fixed(carol_txt, "STAVE")] %>% 
          stri_replace_first_regex("STAVE [[:alpha:]]{1,3}: ", ""))

# only chapters
carol_txt <- carol_txt[-(1:(which(grepl("STAVE I:", carol_txt)))-1)]


data_frame(
  stave = 1:5,
  title = sprintf("Stave %s: %s", stave, carol_txt[stri_detect_fixed(carol_txt, "STAVE")] %>% 
                    stri_replace_first_regex("STAVE [[:alpha:]]{1,3}: ", "") %>% 
                    stri_trans_totitle())
) -> stave_titles


# break text into chapters, paras, sentences, words

data_frame(txt = carol_txt) %>% 
  unnest_tokens(chapter, txt, token = "regex", 
                pattern = "STAVE [[:alpha:]]{1,3}: [[:alpha:] [:punct:]]+") %>% 
  mutate(stave = 1:n()) %>% 
  unnest_tokens(paragraph, chapter, token = "paragraphs") %>% 
  group_by(stave) %>% 
  mutate(para = 1:n()) %>% 
  ungroup() %>% 
  unnest_tokens(sentence, paragraph, token = "sentences") %>% 
  group_by(stave, para) %>% 
  mutate(sent = 1:n()) %>%
  ungroup() %>% 
  unnest_tokens(word, sentence) -> carol_tokens



# compute sentiments

inner_join(carol_tokens, get_sentiments("nrc"), "word") %>% 
  count(stave, index = para, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  left_join(stave_titles, "stave") -> carol_with_sent


# plot
library(hrbrthemes)

ggplot(carol_with_sent) +
  geom_segment(aes(index, sentiment, xend = index, yend = 0, color = title), size = 0.33) +
  scale_x_comma(limits = range(carol_with_sent$index)) +
  scale_y_comma() +
  scale_color_ipsum() +
  facet_wrap(~ title, scales = "free_x", ncol = 5) +
  labs(x = NULL, y = "sentiment",
       title = "Sentiment analysis of A Christmas Carol",
       subtitle = "By stave and chapter",
       caption = "humbug!") +
  theme_ipsum_rc(grid = "Y", axis_title_size = 8, strip_text_face = "italic", strip_text_size = 10.5) +
  theme(legend.position = "none")


















