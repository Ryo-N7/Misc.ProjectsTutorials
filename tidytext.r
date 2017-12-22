library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)


original_books <- austen_books() %>% 
                                    group_by(book) %>% 
                                    mutate(line_number = row_number(),
                                           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                                                   ignore_case = TRUE)))) %>% 
                                    ungroup()
original_books

original_books

tidy_books <- original_books %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_books %>% 
  count(word, sort = TRUE)

library(ggplot2)

tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +   # or just use geom_col()
  xlab(NULL) +
  coord_flip()


library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_hgwells %>% 
  count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 9182, 767))

tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_bronte %>% 
  count(word, sort = TRUE)


# time, eyes, hand  = common in BOTH...

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%   # some have underscores for italics emphasis
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(key = author, value = proportion) %>% 
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)   # doing Jane Austen vs. ELSE

library(scales)

frequency %>% ggplot(aes(proportion, `Jane Austen`, 
                         color = abs(`Jane Austen` - proportion))) + # prop in JA minus in ELSE author's
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(), breaks = pretty_breaks(5)) +
  scale_y_log10(labels = percent_format(), breaks = pretty_breaks(5)) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)

# words in Austen-Bronte closer to zero-slope line > HG Wells
# Austen-Bronte words reach lower frequencies > HG Wells

# correlation between Bronte and Austen?
cor.test(data = frequency[frequency$author == "Bronte Sisters", ], 
         ~ proportion + `Jane Austen`)

# cor = 0.74 !     p < 2.2e-16

# correlation between Wells and Austen?
cor.test(data = frequency[frequency$author == "H.G. Wells", ], 
         ~ proportion + `Jane Austen`)
# cor = 0.42       p < 2.2e-16



# Ch. 2 Sentiment Analysis ------------------------------------------------

library(tidytext)

sentiments

# NRC: Positive, Negative >>> Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, Trust

# BING: Positive or Negative

# AFINN: scores from -5 (Negative) to +5 (Positive)

get_sentiments("afinn")

# try use domain-specific lexicons if available.

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = text)

# dont take out stop_words as many lexicons contain "good", "bad" etc... need for sentiments

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_joy

tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)


library(tidyr)

# count sentiments for each 80 lines = 1 index chunk, each index should have 2 rows (pos, neg)
# spread for `positive` and `negative` as own column
# calculate net sentiment score

jane_austen_sentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(key = sentiment, value = n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

library(ggplot2)

jane_austen_sentiment %>% ggplot(aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Compare sentiment lexicons

pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarize(sentiment = sum(score)) %>%   # sum of -x and +x in afinn lexicon!
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>% 
    mutate(method = "Bing"),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc")) %>% 
    filter(sentiment %in% c("positive", "negative")) %>%  # take out anger, joy, etc...
    mutate(method = "NRC")) %>% 
  count(method, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# combine all lexicons
bind_rows(afinn,
          bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


# afinn = highest absolute positive 
# bing = lowest absolute negative
# nrc = heavy bias positive!

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

# negative  3324
# positive  2312

get_sentiments("bing") %>% 
  count(sentiment)

# negative  4782
# positive  2006

# bing has higher ratio of negative to positive than nrc!


# most common pos-neg

bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

bing_word_counts

bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# miss == not as in "i miss you" but as in "miss jackson", "Ms. Joan" etc.
# custom change code in stop_words

# can't change "miss" to neutral as there are still many instances of "i MISS you" etc.
# so filter out as a stop_word ?

custom_stop_word <- bind_rows(data_frame(word = c("miss"),
                                         lexicon = c("custom")), 
                              stop_words)
custom_stop_word

# word clouds
library(wordcloud)

tidy_books %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

??with()
# with(mtcars, mpg[cyl == 8  &  disp > 350])
# >>> is the same as, but nicer than <<<
# mtcars$mpg[mtcars$cyl == 8  &  mtcars$disp > 350]

# comparison cloud: pos-neg
library(reshape2)

tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%   # spread into matrix
  comparison.cloud(colors = c("darkred", "darkgreen"),
                   max.words = 200)

# Non-word units: sentences, lines, chapters, +ngrams, etc.

# sentimentr, coreNLP, cleanNLP packages

PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(output = sentence, input = text, token = "sentences")
# "mr.", "mrs." is counted as a sentence.... -_-"
# dialogue problems too...

??iconv

PandP_sentences <- data_frame(text = prideprejudice) %>% 
  iconv(text, to = 'latin1') %>% 
  unnest_tokens(output = sentence, input = text, token = "sentences")

# split tokens with regex pattern!

austen_chapters <- austen_books() %>% 
  group_by(book) %>% 
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>% 
  ungroup()

# how many chapters in each book?
austen_chapters %>% 
  group_by(book) %>% 
  summarize(chapters = n())

bing_negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>% 
  group_by(book, chapter) %>% 
  summarize(words = n())


# most negative chapter in each book (by ratio of neg words / all words)

tidy_books %>% 
  semi_join(bing_negative) %>% 
  group_by(book, chapter) %>% 
  summarize(negative_words = n()) %>% 
  left_join(wordcounts, by = c("book", "chapter")) %>% 
  mutate(ratio = negative_words / words) %>% 
  filter(chapter != 0) %>%    # chapter 0 is basically title and author name
  top_n(1) %>% 
  ungroup()




# Chapter 3: Word and Document Frequency >>> tf-idf -----------------------

# using stop_words to filter out common words not optimal when stop_words may be
# important in context of the document in question.
# Solution: inverse document frequency (idf)
# decrease the weight for commonly used words + increase weight for words not used commonly
# within the document or collection of documents as a whole. 
# combine with term frequency (tf) for tf-idf
# tf-idf: term frequency with adjustment for commonality in document

# idf(term) = ln( n-documents    /   n-documents  containing term    )

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = TRUE) %>% 
  ungroup()

book_words

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

total_words

book_words <- left_join(book_words, total_words)

book_words

# n = number of times word is used in that book. 

library(ggplot2)

book_words %>% ggplot(aes(n/total, fill = book)) + 
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# long right tails =  extremely common words!
# similar pattern across books
# >>> many words occur rarely, few words occur frequently!

# Zipf's Law: 
# frequency of word appearance is inversely proportional to rank

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(),         # can use row_number() as already ordered by n!
         `term frequency` = n / total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# negative slope of rank vs. frequency, as per Zipf's Law!
# not constant slope... >>> first section, very smooth second section, third section
# broken power law?

rank_subset <- freq_by_rank %>% 
  filter(rank < 500, 
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# Coefficients:
#   (Intercept)  log10(rank)  
#     -0.6226      -1.1125 

# slope close to -1

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  geom_abline(intercept = -0.6226, slope = -1.1125, color = "darkred", linetype = 2, size = 2) +
  scale_x_log10() +
  scale_y_log10()

# use for compare authors/collections/etc.

# calculation for tf-idf for important but not TOO common words!
bind_tf_idf()

book_words
# contains word, n, total

book_words <- book_words %>% 
  bind_tf_idf(word, book, n)

book_words
# for extremely common words such as "the", "to", "and" 
# the idf and tf-idf is ZERO.

# highest tf-idf?

book_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))
# mainly proper nouns: Elinor, Marianne, Crawford, Darcy, Elliot....
# >>> names that are VERY important for respective novel.
# none of those proper nouns appear in all novels, characteristic/important specifically
# for that text it appears in!

book_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = book)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
  
# what distinguishes one JA novel from another is the people/places
# tf-idf: ID the important words for one doc in collection of docs!

# With Physics texts:

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001), 
                              meta_fields = "author")

physics_words <- physics %>% 
  unnest_tokens(word, text) %>% 
  count(author, word, sort = TRUE) %>% 
  ungroup()

physics_words

physics_tf_idf <- physics_words %>% 
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>%   # sort(x, decreasing = TRUE)
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan",
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))
physics_tf_idf

physics_tf_idf %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# eq? k1? ac? rc? 

library(stringr)

physics %>% 
  filter(str_detect(text, "eq\\.")) %>% 
  select(text)
# eq. is demonstration signifier for graphic in the text book... -_-

physics %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)
# K1 refers to a certain coordinate system...!

physics %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)
# AK refers to a rays/angles/circles in Huygens...!

# custom word list to filter out with anti_join()

my_stop_word <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                    "fig", "gif", "file", "cg", "cb", "cm"))

physics_words <- physics_words %>% anti_join(my_stop_word)  # by = word

physics_tf_idf_2 <- physics_words %>% 
  bind_tf_idf(word, author, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>%   # sort(x, decreasing = TRUE)
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
# 2nd attempt after filter out stopwords
physics_tf_idf_2 %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# more meaningful words show up! success!



# Chapter 4: n-grams and correlations -------------------------------------

# tokenization into consecutive sequences of words >>> ngrams 
unnest_tokens(token = "ngrams", n = X)

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams
# overlap of tokens!

austen_bigrams %>% 
  count(bigram, sort = TRUE)

# split column into multiple columns:
# split bigrams over into two separate columns for each ngram
# >>> word1 and word2 for bigrams

library(tidyr)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word &
           !word2 %in% stop_words$word) 

bigrams_filtered

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
# names with salutation or first/last most common pairings!

# use unite() when want to stick them back together again

bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")

bigrams_united

# trigrams, n = 3
austen_books() %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word & 
         !word2 %in% stop_words$word &
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = TRUE)

# "dear miss woodhouse" appears 23 times! maybe a letter/telegram?

# names of streets appear most often?
bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(bigram, tf_idf), tf_idf, fill = book)) + 
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


# Negation words

bigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)
# not be, not to, not have, not know, ...etc.

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, score, sort = TRUE) %>% 
  ungroup()

not_words
# not like = positive +2  >>> obviously incorrect...

not_words %>% 
  mutate(contribution = n * score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) + 
  xlab("Words preceded by NOT ") +
  ylab("Sentiment scores * number of occurences") +
  coord_flip()

# "not like" and "not help" = largest cause of misidentification!

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = TRUE) %>% 
  ungroup()

negated_words

negated_words %>% 
  mutate(contribution = n * score,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>% 
  group_by(word1) %>% 
  top_n(10, abs(contribution)) %>% 
  ggplot(aes(word2, contribution, fill = n * score > 0)) +
  facet_wrap(~ word1, scales = "free") +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Word preceded by negation term") +
  ylab("sentiment score * number of occurences") + 
  coord_flip()


negated_words %>%
  mutate(contribution = n * score,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * score > 0)) +
  facet_wrap(~ word1, scales = "free") +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment score * # of occurrences") +
  coord_flip()



#### Bigram networks with ggraph ####

# visualize ALL relationships between words SIMULTANEOUSLY
# Components:
# from: node an edge is coming from
# to: node an edge is going toward
# weight: numeric value associated with each edge

library(igraph)
bigram_counts

bigram_graph <- bigram_counts %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# miss, lady, sir, colonel >>> names
# half >>> hour, thousand >>> pounds, etc.

# More Options:
# edge_alpha() for transparency/opacity on common/rare bigrams
# add directionality with arrrow = __  and grid::arrow(), end_cap = __
# network theme, ex. theme_void()

set.seed(2016)
library(grid)

a <- arrow(type = "closed", length = unit(0.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Markov chain: each choice of word depend ONLY on previous word

# with ALL >>> will take quite long...
# bigram_all <- bigram_counts %>% filter(n > 5) %>% graph_from_data_frame()

ggraph(bigram_all, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Create functions for bigrams and visualization ####
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigram <- function(dataset) {
  dataset %>% 
    unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word, 
           !word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams){
  set.seed(2016)
  a <- arrow(type = "closed", length = unit(0.15, "inches"))
  
  bigrams %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) + 
    geom_node_point(color = "light blue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


# KING JAMES BIBLE:
library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)
kjv_bigrams <- kjv %>% 
  count_bigram()

# filter our rare combos and digits 
kjv_bigrams %>% 
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>% 
  visualize_bigrams()

# thou, thy are most common word1s... maybe include as stop_words?


# Counting and correlating PAIRS

# for across-rows operations, necessity to create wide data >>> retidy
# use widyr package

# P&P, divided into 10-line sections
# remove stop_words

austen_section_words <- austen_books() %>% 
  filter(book == "Pride & Prejudice") %>% 
  mutate(section = row_number() %/% 10) %>% 
  filter(section > 0 ) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word)

austen_section_words

# pairwise_count() from widyr package:
# one row for each PAIR of words, count common pairs within each section
library(widyr)

word_pairs <- austen_section_words %>% 
  pairwise_count(item = word, feature = section, sort = TRUE)

austen_section_words %>% glimpse
word_pairs %>% glimpse

word_pairs %>% 
  filter(item1 == "darcy")
# darcy >>> elizabeth
# check out correlations: common appearance relative to appearance separately

# Binary correlations
# Phi coefficient: likelihood either BOTH x, y appear <OR> NEITHER do, VS. one appears > other
# equivalent to Pearson correlation (when applied to binary data)

# pairwise_cor() function in widyr
word_cors <- austen_section_words %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>%                    # filter atleast relatively common...
  pairwise_cor(word, section, sort = TRUE)

word_cors

word_cors %>% 
  filter(item1 == "pounds")
# thousand far away most common = 0.77, ten = only 0.32    rich folks...

# choose particular words of interest to find correlations 
word_cors %>% 
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>% 
  group_by(item1) %>% 
  top_n(6) %>% 
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~item1, scales = "free") +
  coord_flip()

# networkd graph
set.seed(2016)
word_cors %>% 
  filter(correlation > 0.15) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# symmetrical relationships >>> NOT directional
# walk <> park, dance <> ball along with obvious name pairings, colonel <> fitzwilliam, etc.



# Chapter 5: Converting to/from non-tidy formats --------------------------

# Other formats for text mining, important to be able to transform from/to tidy-text to other.

# Document-Term Matrix:
# Each row = one document (book/article)
# Each colun = one term
# Each value = number of appearances of term in document

# Necessity to convert between tidy-text and DTM formats:
# tidy(): turns DTM into tidy-df. from broom package
# cast(): turns tidy-df into matrix. ex. cast_sparse(), cast_dtm(), cast_dfm()...

# Most widely-used implementation >>> DocumentTermMatrix from tm package.
library(tm)

data("AssociatedPress", package = "topicmodels")

AssociatedPress
# contains documents (AP article) and terms (distinct terms)
terms <- Terms(AssociatedPress)

head(terms)
# [1] "aaron"      "abandon"    "abandoned"  "abandoning" "abbott"     "abboud"    

# For analyze with tidy tools >>> convert into df with one-token-per-document-per-row

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
head(ap_td)
# column: document, term, count
# row: one word each

# tidied version has NO rows where count = 0! (ex. no "aaron" or "abandon")

ap_sentiments <- ap_td %>% 
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments %>% head(5)

library(ggplot2)

ap_sentiments %>% 
  count(sentiment, term, wt = count) %>% 
  ungroup() %>% 
  filter(n >= 200) %>% 
  mutate(n = if_else(sentiment == "negative", true = -n, false = n),
         term = reorder(term, n)) %>% 
  ggplot(aes(term, n, fill = sentiment)) +
  geom_col() +
  ylab("Contribution to sentiment") +
  coord_flip()

# Tidy dfm objects: 

library(methods)

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm
# Document-feature matrix of: 58 documents, 9,357 features (91.8% sparse).

inaug_tidy <- tidy(inaug_dfm)

inaug_tidy %>% head(5)

inaugh_tf_idf <- inaug_tidy %>% 
  bind_tf_idf(term, document, count) %>% 
  arrange(desc(tf_idf))

inaugh_tf_idf %>% head(10)

# notable speeches tf-idf
notables <- c("1861-Lincoln", "1933-Roosevelt", "1961-Kennedy", "2009-Obama")

inaugh_tf_idf %>% 
  filter(document %in% notables) %>%
  filter(term != "-") %>% 
  group_by(document) %>% 
  top_n(10, wt = tf_idf) %>% 
  ggplot(aes(reorder(term, tf_idf), tf_idf)) +
  geom_col() +
  facet_wrap(~document, scales = "free") +
  coord_flip()

# Extract year from each document name >>> compute total # words within year

library(tidyr)

year_term_counts <- inaug_tidy %>% 
  extract(document, "year", "(\\d+)", convert = TRUE) %>%  # (from, into, regex, convert = T/F)
  complete(year, term, fill = list(count = 0)) %>% 
  group_by(year) %>% 
  mutate(year_total = sum(count))

# pick a few words of interest and see frequency change over time!

year_term_counts %>% 
  filter(term %in% c("god", "america", "union", 
                     "freedom", "foreign", "constitution")) %>% 
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Frequency (as %) of word in inaugural address")

# Casting tidy text data into matrix:
# Necessity as some algoriths only accept matrix as the input!

# take tidy and cast back into DTM
ap_td %>% 
  cast_dtm(document, term, count)

# take tidy and cast into dfm (quanteda)
ap_td %>% 
  cast_dfm(term, document, count)

# some need Matrix object
library(Matrix)
# cast into Matrix object

m <- ap_td %>% 
  cast_sparse(document, term, count)

class(m)
# Matrix
dim(m)
# 2246  10473

# DTM for Jane Austen:
library(janeaustenr)

austen_dtm <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word) %>% 
  cast_dtm(book, word, n)

austen_dtm

# Tidying corpus objects with meta-data

# Corpus: data structures for documents before tokenization
# text and meta-data >>> ID, date/time, title, language for each document

data("acq")
acq

acq[[1]]
# <<PlainTextDocument>>
#   Metadata:  15
# Content:  chars: 1287

# Corpus: list-like structure
# each item contain text and meta-data
# use tidy() to construct table with one-row-per-document, meta-data as column

acq_td <- tidy(acq)
acq_td

# THen use unnest_tokens()

acq_tokens <- acq_td %>% 
  select(-places) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

# most common
acq_tokens %>% 
  count(word, sort = TRUE)

# tf-idf
acq_tokens %>% 
  count(id, word) %>% 
  bind_tf_idf(word, id, n) %>% 
  arrange(desc(tf_idf))


# Example: Mining financial articles!

# tm.plugin.webmining >>> connect to online feeds to retrieve news articles based on keyword
# ex. financial articles
library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook", 
             "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "IBM", "YHOO", "NFLX")

# removed twitter as in NYSE not NASDAQ... GOOG should be GOOGL but still works...

download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))  
}
# takes 20 most recent articles!

stock_articles <- data_frame(company = company, 
                             symbol = symbol) %>% 
  mutate(corpus = map(symbol, download_articles))  # map(list = symbols, function = d_l)

stock_articles %>% head(5)

# Each item in corpus column = WebCorpus object
# Turn each WebCorpus object into data frame with tidy() >>> unnest() >>> unnest_tokens()

stock_tokens <- stock_articles %>% 
  unnest(map(corpus, tidy)) %>% 
  unnest_tokens(word, text) %>% 
  select(company, datetimestamp, word, id, heading)

stock_tokens %>% head(10)

library(stringr)

stock_tf_idf <- stock_tokens %>% 
  count(company, word) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  bind_tf_idf(word, company, n) %>% 
  arrange(-tf_idf)

stock_tf_idf %>% 
  group_by(company) %>% 
  top_n(8, wt = tf_idf) %>% 
  ggplot(aes(reorder(word, tf_idf), tf_idf)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~company, scales = "free_y")


# sentiments with finance articles:
stock_tokens %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, id, sort = TRUE) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(word) %>% 
  summarize(contribution = sum(n * score)) %>% 
  top_n(12, abs(contribution)) %>% 
  mutate(word = reorder(word, contribution)) %>% 
  ggplot(aes(word, contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Freq/word * AFINN score")

# share, shares counted as POSITIVE
# >>> in context are neutral nouns (e.g. stock share, price of $12/share, etc.)
# AFINN as well as Bing and NRC = NOT useful for financial text data

# For finance: Loughran & McDonald dictionary
# Groups: positive, negative, litigious, uncertain, constraining, superfluous

stock_tokens %>% 
  count(word) %>% 
  inner_join(get_sentiments("loughran"), by = "word") %>% 
  group_by(sentiment) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ylab("Freq of most common words in finance articles")

# much more reasonable appearance of words.

stock_sentiment_count <- stock_tokens %>% 
  inner_join(get_sentiments("loughran"), by = "word") %>% 
  count(sentiment, company) %>% 
  spread(sentiment, n, fill = 0)

stock_sentiment_count

stock_sentiment_count %>% 
  mutate(score = (positive - negative) / (positive + negative),
         company = reorder(company, score)) %>% 
  ggplot(aes(company, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "company", y = "positivity score among most recent articles")

# compare with actual finance data >>> stock prices, other financial metrics...


# Chapter 6: Topic Modeling -----------------------------------------------

# Topic modeling: unsupervised classification of documents >>> i.e. clustering for numeric data
# find natural group of items when not sure what to look for.

# Latent Dirichlet Allocation (LDA): methodology of topic modeling, each document as mixture
# of topics, each topic as mixture of words.
# Documents can overlap in terms of content > separation into discrete groups
# Document

library(topicmodels)

data("AssociatedPress")
AssociatedPress

# Use LDA() function >>> set argument k = 2 for two-topic LDA model.
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# Per-topic-per-word probabilities (Beta)

library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
# returned in one-topic-per-term-per-row format.
# computation of probability of term being generated = from topic x, y, or z, etc.

# top_n() function for 10 most common within each topic.
library(dplyr)

ap_top_terms <- ap_topics %>% 
  group_by(topic) %>% 
  top_n(10, wt = beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
ap_top_terms

library(ggplot2)

ap_top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# topic 1: "percent", "million", "year", "billion", "company" >>> finance/business news?
# topic 2: "president", "government", "soviet", "bush", "states" >>> political news?

# "new", "people", "two" appear in both >>> terms appear in multiple topics = overlap
# more flexible modeling compared to "hard clustering" methods

# Consider terms with -greatest difference in beta- between Topic 1 and Topic 2
# log ratio of topics: log2(Beta2 / Beta1)
# log ratio = symmetrical differences, B2 twice as large = log ratio of 1, if B1 then -1.
# for examine only relevant words, filter out common words >>> Beta > 1/1000 for any topic.

library(tidyr)
beta_spread <- ap_topics %>% 
  mutate(topic = paste0("topic", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic1 > 0.001 | topic2 > 0.001) %>% 
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

beta_spread %>% 
  group_by(direction = log_ratio > 0) %>% 
  top_n(10, wt = abs(log_ratio)) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, log_ratio)) %>% 
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

# Words with greatest difference in Beta (topic 2 vs. topic 1)
# more common in Topic 2 = politics         (Positive log ratio)
# more common in Topic 1 = money/finance    (Negative log ratio)

# Document-Topic Probabilities
# Modeling each document as mixture of topics.
# Examine per-document-per-topic probabilities >>> gamma
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Gamma: estimated proportion of words from document that are generated from topic.
# Ex. 24.8% of words in Doc1 are from Topic1.
# Ex2. 36.15% of words in Doc2 are from Topic1.
# Document 9: 96.6% of words from topic 1!!!
# Document 6: 0.44% of words from topic 1!!!
# Examine Doc6 further:
tidy(AssociatedPress) %>% 
  filter(document == 6) %>% 
  arrange(desc(count))
# article on American govt + Panama + Noriega >>> Topic 2 placement = correct!


# Example: Great Library Heist
# use simple case for testing: collect set of 4 DISTINCT topic documents
# perform topic modeling for whether correct distinguish to 4 DISTINCT topic groups.

# Great Expectations: Charles Dickens
# The War of the Worlds: HG Wells
# 20 Thousand Leagues Under the Sea: Jules Verne
# P & P: Jane Austen

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
library(gutenbergr)
books <- gutenberg_works(title %in% titles) %>% 
  gutenberg_download(meta_fields = "title")

# Divide into chapters >>> unnest_tokens()
# Remove stop_words >>> anti_join()

library(stringr)

reg <- regex("^chapter ", ignore_case = TRUE)

by_chapter <- books %>% 
  group_by(title) %>% 
  mutate(chapter = cumsum(str_detect(text, reg))) %>% 
  ungroup() %>% 
  filter(chapter > 0) %>% 
  unite(document, title, chapter)

by_chapter

by_chapter_word <- by_chapter %>% 
  unnest_tokens(word, text)

word_counts <- by_chapter_word %>% 
  anti_join(stop_words) %>% 
  count(document, word, sort = TRUE) %>% 
  ungroup()

word_counts

chapters_dtm <- word_counts %>% 
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")

chapter_topics
# one-topic-per-term-per-row format  (beta calculation for each topic)

top_terms <- chapter_topics %>% 
  group_by(topic) %>% 
  top_n(5, wt = beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

library(ggplot2)

top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# the 4 topics split distinct between the books.
# captain, nautilus, sea, nemo == 20 THousand Leagues under the Sea
# elizabeth, darcy, jane, bennet == Pride and Prejudice

# Per-Document Classification
# Per-document-per-topic probabilities >>> Gamma

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

# estimated proportion of words from document from a certain topic.
# Ex. Great Expectations, Ch.57 == 0.00135% probability from Topic 1 (Pride & Prejudice)

# Expectation that chapters within book generated from corresponding Topic (Book)
chapters_gamma <- chapters_gamma %>% 
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

chapters_gamma %>% 
  mutate(title = reorder(title, gamma * topic)) %>% 
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)
# Great Expectations = Topic 4... but has somewhat associated with other topics...
# Other topics correspond very well to the actual book title. 
# Topic 1 = P&P, Topic 2 = 20Thousand, TOpic 3 = TWoW

# Any cases wherein topic (book) most associated with chapter actually belonged to another book?
chapter_classifications <- chapters_gamma %>% 
  group_by(title, chapter) %>% 
  top_n(1, wt = gamma) %>%   # top 1 == most associated, so basically the classification.
  ungroup()

chapter_classifications

# Compare each "consensus" topic for each book >>> most common Topic classification in a chapter
book_topics <- chapter_classifications %>% 
  count(title, topic) %>% 
  group_by(title) %>% 
  top_n(1, wt = n) %>% 
  ungroup() %>% 
  transmute(consensus = title, topic)

book_topics

chapter_classifications %>% 
  inner_join(book_topics, by = "topic") %>% 
  filter(title != consensus)
# Great Expecatations Ch. 23 >>> Pride and Prejudice 54%
# Great Expectations Ch. 54 >>> The War of the Worlds 0.48%


# By-Word Assignments: augment `broom`

# Assigning each word in each document to a Topic.
# ^ words in document to Topic >>> ^ Weight (gamma) towards document-topic classification
# Take original document-word pairs >>> find which words in each doc assigned to what Topic.
# usage of augment() function from broom package

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments
# .topic >>> which topic each term was assigned to

assignments <- assignments %>% 
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>% 
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments
# true book == title       Topic (book) assignment == Consensus
# Visualize confusion matrix: how often words from one topic(book) assigned to another

library(scales)
assignments %>% 
  count(title, consensus, wt = count) %>% 
  group_by(title) %>% 
  mutate(percent = n / sum(n)) %>% 
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Which 'topic'(book) words were assigned to", 
       y = "Books the words actually come from",
       fill = "% of assignment")
# see as from before, Words actually from Great Expectations were slightly misassigned!

wrong_words <- assignments %>% 
  filter(title != consensus)

wrong_words %>% 
  count(title, consensus, term, wt = count) %>% 
  ungroup() %>% 
  arrange(desc(n))
# love, lady misclassified >>> both appear very frequently in P&P > Great Expectations.
# others, "flopson" appear ONLY in GE but misassigned to P&P
# >>> LDA algorithm = stochastic, so accidentally land on topic spanning multiple books

# Alternative LDA Implementations:

# Mallet package >>> MALLET Java text classification tools
library(mallet)  # requires rJava

# Mallet format >>> takes non-tokenized documents and performs tokenization itself
# require different stop_words file
collapsed <- by_chapter_word %>% 
  anti_join(stop_words, by = "word") %>% 
  mutate(word = str_replace(word, "'", "")) %>% 
  group_by(document) %>% 
  summarize(text = paste(word, collapse = " "))

# stop_words
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)  # set up model (topics = 4)
mallet_model$loadDocuments(docs)   # load docs into model
mallet_model$train(100)            # train 100 times

# use tidy(), augment() as before

# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# by-word assignments
term_counts <- rename(word_counts, term = word)
term_counts
augment(mallet_model, term_counts)




# Chapter 7: Case Study #1 - Twitter data  --------------------------------

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

tweets_julia <- read_csv("~/R_materials/tweets_julia.csv")
tweets_dave <- read_csv("~/R_materials/tweets_dave.csv")

# convert string timestamps to date-time objects with lubridate!
tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_dave %>% 
                      mutate(person = "David")) %>% 
  mutate(timestamp = ymd_hms(timestamp))

ggplot(tweets, aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

# Julie more active for longer > David

# Word Frequencies

# - Remove tweest from dataset >>> retweets
# - Mutate() remove links and clean ampersands and other symbols

library(tidytext)
library(stringr)

replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>% 
  mutate(text = str_replace_all(text, replace_reg, "")) %>% 
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% 
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# - Group by person
# - Count by # times each person used each word
# - Use left_join() to add column of total # words used by each person  (summarize(total = n()))
# - Calculate frequency for each person + each word

frequency <- tidy_tweets %>% 
  group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(person) %>% 
              summarize(total = n())) %>% 
  mutate(freq = n/total)

frequency

# For plotting >>> need to spread() using tidyr

library(tidyr)

frequency <- frequency %>% 
  select(person, word, freq) %>% 
  spread(person, freq) %>% 
  arrange(Julia, David)

frequency

library(scales)

ggplot(frequency, aes(Julia, David)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

# Comparing word usage

# Likelihood with log odds ratio.

# filter for David/Julia equal Tweets
tidy_tweets <- tidy_tweets %>% 
  filter(timestamp >= as.Date("2016-01-01"),
         timestamp < as.Date("2017-01-01"))

# - str_detect() for remove usernames from `word` column
# - Count how many times each person uses each word >>> keep only n > 10
# - spread()
# - Calculate log odds ratio for each word  (David/Julia)

word_ratios <- tidy_tweets %>% 
  filter(!str_detect(word, "^@")) %>% 
  count(word, person) %>% 
  filter(sum(n) > 10) %>% 
  ungroup() %>% 
  spread(person, n, fill = 0) %>% 
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>% 
  # mutate only on columns = numeric >>>  `. + 1`/`sum(.+1)` for DAVID and for JULIA
  mutate(logratio = log(David / Julia)) %>% 
  arrange(desc(logratio))

word_ratios %>% 
  arrange(abs(logratio))

# equally likely to tweet about maps, email, APIs, functions.

# - top 15 most distinctive words from each account

word_ratios %>% 
  group_by(logratio < 0) %>% 
  top_n(15, wt = abs(logratio)) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, logratio)) %>% 
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (D/J)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))

# Changes in word use (over time)

# - Define new time variable >>> unit of time for each posted tweet
# - floor_date() function with unit of choosing >>> 1 month
# - Count # times each person used each word in each time unit
# - Add column for total # words used each time unit by each person
# - Add column for total # times each word used by each person
# - Filter ONLY keep words n > 30

words_by_time <- tidy_tweets %>% 
  filter(!str_detect(word, "^@")) %>% 
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>% 
  count(time_floor, person, word) %>% # count # times each person sued each word in each time unit
  ungroup() %>% 
  group_by(person, time_floor) %>% 
  mutate(time_total = sum(n)) %>%    # total # of words used each time unit by each person
  group_by(word) %>% 
  mutate(word_total = sum(n)) %>%    # total # times each word used by each person
  ungroup() %>% 
  rename(count = n) %>% 
  filter(word_total > 30)

# Each row in df >>> one person using one word in given time unit (1 month)
# Count >>> how many times that person used that word in that time unit (1 month)
# time_total >>> how many words person used during that time unit 
# word_total >>> how many times person used that word over whole year (entire time unit duration)

# use nest() for each word

nested_data <- words_by_time %>% 
  nest(-word, -person)

# one row for each person-word combination 
# data >>> list column with df, one for each combination of person and word
# map() to apply modeling to each df
# count data >>> binomial (logit) model
# given word mentioned in given time unit? TRUE / FALSE
# How does count of word depend on time unit?

library(purrr)

nested_models <- nested_data %>% 
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., family = "binomial")))

nested_models

# pull slopes of each model >>> find significant
# search for significant with adjustment to p-values for multiple comparisons

library(broom)

slopes <- nested_models %>% 
  unnest(map(models, tidy)) %>% # give tidy output for each model
  filter(term == "time_floor") %>% 
  mutate(adjusted_p_value = p.adjust(p.value))

slopes

top_slopes <- slopes %>% 
  filter(adjusted_p_value < 0.1) %>%   # any less than alpha = 0.1
  select(-statistic, -p.value)

top_slopes

words_by_time %>% 
  inner_join(top_slopes, by = c("word", "person")) %>% 
  filter(person == "David") %>% 
  ggplot(aes(time_floor, count / time_total, color = word, lty = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

# tweet ALOT about UseR2016 >>> drop off after end
# tweet gradually increase stack overflow
# tweet gradually decrease ggplot2

words_by_time %>% 
  inner_join(top_slopes, by = c("word", "person")) %>% 
  filter(person == "Julia") %>% 
  ggplot(aes(time_floor, count / time_total, color = word, lty = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

# all negative >>> not tweeted high rate of any specifc, use ^^variety of different words


# Favorites & ReTweets

# What words more likely to be retweeted or favorited by Dave vs. Julia?

# User downlaod of Twitter archive >>> favorites + retweets NOT included
# construct new dataset via Twitter API

tweets_julia <- read_csv("~/R_materials/juliasilge_tweets.csv")
tweets_dave <- read_csv("~/R_materials/drob_tweets.csv")

tweets <- bind_rows(tweets_julia %>% 
                      mutate(person = "Julia"),
                    tweets_dave %>% 
                      mutate(person = "David")) %>% 
  mutate(timestamp = ymd_hms(created_at))

# remove all retweets and replies
# unnest_tokens

tidy_tweets <- tweets %>% 
  select(-source) %>% 
  filter(!str_detect(text, "^(RT|@)")) %>%  # remove Retweets and replies @
  mutate(text = str_replace_all(text, replace_reg, "")) %>% 
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% 
  anti_join(stop_words)

# # of times each D/J tweets = retweeted

totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  summarize(re_tweet = sum(retweets)) %>% 
  group_by(person) %>% 
  summarize(total_re_tweet = sum(re_tweet))

totals

# median retweets for each word + each person

# 


word_by_re_tweets <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarize(re_tweet = sum(retweets)) %>% 
  group_by(person, word) %>% 
  summarize(median_retweets = median(re_tweet), uses = n()) %>% 
  left_join(totals) %>% 
  filter(median_retweets != 0) %>% 
  ungroup()

word_by_re_tweets %>% 
  filter(uses >= 5) %>% 
  arrange(desc(median_retweets))

# tweets on packages worked on by D/J are most retweeted!

word_by_re_tweets %>% 
  filter(uses >= 5) %>% 
  group_by(person) %>% 
  top_n(10, wt = median_retweets) %>% 
  arrange(median_retweets) %>% 
  ungroup() %>% 
  mutate(word = factor(word, unique(word))) %>% 
  ungroup() %>% 
  ggplot(aes(word, median_retweets, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, y = "Median # of retweets for tweets containing each word")


# Favorited tweets?

totals <- tidy_tweets %>% 
  group_by(person, id) %>% 
  summarize(faves = sum(favorites)) %>% 
  group_by(person) %>% 
  summarize(total_faves = sum(faves))

totals

word_by_faves <- tidy_tweets %>% 
  group_by(id, word, person) %>% 
  summarize(faves = sum(favorites)) %>% 
  group_by(person, word) %>% 
  summarize(favorites = median(faves), uses = n()) %>% 
  left_join(totals) %>% 
  filter(favorites != 0) %>% 
  ungroup()

word_by_faves

word_by_faves %>% 
  filter(uses >= 5) %>% 
  group_by(person) %>% 
  top_n(10, wt = favorites) %>% 
  arrange(favorites) %>% 
  ungroup() %>% 
  mutate(word = factor(word, unique(word))) %>% 
  ungroup() %>% 
  ggplot(aes(word, favorites, fill = person)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ person, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL,
       y = "Median # of favorites for tweets containing each word")

# similar words from retweets data frame




# Chapter 8: Mining NASA dataset metadata ---------------------------------

# ~32,000 datasets hosted/maintainted by NASA
# Understand connections between each
# title, description field, organization, keywords, etc.




























































































































































































































































































































































































































































































































































































































































































