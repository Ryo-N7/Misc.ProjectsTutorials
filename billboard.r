# Billboard
library(tidyverse)
library(billboard)
library(visdat)


# 57 years of Billboard Hot 100 songs >>> wikipedia.

str(wiki_hot_100s)
# 5701 obs of 4 variables: #, title, artist, year

vis_dat(wiki_hot_100s)
# everything is a character variable + no missing data to be seen!

billboard_raw <- as_tibble(wiki_hot_100s)
# use tibble as only output max. 10 when print out

billboard_raw
# no and year should be NUMERIC > chr.

billboard_raw %>% 
  mutate(rank = as.numeric(no),
         year = as.numeric(year))
# NAs via coercion...

billboard_raw %>% 
  mutate(rank = as.numeric(no),
         year = as.numeric(year)) %>% 
  filter(is.na(rank))
# "Tie" as rank num.... -_-"

# instead of using view() to find the rows/columns associated with these "Ties"...
# create new variable with all the row numbers, filter on "Tie"
# `pull()` as vector > select as df

row_tie <- billboard_raw %>% 
  mutate(row_num = 1:n()) %>% 
  filter(no == "Tie") %>% 
  pull(row_num)
# show vector of row num. for observations/rows with "Tie"

# before + after "Tie" ranks

row_pad <- c(row_tie, 
             row_tie + 1,
             row_tie - 1) %>% 
  sort()

row_pad

billboard_raw %>% slice(row_pad)
# "Tie" exists when similar rank to value of previous row/song/entry

x <- c(1,2,3)
lag(x)

# insert into billboard_raw
billboard_raw %>% 
  mutate(rank = as.numeric(no)) %>% 
  mutate(rank_lag = lag(rank)) %>% 
  select(no, rank, rank_lag) %>% 
  mutate(rank_rep = if_else(is.na(rank),
                            true = rank_lag,
                            false = rank)) %>% 
  filter(is.na(rank))

#       no  rank rank_lag rank_rep
#   <chr> <dbl>    <dbl>    <dbl>
# 1   Tie    NA      100      100
# 2   Tie    NA       37       37
# 3   Tie    NA       54       54
# 4   Tie    NA       85       85
# 5   Tie    NA       91       91

billboard_raw %>% 
  mutate(rank = as.numeric(no)) %>% 
  mutate(rank_lag = lag(rank)) %>% 
  select(no, rank, rank_lag) %>% 
  mutate(rank_rep = if_else(is.na(rank),
                            true = rank_lag,
                            false = rank)) %>% 
  slice(row_pad)


billboard_clean <- billboard_raw %>%
  mutate(rank = as.numeric(no),
         year = as.numeric(year),
         rank_lag = lag(rank),
         rank_rep = if_else(condition = is.na(rank),
                            true = rank_lag,
                            false = rank)) %>%
  select(rank_rep,
         title,
         artist,
         year) %>%
  rename(rank = rank_rep)

# One hit wonders:

billboard_clean %>% 
  add_count(artist)

# one hit wonder == reached Rank #1 AND appeared only once at that position (n = 1)
billboard_clean %>% 
  add_count(artist) %>% 
  filter(rank == 1, n == 1)
# 15 songs! 
# some may still Rank #1 again in the future... 

# Distance between first appearance and most recent appearance
billboard_clean %>% 
  group_by(artist) %>% 
  summarize(year_dist = max(year) - min(year)) %>% 
  filter(year_dist > 0) %>% 
  arrange(-year_dist) %>% 
  filter(year_dist > 10) %>% 
  head(20) # or slice(1:20)

# Cher >>> 34 years!
# how about only appear twice.

billboard_clean %>% 
  add_count(artist) %>% 
  group_by(artist) %>% 
  mutate(year_dist = max(year) - min(year)) %>% 
  filter(year_dist > 0) %>% 
  filter(n == 2) %>% 
  arrange(-year_dist) %>% 
  filter(year_dist > 10)

# Artists with multiple Rank #1s?
billboard_clean %>% 
  filter(rank == 1) %>% 
  add_count(artist) %>% 
  filter(n > 1)
# The Beatles.... obviously! 

billboard_clean %>% 
  filter(artist == "The Beatles")
# 26 times overall

# Number of times appearance in TOp 100 for each artist:
billboard_clean %>% 
  group_by(artist) %>% 
  summarize(n_in_Top100 = n()) %>% 
  arrange(-n_in_Top100)
# Madonna with 35 appearances!

# Top 20 Artists by appearance in top 100
billboard_clean %>% 
  group_by(artist) %>%
  summarise(n_times_in_100 = n()) %>%
  arrange(-n_times_in_100) %>%
  top_n(wt = n_times_in_100,
        n = 20) %>%
  ggplot(aes(x = n_times_in_100,
             y = reorder(artist,n_times_in_100))) + 
  ggalt::geom_lollipop(horizontal = TRUE,
                       colour = "navy") + 
  labs(x = "# Times Appeared in top 100\nfrom 1960-2017",
       y = "Artist") +
  theme_minimal()


# Growth in fame:
billboard_clean %>% 
  # add a grouping category for the growth
  arrange(artist,year) %>%
  group_by(artist) %>%
  mutate(rank_tally = 1:n()) %>%
  ungroup() %>%
  filter(artist == "Madonna")

billboard_clean_growth <- billboard_clean %>% 
  add_count(artist) %>%
  # add a grouping category for the growth
  arrange(artist,year) %>%
  group_by(artist) %>%
  mutate(rank_tally = 1:n()) %>%
  ungroup()

# Appearance over time (n > 20), from 1990 onwards
billboard_clean_growth %>%
  filter(n >= 20, year > 1990) %>%
  ggplot(aes(x = year,
             y = rank_tally,
             group = artist,
             colour = artist)) +
  geom_line()


library(ggrepel)

billboard_clean_growth %>%
  filter(n >= 21) %>%
  ggplot(aes(x = year,
             y = rank_tally,
             group = artist,
             colour = artist)) +
  geom_line() +
  geom_label_repel(data = filter(billboard_clean_growth,
                           n >= 21,
                           rank_tally == n),
             aes(label = artist,
                 fill = artist),
             colour = "white") +
  theme_dark() + 
  expand_limits(x = c(1964,2011)) +
  theme(legend.position = "none") 







