rm(list = ls())
# Factors
library(tidyverse)
library(forcats)
library(gapminder)
gapminder
str(gapminder)
gapminder %>% str('continent')

gapminder$continent %>% levels()
?levels()
levels(gapminder$continent)
gapminder %>% levels(continent)


gapminder$continent %>% nlevels()
gapminder$continent %>% class()

summary(gapminder$continent)
gapminder$continent %>% summary()

gapminder %>% count(continent)
# OR using forcats::fct_count()
fct_count(gapminder$continent)
gapminder$continent %>% fct_count()

# DROPPING unused levels:
# drop rows when filter = NOT change level of factors, ex:
h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")
h_gap <- gapminder %>% 
  filter(country %in% h_countries)
nlevels(h_gap$country)  # STILL 142 even when filter using h_countries!!! 
# Usage droplevels(): 
h_gap_dropped <- h_gap %>% 
  droplevels()
nlevels(h_gap_dropped$country)   # now only 5, as filtered by h_countries. 
# OR fct_drop() on free-range factor:
h_gap$country %>% 
  fct_drop() %>% 
  levels()
# "Egypt"    "Haiti"    "Romania"    "Thailand"    "Venezuela"


# Default = factors ordered alphabetically........
# Necessity change to: Frequency or to certain summary statistic variable.
# Ex. Order gapminder data countries by life expectancy: 
gapminder$continent %>% 
  levels()
# Levels: Africa, Americas, Asia, Europe, Oceania = ALPHABET ORDER

gapminder$continent %>% 
  fct_infreq() %>% 
  levels () %>% head()
?fct_infreq
# Levels: Africa, Asia, Europe, Americas, Oceania = FREQUENCY
gapminder %>%
  group_by(continent) %>% 
  summarise(n_distinct(country))   # summarize # of DISTINCT countries in each continent

gapminder %>%
  group_by(continent == "Asia") %>% 
  summarize(n_distinct(country))


gapminder$country %>%
  levels() %>% 
  group_by(continent)
  

n_distinct(gapminder$country) %>% 
  filter(continent == "Africa")

gapminder %>% 
  filter(continent == "Africa") %>% 
  n_distinct()
#---------------------------
gapminder$continent %>% 
  fct_infreq() %>% 
  levels() %>% head()
# Levels : Africa, Asia, Europe, Americas, Oceania

gapminder$continent %>% 
  fct_infreq() %>% 
  fct_rev() %>% 
  levels() %>% head()
?fct_rev
?fct_infreq
# Levels : Oceania, Americas, Europe, Asia, Africa = BACKWARDS by using fct_rev()

## order countries by median life expectancy
fct_reorder(gapminder$country, gapminder$lifeExp) %>% levels()

fct_reorder(gapminder$country, gapminder$lifeExp) %>% 
  levels() %>% head(3)


## order according to max life exp instead of median
fct_reorder(gapminder$country, gapminder$lifeExp, min) %>%  
  levels() %>% head()

?desc()
## backwards! = according to max life exp:
fct_reorder(gapminder$country, gapminder$lifeExp, .desc = TRUE) %>% 
  levels() %>% head()

?fct_reorder

gapminder %>% group_by(continent) %>% 
  summarise(n = n())

# Reorder for easier plotting! 
gap_asia_2007 <- gapminder %>% filter(year == 2007, continent == "Asia")

# Factor order = unclear relationship in plot:
ggplot(gap_asia_2007, aes(x = lifeExp, y = country)) + geom_point()

# Reorder factor order = clear relationshpi in plot (fct_reorder = desc.order): 
ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point()

ggplot(gap_asia_2007, aes(x = lifeExp, y = fct_reorder(country, lifeExp, .desc = TRUE))) +
  geom_point()


# Factors with color, reorder according to data.  # fct_reorder2 = asc.order
h_countries <- c("Egypt", "Haiti", "Romania", "Thailand", "Venezuela")

h_gap <- gapminder %>%
  filter(country %in% h_countries) %>% 
  droplevels()

h_gap

ggplot(h_gap, aes(x = year, y = lifeExp, color = country)) +
  geom_line()

ggplot(h_gap, aes(x = year, y = lifeExp,
                  color = fct_reorder2(country, year, lifeExp))) +
  geom_line() +
  labs(color = "country")

?fct_reorder2

# Custom change factor order, fct_relevel(): 
h_gap <- gapminder %>%
  filter(country %in% h_countries) %>% 
  droplevels()

h_gap$country %>% levels()

h_gap$country %>% fct_relevel("Romania", "Venezuela", "Egypt") %>% levels()

# Recode factor levels, fct_recode(): 
i_gap <- gapminder %>% 
  filter(country %in% c("United States", "Australia", "Sweden")) %>% 
  droplevels()

i_gap$country %>% levels()

i_gap$country %>% fct_recode("USA" = "United States", "Ozzy" = "Australia", "Swedd" = "Sweden") %>% 
  fct_relevel("USA", "Ozzy", "Swedd") %>% 
  levels()

