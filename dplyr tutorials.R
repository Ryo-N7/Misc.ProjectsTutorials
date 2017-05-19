# dplyr tutorial
library(tidyverse)
library(nycflights13)
dim(flights)
head(flights)
filter(flights, month == 2, day == 20)
?filter
filter(flights, month == 1 | month == 3, day == 30)

slice(flights, 2:5)

arrange(flights, year, day)
arrange(flights, year, month, day)
arrange(flights, carrier, flight, month, day)
arrange(flights, desc(arr_delay))
arrange(flights, dep_time)

select(flights, dep_time, month, flight, carrier)
select(flights, year:day)
select(flights, -(year:day))
# Rename variables
select(flights, tail_num = tailnum)
head(flights)
# (data, NEW = OLD)
rename(flights, tail_num = tailnum)
rename(flights, flt = flight)
rename(flights, deptime = dep_time)

distinct(flights, carrier)
distinct(flights, flight, tailnum)


mutate(flights, 
       gain = arr_delay - dep_delay,
       speed = distance / air_time *60)

select(mutate(flights, 
              gain = arr_delay - dep_delay,
              speed = distance / air_time *60), gain, speed)

select(flights, day, year)
str(flights)

# Only keep new variables created: transmute()
transmute(flights, 
          gain = arr_delay - dep_delay,
          speed = distance / air_time*60)

summarise(flights, 
          delay = mean(dep_delay, na.rm = TRUE))

sample_n(flights, 10)
sample_frac(flights, 0.00001)
?sample_frac
sample_frac(flights, 0.0005)
sample_n(flights, 5, replace = TRUE)

# Groupby()
# Split dataset by individual airplanes (tailnum).
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
# Only for airplanes with more than 20 flights, and travelling less than 2000 miles. 
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth(se = FALSE) +
  scale_size_area()

?geom_smooth

# Aggregate
# # of unique planes to each destination. 
destination <- group_by(flights, dest)
summarise(destination, 
          planes = n_distinct(tailnum), 
          flights = n())

daily <- group_by(flights, year, month, day)
daily
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))
per_year

# Using the %>% pipe for easier coding
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

newflights <- flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

head(newflights)


# Window Functions: 
library(dplyr)
install.packages("Lahman")
library(Lahman)
batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H) 
batting <- arrange(batting, playerID, yearID, teamID)
players <- group_by(batting, playerID)

x <- c(1,1,2,2,2)
cume_dist(x)
?cume_dist()
row_number(x)
min_rank(x)
dense_rank(x)
percent_rank(x)

filter(players, min_rank(desc(G)) < 2)
filter(players, min_rank(G) < 2)
?lahman
filter(players, cume_dist(G) < 0.1)

by_team_player <- group_by(batting, teamID, playerID)
by_team <- summarise(by_team_player, G = sum(G))
by_team_quartile <- group_by(by_team, quartile = ntile(G, 4))
summarise(by_team_quartile, mean(G))

x <- 1:5
x
lag(x)
lead(x)
# Compute the relative change in games played
mutate(players, G_delta = G - lag(G))

# Find when a player changed teams
filter(players, teamID != lag(teamID))



# Necessity usage order_by
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]

wrong <- mutate(scrambled, running = cumsum(value))
arrange(wrong, year)

right <- mutate(scrambled, running = order_by(year, cumsum(value)))
arrange(right, year)


# Example #2: Houston Flights
rm(list = ls())
library(ggplot2)
library(tidyverse)
install.packages("hflights")
library(hflights)
head(hflights)

# 1. Use the hflights dataset
# 2. group_by the Year, Month, and Day
# 3. Select out only the Day, the arrival delay, and the departure delay variables
# 4. Use summarize to calculate the mean of the arrival and departure delays
# 5. Filter the resulting dataset where the arrival delay or the departure delay 
# is more than 30 minutes.

# Base R code:
filter(
  summarise(
    select(
      group_by(hflights, Year, Month, DayofMonth),
      Year:DayofMonth, ArrDelay, DepDelay
      ),
        arr = mean(ArrDelay, na.rm = TRUE),
        del = mean(DepDelay, na.rm = TRUE)
    ),
    arr > 30 | del > 30)

# dplyr code using %>%

hflights %>%
  group_by(Year, Month, DayofMonth) %>%
  select(Year:DayofMonth, ArrDelay, DepDelay) %>%
  summarise(arr = mean(ArrDelay, na.rm = TRUE), 
            del = mean(DepDelay, na.rm = TRUE)) %>%
  filter(arr > 30 | del > 30)

# Example 2.1: Using with ggplot2
library(ggplot2)

# #1.Group 'iris' dataset by 'Species' 
# #2. Summarize it by calculating the mean of the 'Sepal.Length' 
# #3. Use ggplot2 to make a simple bar plot.

iris %>%
  group_by(Species) %>%
  summarise(meanSepalLength = mean(Sepal.Length, na.rm = TRUE)) %>%
  ggplot(aes(Species, meanSepalLength)) + 
  geom_bar(stat = "identity") +
  labs(x = "Species of Iris \n Flower Thingys", y = "Avg. Sep. Length")

iris %>% head()



# Stat545 dplyr tutorial
install.packages("gapminder")

library(tidyverse)
library(gapminder)

# Compact data frames with tibble
gapminder
as.data.frame(gapminder)
as_tibble(gapminder)
as_tibble(iris)

class(gapminder)

(canada <- gapminder[241:252, ])
# subset?? NOT GOOD. 
# It is not self-documenting. What is so special about rows 241 through 252?
# It is fragile. This line of code will produce different results if 
# someone changes the row order of gapminder, 
# e.g. sorts the data earlier in the script.

filter(gapminder, country == "Canada")
# Self-explanatory + robust against changes in row order in script! 


# Usage filter
gapminder %>% filter(lifeExp < 35)
gapminder %>% filter(country == "Rwanda", year > 1985)
gapminder %>% filter(country %in% c("Rwanda", "Afghanistan"), year > 1980)

# Comparison with base R code:
gapminder[gapminder$lifeExp < 29, ] ## repeat `gapminder`, [i, j] indexing is distracting
subset(gapminder, country == "Rwanda") ## almost same as filter; quite nice actually

# %>% shortcut: Cntrl + Shift + 'M'
gapminder %>% head()
gapminder %>% head(3)

# Select()
gapminder %>% 
  select(year, lifeExp) %>% 
  head(4)
# base R code: 
head(select(gapminder, year, lifeExp), 4)


#
gapminder %>% 
  filter(country == "Cambodia", year > 1980) %>% 
  select(year, lifeExp)

# base R code:
gapminder[gapminder$country == "Cambodia", c("year", "lifeExp")]



# Create copy of gapminder dataset
(my_gap <- gapminder)

my_gap %>% filter(country == "Canada")
# vs. 
my_precious <- my_gap %>% filter(country == "Canada")

# Use mutate() to create new 'GDP' variable
my_gap %>% 
  mutate(gdp = pop * gdpPercap)

?nlevels

candata <- my_gap %>% 
  filter(country == "Canada")
my_gap <- my_gap %>% 
  mutate(tmp = rep(candata$gdpPercap, nlevels(country)), 
         gdpPercapRelative = gdpPercap / tmp,
         tmp = NULL)

# Check:
my_gap %>% 
  filter(country == "Canada") %>% 
  select(country, year, gdpPercapRelative)
my_gap %>% 
  filter(country == "Afghanistan") %>% 
           select(country, year, gdpPercapRelative)
my_gap %>% 
  distinct(country)
# vs. base R code:
unique(my_gap$country)

my_gap %>% 
  filter(country == "Japan") %>% 
  select(country, year, gdpPercapRelative)
# Relative to Canada, gdp/capita in Japan increased from 1952 onwards, surpassing 
# Canada's gdp/capita in 1992 but declined afterwards... 

# Distribution of ALL countries gdp/capita relative to Canada gdp/capita
summary(my_gap$gdpPercapRelative)

as.data.frame(my_gap) %>%
  filter(year == 2007) %>% 
  arrange(desc(gdpPercap)) %>%
  select(country, lifeExp, pop, gdpPercap) %>% 
  head(25)

# snake_case or CamelCase for variable names???? 
my_gap %>% 
  rename(life_exp = lifeExp, 
         gdp_percap = gdpPercap,
         gdp_percap_relative = gdpPercapRelative)
my_gap

# Use select to rename variables! 
my_gap %>% 
  filter(country == "Japan", year > 1990) %>% 
  select(yr = year, lifeExp, gdpPercap) %>% 
  select(gdpPercap, everything())
?everything()

# Using group_by()
# How many observations per continent?
my_gap %>%
  group_by(continent) %>%
  summarize(n = n())

# How many observations per country?
my_gap %>% 
  group_by(country) %>% 
  summarize(n = n())

# Using base R code: 
table(my_gap$continent)
str(table(my_gap$continent))
# continent stored as chr 'names' rather than factors
# Also use tally() for counting rows:
my_gap %>% 
  group_by(continent) %>% 
  tally()

# Use count() for grouping + counting:
my_gap %>% 
  count(continent)

# Distinct countries per continent (multiple computations in sumamrize() is possible):
my_gap %>%
  group_by(continent) %>%
  summarize(n = n(),
            n_countries = n_distinct(country))


my_gap %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise_each(funs(mean, median), lifeExp, gdpPercap)


my_gap %>%
  filter(continent == "Africa") %>%
  group_by(year) %>%
  summarize(min_lifeExp = min(lifeExp), 
            max_lifeExp = max(lifeExp))

# Years of LifeExpectancy gained/lost from base yr = 1952
# for years 1980-2012
my_gap %>% 
  group_by(country) %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp)) %>% 
  filter(year %in% c(1980:2012), country == "Japan")
# How to set custom base yr? (NOT use first() for non-base yr....)





# Window functions
# Max + Min lifeExp countries for each observation year. 

# Start with.... 
my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  group_by(year)

# Include filter of min + max rank for each year
my_gap %>%
  filter(continent == "Asia") %>%
  select(year, country, lifeExp) %>%
  group_by(year) %>%
  filter(min_rank(desc(lifeExp)) < 2 | min_rank(lifeExp) < 2) %>% 
  arrange(year) %>%
  print(n = Inf)
??top_n
?print()
?rt()

# Using mutate to create temporary variables for min + max rank
my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  group_by(year) %>% 
  mutate(le_rank = min_rank(lifeExp),
         le_desc_rank = min_rank(desc(lifeExp))) %>%       
          # minr_rank(desc()) = max_rank
  filter(country %in% c("Afghanistan", "Thailand", "Japan"), year > 1995)

# top_n function for only min. OR max.:
my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  top_n(1, wt = lifeExp)   # Max.

my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  top_n(-1, wt = desc(lifeExp)) # Max.

my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  top_n(1, wt = desc(lifeExp))  # Min. 

my_gap %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  top_n(-1, wt = lifeExp)  # Min. 

# Check worst life expectancy changes in 5 year period.

my_gap %>% 
  select(country, year, continent, lifeExp) %>% 
  group_by(continent, country) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
  top_n(-1, wt = worst_le_delta) %>% 
  arrange(worst_le_delta)
# Countries with smallest life exp. delta (little to no change in a 5 year period): 
my_gap %>% 
  select(country, year, continent, lifeExp) %>% 
  group_by(continent, country) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
  top_n(1, wt = worst_le_delta) %>% 
  arrange(worst_le_delta)
# Countries with largest life exp. delta (large positive change in 5 year period);
my_gap %>% 
  select(country, year, continent, lifeExp) %>% 
  group_by(continent, country) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  summarize(best_le_delta = max(le_delta, na.rm = TRUE)) %>% 
  top_n(1, wt = best_le_delta) %>% 
  arrange(desc(best_le_delta))

# Top 3 increase, by continent
my_gap %>% 
  select(country, year, continent, lifeExp) %>% 
  group_by(continent, country) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  summarize(best_le_delta = max(le_delta, na.rm = TRUE)) %>% 
  top_n(3, wt = best_le_delta) %>% 
  arrange(continent, desc(best_le_delta))


# Top 3 decrease, by continent
my_gap %>% 
  select(country, year, continent, lifeExp) %>% 
  group_by(continent, country) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>% 
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>% 
  top_n(-3, wt = worst_le_delta) %>% 
  arrange(continent, worst_le_delta)


# LORD OF THE RINGS dataset: 
lotr.clean <- read.table("~/R materials/lotr_clean.tsv", header=TRUE, sep="\t", fileEncoding="windows-1252")
lotr.tidy <- read.csv("~/R materials/lotr_tidy.csv", header = TRUE)

# Total # words by Male.Hobbits?
lotr.tidy %>% 
  count(Gender, Race, wt = Words)
# 8780 words!

# Which race has most words across three movies? 
(by_race_film <- lotr.tidy %>% 
    group_by(Film, Race) %>% 
    summarize(Words = sum(Words)) %>% 
    arrange(Film, desc(Words)))
# how order/arrange by custom (Fellowship > 2 Towers > Return)?????? 

# Plot results:
plot <- ggplot(by_race_film, aes(x = Film, y = Words, fill = Race))
plot + geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + guides(fill = guide_legend(reverse = TRUE)) +   # use guide_legend(reverse = TRUE) for order legend according to amount
  theme_economist()

# Tidying LotR data
# load separate datasets:
fship <- read.csv("~/R materials/lotr/The_Fellowship_Of_The_Ring.csv")
ttow <- read.csv("~/R materials/lotr/The_Two_Towers.csv")
rking <- read.csv("~/R materials/lotr/The_Return_Of_The_King.csv")
# dataframe per film, 4 common variables across sets. 
# bind rows using bind_rows(): 
lotr_untidy <- bind_rows(fship, ttow, rking)
str(lotr_untidy)
lotr_untidy

# Words = split over 'Female' and 'Male' variables. 
# Necessity gather Words into single variable with new variable 'Gender' to differentiate
# between 'Male' and 'Female'. Use gather(): 
lotr_tidy <-
  gather(lotr_untidy, key = 'Gender', value = 'Words', Male, Female)
lotr_tidy
# Read from LEFT to RIGHT.
# gather -values- of 'Male' and 'Female' into 'Words' >>> 
# create new variable 'Gender' as a -key- that signals whether 'Male' or 'Female'
# All OTHER variable = remain unchanged

# Write to csv file using write_csv(): 
write_csv(lotr_tidy, path = "~/R materials/lotr/lotr_tidyryo.csv")

# Practice:
Female <- read.csv("~/R materials/lotr/Female.csv")
Male <- read.csv("~/R materials/lotr/Male.csv")
Female
Male

lotr_tidy_genderinho <- bind_rows(Female, Male)
lotr_tidy_genderinho

# Need to gather 'Words' for each 'Race'
lotr_tidy2.0 <-
  gather(lotr_tidy_genderinho, key = 'Race', value = 'Words', Man, Hobbit, Elf) %>% 
  select(Film, Race, Gender, Words)

lotr_tidy2.0

lotr_tidy2.0 %>% 
  select(Film, Gender, Race, Words) %>% 
  group_by(Race, Film, Gender = 'Female') %>% 
  mutate(totalwords = sum(Words))

# Using spread(), make data untidy:
lotr_tidy2.0

# On 'Race' 
lotr_tidy2.0 %>% 
  spread(key = Race, value = Words)

# On 'Gender'
lotr_tidy2.0 %>% 
  spread(key = Gender, value = Words)

# Use unite() to combine 'Race' and 'Gender': 
lotr_tidy2.0 %>% 
  unite(Race_Gender, Race, Gender) %>% 
  spread(key = Race_Gender, value = Words)

# On 'Film'
lotr_tidy2.0 %>% 
  spread(key = Film, value = Words)



library(reshape2)
lotr_untidy
melt(lotr_untidy, value.name = 'Words')


# dplyr JOIN functions! 
rm(list = ls())
suppressPackageStartupMessages(library(dplyr))
library(readr)

superheroes <- "
name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes <- read_csv(superheroes, trim_ws = TRUE, skip = 1)

publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"
publishers <- read_csv(publishers, trim_ws = TRUE, skip = 1)

# INNER_JOIN(x,y): Return all rows from x where there are matching values in y, 
# and all columns from x and y. If there are multiple matches between x and y, 
# all combination of the matches are returned. This is a mutating join.
(ijsp <- inner_join(superheroes, publishers))

(ijps <- inner_join(superheroes, publishers))

# SEMI_JOIN(x,y): Return all rows from x where there are matching values in y, 
# keeping just columns from x. A semi join differs from an inner join because an 
# inner join will return one row of x for each matching row of y, where a semi join 
# will never duplicate rows of x. This is a filtering join.

(sjsp <- semi_join(superheroes, publishers))

(sjps <- semi_join(publishers, superheroes))

# similar to ^inner_join but contains only variables from x = superheroes, order row = changed. 


# LEFT_JOIN(x,y): Return all rows from x, and all columns from x and y. If there are multiple 
# matches between x and y, all combination of the matches are returned. This is a mutating join. 

(ljsp <- left_join(superheroes, publishers))

(ljps <- left_join(publishers, superheroes))

# ANTI_JOIN(x,y): Return all rows from x where there are not matching values in y, keeping just 
# columns from x. This is a filtering join.
(ajsp <- anti_join(superheroes, publishers))

(ajps <- anti_join(publishers, superheroes))

# FULL_JOin(x,y): Return all rows and all columns from both x and y. Where there are not matching 
# values, returns NA for the one missing. This is a mutating join.
(fjps <- full_join(publishers, superheroes))
(fjsp <- full_join(publishers, superheroes))


# TABLE LOOK-UP:
rm(list = ls())
library(gapminder)

mini_gap <- gapminder %>% 
  filter(country %in% c("Belgium", "Canada", "United States", "Mexico"),
         year > 2000) %>% 
  select(-pop, -gdpPercap) %>% 
  droplevels() 
?droplevels
mini_gap

# FOOD lookup table:
food <- tribble(
  ~ country, ~ food,
  "Belgium", "waffle",
  "Canada", "poutine", 
  "United States", "Twinkie"
  )
food

# match(x,table):  reports where the values in the key x appear in the lookup variable table. 
# It returns positive integers for use as indices.
(indices <- match(x = mini_gap$country, table = food$country))
add_column(food[indices, ], x = mini_gap$country)

mini_gap %>% 
  mutate(food = food$food[indices])

#         country continent  year lifeExp    food
#          <fctr>    <fctr> <int>   <dbl>   <chr>
# 1       Belgium    Europe  2002  78.320  waffle
# 2       Belgium    Europe  2007  79.441  waffle
# 3        Canada  Americas  2002  79.770 poutine
# 4        Canada  Americas  2007  80.653 poutine
# 5        Mexico  Americas  2002  74.902    <NA>
# 6        Mexico  Americas  2007  76.195    <NA>
# 7 United States  Americas  2002  77.310 Twinkie
# 8 United States  Americas  2007  78.242 Twinkie

# OR JUST USE JOIN:
(food_mini_gap <- mini_gap %>% 
  left_join(food))

# Indexing by name:
(food_vec <- setNames(food$food, food$country))
?setNames

mini_gap %>% 
  mutate(food = food_vec[country])
#         country continent  year lifeExp    food
#          <fctr>    <fctr> <int>   <dbl>   <chr>
# 1       Belgium    Europe  2002  78.320  waffle
# 2       Belgium    Europe  2007  79.441  waffle
# 3        Canada  Americas  2002  79.770 poutine
# 4        Canada  Americas  2007  80.653 poutine
# 5        Mexico  Americas  2002  74.902 Twinkie
# 6        Mexico  Americas  2007  76.195 Twinkie
# 7 United States  Americas  2002  77.310    <NA>
# 8 United States  Americas  2007  78.242    <NA>

# TWINKIES =/ Mexico's food......................
unclass(mini_gap$country)
# attr(,"levels")
# [1] "Belgium"       "Canada"        "Mexico"        "United States"
# mini_gap$country = FACTOR, necessity coerce $country into CHAR. 
mini_gap %>% 
  mutate(food = food_vec[as.character(country)])


# SPLIT-APPLY-COMBINE
# Nesting as an extension of grouping. 
# Subset data and create new data set in environment or filter/subset within code?
snippet <- my_big_dataset %>%
  filter(some_variable == some_value)
## or
snippet <- subset(my_big_dataset, some_variable == some_value)

# Ex. 2
canada_fit <- lm(lifeExp ~ year, data = gapminder, subset = country == "Canada")
canada_fit
# Ex. 3
oceania_ttest <- gapminder %>% 
  filter(continent == "Oceania") %>% 
  t.test(gdpPercap ~ country, data = .)
oceania_ttest
                  
# SPLIT data into pieces
# APPLY function to pieces
# COMBINE results back together
# Use group_by or summarize() NOT effective past more than single #. 

# Use NESTING()
# Create list-columns by collapsing to single row/group.
gap_nested <- gapminder %>% 
    group_by(continent, country) %>% 
  nest()
# data variable as a list
# View:
gap_nested[[1, "data"]]
gap_nested$data[[1]]                  
# 1 = Afghanistan in this case...

# Fit model to Afghanistan data:
(fit <- lm(lifeExp ~ I(year - 1950), data = gap_nested[[1, "data"]]))
# Intercept: 29.3566
# I(year - 1950): 0.2753
le_vs_year <- function(df) {
  lm(lifeExp ~ I(year - 1950), data = df)
}
le_vs_year(gap_nested[[1, "data"]])
# Intercept: 29.3566
# I(year - 1950): 0.2753

le_vs_year(gap_nested[[2, "data"]])
# Intercept: 58.5598
# I(year - 1950): 0.3347

# map() to apply function to other elements of gap_nested$data
fits <- map(gap_nested$data[1:2], le_vs_year)
fits

# scale to ALL countries? Use map() INSIDE mutate()
gap_nested <- gap_nested %>% 
  mutate(fit = map(data, le_vs_year))
gap_nested

# list-column = temoporary, pull info from using tidy(), augment(), glance()
library(broom)
library(tidyverse)
tidy(gap_nested$fit[[1]])

(gap_nested <- gap_nested %>% 
  mutate(tidy = map(fit, tidy)))

# Unnest() to return data into tibbles
gap_coefs <- gap_nested %>% 
  select(continent, country, tidy) %>% 
  unnest(tidy)
gap_coefs
# Rename variable names:
(gap_coefs <- gap_coefs %>% 
  mutate(term = recode(term, `(Intercept)` = "intercept",
                       `I(year-1950)` = "slope")))
gap_coefs

(gap_ests <- gap_coefs %>% 
  select(continent:estimate) %>% 
  mutate(term = recode(term, `(Intercept)` = "intercept",
                       `Iyear-1950)` = "slope")))` %>% 
  spread(key = term, value = estimate))
gap_ests
gap_ests %>% 
  select(intercept, slope) %>% 
  summary()

#########################
#########################
# dplyr with supercars dataset:

df.car_torque <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"))
df.car_0_60_times  <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
df.car_engine_size <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"))
df.car_horsepower  <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"))
df.car_top_speed   <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"))
df.car_power_to_weight <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"))

#-------------------------
# Inspect data with head()




#-------------------------
head(df.car_torque)
head(df.car_0_60_times)
head(df.car_engine_size)
head(df.car_horsepower)
head(df.car_top_speed)
head(df.car_power_to_weight)

df.car_top_speed %>% 
  head() %>% 
  select(car_full_nm)

# Search for duplicate records
# group_by car name, summarize by COUNT, filter NOT n = 1...

df.car_torque      %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_0_60_times  %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_horsepower  %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_top_speed   %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)

# 1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
# 2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
# 3                   Pontiac Bonneville 6.4L V8 - [1960]     2

# Remove duplicates by selecting only distinct: 
?distinct()

df.car_0_60_times  <- distinct(df.car_0_60_times ,car_full_nm, .keep_all = TRUE)
df.car_engine_size <- distinct(df.car_engine_size ,car_full_nm)
df.car_horsepower  <- distinct(df.car_horsepower ,car_full_nm)
df.car_top_speed   <- distinct(df.car_top_speed ,car_full_nm)
df.car_torque      <- distinct(df.car_torque ,car_full_nm)
df.car_power_to_weight <- distinct(df.car_power_to_weight, car_full_nm)

# JOIN datasets into one dataframe: 

df.car_spec_data <- left_join(df.car_horsepower, df.car_torque, by="car_full_nm")      # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_0_60_times, by="car_full_nm")   # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_engine_size, by="car_full_nm")  # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_top_speed, by="car_full_nm")    # count after join: 1578
df.car_spec_data <- left_join(df.car_spec_data, df.car_power_to_weight, by="car_full_nm") # count after join: 1578


# Test duplicates
df.car_spec_data  %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)

# local data frame [0 x 2] 
# i.e., NO duplicate records found...! 

str(df.car_spec_data)
head(df.car_spec_data)

# Create new variable

df.car_spec_data <- mutate(df.car_spec_data, year=sub(".*\\[([0-9]{4})\\]","\\1",car_full_nm))

str(df.car_spec_data$year)

#----------------
# NEW VAR: decade
#----------------

df.car_spec_data <- mutate(df.car_spec_data, 
                           decade = as.factor(
                             ifelse(substring(df.car_spec_data$year,1,3)=='193','1930s',
                                    ifelse(substring(df.car_spec_data$year,1,3)=='194','1940s',
                                           ifelse(substring(df.car_spec_data$year,1,3)=='195','1950s',
                                                  ifelse(substring(df.car_spec_data$year,1,3)=='196','1960s',
                                                         ifelse(substring(df.car_spec_data$year,1,3)=='197','1970s',
                                                                ifelse(substring(df.car_spec_data$year,1,3)=='198','1980s',
                                                                       ifelse(substring(df.car_spec_data$year,1,3)=='199','1990s',
                                                                              ifelse(substring(df.car_spec_data$year,1,3)=='200','2000s',
                                                                                     ifelse(substring(df.car_spec_data$year,1,3)=='201','2010s',"ERROR"
                                                                                     )))))))))
                           )
)

head(df.car_spec_data)
str(df.car_spec_data)


#-------------------------------
# NEW VAR: make_nm 
#  (i.e., the "make" of the car; 
#  the "brand name" of the car) 
#-------------------------------

df.car_spec_data <- mutate(df.car_spec_data, make_nm = gsub(" .*$","", df.car_spec_data$car_full_nm))


#--------------------------
# NEW VAR: car_weight_tons 
#--------------------------

df.car_spec_data <- mutate(df.car_spec_data, car_weight_tons = horsepower_bhp / horsepower_per_ton_bhp)


#--------------------------
# NEW VAR: torque_per_ton 
#--------------------------

df.car_spec_data <- mutate(df.car_spec_data, torque_per_ton = torque_lb_ft / car_weight_tons)










