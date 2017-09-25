# Purrr tutorials
library(dplyr)
library(purrr)

# Background basics -------------------------------------------------------

# Vectors and lists
# a few atomic vectors
(v_log <- c(TRUE, FALSE, FALSE, TRUE))    # logical vector
(v_int <- 1:4)                            # integer vector
(v_doub <- 1:4*1.2)                       # double vector
(v_char <- letters[1:4])                  # character vector
# atomic vectors = homogenous. each atom = scalar, same type/storage mode
(logs <- c(log(5), log(6), log(7), log(8)))
is.logical(logs)  # F
is.numeric(logs)  # T
is.double(logs)   # T

as.integer(logs)   # convert to 1 1 1 2

# Indexing a vector
# address specific elements or atoms with x[___]
v_char   # a     b      c      d 
v_char[c(FALSE, FALSE, TRUE, TRUE)]   # c   d 
v_log #  TRUE,  FALSE, FALSE, TRUE
v_char[v_log] # a  d 

v_doub[2:3]
v_char[-2]

v_int[0]
# integer(0)    vector with length 0...
typeof(v_int[0])
# [1] "integer"
v_doub[100]
# [1] NA
typeof(v_doub[100])
# "double"

# Coercion
# convert or coerce a vector type into another:
v_log
# [1]  TRUE FALSE FALSE  TRUE
as.integer(v_log)
# [1] 1 0 0 1
v_int
# [1] 1 2 3 4
as.numeric(v_int)
# [1] 1 2 3 4
v_doub
# [1] 1.2 2.4 3.6 4.8
as.character(v_doub)
# [1] "1.2" "2.4" "3.6" "4.8"
as.character(as.numeric(as.integer(v_log)))
# [1] "1" "0" "0" "1"

v_doub_copy <- v_doub
str(v_doub_copy)
# num [1:4] 1.2 2.4 3.6 4.8
v_doub_copy[3] <- "uhohoh"   # add string
str(v_doub_copy)
# chr [1:4] "1.2" "2.4" "uhohoh" "4.8"     # changes entire vector type to character!

(big_plans <- rep(NA_integer_, 4))   # vector of NAs
# [1] NA NA NA NA
str(big_plans)
#  int [1:4] NA NA NA NA
big_plans[3] <- 5L                       # use L to explicitly request integer
# note that big_plans is still integer!
str(big_plans)
#  int [1:4] NA NA 5 NA
# note that omitting L results in coercion of big_plans to double
big_plans[1] <- 10
str(big_plans)
#  num [1:4] 10 NA 5 NA

as.logical(letters)  # coerce to vector of NAs

# Lists:
# individuals atoms = length > 1
# individual atoms =/ same type

(x <- list(1:3, c("four", "five")))
(y <- list(logical = TRUE, integer = 4L, double = 4*1.2, character = "character"))
(z <- list(letters[26:22], transcendental = c(pi, exp(1)), f = function(x) x^2))
# lists can be heterogenous, of different types, different lengths, have or not have names
is.list(x); is.list(y); is.list(z)
is.atomic(x); is.atomic(y); is.atomic(z)
is.recursive(x); is.recursive(y); is.recursive(z)

# Indexing lists:
# 1. single square brackets >>> always returns list
x[c(FALSE, TRUE)]
y[2:3]
z["transcendental"]

# 2. double square brackets >>> access single component, returns naked component, by name or integer
x[[2]]
x[2]
y[["double"]]
z["f"]
z[["f"]]

# 3. with $ >>> access single component such as in [[]]   necessity specify component by NAME 
z$transcendental

length(z["transcendental"]); length(z[["transcendental"]]); length(z$transcendental)

my_vec <- c(a = 1, b = 2, c = 3)
my_list <- list(a = 1, b = 2, c = 3)
my_vec[2:3]   
my_vec[[2:3]] # error
my_vec$b  # invalid for atomic vectors

# Vectorized operations
n <- 5
res <- rep(NA_integer_, n) 

for (i in seq_len(n)) {
  res[i] <- i ^ 2
}

res   #   1  4  9  16  25

# in R:
n <- 5

seq_len(n)^2  #   1  4  9  16  25

# not possible for lists, lists = heterogenous so makes sense...
exp(v_doub)
(l_doub <- as.list(v_doub))
exp(l_doub)  # ERROR: non-numeric arg to mathematical function
# So how apply function to each element in a list?
# purrr::map()
library(purrr)
map(l_doub, exp)   # == exp(v_doub)    yay!

# under the hood:
my_list <- list(...)

my_output <- 
  for(i in seq_along(my_list)) {
    my_output[[i]] <- f(my_list([[i]]))
  }
# but use purrr::map() to avoid having to do this each time..!


# Base vs. purrr functions ------------------------------------------------

# iterate over...
# elements of a list...
# rows or columns of a 2D object...
# sub-dataframes indced by one or more factors...
# tuples formed from the i-th element of several vectors of equal length...

# possible using base R with for() loops, apply(), by()
# but many times inconsistent and require further checking 

# Solutions: Purr package!
# - map() functions highly consistent 
# - type-safe simplification to atomic vector or dataframe >>> output ready for next step in process
# - concise syntax!
library(purrr)
library(repurrrsive)

# lapply() vs. purrr::map()
# - list in >>> list out
# - shortcuts for index by name or position, create anonymous functions

lapply(got_chars[1:3], 
       function(x) x[["name"]])
# vs.
map(got_chars[1:3], "name")

# sapply() vs.  ???
# - sapply() for simplification of output from lapply()

# GoT characters have one, some or no aliases
# >>> depending on which, sapply() call can return list OR character vector = Raise bugs & problems!
aliases1 <- sapply(got_chars[20:22], function(x) x[["aliases"]])
str(aliases1) # list of 3 

aliases2 <- sapply(got_chars[c(3, 22, 27)], function(x) x[["aliases"]])
str(aliases2) # charactor vector of length = 3!

got_chars[2:4]
map_chr(got_chars[2:4], "aliases")
# ERRROR: no aliases so does not turn into vector...

# vapply() vs. map_*()
# - vapply() requires specify template for return value >>> safer alternative to sapply()
# - in purrr = type-specific mapping functions: map_lgl(), map_int(), map_dbl(), map_chr()
# - list in >>> atomic vector out

vapply(got_chars[1:3],
       function(x) x[["name"]],
       character(1))
# [1] "Theon Greyjoy"     "Tyrion Lannister"  "Victarion Greyjoy"

map_chr(got_chars[1:3], "name")
# [1] "Theon Greyjoy"     "Tyrion Lannister"  "Victarion Greyjoy"

# vapply() ALWAYS simplifies
# template allows specification of form of each individual result but NOT specify form or dimension of OVERALL result
f <- function(x, n) rep(x, n)
n <- 3
vapply(c("a", "b"), f, character(n), n = n)
#>      a   b  
#> [1,] "a" "b"
#> [2,] "a" "b"
#> [3,] "a" "b"
n <- 1
vapply(c("a", "b"), f, character(n), n = n)
#>   a   b 
#> "a" "b"

# simplification done INSIDE vapply()

# ??? vs. map_df()
# purrr::map_df() function = list in >>> data frame out
# produce dataframe from list without using do.call()

l <- lapply(got_chars[23:25],
            `[`, c("name", "playedBy"))
mat <- do.call(rbind, l)  # use rbind() to combine rows
str(mat)  # list

(df <- as.data.frame(mat, stringsAsFactors = FALSE)) # create dataframe
str(df)   # df
# vs. in purr
map_df(got_chars[23:25],
       `[`, c("name", "playedBy"))

# extracting elemtns of different types:
data.frame(
  name = vapply(got_chars[23:25], `[[`,
                character(1), "name"),
  id = vapply(got_chars[23:25], `[[`,
              integer(1), "id"),
  stringsAsFactors = FALSE
)
# vs. in purr
tibble(
  name = map_chr(got_chars[23:25], "name"),   # character vector
  id = map_int(got_chars[23:25], "id")        # integer
)


# mapply() vs. map2() and pmap()
# iterate over 2 or more vectors/list in parallell

nms <- vapply(got_chars[16:18],
              `[[`, character(1), "name")
birth <- vapply(got_chars[16:18],
                `[[`, character(1), "born")
mapply(function(x, y) paste(x, "came into this cruel world", y),
       nms, birth)
# or in purrr
nms <- got_chars[16:18] %>% 
  map_chr("name")
birth <- got_chars[16:18] %>% 
  map_chr("born")

map2_chr(nms, birth, ~ paste(.x, "came into this cruel world", .y))  # .x = nms,  .y = birth


# aggregate() vs. dplyr::summarize()
library(dplyr)
library(gapminder)

(mini_gap <- gapminder %>% 
    filter(country %in% c("Canada", "Germany"), year > 2000) %>% 
    droplevels())
?droplevels

aggregate(lifeExp ~ country, mini_gap, mean)
# vs. in purrr
mini_gap %>% 
  group_by(country) %>% 
  summarize(lifeExp = mean(lifeExp))

# summaries of 2 variables for each country >>>> mean(lifeExp) and mean(GDP/capita)
aggregate(cbind(lifeExp, gdpPercap) ~ country, mini_gap, mean)
# or
aggregate(mini_gap[c("lifeExp", "gdpPercap")], list(mini_gap$country), mean)
# or
tapply(mini_gap$lifeExp, mini_gap$country, mean)
# or
tapply(mini_gap$lifeExp, mini_gap$country, mean, simplify = FALSE)
# vs. in purrr
mini_gap %>% 
  group_by(country) %>% 
  summarise_at(vars(lifeExp, gdpPercap), mean)

# bivariate sumamry of two variables for each country >>> correlation of lifeExp and year
by_obj <- by(gapminder, gapminder$country, 
             function(df) cor(df$lifeExp, df$year))
head(by_obj)
# or
by_obj <- by(gapminder, gapminder$country, 
             function(df) cor(df$lifeExp, df$year), 
             simplify = FALSE)
head(by_obj)
# vs. in purrr
gapminder %>% 
  group_by(country) %>% 
  summarise(cor = cor(lifeExp, year)) %>% 
  head(3)

# by() vs. tidyr::nest()
# fit linear model of lifeExp vs. year
# tidyverse: create nested dataframe with one meta-row/country

by_obj <- by(gapminder,
             gapminder$country,
             function(df) lm(lifeExp ~ year, data = df))
str(by_obj[1:2], max.level = 1)
by_obj[[1]]
# vs. in purrr
library(tidyr)
nested_df <- gapminder %>% 
  group_by(country, continent) %>% 
  nest() %>% 
  mutate(fit = map(data, ~ lm(lifeExp ~ year, data = .x)))
str(nested_df$fit[1:2], max.level = 1)
nested_df$fit[1:2]  # model for first two countries...!

# inspect for specific continent?
# base R: country info in names and continent info NOT directly ilnked to model fits
# tidyverse: fits live inside dataframe with country and continent info! use filter()!

o_countries <- as.character(unique(gapminder$country[gapminder$continent == "Oceania"]))
by_obj[names(by_obj) %in% o_countries]
# vs. in purrr
nested_df %>% 
  filter(continent == "Oceania") %>% 
  .$fit

# form data frame with one row/country with variables country, continent, estimated intercept + slope
# country and continent factors have same levels as in gapminder dataset
coefs <- lapply(by_obj, coef)
coefs <- do.call(rbind, coefs)
coefs <- data.frame(
  country = I(rownames(coefs)),
  coefs
)
coefs$continent <- gapminder$continent[match(coefs$country, gapminder$country)]
coefs$continent <- factor(coefs$continent, levels = levels(gapminder$continent))
coefs$country <- factor(coefs$country, levels = levels(gapminder$country))

head(coefs)    # UGH FINALLY

# vs. in purrr/tidyverse
nested_df %>% 
  mutate(coefs = map(fit, coef), 
         intercept = map_dbl(coefs, 1),
         slope = map_dbl(coefs, 2)) %>% 
  select(country, continent, intercept, slope)
?map()
nested_df$fit[[1]]
nested_df$data[[2]]
nested_df$fit[[2]][[1]][[1]]
nested_df$fit[[2]][[1]][[2]]
nested_df$fit[[2]][[2]]
nested_df$fit[[1]][[1]]

# This illustrates the post-processing that is often necessary in a base workflow. 
# We need to restore the country info from the names of by_obj, use them to look up 
# the continents in gapminder, and then restore the original factor levels for both 
# country and continent. It illustrates the payoff for temporarily tolerating the data 
# and fit list-columns in the nested data frame used in the tidyverse workflow. The country 
# and continent factors remain intact and directly associated with the data and fits.








