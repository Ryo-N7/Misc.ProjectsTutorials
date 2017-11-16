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



# Explore purrr -----------------------------------------------------------
library(purrr)
library(repurrrsive)
library(listviewer)
# necessity to understand the lists being examined.
# - length of list, homogeneous components/structure, names, types of objects inside
# with str() use max.level and list.len args to limit volume of list output

listviewer::jsonedit(got_chars, mode = "view")

?str()
# max.level: displaying specified maximum level of nested structures - list of sublists, default display ALL nesting levels 
# list.len: numeric -- max. # of list elements to display within level - default = 99!

str(wesanderson, max.level = 1)
str(wesanderson, max.level = 0)
str(wesanderson, max.level = 2)

str(got_chars, max.level = 0)
str(got_chars, max.level = 1)
str(got_chars, max.level = 2)
str(got_chars, max.level = 3)

str(got_chars[[1]], list.len = 3)  # show 3 and rest truncated
str(got_chars[[1]], list.len = 5)  # shows 5 and rest truncated
str(got_chars[[1]], list.len = 1)  

str(got_chars, ma.level = 1, list.len = 3)
str(got_chars, ma.level = 2, list.len = 4)

str(got_chars[[1]], max.level = 1, list.len = 5) # show url, id, name, gender, culture!



# Intro to map(): extracting elements! ------------------------------------
library(purrr)
library(repurrrsive)
library(listviewer)

# vectorized/list-ized operations
(3:5)^2
sqrt(c(9, 16, 25))
# through R: raise to power of 2 and take sqrt() applied to each individual element in vector
# inside is a for() loop:
for (i in 1:n) {
  output[[i]] <- f(input[[i]])
}

# IF input = list, apply function f() to each lement of list to list-ize the computation
# necessity guarantee that function f() can be apply to each element in list specified!
# purrr::map() is a function for applying a function to each element of a list, lapply()
map(c(9, 16, 25), sqrt)
# template: map(your_list, your_function)

# Extract elements with "name"
map(got_chars[1:4], "name")
map(got_chars[1:4], "culture")
map(got_chars[1:4], "gender")
got_chars %>% map("name")
# shortcut to create a function that extract elements based on its name
# >>> provide "TEXT" to extract the element named "TEXT"

# shortcut 2: use positive integer to extract element based on position
map(got_chars[5:8], 3)   # position 3 = "name"
got_chars %>% map(3)
# provide i to extract the i-th element

got_chars[[1]] %>% names()
# playedBy = 18th position 
got_chars %>% map("playedBy")
got_chars[3:7] %>% map(18)
got_chars %>% map("nothere")  # returns list of naked NULL elements
got_chars[3:7] %>% map(255)   # returns list of naked NULL elements

# Type-specific mapping()
# map() always returnst LIST >>> even if all elements = same and length = 1
# prefer return atomic vector >>> use type-specific map()

map(got_chars[9:12], "name")           # compare this with below:
map_chr(got_chars[9:12], "name")
# [1] "Daenerys Targaryen" "Davos Seaworth"     "Arya Stark"         "Arys Oakheart"
map_chr(got_chars[9:12], 3)

str(got_chars[[1]])   # id is integer
got_chars[9:12] %>% map_int(2)
# [1] 1303 1319  148  149          NOICE

str(got_chars[1], max.level = 2)  # alive  is logical object
got_chars[10:18] %>% map_lgl("alive")
# [1]  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE        NOICE

# Extracting multiple values:
got_chars[[3]][c("name", "culture", "gender", "born")]
# usage of single [] and character vector to index by name of elements
# in map() we can map [] just like any other function!
map(.x, .f, ...)  # .f will be []
x <- got_chars %>% map(`[`, c("name", "culture", "gender", "born"))
str(x[16:17])   # list of 2 of a list of 4 each
# or use the extract() function from magrittr
library(magrittr)
x <- got_chars %>% map(extract, c("name", "culture", "gender", "born"))
str(x[16:17])   # output is the same as above!

got_chars[[1]] %>% names()    # name = 3, gender = 4, culture = 5, born = 6, died = 7
x <- got_chars %>% map(extract, c(3, 4, 5, 6, 7))
str(x[16:17])   # NOICE


# Data frame output:

# [] is non-simplifying, elements returned as LIST. map() itself returnst as LIST.
# went from recursive list to recursive list output >>> how to return a dataframe instead?
got_chars[2:4] %>% map_df(extract, c("name", "culture", "gender", "born"))
# A tibble: 3 x 4
# name  culture gender                         born
# <chr>    <chr>  <chr>                        <chr>
# 1  Tyrion Lannister            Male  In 273 AC, at Casterly Rock
# 2 Victarion Greyjoy Ironborn   Male In 268 AC or before, at Pyke
# 3              Will            Male                             

# Variables = automatically type-converted. 
# Much safer to explicit specify type and build dataframe manually:
library(tibble)
got_chars %>%  {
  tibble(
    name = map_chr(., "name"),           # '.' is placeholder for got_chars input
    culture = map_chr(., "culture"),     # {} prevent got_chars as arg in tibble()
    gender = map_chr(., "gender"),
    id = map_int(., "id"),
    born = map_chr(., "born"),
    alive = map_lgl(., "alive")
  )
}

# with positive integers to specify element:
got_info <- got_chars %>% 
  map_df(extract,
         c(2, 3, 4, 5, 6, 7, 8))
str(got_info)                  # integer, chr, logical types automatically converted...!



# Simplifying data from GitHub users list ---------------------------------
library(repurrrsive)
library(listviewer)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)

# Non-rectangular data structure >>> create neat data frame
# Objects: ex. JSON, XML format from API   (when csv is not option)

# gh_users: 
# - one component for each of the 6 users
# - each component is LIST with user info

str(gh_users)
str(gh_users, max.level = 1)
str(gh_users, list.len = 5)
str(gh_users[[1]], list.len = 5)
str(gh_users, list.len = 1)
str(gh_users[5], list.len = 4)


map(.x, .f, ... )
# .x = list   >>> gh_users
# .f = function to each component of list
# ... = etc.

map(gh_users, "login")  # extract element "login" from gh_users

str(gh_users, max.level = 2, list.len = 18)
# element 18 gives us the full name of each github user:
map(gh_users, 18)

# provide “TEXT” to extract the element named “TEXT”
# >>> equivalent to function(x) x[["TEXT"]]
# provide i to extract the i-th element
# >>> equivalent to function(x) x[[i]]
# OR with %>% 
gh_users %>% 
  map("login")

gh_users[[1]] %>% names()
gh_users %>% 
  map("created_at")
# OR
gh_users %>% 
  map(29)

# Extract multiple values
gh_users[[1]][c("name", "login", "id", "location")]

x <- map(gh_users, `[`, c("login", "name", "id", "location"))
str(x[1:2])
# OR 
x <- map(gh_users, magrittr::extract, c("login", "name", "id", "location"))
str(x[1:2])

x <- map(gh_users, magrittr::extract, c(1, 18, 2, 21))
str(x[1:2])

# Data frame output:
map_df(gh_users, magrittr::extract, c(1, 18, 2, 21))
# nice df output!
# automatic type conversion by map_df()
# may be necessary to explicitly specify type and build df 

gh_users %>% {
  tibble(
    login = map_chr(., "login"),
    name = map_chr(., 18),
    id = map_int(., 2),
    location = map_chr(., "location")
  )
}

# gh_repos
# - one component for each 6 users
# - each component = list of repos
# - several of repo list also repo
str(gh_repos, max.level = 1)
str(gh_repos, max.level = 2)

str(gh_repos[[2]], max.level = 1)


# Vector input to extraction shortcuts
# rather than name or position >>> use vector
# - the j-th element addresses the j-th level of the hierarchy

gh_repos %>% 
  map_chr(c(1, 3))   # 1 = first repo,  3 = full name
# NOT elements 1 and 3 of gh_repos

str(gh_repos[[2]], max.level =  3, list.len = 10)

str(gh_repos[[2]], max.level =  2)
str(gh_repos[[2]], max.level =  1)

gh_repos %>% 
  map_chr(c(1, 13))

gh_repos %>% 
  map_chr(c(1, 4, 1))

# List inside a data frame ####

# Get df with one row/repo, variables ID which GitHub user ownership, repo name, etc.

# 1. Put gh_repos list into data_frame, ID along usernames

user_names <- map_chr(gh_repos, c(1, 4, 1))

user_df <- gh_repos %>% 
  set_names(user_names) %>% 
  enframe("username", "gh_repos")

user_df
# username + list of 30 lists from gh_repos

user_df %>% 
  mutate(num_repos = map_int(gh_repos, length))
# count length of repos as number of repos within gh_repos for each username

# all repos for user one
user_one <- user_df$gh_repos[[1]]

one_repo <- user_one[[1]]
one_repo

# insidie is list of repo info
str(one_repo, max.level = 1, list.len = 5)

one_repo[c("name", "fork", "open_issues")]

# create df of all of user one's repos >>> show repo name, fork and open_issues
map_df(user_one, `[`, c("name", "fork", "open_issues"))

# create above for each user's each repo inside repo_info (name, fork, open_issues)!
user_df %>% 
  mutate(repo_info = gh_repos %>% 
           map(. %>% map_df(`[`, c("name", "fork", "open_issues"))))

# user-specific tibble in repo_info, open lists up into data frame
rdf <- user_df %>% 
  mutate(repo_info = gh_repos %>% 
           map(. %>% map_df(`[`, c("name", "fork", "open_issues")))
  ) %>% 
  select(-gh_repos) %>% 
  tidyr::unnest()

# now name, fork, open_issues are own column
# each row contains username + specific repo


# show repos of each user with most open issues
rdf %>% 
  filter(!fork) %>% 
  select(-fork) %>% 
  group_by(username) %>% 
  arrange(username, desc(open_issues)) %>% 
  slice(1:3)                                # show only top 3 repos (rows)



# Specifying the function in map() + parallel mapping ---------------------

library(purrr)
library(repurrrsive)

# map()
# map(vector_or_list_input, function_to_apply, other_options)

# specify general .f: 
# - an existing function
# - anonymous function
# - formula

aliases <- set_names(map(got_chars, "aliases"), 
                     map_chr(got_chars, "name"))
aliases

aliases <- aliases[c("Theon Grejoy", "Asha Greyjoy", "Brienne of Tarth")]
aliases

# Existing function:
my_fun <- function(x) paste(x, collapse = " | ")

# collapse list elements into one separated by | 
map(aliases, my_fun)

# Anonymous function:
map(aliases, function(x) paste(x, collapse = " | "))
# same result as previous!
map(aliases, paste, collapse = " | ")
# same as other previous, insert collapse as "..." part of map()

# Anonymous function, formula:
map(aliases, ~ paste(.x, collapse = " | "))
# same as above, use ~ to define as function, use .x to refer to input


# In workflow:
# pilot idea on SINGLE ELEMENT >>> scale up to all elements in vector/list

# Step 1:
(a <- map(got_chars, "aliases")[[19]]) ## OOPS! NULL --> a useless example
#> NULL

# Step 2:
(a <- map(got_chars, "aliases")[[16]]) ## ok good
#> [1] "Bran"            "Bran the Broken" "The Winged Wolf"

# Step 3:
paste(a, sep = " | ")                  ## OOPS! not what I want
#> [1] "Bran"            "Bran the Broken" "The Winged Wolf"

# Step 4:
paste(a, collapse = " | ")             ## ok good
#> [1] "Bran | Bran the Broken | The Winged Wolf"

# Step 5:
got_chars[15:17] %>%                   ## I am a programming god
  map("aliases") %>% 
  map_chr(paste, collapse = " | ")
#> [1] "Varamyr Sixskins | Haggon | Lump"                         
#> [2] "Bran | Bran the Broken | The Winged Wolf"                 
#> [3] "The Maid of Tarth | Brienne the Beauty | Brienne the Blue"

# List to DATA FRAME:

# Simplied all aliases to single string for each character >>> hold as atomic character
# vector instead of as list!
# use enframe() function to take named vector and promote as proper variable!

aliases <- set_names(map(got_chars, "aliases"), 
                     map_chr(got_chars, "name"))
aliases

map_chr(aliases[c(3, 10, 20, 24)], ~ paste(.x, collapse = " | ")) %>% 
  tibble::enframe(value = "aliases")  # value = set column/variable name!

??enframe()

# alternative:
tibble::tibble(
  name = map_chr(got_chars, "name"),
  aliases = got_chars %>% 
    map("aliases") %>% 
    map_chr(~ paste(.x, collapse = " | "))
) %>% 
  dplyr::slice(c(3, 10, 20, 24))

# THREE different ways to specify function `.f` in map() function in purrr
####
map(aliases, function(x) paste(x, collapse = "|")) 
map(aliases, paste, collapse = "|")
map(aliases, ~ paste(.x, collapse = " | "))
####

got_chars[[1]]

library(tibble)
allegiances <- tibble(
  name = map_chr(got_chars, "name"),
  house = got_chars %>% 
    map("allegiances") %>% 
    map_chr(~ paste(.x, collapse = " | ")))


# Parallel map ####

# map2()
# map function over two vectors/lists in parallel

# map2(.x, .y, .f, ... )
# map2(input_one, input_two, function_to_apply, other_options)

names <- got_chars %>% 
  map_chr("name")

birth <- got_chars %>% 
  map_chr("born")

my_fun <- function(x, y) paste(x, "was born", y)

# map over existing function:
map2_chr(names, birth, my_fun) %>% head(3)
# [1] "Theon Greyjoy was born In 278 AC or 279 AC, at Pyke"    
# [2] "Tyrion Lannister was born In 273 AC, at Casterly Rock"  
# [3] "Victarion Greyjoy was born In 268 AC or before, at Pyke"

# map over anonymous function (conventional form)
map2_chr(names, birth, function(x, y) paste(x, "was born", y)) %>% head(3)

# map over anonymous function (~ formula form)
map2_chr(names[1:3], birth[1:3], ~ paste(.x, "was born", .y))
map2_chr(names, birth, ~ paste(.x, "was born", .y)) %>% head(3)

# pmap() ####

# what if you need to map function over two or MORE vectors/lists in parallel?

# pmap(.l, .f, ... )
# pmap(list_of_input_lists, function_to_apply, other_options)

df <- got_chars %>% {
  tibble::tibble(
    name = map_chr(., "name"),
    aliases = map_chr(., "aliases"),
    allegiances = map_chr(., "allegiances")
  )
}

my_fun <- function(name, aliases, allegiances) {
  paste(name, "has", length(aliases), "aliases and", 
        length(allegiances), "allegiances")
}

df %>% 
  pmap_chr(my_fun) %>% 
  head(3)

# gh_users cuz GOT is not cooperating...
df <- gh_users %>% {
  tibble::tibble(
    name = map_chr(., "name"),
    id = map_chr(., "id"),
    location = map_chr(., "location")
  )
}

my_fun <- function(name, id, location) {
  paste(name, "a.k.a.", id, "is based in", 
        location, "it's a wonderful place")
}

df %>% 
  pmap_chr(my_fun) %>% 
  head(3)
# LOL

























