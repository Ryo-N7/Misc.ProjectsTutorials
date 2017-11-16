#' ---
#' title: "Live code from 'Writing functions' EARL workshop"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#' ## Where to find this document
#'
#' Shortlink humans can type:
#'
#'   * <http://bit.ly/jenny-live-code>
#'
#' Horrible link that reveals how this is done:
#'
#'   * <https://www.dropbox.com/s/2b8mi4rir23pvnx/jenny-live-code.R?raw=1>
#'
#' Using the `raw=1` query trick for rendering a DropBox-hosted file in the browser:
#'
#'   * <https://www.dropbox.com/en/help/desktop-web/force-download>
#' learned from [Michael Levy](https://twitter.com/ucdlevy).
#'
#' How this works:
#'
#'   * I code live in an R script locally.
#'   * This file lives in a directory synced to DropBox.
#'   * You open the DropBox file at <http://bit.ly/jenny-live-code> and refresh as needed.
#'   * Should allow you to see, copy, paste everything I've typed and save the
#'   entire transcript at the end. This file is highly perishable, so save your
#'   own copy if you want it.
#'
#' ## Writing R functions for fun and profit
#'
#' For future reference, consult these persistent resources:
#'
#'   * Function-writing content in STAT545.com
#'     - http://stat545.com/block011_write-your-own-function-01.html
#'     - and subsequent parts linked from above
#'   * purrr tutorial
#'     - https://jennybc.github.io/purrr-tutorial/
#'
#' ## Icebreaker
#'
#' * Introduce yourself to your neighbor(s).
#' * Rose and Thorn
#'   - Rose: a useful thing recently learned re: R
#'   - Thorn: something re: R that vexes or puzzles you; bonus points if we
#'   might be able to remedy it today!
#'
#+ setup, include = FALSE
knitr::opts_chunk$set(error = TRUE, collapse = TRUE)
#'
#' ## Load and say hi to gapminder

## install.packages("gapminder")
library(gapminder)

#' Make a quick plot to give a sense of this dataset.
#' Code from <http://socviz.co/groupfacettx.html>.
library(ggplot2)
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))
p + geom_line(color="gray70", aes(group = country)) +
  geom_smooth(size = 1.1, method = "loess", se = FALSE) +
  scale_y_log10(labels=scales::dollar) +
  facet_wrap(~ continent, ncol = 5) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita on Five Continents")

#' ## Max minus min: (maximum - minimum)
library(tidyverse)
gapminder %>% select(lifeExp) %>%  head(10)
max(gapminder$lifeExp) # 82.6
min(gapminder$lifeExp) # 23.599
range(gapminder$lifeExp)
diff(range(gapminder$lifeExp)) # 59.004

max(gapminder$lifeExp) - min(gapminder$lifeExp) # 59.004
with(gapminder, max(lifeExp) - min(lifeExp))    # 59.004
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]   # range()[2] is max, [1] is min

# writing functions: minimum viable product = limited-but-functioning is important!
mmm <- function(x) max(x) - min(x)   # name function differently............
mmm(gapminder$lifeExp)               # 59.004   (yay)

# informal testing: explore different inputs!
mmm(1:10)
mmm(runif(100))   # should be close to 1...
mmm(gapminder$gdpPercap)
mmm(gapminder$pop)

mmm("hello world")   # error: non-numeric arg to binary operator.
mmm(gapminder)       # error: only defined on a df with all numeric vars

mmm(gapminder$country) # error: 'max'/'min' NOT meaningful for factors

mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])   # took all num vars as large vector and max/min -_-"

mmm(c(TRUE, FALSE, FALSE, TRUE))  # TRUE = 1, FALSE = 0, therefore 1.

mmm(as.character(gapminder$lifeExp))

# Check validity of arguments with stopifnot()
mmm <- function(x) {
  stopifnot(is.numeric(x))    # stop code if input is NOT numeric  
  max(x) - min(x)
  
}

mmm(c(TRUE, FALSE, FALSE, TRUE))  # error: is.numeric(x) is not TRUE

mmm(as.character(gapminder$lifeExp)) # error: is.numeric(x) is not TRUE

mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')]) # error: is.numeric(x) is not TRUE

# Check validity of arguments with if() stop()
mmm <- function(x) {
  if(!is.numeric(x)) {
    stop("not numeric!")    # message for stop 
  }
  max(x) - min(x)
  
}

mmm(c(TRUE, FALSE, FALSE, TRUE))  # error: not numeric!

mmm(as.character(gapminder$lifeExp)) # error: not numeric!

mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])  # error: not numeric!

mmm2 <- function(x) {
  if(!is.numeric(x)) {
    stop("sorry, this function only works for numeric inputs.",
         " You provided class: ", class(x)[1], 
         call. = FALSE)    # message for stop 
  }
  max(x) - min(x)
  
}

mmm2(gapminder[c('lifeExp', 'gdpPercap', 'pop')])  # error: "sorry, this function..."

#' ## End of part 1, recap
#'
#' What have we done?
#'   * Wrote a working function from working top-level code in a specific example.
#'   * Formal validity checking of input
#'   * Informal testing


# Part 2
# Generalize to taking difference of quantiles!
quantile(gapminder$lifeExp)

quantile(gapminder$lifeExp)[1]
quantile(gapminder$lifeExp)[4]
quantile(gapminder$lifeExp)[5]

(quantile(gapminder$lifeExp)[5] - quantile(gapminder$lifeExp)[1])

median(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = 0.5)

quantile(gapminder$lifeExp, probs = c(0.25, 0.5, 0.75))
quantile(gapminder$lifeExp, probs = c(0.0, 1.0))

boxplot(gapminder$lifeExp, plot = FALSE)$stats

the_probs <- c(0.25, 0.75)
the_quantiles <- quantile(x, probs = the_probs)
max(the_quantiles) - min(the_quantiles) # 22.6475
IQR(gapminder$lifeExp)                  # 22.6475

# turn interactively working code into one function!
qdiff <- function(x, probs){
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

# Informal testing
qdiff(gapminder$lifeExp, probs = c(0.0, 1.0))     # 59.004
mmm(gapminder$lifeExp)                            # 59.004

qdiff(gapminder$lifeExp, probs = c(0.25, 0.75))   # 22.6475
IQR(gapminder$lifeExp)                            # 22.6475

qdiff(gapminder$gdpPercap, probs = c(0.25, 0.75))    # check to make sure not tied to just gapmidner$lifeExp!
IQR(gapminder$gdpPercap)  

# Argument names: freedoms + conventions!
qdiff2 <- function(zeus, hera) {
  stopifnot(is.numeric(zeus))
  the_quantiles <- quantile(x = zeus, probs = hera)
  max(the_quantiles) - min(the_quantiles)
}

qdiff2(zeus = gapminder$lifeExp, hera = 0:1)  # -_-""
# just use x, y, z, etc..................

qdiff3 <- function(my_x, my_probs) {
  stopifnot(is.numeric(my_x))
  the_quantiles <- quantile(x = my_x, probs = my_probs)
  max(the_quantiles) - min(the_quantiles)
}


my_x <- gapminder$lifeExp
my_probs <- c(0.25, 0.75)
qdiff_oops <- function(my_x, my_probs) {
  stopifnot(is.numeric(my_x))
  the_quantiles <- quantile(x = my_x, probs = my_probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff_oops(gapminder$gdpPercap, c(0.0, 1.0))


# What a function returns
# can use return() explicitly 
#+ eval = FALSE
return(max(the_quantiles) - min(the_quantiles))

# but usually for early returns as default just last expression typed in function

qdiff <- function(x, probs){
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)      
  # last expression^ = evaluated/computed and returned BY DEFAULT
}

# default values: 
qdiff(gapminder$lifeExp)  # error: argument "probs" is missing, NO default

qdiff4 <- function(x, probs = c(0, 1)){    # define default inside function()
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

qdiff4(gapminder$lifeExp)  # 59.004

# Validity checks
# to-add: check probs = between 0-1, is.numeric(), not assume order, not assume # of probs 

# restart R
library(gapminder)

qdiff4 <- function(x, probs = c(0, 1)){    
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff4(gapminder$lifeExp)  # 59.004,,,, ok working again

z <- gapminder$lifeExp
z[3] <- NA    # assign NA to 3rd element for explore functions and NAs
quantile(z)   # error: missing values/NaN's not allowed if na.rm = FALSE!
# add na.rm = T/F inside function!

qdiff5 <- function(x, probs = c(0, 1)){    
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  max(the_quantiles) - min(the_quantiles)
}
qdiff5(z)    # 59.004

qdiff6 <- function(x, probs = c(0, 1), na.rm = TRUE){   # set default
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = na.rm)
  max(the_quantiles) - min(the_quantiles)
}
qdiff6(z)    # 59.004
qdiff6(z, na.rm = FALSE)    # error!

# The `...` argument
# allow other arguments not specified...
# ex. type arg in quantile()
qdiff7 <- function(x, probs = c(0, 1), na.rm = TRUE, ...){   # set default
  stopifnot(is.numeric(x))
  the_quantiles <- 
    quantile(x, probs = probs, na.rm = na.rm, ...)
  max(the_quantiles) - min(the_quantiles)
}
qdiff7(gapminder$lifeExp)    # 59.004
qdiff7(gapminder$lifeExp, probs = c(0.25, 0.75), type = 1)  # 22.686
qdiff7(gapminder$lifeExp, probs = c(0.25, 0.75), type = 9)  # 22.66438  0_o

foo <- function(...) {
  x <- list(...)
  x[1]   # return the first element
}

foo(a = 1, b = 2)

?do.call
?purrr::lift


# Formal unit testing
# install.packages("testthat")
library(testthat)

qdiff7("eggplants")

test_that("invalid args are detected", {
  expect_error(qdiff7("eggplants"),                 # string
               "is.numeric\\(x\\) is not TRUE")
  expect_error(qdiff7(iris))                        # data frame
})

test_that("NA handling works", {
  expect_error(qdiff7(c(1:5, NA), na.rm = FALSE))
  expect_silent(qdiff7(c(1:5, NA), na.rm = TRUE))
  expect_equal(qdiff7(c(1:5, NA), na.rm = TRUE), 4)
})
# Test success = NO return

# try diff without NA
qdiff_no_NA <- function(x, probs = c(0, 1)){   # no na.rm arg set
  stopifnot(is.numeric(x))
  the_quantiles <- 
    quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

test_that("NA handling works", {
  expect_equal(qdiff_no_NA(c(1:5, NA), na.rm = TRUE), 4)
})
# error: test failed >>> unused arg (na.rm = TRUE)

# map() = lapply()

# error messages  >>> package: glue











#' ---
#' title: "Live code from 'Writing functions' EARL workshop"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#' ## Where to find this document
#'
#' Shortlink humans can type:
#'
#'   * <http://bit.ly/jenny-live-code>
#'
#' Horrible link that reveals how this is done:
#'
#'   * <https://www.dropbox.com/s/2b8mi4rir23pvnx/jenny-live-code.R?raw=1>
#'
#' Using the `raw=1` query trick for rendering a DropBox-hosted file in the browser:
#'
#'   * <https://www.dropbox.com/en/help/desktop-web/force-download>
#' learned from [Michael Levy](https://twitter.com/ucdlevy).
#'
#' How this works:
#'
#'   * I code live in an R script locally.
#'   * This file lives in a directory synced to DropBox.
#'   * You open the DropBox file at <http://bit.ly/jenny-live-code> and refresh as needed.
#'   * Should allow you to see, copy, paste everything I've typed and save the
#'   entire transcript at the end. This file is highly perishable, so save your
#'   own copy if you want it.
#'
#' ## Writing R functions for fun and profit
#'
#' For future reference, consult these persistent resources:
#'
#'   * Function-writing content in STAT545.com
#'     - http://stat545.com/block011_write-your-own-function-01.html
#'     - and subsequent parts linked from above
#'   * purrr tutorial
#'     - https://jennybc.github.io/purrr-tutorial/
#'
#' ## Icebreaker
#'
#' * Introduce yourself to your neighbor(s).
#' * Rose and Thorn
#'   - Rose: a useful thing recently learned re: R
#'   - Thorn: something re: R that vexes or puzzles you; bonus points if we
#'   might be able to remedy it today!
#'
#+ setup, include = FALSE
knitr::opts_chunk$set(error = TRUE, collapse = TRUE)
#'
#' ## Load and say hi to gapminder

## install.packages("gapminder")
library(gapminder)

#' Make a quick plot to give a sense of this dataset.
#' Code from <http://socviz.co/groupfacettx.html>.
library(ggplot2)
p <- ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap))
p + geom_line(color="gray70", aes(group = country)) +
  geom_smooth(size = 1.1, method = "loess", se = FALSE) +
  scale_y_log10(labels=scales::dollar) +
  facet_wrap(~ continent, ncol = 5) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita on Five Continents")

#' ## Max minus min: get something that works
gapminder$lifeExp
min(gapminder$lifeExp)
max(gapminder$lifeExp)
range(gapminder$lifeExp)

max(gapminder$lifeExp) - min(gapminder$lifeExp)
with(gapminder, max(lifeExp) - min(lifeExp))
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]
with(gapminder, range(lifeExp)[2] - range(lifeExp)[1])
diff(range(gapminder$lifeExp))

#' ## Build our minimum viable product

mmm <- function(x) max(x) - min(x)
mmm(gapminder$lifeExp)

#' ## Informal testing

mmm(1:10)
mmm(runif(100))

mmm(gapminder$gdpPercap)
mmm(gapminder$pop)

mmm("hi there")
mmm(gapminder)
mmm(gapminder$country)

mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
mmm(c(TRUE, FALSE, FALSE, TRUE))

#' ## Check validity of arguments with stopifnot()

mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}
mmm("hi there")
mmm(gapminder)
mmm(gapminder$country)

mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])
mmm(c(TRUE, FALSE, FALSE, TRUE))

#' ## Check validity of arguments with if() stop()

mmm <- function(x) {
  if(!is.numeric(x)) {
    stop("not numeric!")
  }
  max(x) - min(x)
}
mmm(gapminder[c('lifeExp', 'gdpPercap', 'pop')])

mmm2 <- function(x) {
  if(!is.numeric(x)) {
    stop('I am so sorry, but this function only works for numeric input!\n',
         'You have provided an object of class: ', class(x)[1],
         call. = FALSE)
  }
  max(x) - min(x)
}
mmm2(gapminder[c('lifeExp', 'gdpPercap', 'pop')])

#' ## End of part 1, recap
#'
#' What have we done?
#'   * Wrote a working function from working top-level code in a specific example.
#'   * Formal validity checking of input
#'   * Informal testing
#'
#' ## Generalize to taking difference of quantiles

quantile(gapminder$lifeExp)
## ?quantile
quantile(gapminder$lifeExp, probs = 0.5)
median(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs =c(0.25, 0.75))
boxplot(gapminder$lifeExp, plot = FALSE)$stats

the_probs <- c(0.25, 0.75)
the_quantiles <- quantile(gapminder$lifeExp, probs = the_probs)
max(the_quantiles) - min(the_quantiles)
IQR(gapminder$lifeExp)

#' ## Turn interactively working code into a function
library(gapminder)
qdiff1 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

#' ## Informal testing
qdiff1(gapminder$lifeExp, probs = c(0.25, 0.75))
IQR(gapminder$lifeExp)
qdiff1(gapminder$lifeExp, probs = c(0, 1))
mmm(gapminder$lifeExp)
qdiff1(gapminder$gdpPercap, probs = c(0.25, 0.75))
IQR(gapminder$gdpPercap)

#' ## Argument names: freedom and conventions
qdiff2 <- function(zeus, hera) {
  stopifnot(is.numeric(zeus))
  the_quantiles <- quantile(x = zeus, probs = hera)
  max(the_quantiles) - min(the_quantiles)
}
qdiff2(zeus = gapminder$lifeExp, hera = 0:1)

qdiff3 <- function(my_x, my_probs) {
  stopifnot(is.numeric(my_x))
  the_quantiles <- quantile(x = my_x, probs = my_probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff3(my_x = gapminder$lifeExp, my_probs = 0:1)

qdiff1

#' ## What a function returns

#' You can use `return()` explicitly, like so:

#+ eval = FALSE
return(max(the_quantiles) - min(the_quantiles))

#' But it is more typical to save that for early returns.

#+ eval = TRUE
qdiff1 <- function(x, probs) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

#' ## Default values

qdiff1(gapminder$lifeExp)

qdiff4 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff4(gapminder$lifeExp, c(0, 0.4, 0.7))
qdiff4(gapminder$lifeExp, c(0.7, 0.4))

#' ## Revisiting our validity checks

#' What else could we add?
#'
#'   * are the probs between 0 and 1?
#'   * are there two probs?
#'   * is probs numeric?
#'   * are the probs in order?

#' Restart R here
library(gapminder)
qdiff4 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff4(gapminder$lifeExp)

z <- gapminder$lifeExp
z[3] <- NA
quantile(z)
qdiff4(z)

qdiff5 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  max(the_quantiles) - min(the_quantiles)
}
qdiff5(z)

qdiff6 <- function(x, probs = c(0, 1), na.rm = TRUE) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = na.rm)
  max(the_quantiles) - min(the_quantiles)
}
qdiff6(z)
qdiff6(z, na.rm = FALSE)

#' ## The `...` argument
qdiff7 <- function(x, probs = c(0, 1), na.rm = TRUE, ...) {
  stopifnot(is.numeric(x))
  the_quantiles <-
    quantile(x, probs = probs, na.rm = na.rm, ...)
  max(the_quantiles) - min(the_quantiles)
}
qdiff7(gapminder$lifeExp)
qdiff7(gapminder$lifeExp, probs = c(0.25, 0.75), types = 1)
qdiff7(gapminder$lifeExp, probs = c(0.25, 0.75), type = 9)

foo <- function(...) {
  x <- list(...)
  x[1]
}
foo(a = 1, b = 2)
## ?do.call
## ?purrr::lift

#' ## Formal unit testing

## install.packages("testthat")
library(testthat)

test_that("invalid args are detected", {
  expect_error(qdiff7("eggplant"),
               "is.numeric\\(x\\) is not TRUE")
  expect_error(qdiff7(iris))
})

test_that("NA handling works", {
  expect_error(qdiff7(c(1:5, NA), na.rm = FALSE))
  expect_silent(qdiff7(c(1:5, NA), na.rm = TRUE))
  expect_equal(qdiff7(c(1:5, NA), na.rm = TRUE), 4)
})

qdiff_no_NA <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <-
    quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
test_that("NA handling works", {
  #expect_equal(qdiff_no_NA(c(1:5, NA), na.rm = TRUE), 4)
  expect_equal(qdiff_no_NA(c(1:5, NA)), 4)
})

