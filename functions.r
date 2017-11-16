# Functions!

# use when copy-paste more than twice......

df <- tibble(
  a = rnorm(10), 
  b = rnorm(10), 
  c = rnorm(10),
  d = rnorm(10)
)
# rescale each column to range = 0.0-1.0
df$a <- (df$a - min(df$a, na.rm = T)) / (max(df$a, na.rm = T) - min(df$a, na.rm = T))
df$b <- (df$b - min(df$b, na.rm = T)) / (max(df$b, na.rm = T) - min(df$b, na.rm = T))
df$c <- (df$c - min(df$c, na.rm = T)) / (max(df$c, na.rm = T) - min(df$c, na.rm = T))
df$d <- (df$d - min(df$d, na.rm = T)) / (max(df$d, na.rm = T) - min(df$d, na.rm = T))

# Analyze code:
# how many inputs?
df$a <- (df$a - min(df$a, na.rm = T)) / (max(df$a, na.rm = T) - min(df$a, na.rm = T))
# one input: df$a
# Rewrite using temporary variables/general names
x <- df$a
(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
# Computing range three times >>> reduce to once.
rng <- range(x, na.rm = T) # range = min, max
(x - rng[1]) / (rng[2] - rng[1])

# Pull intermediate calculations into named variables = clarity!
rescale01 <- function(x) {
  rng <- range(x, na.rm =  TRUE)
  (x - rng[1]) / (rng[2]- rng[1])
}
# 1. pick function name
# 2. list inputs/arguments to function inside function(...)
# 3. Place code in body of function(){...}

# Check function with different inputs
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

# Unit-testing for formal automated testing of functions >>> http://r-pkgs.had.co.nz/tests.html

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
# much better! still some duplication involved as done across columns

# If requirements change, only change in one place:
# if variables contain infinite values... rescale01() will fail!
x <- c(1:10, Inf)
rescale01(x)

# Fix:
rescale01 <- function(x) {
  rng <- range(x, na.rm =  TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2]- rng[1])
}
rescale01(x)

# DRY: DONT REPEAT YOURSELF.
# More repetition = more updates necessary for change in different places!

# Exercises:
# 1. Why is TRUE not parameter in rescale01()? if x had NA value and na.rm = FALSE?
# if any NA and na.rm = F then function would return NA

# 2. Remap -Inf as 0 and Inf as 1.
rescale01 <- function(x) {
  rng <- range(x, na.rm =  TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2]- rng[1])
  y[y == -Inf] <- 0
  y[y== Inf] <- 1
}

rescale01(x)

# 3. Practice:
# a. mean(is.na(x))

avg_na <- function(x) {
  mean(is.na(x))
}
avg_na(c(NA, 0, NA, 0, NA))
# b. x / sum(x, na.rm = TRUE)
prop_x <- function(x) {
  x / sum(x, na.rm = TRUE)
}
prop_x(0:5)
sum(prop_x(0:5))
# c. sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
var_x <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
var_x(runif(10))

# Function for variance
x <- x[!is.na(x)] # x(i)
n <- length(x)    # n
m <- mean(x)     # x_bar
sq_err <- (x - m)^2 # x - x_bar >>> squared
sum(sq_err) / (n - 1)  # summa (sqr.error)  /   (n - 1)

variance_x <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  sq_err <- (x - m)^2
  sum(sq_err)/(n-1)
}
variance_x(1:10)  # 9.16
var(1:10)         # 9.16

# Function for skewness
x <- x[!is.na(x)]
n <- length(x)
m <- mean(x)
a <- sum((x - m )^3) / n
b <- sqrt(sum((x-m)^2) / (n- 1))

skew_x <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  m3 <- sum((x - m)^3)/ n 
  s3 <- sqrt(sum((x-m)^2) / (n-1))
  m3/s3
}

skew_x(rgamma(10, 1, 1))

# Create own functions (economics) ----------------------------------------

GDP_exp <- function(C, I, G, Xn) {
  C + I + G + Xn
}

GDP_exp(5, 4, 20, 5)

GDP_inc <- function(W, I, R, P) {
  W + I + R + P
}

GDP_inc(45, 500.5, 42587, 23987)


# Conditional execution ---------------------------------------------------

# if_else statements

# example:

has_name <- function(x) {
  nms <- names(x)
  if(is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

# return logical vector (T/F) describing whether each element of vector = named!

# Conditions
if(c(TRUE, FALSE)) {}

if(NA) {}

# Use || (or), && (and) to combine multiple logical expressions
# ==, |, & are vectorized >>> more than one output
# identical() = non-vectorized

identical(0L, 0)

# floating point numbers.
x <- sqrt(2)^2
x
x == 2
x - 2

# Multiple conditions:
# chain multiple if statements

if(this) {
  # do that
} else if (that) {
  # do something else
  else {
    #
  }
}

# if multiple long if statements >>> use switch() function instead

operations <- function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unknown op!")
         )
}

operations(5,2, "divide")
operations(10, 58, "times")

# or use cut() to discretize continuous variables

# Code style:

# if , function followed by {} brackets, contents indented by two spaces.

# Good:
if (y < 0 && debug) {
  message("Y is negative")
}

# Bad:
if (y < 0 && debug)
  message("Y is negative")

# Good:
if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad:
if (y == 0) {
  log(x)
}
else {
  y ^ x
}

# Not need {} if fit in one line:
y <- 10
x <- if (y < 20) "Too low" else "Too high"

# better if:
if (y < 20) {
  x <- "Too low"
} else {
  x <- "Too high"
}


# if: tests single condition
# if_else: tests each element


# Exercise:

# greeting function depending on time of day

greetings <- function(time = lubridate::now(x)) {
  hour <- lubridate::hour(time)
  if(hour < 12) {
    print("good morning")
    } else if (hour < 17) {
    print("good afternoon")
    } else {
      print("good evening")
    }  
}

greetings(time = lubridate::ymd_h("2017-01-07:12"))

greetings(time = lubridate::ymd_h("2017-01-07:08"))



# Function arguments ------------------------------------------------------

# arguments: 
# data = supply data for computation
# details: arguments for details of computation

# log() >>> data = x, detail = log base for algorithm
# log(x, base = exp(5))

# Generally: data arguments come first. Detail arguments at end. Should have defaults.
mean_confint <- function(x, conf = 0.95) { # set conf default as 0.95!
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_confint(x)
mean_confint(x, conf = 0.99)

# Choosing argument names:

# common names:
# vectors: x, y, z
# vector of weights: w
# data frame: df
# numeric indices: i (rows), j (columns)
# length, # of rows: n
# number of columns: p
# use na.rm to match other R functions

# Checking values: 

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

# what IF x and w NOT same length?
wt_mean(1:6, 1:3)
# 7.66667           vector recycling = NO ERROR SHOWN!

# use pre-conditions!
wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be same length", call. = FALSE)
  }
  sum(x * w) / sum(w)
}

wt_mean(1:6, 1:3)
# Error: `x` and `w` must be same length

# Don't go too overboard! 
# Can use built-in stopifnot() function to check each argument = TRUE
# stopifnot(): check for TRUE rather than wrong

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")  # na.rm is NOT logical, therefore error shown




# Dot dot dot... ----------------------------------------------------------

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

commas(letters, collapse = "-")
# ERROR: formal argument collapse =  matched by multiple arguments!
# Tries to run collapse = ", "  AND collapse = "-"
# This is because (...) passed along  collpase argument to stringr::str_c()



# Return values -----------------------------------------------------------

# 1. Return result value early = function interpretability?
# 2. Make function pipe-able?

# Explicitly return early to return with simpler solution at earlier part of function code.
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)    # if both length of x and y are 0, then just return 0 without following rest of code
  }
  
  # complicated code 
  
}

# can use with if else statements
# early return before long condition >>> if (!x)  return(whatever_value)

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

show_missings(mtcars)
# Missing values: 0
# doesn't show df!

x <- show_missings(mtcars)
x # still shows df
class(x)
dim(x)


# Environment -------------------------------------------------------------

# lexical scoping to search for non-defined-in-function variables inside all environment
# necessity to be careful, restart R session regularly!



























