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































































































