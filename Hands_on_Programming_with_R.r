
# Project 1: Dice-rolling -------------------------------------------------
# Chapter 1: Basics -------------------------------------------------------

die <- 1:6
die

# list objects in environment
ls()


die - 1

die / 2

die * die
# element-wise execution > not always matrix multiplication
# die - 1 = subtract one from EACH element in `die`
# Vector recycling: unequal lenghts = repeat shorter vector until longer vector length

# matrix multiplication:
# inner multiplication: %*%
# outer multiplication: %o%
die %*% die
die %o% die

# transpose: t()
t(die)
die
# determinant: det()

# Functions ####
round(pi)
round(3.5834598)
factorial(pi)
factorial(3)

mean(1:6)
mean(die)
round(mean(die))

# simulating rolling dice:
sample(x = 1:4, size = 2)
# between 1-4, sample twice!

sample(x = die, size = 1)
# rolling die!

# Look up function's arguments with args()
args(round)
# shows default values as well!
args(dplyr::inner_join)

# Sample with replacement ####
sample(die, size = 2)
# default = without replacement!
# need to set replace = TRUE for replacement
sample(die, size = 2, replace = TRUE)
# Create independent random samples: 
dice <- sample(die, size = 2, replace = T)
dice
dice
sum(dice)

# Write dice-roll function!
die <- 1:6
# create roll() function
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = T)
  return(sum(dice))
}

roll()
# 6    6   4   3   3    9      ... gives sum of two random dice rolls!

roll2 <- function(num_die, size) {
  dice <- sample(x = num_die, size = size, replace = T)
  return(sum(dice))
}

roll2(1:20)
roll2(1:8)
roll2(1:10, size = 3)  # roll dice of 1:10, three times!



# Chapter 2: Packages ####
library(ggplot2)

x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
y <- x^3

plot(x, y)
qplot(x, y)
ggplot() + geom_point(aes(x, y))

x <- c(1, 2,2,2, 3,3)
qplot(x, binwidth = 1)

x2 <- c(1,1,1,1,1, 2,2,2,2, 3,3, 4)
qplot(x2, binwidth = 1)

x3 <- c(0, 1,1, 2,2,2, 3,3, 4)
qplot(x3, binwidth = 1)

# Replicate()
replicate(3, 1 + 1)   # replicate 3 times, the operation `1 + 1`
replicate(10, roll()) # replicate 10 times, the roll function

# sim 10,000 rolls
rolls <- replicate(1000, roll())
qplot(rolls, binwdith = 1)
# approximately normal distribution! as it should as a RNG

# Create loaded dice! weighted more for Number 6!

loaded_roll <- function(size) {
  die <- 1:6
  roll <- sample(die, size = size, replace = TRUE,
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  return(sum(roll))
}

loaded_roll(size = 2)
loaded_roll(size = 5)
roll()
roll2(num_die = 1:20, size = 5)

rolls <- replicate(1000, loaded_roll(size = 2))
qplot(rolls, binwidth = 1) # slight skewness seen 



# Project 2: Playing Cards ------------------------------------------------

# Task 1: Build deck of cards.
# Task 2: Write function that deal & shuffle.
# Task 3: Change point values for specific card games.
# Task 4: Management of deck state (memory, environment, scoping)




# Atomic vectors: 
die <- c(1, 2, 3, 4, 5, 6)

is.vector(die)
# TRUE

five <- 5
five
is.vector(five)
# TRUE

length(five)
length(die)


int <- 1L
text <- "ace"

int <- c(1L, 5L)
text <- c("ace", "hearts")

# Doubles: 
typeof(die)
# "double"

# Integers: 
int <- c(-1L, 2L, 4L)
int

# integer numbers without L == saved as doubles
# each double accurate to 16 significant digits. 

# floating point errors: sqrt(2)^2-2 = 0 >>> but shows as 4.440982e^-16 instead.

# Characters: 
text <- c("Hello", "World")
text

typeof(text)
# "character"

# individual elements of character vector = STRINGS

typeof("1") # character!

# Logicals: 
3 > 4
# FALSE

logic <- c(TRUE, FALSE, TRUE)
logic
typeof(logic)
# "logical"


# Complex and raw:
comp <- c(1 + 1i, 1 + 2i, 1 + 3i)
comp
typeof(comp)
# "complex"

raw(3)
# 00 00 00 
typeof(raw(3))
# "raw"

cards <- c("ace", "king", "queen", "jack", "ten")
typeof(cards)
# character



# Attributes: 
# piece of info attached to atomic vector/R object >>> meta-data
attributes(die)
# NULL

# Names:
# most common attributes: names, dimensions (dim), classes

names(die) # NULL
names(die) <- c("one", "two", "three", "four", "five", "six")
names(die) # "one", "two", "three", "four", "five", "six"

attributes(die)
# $names >>> "one", "two", "three", "four", "five", "six"

die
die + 1

# Remove names attribute == set to NULL
names(die) <- NULL

# Dimension: dim(rows, columns)
dim(die) <- c(2, 3)
die

dim(die) <- c(3, 2)
die

dim(die) <- c(1, 2, 3)
die

# Matrices:
m <- matrix(die, nrow = 2)
m

# fill by row instead
m <- matrix(die, nrow = 2, byrow = TRUE)
m

# Arrays: 
ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
ar

royal_flush <- c("ace", "king", "queen", "jack", "ten", rep("spades", 5))
royal_flush
dim(royal_flush) <- c(5, 2)
royal_flush

# Class
dim(die) <- c(2, 3)
typeof(die)
# "double"
class(die)
# "matrix"

# class attribute added when `die` changed dimensions

attributes(die)


# Dates and Times 
now <- Sys.time()

# shows as character string, data type == "double", class == "POSIXct", "POSIXt"
typeof(now)
# "double"
class(now)
# "POSIXct", "POSIXt"

unclass(now)

mil <- 1000000
mil
class(mil) <- c("POSIXct", "POSIXt")
mil
# "1970-01-12 14:46:40 BST"   >>>> 1 million seconds after Midnight Jan. 1, 1970.


# Factors: 
# storing categorical information

gender <- factor(c("male", "female", "female", "male"))
typeof(gender)
# "integer"

attributes(gender)
# $ levels      "female"   "male"
# $ class     "factor"

gender
as.character(gender)

card <- c("ace", "heart", 1)
card
class(card)
# R coerces all values to character strings.............

# Coercion: 
# IF character >>> convert all to character string, ELSE logicals >>> numerics
# sum() of logical vectors == count all TRUEs, mean(), median() etc.


# Lists: 
# NOT group together individual values 
# >>> element 1: numeric vector of lenght 31, element 2: character vector length 1, etc.

list1 <- list(100:130, "R", list(TRUE, FALSE))
list1

# Create card 2.0
card <- list("ace", "hearts", 1)
card

# Data frame: 
# 2-dimensional list. storage as sequence of column == of different types
# each column must be same length

df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"), 
                 value = c(1, 2, 3))
df

typeof(df)
class(df)

str(df)
# 'data.frame':	3 obs. of  3 variables:
# $ face : Factor w/ 3 levels "ace","six","two": 1 3 2
# $ suit : Factor w/ 1 level "clubs": 1 1 1
# $ value: num  1 2 3

# face and suit saved as FACTORS!
df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"), 
                 value = c(1, 2, 3),
                 stringsAsFactors = FALSE)
str(df)
# face and suit kept as characters

deck <- read.csv("~/R_materials/deck.csv", stringsAsFactors = FALSE)





# Chapter 4: Notation -----------------------------------------------------
deck <- read.csv("~/R_materials/deck.csv", stringsAsFactors = FALSE)

# Selecting value:
# Extract values from R objects >>> data frame == []
deck[1, 2]
deck[3, 5]

# Positive integers:
# similar to i,j notation in linear algebra
# Ex. deck[i, j]
head(deck)
deck[1, 1]

# Extract more than one value >>> vector of positive integers
# Return first row of deck:
deck[1, c(1, 2, 3)]
deck[1, 1:3]

# repeat number in index == repeat return of values 
deck[c(1, 1), c(1, 2, 3)]

vec <- c(6, 1, 3, 6, 10, 5)
vec[1:3]

# Indexing beings at 1.
# Select 2 or more columns >>> data frame
# Select single column >>> vector
# Prefer data frame with single column >>> drop = FALSE argument

# Negative integers: 
# Opposite of positive, return every element except specified integer

deck[-(2:52), 1:3]
deck[-1, 1]

# Zero
deck[0, 0]

# Blank spaces
# for extract -every- value in that dimension
deck[1, ]

# Logical values: 

deck[1, c(TRUE, TRUE, FALSE)]
# for first row >>> show first two columns but NOT third column

# Names
deck[1, c("face", "suit", "value")]
deck[ , "value"]

# Deal card custom function:
deal <- function(cards) {
  cards[1, ]
}

deal(deck)
# always returns same card! 
# Shuffle deck?

deck2 <- deck[1:52, ]
head(deck2)

deck3 <- deck[c(2, 1, 3:52), ]
head(deck3)
# in different order!

random <- sample(1:52, size = 52)
random

deck4 <- deck[random, ]
head(deck4)

shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deal(deck)

deck2 <- shuffle(deck)

deal(deck2)


# Dollar signs and double brackets

deck$value
deck$face

mean(deck$value)
median(deck$value)

lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst
lst[1]

sum(lst[1])  # error!
lst$numbers  # correct.
sum(lst$numbers)

lst[1]   # smaller list 
lst[[1]] # just the values inside smaller list

lst["numbers"]
lst[["numbers"]]


# DONT USE ATTACH()




# Chapter 5: Modifying values ---------------------------------------------

# Change values of cards for specific games! war, hearts, blackjack

deck2 <- deck

# Changing values in place

vec <- c(rep(0, 5))
vec
vec[1]
vec[[1]]

vec[1] <- 1000

vec

# replace multiple values >>> necessity new values equal number of selected values
vec[c(1, 3, 5)] <- c(1, 1, 1)
vec

# create values not exist yet in object >>> expand object automatically to accomodate!
vec[7] <- 0

deck2$new <- 1:52
head(deck2)

# aces = value of 14 instead.
# now that aces every 13th card (unshuffled) in created deck set.
deck2[c(13, 26, 39, 52), ]
deck$value[c(13, 26, 39, 52)]

deck$value[c(13, 26, 39, 52)] <- c(rep(14, 4))
deck$value[c(13, 26, 39, 52)] <- 14
deck$value[c(13, 26, 39, 52)]

head(deck2, 13)

# BUT what if deck was shuffled???
deck3 <- shuffle(deck)

head(deck3)

# Use logical subsetting!
1 > 2
1 > c(0, 1, 2)
1 %in% c(3, 4, 5)

deck2$face

deck2$face == "ace"
sum(deck2$face == "ace")

deck3$face == "ace"

deck3$value[deck3$face == "ace"]
deck3$value[deck3$face == "ace"] <- 14


# for Hearts: all values == 0
deck4 <- deck
deck4$value <- 0
head(deck4, 13)

deck4$suit == "hearts"
deck4$value[deck4$suit == "hearts"] <- 1

deck4$value[deck4$suit == "hearts"]

deck4[deck$face == "queen", ]
deck4[deck4$suit == "spades", ]
# need to find face value = "queen" AND suit value = "spades"!

# Boolean operators: 

a <- c(1, 2, 3)
b <- c(1, 2, 3)
c <- c(1, 2, 4)

a == b
b == c
a == b & b == c

deck4$face == "queen" & deck4$suit == "spades"

QueenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"

deck4[QueenOfSpades, ]
deck4$value[QueenOfSpades] <- 13
deck4[QueenOfSpades, ]

# Exercise:
w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

w > 0
x > 10 & x < 20
y == "February"


# Blackjack! face card == value 10, ace == 11 OR 1...
deck5 <- deck

head(deck5, 13)
facecard <- deck5$face %in% c("king", "queen", "jack")
deck5[facecard, ]

deck5$value[facecard] <- 10
head(deck5, 13)

# How to fix aces to 11 OR 1 depending on conditions?

# Missing info
c(NA, 1:50)
mean(c(NA, 1:50))
# NA
mean(c(NA, 1:50), na.rm = TRUE)
# 25.5

# is.na()
NA == NA
c(1, 2, 3, NA) == NA

is.na(NA)

vec <- c(1, 2, 3, NA)
is.na(NA)

deck5$value[deck5$face == "ace"] <- NA
head(deck5, 13)


# Chapter 6: R Environments -----------------------------------------------

deal(deck)
# same card over and over again...
# shuffle uses deck but not manipulate directly in environment!

as.environment("package:stats")
# "C:/Program Files/R/R-3.4.0/library/stats"

globalenv()
baseenv()
emptyenv()

parent.env(globalenv())
parent.env(emptyenv())

ls(emptyenv())
# list all objects in environment:
ls(globalenv())

head(globalenv()$deck, 3)

# assign into environment manually:
assign("new", "Hello Global", envir = globalenv())
globalenv()$new

# Active environment:
environment()

# Scoping Rules:
# 1. R looks for objects in current active environment
# 2. At command line: active environment = global env.
# 3. When not find object in environment >>> look in parent environment and upwards


# Assignment: 
new

new <- "Hello Active"

new

# for function names/objects >>> creation of new active environment for evaluating function


DECK <- deck

deck <- deck[-1, ]

head(deck, 3)

deal <- function() {
  card <- deck[1, ]
  deck <- deck[-1, ]
  card
}

# assign deck[-1, ] to object deck in global environment

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card
}

deal()
# now shows different card each time as remove previously selected

head(deck, 3)
a <- shuffle(deck)
head(deck, 3)
head(a, 3)

# Shuffle NOT shuffle deck, return copy of deck with missing cards that were removed by deal()

shuffle <- function() {
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

# create reordered copy of DECK and save as "deck" in global environment!

shuffle()
deal()   # -1   to 51 obs(cards)
deal()   # -1   to 50 obs(cards)

# how about we store DECK and deck elsewhere so doesnt accidentally get modified??

# Create new function "setup":
# - deck as argument, save copy of deck as DECK
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function() {
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
  
  return(list(deal = DEAL, shuffle = SHUFFLE))
}

cards <- setup(deck)

deal <- cards$deal
shuffle <- cards$shuffle

deal
shuffle
# both functions saved in origin (runtime) environment  NOT global!

# Closure: separate runtime environment encloses the deal + shuffle functions
# deal and shuffle work with objects within contained/enclosed environment but nothing else!

# deal and shuffle still update deck in globa en
# need for those 2 functions to work excluve in enclosing/parent environment of runtime env.
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))  # change to parent.env
    card
  }
  
  SHUFFLE <- function() {
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment())) # change to parent.env
  }
  
  return(list(deal = DEAL, shuffle = SHUFFLE))
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle

rm(deck)

shuffle()
deal()
deal()
# now doesn't delete from "deck" in global environment only within own environment

























