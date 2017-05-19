# R Programming
# oddcount() to count odd numbers in vector of integers
oddcount <- function(x) {
  k <- 0 # assign 0 to k
  for (n in x) {
    if (n %% 2 == 1) k <- k+1 # %% modulo operator
  }
  return(k)
}
oddcount(c(1,3,5)) # = 3
oddcount(3)        # = 1
length(c(1,3,5,6)) # = 4
38 %% 7            # = 3

# Default arguments
g <- function(x , y = 2, z = TRUE)
g(12, z = FALSE)
g

# Scalars
x <- 8
x

# Character strings: single-element vectors of mode character
x <- c(5, 12, 13)
x
length(x)
mode(x)
y <-"abc"
y
length(y)
mode(y)
z <- c("abc", " 29 88")
length(z)
mode(z)

u <- paste("My", "name", "is", "Methos")
u
?strsplit
strsplit(u, " ")

# Matrices
m <- rbind(c(1,4), c(2,2))
m
m %*% c(1,1)
a <- rbind(c(3,3))
a
m %*% c(3,3)

# Extract sub-matrices, subvectors
m[2, ]  # row 1
m[, 2]

# Lists
x <- list(u = 2, v= "abc")
x
x$u
x$v
hist(Nile)
hn <- hist(Nile)
hn
str(hn)

# Data frames
d <- data.frame(list(kids = c("Jack", "Jill"), ages = c(12,10), gender = c("Male", "Female")))
d
d$ages
d$kids
d$gender

# Classes
print(hn)
?print
summary(hn)

# 1.5 Regression Analysis example
examsquiz <- read.table("ExamsQuiz.txt", header = FALSE)
?read.table
attributes(hn)
getwd()
?"for"
?"%%"
example(seq)
example(read.table)
example(rbinom)
example(persp)
?persp

help.search("poisson distribution")
?files
example(file.exists)


# 2: Vectors
x <- c( 88, 5 ,12 ,13)
x
x <- c(x[1:3], 168, x[4])   # insert 168 before '13'
x
x <- c(1, 2, 4)
length(x)
first1 <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == 1) break # break out of loop
  }
  return(i)
}

x <- c()
x
length(x)
1:length(x)

m
m + 10:13

# Declarations
z <- 3

y[1] <- 5   # NOT WORK
y[2] <- 12  # NOT WORK
# Necessity create vector before when reference specific elements of vector 
y <- vector(length = 2)
y[1] <-5
y[2] <- 12
# OR 
y <- c(5,12)

# Recycling
c(1,2,4) + c(6,0,9,20,22)
rm(list = ls())
help.search("create matrix")
x <- mat.or.vec(2,2)
x[1,1] <- 5
x[1,2] <- 3
x[2,1] <- 4
x[2,2] <- 6
x + c(1,2)

z <- c(4,2,3,56,23)
z[-2:-3]
q <- c(5, 12, 13)
q[1:(length(q)-2)]
q[-length(q)]
z[-length(q)]
5:8
5:1
2.5:6

i <- 2
1:i-1      # output: 0   1
1:(i-1)    # output: 1           ORDER OF OPERATIONS!!!!!!!

# Seq()
seq(from = 12, to = 30, by =3)
seq(from = 1.1, to = 2, length = 5)
for (i in seq(x))
  x <- c( 5 , 12, 13)
x
seq(x)
x <- NULL
seq(x)

# rep()
x <- rep(8,4)
x
rep(c( 5, 12, 13), 3)
rep(1:3,2)
rep(c(5,12,13), each = 2)

# all(), any()
x <- 1:10
any(x > 8)
all(x < 2)

findruns <- function(x,k) {
  n <- length(x)
  runs <- NULL
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)] == 1)) runs <- c(runs, i)
  }
  return(runs)
}

y <- c(1, 0 , 0, 1,1,1,0 , 1,1)
findruns(y, 3)
findruns(y, 2)

findruns <- function(x,k) {
  n <- length(x)
  runs <- vector(length = n)
  count <- 0
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)] == 1)) {
      count <- count + 1 
      runs[count] <- i
    }
  }
  if (count > 0) {
    runs <- runs[1:count]
  } else runs <- NULL
  return(runs)
}
findruns(y, 3)

# Predicting discrete-valued time series
# rain weather: 1 = Rain, 0 = NO rain
# For some 'k', predict tomorrow weather based on the weather record of last 'k' days. 
# IF 1s in 'k' time period is at least k/2, prediction next value = 1, ELSE prediction = 0
# HOW choose correct 'k' days value?
# Usage training data, analyze how well various values of 'k' perform prediction! 

X <- 500; sample(c(0,1), replace=TRUE, size=X)

preda <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  # vector 'pred' will contain predicted values
  pred <- vector(length = n - k)
  for (i in 1:(n-k)) {
    if (sum(x[i:(i+(k-1))]) >= k2) pred[i] <- 1 else pred[i] <- 0
  }
  
  return(mean(abs(pred-x[(k+1):n])))
}

preda(x, 4)
preda(c(1,0,1,1,1,1,1,0,1,0,1,0,0,0,1,0,1,1,0,0,1), 2)

