# Data structures:

## Vectors ##
v <- c(10, 20, 30)
names(v) <- c("Moe", "Larry", "Curly")
print(v)
str(v)
v[2] # second element of v
v[c(2,3)] # multiple elements of vector - subvector
v["Larry"] # select vector elements by name (if assigned)

## Lists ##
vlist <- list("Moe", "Larry", "Curly")
vlist[[2]]   # second element of vlist
vlist[c(2, 3)]  # subset of vlist, second + third elements
vlist[["Moe"]]
vlist["Moe"]
vlist$Moe

## Mode (physical type) ##
mode(3.1414)   # "numeric"
mode(c(2.7235, 24957834.23))   # "numeric"
mode("Moe")   # character
mode(list("Moe", "Larry", "Burt"))   # list

## Class (abstract type) ##
d <- as.Date("2010-03-15")
mode(d)   # "numeric"
length(d) # 1
class(d)  # "Date"

## Scalars ##
# A vectors that contains exactly ONE element.
pi   # scalar: 3.1415>>>>>
length(pi)   # length: 1
pi[1]        # 3.1415>>>>>
pi[2]        # NA

## Matrices ##
# A vectors with dimensions.
A <- 1:6
dim(A)   # NULL
print(A) # 1 2 3 4 5 6 
dim(A) <- c(2, 3)   # set dimension of vector 'A'
print(A)    # 2 x 3 matrix!

B <- list(1, 2, 3, 4, 5, 6)
dim(B)   # NULL
dim(B) <- c(2, 3)
print(B)   # 2 x 3 matrix from list!

## Arrays ## 
D <- 1:12
dim(D) <- c(2, 3, 2)
print(D)    # 3-D array

C <- list(1, 2, 3, "X", "Y", "Z")
dim(C) <- c(2, 3)
print(C)   # create matrices with heterogenous lists. NOT just numeric in R!

# Appending data to vector # 
v <- c(v, 3, 2)

v <- c(1,2,3)
v <- c(v, 4)
v  # 1 2 3 4
w <- c(5,6,7,8)
v <- c(v, w)
v  # 1 2 3 4 5 6 7 8

v <- c(1,2,3)
v[10] <- 10
v  # 1 2 3 NA NA NA NA NA NA 10
# automatically extends vector length

# Recycling Rule for vectors # 
(1:6) + (1:3)


# Vector #
f <- factor(c("Win", "Win", "Lose", "Tie", "Win", "Lose"))
f

f <- factor(wday)
f <- factor(wday, c("Mon", "Tue", "Wed", "Thu", "Fri"))
# explicitly state all levels of factor

# Combining vectors #
comb <- stack(list(fresh = freshmen, soph = sophomores, jrs = juniors))
aov(values ~ ind, data = comb)

# Lists #
# Can contain elements of different 'modes' (unlike vectors)
lst <- list(3.14, "Moe", c(1,1,2,3), mean)
lst

lst <- list()
lst[[1]] <- 3.14
lst[[2]] <- "Moe"
lst[[3]] <- c(1,1,2,3)
lst[[4]] <- mean

lst <- list(mid = 0.5, right = 0.3845, far.right = 0.234)
lst
# Name elements of list

## Selecting in list ##
years <- list(1960, 1964, 1976, 1994)
years[[1]]        # select single element with [[n]]    n = position in list
years[c(1,2)]     # select sublist of elements with [c(x,y,z)]

class(years[[1]])   # "numeric"
class(years[1])     # "list"

lst[["name"]] # select element with "name"
lst$name      # same as above

lst[c(name1, name2, ... , nameK)] # select list of indicated elements with "name."

# Flatten list into vector #
iq.scores <- list(c(12, 13, 435, 349, 28, 5, 0))
mean(iq.scores)   # WARNING: argument = NOT numeric/logical >>> NA

mean(unlist(iq.scores))  # 120.2857

cat(iq.scores, "\n")   # argument type 'list' cannot be hanlded by 'cat'
cat("IQ Scores:", unlist(iq.scores), "\n")
# IQ Scores: 12, 13, 435, 349, 28, 5, 0


## Remove NULL elements from list ##
lst <- list("Moe", NULL, "Curly")
lst
lst[sapply(lst, is.null)] <- NULL
lst   # NULL removed.


iq.scores[is.na(iq.scores)] <- NULL
iq.scores

# Preallocating data frame space
N <- 10000
dfm <- data.frame(colnm = numeric(5), colnm2 = character(5), 
                  colnm3 = factor(N, levels = c("AR", "BJ", "AL")), response = numeric(N))
dfm

suburbs









