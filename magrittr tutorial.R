install.packages("magrittr")

library(magrittr)

car_data <- 
  mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  transform(kpl = mpg %>% multiply_by(0.4251)) %>%
  print
?aggregate

car_data %>%
{ 
  if (nrow(.) > 0)
    rbind(head(., 1), tail(., 1))
  else .
}


iris$Sepal.Length %<>% sqrt
irisisi %<>% as.data.frame(iris$Sepal.Length)


library(dplyr)
install.packages("nycflights13")
library(nycflights13)

str(flights)

mutate(Date = ymd(paste(year, "-", month, "-01", sep = "")))
       
       



flights$date <- strptime(flights$date, %d/ %m/%Y %H:%M)

hourly_delay <- filter( 
  summarise(
    group_by( 
      filter(
        flights, 
        !is.na(dep_delay)
      ), 
      date, hour
    ), 
    delay = mean(dep_delay), 
    n = n()
  ), 
  n > 10 
) 


hourly_delay <- flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(date, hour) %>% 
  summarise( 
    delay = mean(dep_delay), 
    n = n() ) %>% 
  filter(n > 10)

# Let's play with some strings

str1 = "A scratch? Your arm's off."
str2 = "I've had worse."

str1 %>% substr(3,9)   
#Evaluates to "scratch"

str1 %>% strsplit('?',fixed=TRUE)
# "A scratch"        " Your arm's off."

# Pipes can be chained as well
str1 %>% paste(str2) %>% toupper()
# "A SCRATCH? YOUR ARM'S OFF. I'VE HAD WORSE."

# Let's see how pipes might work with drawing random variables
# I like to define a function that allows an element by element maximization

vmax <- function(x, maximum=0) x %>% cbind(0) %>% apply(1, max)
-5:5 %>% vmax
# 0 0 0 0 0 0 1 2 3 4 5

# This is identical to defining the function as:
vmax <- function(x, maximum=0) apply(cbind(x,0), 1, max)
vmax(-5:5)

# Notice that the latter formation uses the same number of parenthsize
# and be more readable.
?apply

# However recently I was drawing data for a simulation in which I wanted to 
# draw Nitem values from the quantiles of the normal distribution, censor the
# values at 0 and then randomize their order.

Nitem  <- 100
ctmean <- 1
ctsd   <- .5

draws <- seq(0, 1, length.out = Nitem+2)[-c(1,Nitem+2)] %>% 
  qnorm(ctmean,ctsd) %>% vmax %>% sample(Nitem)

# While this looks ugly, let's see how worse it would have been without pipes
draws <- sample(vmax(qnorm(seq(0, 1, length.out = Nitem+2)[-c(1,Nitem+2)]
                           ,ctmean,ctsd)),Nitem)

# Both functional sequences are ugly though I think I prefer the first which
# I can easily read as seq is passed to qnorm passed to vmax passed to sample

# A few things to note with the %>% operator. If you want to send the value to
# an argument which is not the first or is a named value, use the '.'

mydata <- seq(0, 1, length.out = Nitem+2)[-c(1,Nitem+2)] %>% 
  qnorm(ctmean,ctsd) %>% vmax %>% sample(Nitem) %>%
  data.frame(index = 1:Nitem , theta = .)
str(mydata)

# Also not that the operator is not as slow as you might think it should be.
# Thus:

1 + 8 %>% sqrt
# Returns 3.828427

# Rather than
(1 + 8) %>% sqrt
# [1] 3

