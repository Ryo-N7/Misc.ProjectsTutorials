install.packages("magrittr")

library(magrittr)

car_data <- 
  mtcars %>%                 # take mtcars dataset
  subset(hp > 100) %>%       # subset by hp > 100
  aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%  # aggregate based on # of cyl
  transform(kpl = mpg %>% multiply_by(0.4251)) %>%     # add kpl variable
  print
?aggregate

car_data %>%
{ 
  if (nrow(.) > 0)
    rbind(head(., 1), tail(., 1))
  else .
}


iris$Sepal.Length %<>% sqrt
iris %<>% as.data.frame(iris$Sepal.Length)


library(dplyr)
install.packages("nycflights13")
library(nycflights13)

str(flights)

mutate(Date = ymd(paste(year, "-", month, "-01", sep = "")))
       
?ymd()



flights$date <- strptime(flights$date, "%d/%m/%Y %H:%M")

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




# a tour of the tibble package:
library(tibble)
library(magrittr)

tibble(x = 1:5, x_squared = x ^ 2)
# sequential evaluation allows for additional columns to be created from
# the manipulation of earlier defined ones!

# tibble = data defined column-column
# tribble = data defined row-row

tribble(
  ~ Film, ~ Year,
  "A New Hope", 1977,
  "The Empire Strikes Back", 1980,
  "Return of the Jedi", 1983
)
# try in tibble instead...
tibble(Film = c("Dank Hope", "Empire Strike Out", "Jedi Reversi"), Year = c(1977, 1980, 1983))

# Recoding: create labels for plot then JOIN into dataset
# Exclusion: ID observations for exclusion, remove with ANTI-JOIN
library(dplyr)

plot_labs <- tribble(
  ~ Group, ~ GroupLabs,
  "TD", "Typically Dev",
  "CI", "Coch",
  "ASD", "Autistic"
)

dataset <- tibble(Group = c("TD", "CI", "ASD"))

dataset <- left_join(dataset, plot_labs, by = "Group")
dataset

# Reducto!
ids_to_exclude <- tibble::tribble(
  ~ Study, ~ ResearchID,
  "TimePoint1", "053L",
  "TimePoint1", "102L",
  "TimePoint1", "116L"
)

reduced_dataset <- anti_join(dataset, ids_to_exclude)

# Conversion to tibble
as_tibble(mtcars)

# Vectors into tibbles with enframe()
quantiles <- quantile(mtcars$hp, probs = c(.1, .25, .5, .75, 0.9))
quantiles

quantibble <- enframe(quantiles, "quantile", "value")
quantibble

# dataframe vs. tibbles in VIEW/PRINT

df <- as.data.frame(replicate(26, 1:200)) %>%   # 200 x 26 df
  setNames(letters) %>% 
  as_tibble()
df

glimpse(df)    # look at few values from EACH column

# Add new rows add_row(), add new columns add_column()
df <- tibble(comment = "original", x = 1:2, x_squared = x ^ 2)
df
df <- df %>% 
            add_row(comment = "append", x = 3:4, x_squared = x ^ 2) %>% # default: adds to bottom
            add_row(comment = "prepend", x = 0, x_squared = x ^ 2,
                    .before = 1)   # .before to add new BEFORE = 'x'
df

df %>% 
  add_row(x = 5, comment = "NA defaults") %>% 
  add_row(x_squared = 36, x = 6, comment = "order doesn't matter")  # naming columns in add so order = irrelevant

df %>% add_column(comment2 = "inserted column", 
                  .after = "comment")  # add new AFTER "comment" column
# either add_column or dplyr:::mutate! 

as_tibble(mtcars)  # * represent row-names of data labels...
mtcars %>% 
  as_tibble() %>% 
  rownames_to_column("model")







