library(dplyr)
library(rlang)


# take dataset input, extract values from single column matching certain value

wrangle_data <- function(data, column, val) {
  
  data[data[[column]] == val, column, drop = FALSE]
  
}

wrangle_data(iris, "Species", "versicolor") %>% head()


# re-write with dplyr and pipes

one_col <- select(iris, Species)
one_col %>% filter(Species == "versicolor") %>% head()

wrangle_data <- function(data, column, val) {
  
  one_col <- select(data, column) 
  one_col %>% filter(column == val)
  
}

wrangle_data(iris, "Species", "versicolor")
# returns NO rows...
# this is due to NSE: select() and filter() NOT work as looking for column named "column"
# use TIDYEVAL!

wrange_data <- function(x, column, val) {
  
  one_col <- select(x, !!sym(column))
  one_col %>% filter(!!sym(column) == val)
  
}

wrange_data(iris, "Species", "versicolor") %>% head()
# HOORAY

??sym
# take strings as input >>> turn them into symbols


################################











