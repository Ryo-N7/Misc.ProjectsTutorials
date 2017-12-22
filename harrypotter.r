library(tidytext)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(harrypotter)
library(purrr)

glimpse(harrypotter)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

#names(books) <- seq_along(titles)
#names(books) <- sprintf("Book%i", seq_along(books))
#names(books$Book1) <- sprintf("Chapter_%i", seq_along(books$Book1))
#names(books$Book2) <- sprintf("Chapter_%i", seq_along(books$Book2))


# with a for loop:

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}


# map() 

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)


books <- setNames(books, titles)

books %>% glimpse()

hp_books <- map(books, function(book){
  
  return(tibble(text = book, chapter = seq(1:length(book))))
  
}) %>% 
  bind_rows(.id = "Book")

str(hp_books)



# map2_df()

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

class(titles)
class(books)

hp_df <- map2_df(titles, books, 
            ~ tibble(book = .x, 
                     chapter = seq_along(.y), 
                     text = .y))

str(hp_df)

seq_along(books)



# map() 2:

books <- tibble(title = c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
                          "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
                          "Deathly Hallows"), 
                text = list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
                            goblet_of_fire, order_of_the_phoenix, half_blood_prince,
                            deathly_hallows), 
                chapter = map(text, seq_along))

books


# map() 3: >>> map_dfr()
