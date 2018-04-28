library(repurrrsive)
library(purrr)
library(dplyr)
library(tidyr)



glimpse(sw_people)

length(sw_people)

sw_people[1]
sw_people[[1]]

# map(.x, .f, ...)
# for each element of .x do .f
# .x == vector, list, data frame (each col)

luke <- sw_people[[1]]

length(luke$starships)

# ~ == formula, .f(.x$__)
map(sw_people,
    ~ length(.x$starships))

planet_lookup <- sw_planets %>% 
  map_chr("name") %>% 
  set_names(map_chr(sw_planets, "url"))

map(sw_people, ~ planet_lookup[.x$homeworld])



sw_people <- sw_people %>% set_names(map_chr(sw_people, "name"))

map_int(sw_people, ~length(.x[["starships"]]))
map(sw_people, "starships") %>% map_int(length)

map_chr(sw_people, ~.x[["hair_color"]])
map_chr(sw_people, "hair_color")

map_lgl(sw_people, ~.x[["gender"]] == "male")

map_chr(sw_people, ~ .x[["mass"]]) # mass == string, NOT as numeric....

map_chr(sw_people, ~.x[["mass"]]) %>% readr::parse_number(na = "unknown")


sw_films
map(sw_films, "characters") %>% 
  map_int(length) %>% 
  set_names(map_chr(sw_films, "title")) %>% 
  sort(decreasing = TRUE)

sw_species
map(sw_species, "eye_colors") %>% 
  stringr::str_split(", ") %>% 
  map_int(length) %>% 
  set_names(map_chr(sw_species, "name")) %>% 
  sort(decreasing = TRUE)


# walk()
# expect nothing in return, .x return invisibly
# for functions called for SIDE EFFECTS:
# print to screen, plot to graphics, file manipulation (save/write/load/move), system calls








library(repurrrsive)
library(ggplot2)

gap_split_small <- gap_split[1:10]

countries <- names(gap_split_small)

gap_split_small[[1]] %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line() + 
  labs(title = countries[[1]])


plots <- map2(.x = gap_split_small, .y = countries,
     ~ ggplot(.x, aes(year, lifeExp)) +
       geom_line() +
       labs(title = .y))

plots[[1]]
plots[[10]]

# display all
# walk(plots, print)

# purrr and list cols
## reformat into data frame when able!

library(tibble)

people_tbl <- tibble(
  
  name = sw_people %>% map_chr("name"),
  
  films = sw_people %>% map("films"),
    
  height = sw_people %>% map_chr("height") %>% readr::parse_number(na = "unknown"),
    
  species = sw_people %>% map_chr("species", .null = NA_character_)
  
)


people_tbl %>% glimpse()


people_tbl$films

film_number_lookup <- map_chr(sw_films, "url") %>% 
  map(~ stringr::str_split_fixed(.x, "/", 7)[, 6]) %>% 
  as.numeric() %>% 
  set_names(map_chr(sw_films, "url"))


people_tbl <- people_tbl %>% 
  mutate(
    film_numbers = map(films, ~film_number_lookup[.x]),
    n_films = map_int(films, length)
  )

people_tbl %>% select(name, film_numbers, n_films) %>% arrange(desc(n_films))

people_tbl <- people_tbl %>% 
  mutate(films_squashed = map_chr(film_numbers, paste, collapse = ", "))



people_tbl$film_numbers[[1]]




# challenges --------------------------------------------------------------

mtcars

# fit regression model: mpg vs. disp for cars of similar cyl
# summarize by r^2 and estimated slope

mtcars_by_cyl <- mtcars %>% split(mtcars$cyl)

glimpse(mtcars_by_cyl)

mtcars_by_cyl <- mtcars_by_cyl %>% 
  map(~lm(.x$mpg~.x$disp))

# or
library(broom)

mtt <- mtcars %>% 
  group_by(cyl) %>% 
  nest() %>% 
  mutate(mods = map(data, ~tidy(lm(mpg ~ disp, data = .x)))) 

mtt$mods

mtt$mods %>% map("p.value")
mtt %>% mutate(pval = mods %>% map("p.value")) %>% View()

mtt %>% map(mods, ~broom::glance)

mtcars_by_cyl$`4`
mtcars_by_cyl[[1]] %>% glimpse()

mtcars_by_cyl %>% 
  map("coefficients")


# Challenege #2

# Below you will find code that downloads, manipulates, plots and saves
# one day, Dec 8, of weather data for Corvallis.  Your job is to rewrite 
# this to replicate for Dec 6 - Dec 10

library(tidyverse)

# Downloading data --------------------------------------------------------
# note date is part of url
download.file("https://www.wunderground.com/history/airport/KCVO/2016/12/8/DailyHistory.html?format=1", 
              "dec8.csv")

# Read in file -----------------------------------------------------------
col_types <- c("cnnnnnccccccnc")

dec8 <- read_csv("data/weather/dec8.csv", skip = 1, na = c("-", "N/A"), 
                 col_types = col_types)

# Add date columns --------------------------------------------------------

dec8 <- mutate(dec8, year = 2016, month = 12, day = 8)
# add add a datetime variable
dec8 <- mutate(dec8,
               datetime = as.POSIXct(strptime(
                 paste(year, month, day, TimePST, sep = " "), "%Y %m %d %I:%M %p")))

# Make and save plot -------------------------------------------------------

qplot(datetime, TemperatureF, data = dec8,
      geom = "line") +
  ggtitle("Dec 8")
ggsave("data/weather/plots/dec8.png")









