# Dealing with character data: 
library(tidyverse)
library(stringr)
fruit
straws <- str_detect(fruit, "fruit")
sum(straws, na.rm = TRUE)

(my_fruit <- str_subset(fruit, "fruit"))

# string split by delimiter: 
str_split(my_fruit, " ")
# for character matrix output: 
str_split_fixed(my_fruit, " ", n = 2)

# Separate into variables: 
my_fruit_df <- tibble(my_fruit)
my_fruit_df %>% 
  separate(my_fruit, into = c("pre", "post"), sep = "f")

# Count characters in strings, str_length(): 
length(my_fruit) # length of vector
str_length(my_fruit) # length of each string in vector

# Snip substrings based on character positions, str_sub():
head(fruit) %>% 
  str_sub(1, 4)   # from 1st to 4th letter
head(fruit) %>% 
  str_sub(2, 3)   # from 2nd to 3rd letter

tail(fruit) %>% 
  str_sub(1, 4)
tail(fruit) %>% 
  str_sub(2,3)
# Combine inside mutate() to create variable based on snippet: 
tibble(fruit) %>% 
  head() %>% 
  mutate(snippet = str_sub(fruit, 1:6, 3:8))
?str_sub()

data.frame(fruit) %>% 
  head() %>% 
  mutate(snips = str_sub(fruit, 1:6, 3:8))

# str_sub as an assignment <- : 
x <- head(fruit, 5)
x
str_sub(x, 2, 4) <- "AaA"
x

# Collapse vector, str_c(): 
?str_c()
head(fruit, 3)
head(fruit, 3) %>% 
  str_c(collapse = ", ")

# Create character vector by concatenating multiple, str_c():
str_c(fruit[1:4], fruit[5:8], sep = " & ")
str_c(fruit[1:4])
str_c(fruit[5:8], fruit[12:15], sep = " & ")

# Combine AND collapse:
str_c(fruit[1:4], fruit[5:8], sep = " & ", collapse = ", ")

# Vectors as variables, combine with unite(): 
fruit_df <- tibble(fruit1 = fruit[1:4], fruit2 = fruit[5:8])

fruit_df

fruit_df %>% 
  unite("flavor_combo", fruit1, fruit2, sep = " & ")

# Replace with, str_replace(): 
str_replace(my_fruit, "fruit", "THINGY")

melons <- str_subset(fruit, "melon")
melons   # canary melon, rock melon, watermelon
melons[2] <- NA   # replace rock melon with NA
melons   # canary melon, NA, watermelon

str_replace_na(melons, "UNKNOWN MELON")   # fill in NA with str_replace_na()

tibble(melons) %>% 
  replace_na(replace = list(melons = "UNKNOWN MELON"))

###################       Regexes   ##################
library(gapminder)
countries <- levels(gapminder$country)

# . as any single character, \n as newline, ^ as beginning of string, $ as end of string
str_subset(countries, "a.r")  # "BAhRain", "MAuRitania", "MAuRitius"
str_subset(countries, "b.e")  # "ZimbaBwE"   
str_subset(countries, "i.e$") # "ChIlE", "Cote d'IvoIrE", "Sao Tome and PrincIpE"
str_subset(my_fruit, "d")     # "breaDfruit", "Dragonfruit"
str_subset(my_fruit, "^s")    # "star fruit" 

# \(letter) = word boundary, \(Capital letter) = NOT word boundary:
str_subset(fruit, "melon")      # [1] "canary melon" "rock melon"   "watermelon"
str_subset(fruit, "\\bmelon")   # [1] "canary melon" "rock melon"
str_subset(fruit, "\\Bmelon")   # [1] "watermelon"

str_subset(countries, "ia$")
str_subset(countries, "[nls]ia$")
str_subset(countries, "[rt]ia$")

str_subset(countries, "[^nlsrtbd]ia$")   # "BoliV-ia", "EthioP-ia"

# \s as space, \S as NOT space, [:space:] as space, \d as digit, \D as NOT digit: 
str_split_fixed(my_fruit, " ", 2)
str_split_fixed(my_fruit, "\\s", 2)
str_split_fixed(my_fruit, "[[:space:]]", 2)

# Strings with punctuation, [:punct:] 
str_subset(countries, "[[:punct:]]")     # "Congo, Dem. Rep.", "Yemen, Rep.", etc........

# quantifier 	meaning 	quantifier 	meaning
#   * 	    0 or more 	{n} 	      exactly n
#   + 	    1 or more 	{n,} 	      at least n
#   ? 	    0 or 1 	    {,m} 	      at most m
#                       {n,m} 	    between n and m, inclusive

# with *, any string with (letter).*(secondletter): 
(matches <- str_subset(fruit, "l.*e"))
list(match = intersect(matches, str_subset(fruit, "l.+e")),    # belL pEpper, LimE, oLivE
     no_match = setdiff(matches, str_subset(fruit, "l.+e")))   # appLE, LEmon, pineappLE

list(match = intersect(matches, str_subset(fruit, "l.?e")),
     no_match = setdiff(matches, str_subset(fruit, "l.?e")))

list(match = intersect(matches, str_subset(fruit, "le")),
     no_match = setdiff(matches, str_subset(fruit, "le")))

# Escape: use backslash when usage special characters within string as NOT regex signifier
cat("Do you use \"airquotes\" much?")   
# Use double quotes within single quotes, or vice-versa....

# newline, \n: 
cat("before the newline\nafter the newline")
cat("before the tab\tafter the tab")

# Isolate countries names with a . : 
str_subset(countries, "[[:punct:]]")  # all punctuation
str_subset(countries, "\\.")          # just periods

