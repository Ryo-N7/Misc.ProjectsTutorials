# string practice
library(tidyverse)
library(stringr)
# can use either single '' or double ""!

string1 <- "this is a string"
string1

string2 <- 'include a "quote" inside a string, use the singles'
string2

# Include literal single or double quote in string, use \ to "escape" it:
double_quote <- "\""
double_quote

single_quote <- '\''
single_quote

x <- c("\"", "\\")
x
# printed representation SHOWS the escapes, show raw content with writeLines():
writeLines(x)

# Other special characters include "\n" for new line, "\t" for tab
# other like...
x <- "\u00b5"               # shows greek mu
x
# multiple strings stored as character vector:
c("one", "two", "three")

## String length: # of characters in a string
str_length(c("a", "R for data science", NA))
# 1 in "a", 18 in "R for data science", NA for NA

## Combinging strings: str_c()
str_c("x", "y")
# shows "xy"
str_c("x", "y", "z")
# shows "xyz"
# use sep = __ to control how each separated:
str_c("x", "y", sep = ", ")
# shows "x, y"

# if print NA, use str_replace_na()
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

# str_c() is vectorized, automatically recycles shorter ectors to length of longest vector
str_c("prefix-", c("a", "b", "c"), "-suffix")

# Objects of length = 0 are dropped, useful with if statements:
name <- "Rhaegar"
time_of_day <- "evening"
birthday <- FALSE

str_c("Good ", time_of_day, " ", name, 
      if (birthday) " and Feliz Cumpleanos" else " ...yer da", ".")

# collapse vector of strings into single string, use collapse = "___"
str_c(c("x", "y", "z"), collapse = ", ")

## Subset strings 
# Extract parts of string with str_sub()
x <-c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)    # start from 1st character to 3rd character

str_sub(x, -3, -1)  # start from 3rd from rear to 1st from rear
str_sub("a", 1 , 5)  # return as much as possible 

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x


# Locales: 
# str_to_lower() to change to lower case, vice-versa with str_to_upper()/_title()
# Specify locale for case-change rules
# ex. Turkey has two i's: with + without dot on top
str_to_upper(c("i", "i"))

str_to_upper(c("i", "i"), locale = "tr")

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")  # English
#> [1] "apple"    "banana"   "eggplant"

str_sort(x, locale = "haw") # Hawaiian
#> [1] "apple"    "eggplant" "banana"

# 14.2.5. Exercises
# paste() and str_c()?
?paste()

# Difference between sep and collapse:
# collapse is taking a single character vector of strings and collpasing into one with specified character
# sep is string inserted between arguments in str_c()

# Extract middle character from string
asdf <- c("jim", "went", "back" , "in", "time")
L <- str_length(asdf)
L                     # 3 4 4 2 4
m <- ceiling(L / 2)
m                     # 2 2 2 1 2
str_sub(asdf, m, m)

# str_wrap(): wrap text to fit within certain width
?str_wrap()

# str_trim(): deletes whitespace from string
str_trim(" blahblah there is space              ", side = "right") # or "left" can be used, default = "both"

# str_pad(): adds characters to each side
str_pad("blahblahspace", 20, side = "left")
str_pad("blahblahspace", 30, side = "both")

# Write function that turns vector c("a", "b", "c") into string
str_commasep <- function(x, sep = ", ", last = ", and ") {
  if (length(x) > 1) {
    str_c(str_c(x[-length(x)], collapse = sep), 
          x[length(x)],
          sep = last)
  } else {
    x
  }
}
str_commasep("")
str_commasep(c("blahdeblah", "blub",  "yub", "yub", "commander"))

# 10.3 Matching Patterns with Regex
# Regex: allows describe patterns in strings
# use str_view() and str_view_all() for interpretation.

x <- c("apple", "banana", "pear")
str_view(x, "an")   # search for "an" 
str_view(x, ".a.")  # search for -any- a -any-
# IF '.' match for any character, how to explicitly match for '.' sign?
# Use escape \ then \ escape again >>> "\\."
dot <- "\\."
writeLines(dot) # shows as \. 
str_view(c("abc", "a.c", "bef"), "a\\.c")

# for matching \:
# first escape using \ >>> \\
# then create regex of \\ means add \ >>> \\\
# escape of \\\ means add \ >>> \\\\
x <- "a\\b"
writeLines(x)  # show as a\b
str_view(x, "\\\\")

# Exercise:
# match \ in string
# \ : escape next character in R string
# \\ : resolve to \ in regex, escape next character in R string
# \\\ : first 2 \\ resolve to \ in regex, escape an escaped character in R string
# \\\\ : correct

# How match "'\  ?
x <- "lol\"\'\\stoopid"
str_view(x, "\"'\\\\")   # first use \ to escape " and ' then \\\\ for \ 

# How match "\..\..\.."
x <- "lol\\.a lasjdfl \\.a\\.s\\.df "
str_view(x, "\\..\\..\\..")

# Anchors: 
# default: regex match ANY part of string
# Useful to anchor regex so matches from start OR end of string
# Use: ^ for match start of string, $ for match end of string
x <- c("apple", "banana", "pear")
str_view(x, "^a")  # beginning 'a' in "apple"
str_view(x, "r$")  # ending 'r' in "pear"
# force regex to match ONLY complete string, use both ^ and $
x <- c("apple", "banana", "pear", "apple cake")
str_view(x, "apple")    # match both "apple" and "apple" in "apple cake"
str_view(x, "^apple$")  # match ONLY "apple"

# Match boundary between words with \b
# search \bsum\b  to search for sum() function and NOT summarize(), summary(), etc.
x <- c("blah $^$", "bludblud $$$ billa yall")
y <- c("$^$", "ab$^$sfas")
str_view(x, "\\$\\^\\$")    # search for "$^$" in any part of string
str_view(y, "^\\$\\^\\$$")  # first ^ at start, then escape twice $, escape twice ^, escape twice $ then $ at end of string match
word <- stringr::words %>% head()
word   # "a", "able", "about", "absolute", "accept", "account"
str_view(word, "^a")   # words start with "a"
str_view(word, "e$")   # words end with "e"
doh <- c("let", "bothan", "peregrine", "jets", "run")
str_view(doh, "^...$")   # exactly 3 letters long
str_view(doh, "^.......") # seven letters or more

# \d = match any digit
# \s = match any whitespace (space, tab, newline)
# [abc] = match a,b,c
# [^abc] matches ALL except a,b,c
# For creation of regex containing \d or \s, necessity escape \ for string: "\\d", "\\s"
numbers <- c(234324, 4949, "duh", "word", "whoah that's 5")
str_view(numbers, "^\\d")   # string starting in number
str_view(numbers, "\\d$")   # string ending in number
str_view(numbers, "\\d")
str_view(numbers, ".\\d....")

# use alternation to pick between one or more alternative patterns
# ex. abc|d..f = match "abc" or "d__f"
# if in doubt about precedence, use ()
str_view(c("grey", "gray"), "gr(e|a)y")

# Create regex to find all words:
# a. starting with vowel
words <- stringr::words %>% head()
str_view(words, "^[aeiou]")
grep("^[aeiou]", c("blah", "ounce", "jet", "arbalest"))  #  2 (ounce) and 4 (arbalest)

# b. only contain consonants
words_all <- stringr::words
consonantes <- c("dry", "my", "gym", "dig", "apple", "life", "hymn")
str_view(consonantes, "^[^aeiou]+$")

# c. end with 'ed' but not with 'eed'
word_eds <- c("stead", "fed", "feed", "indeed", "lead", "education")
str_view(word_eds, "[^e]ed$")

# d. end with ing or ise
wordy <- c("realise", "barking", "quiet", "shout", "demoralise", "kicking")
str_view(wordy, "ing$|ise$")
# or 
str_view(wordy, "i(ng|se)$")

# Repetition: controlling amount of times pattern matches
# ? : 0 or 1
# + : 1 or more
# * : 0 or more
x <- "1888 is the longest year in Roman numerials: MDCCCLXXXVIII"
x
str_view(x, "CC")
str_view(x, "CC+")
str_view(x, "C[LX]")
str_view(x, "C[LX]+")
str_view(x, "I{2}$", match = TRUE)


# specify number of matches:
# {n} : exactly n
# {n, } : n or more
# {, m} : at most m
# {n,m} : between n and m
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

# Match longest string possible. For shortest string possible 'lazy' need '?' after.
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+")
str_view(x, "C[LX]+?")

# Exerises
str_view(x, "C{,1}")  # no more than one
str_view(x, "C{1,}")  # one or more == ?

str_view(x, "^.*$")   # any string

y <- "blahde blahrgh {yar} ded hoho"
str_view(y, "\\{.+\\}", match = TRUE)

z <- "zyd mars, kett. kjlg asag qwr po ui nm azcx klmn ouin"
str_view(words, "^[-aeiou]{3}", match = T)           # starting with 3 consonants
str_view(z, "^[aeiou]{3,}", match = T)               # three or more vowels in a row

str_view(words, "[aeiou][-aeiou]{2,}", match = TRUE) # two or more, vowel + consonant pairs 

## Grouping & Backreferences
# () to disambiguate complex expressions
# also define groups to refer with backreferences     ex. \1   \2     \3    etc....
# find all fruits that have repeated pair of letters:
str_view(fruit, "(..)\\1", match = TRUE)

str_view(fruit, "(.)\\1\\1", match = TRUE) # match all with repeated letter three times in row ex. "aaa"
str_view(fruit, "(.)(.)\\2\\1", match = TRUE)  # match pair of characters by same pair of characters 
# in reverse order     ex. "abba" "p-eppe-r" in fruit dataset
str_view(fruit, "(..)\\1", match = TRUE)  # any two caracters repeated    ex. "a1a1"
str_view(fruit, "(.).\\1.\\1", match = TRUE) # a character followed by ANY character followed by original
# character, followed by ANY character, follow original character again.

str_view(fruit, "(.)(.)(.).*\\3\\2\\1", match = TRUE)             # 3 specific characters in a row, ANY character, any character for zero
# or more times, followed by 3 specific characters in reverse order.
# ex. abccba, abc1cba, abc2397asdfjlcba

# start + end with same character
s <- "asdf afffa jedlock kerpak"
str_view(words, "^(.).*\\1$", match = TRUE)

# contain repeated pair of letters >>> "church" = "ch" repeated
asdf <- "church has gotten blargh dhurdh larlar"
str_view(asdf, "(..).*\\1", match = TRUE)
str_view(words, "(..).*\\1", match = TRUE)
# contain one letter repeated i nat least three places >>> eleven = 'e' in three different places
str_view(words, "(.).*\\1.*\\1", match = TRUE)

## stringr functions
# 1. determine which strings to match a pattern
# 2. find positions of matches
# 3. extract content of matches
# 4. replace matches with new values
# 5. split string based on match

# Detect matches: str_detect()
# logical vector with same length as input
x <- c("apple", "banana", "pear")
str_detect(x, "e")
# TRUE FALSE TRUE

# logical vectors in numeric context: FALSE = 0, TRUE = 1
# how many words start with t?
sum(str_detect(words, "^t"))          # 65!
# what proportion of words end with vowel?
mean(str_detect(words, "[aeiou]$"))   # ~0.277

# for complex logical conditions = combine multiple str_detect() with logical operators...
# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting of only consonants
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)                    # TRUE

# Detect elements with a match pattern
words[str_detect(words, "x$")]   # box, sex, six, tax
str_subset(words, "x$")          # box, sex, six, tax
# usually strings inside column in df 
df <- tibble(
  word = words,
  i = seq_along(word)   # set sequence accordingly
)
df %>% filter(str_detect(words, "x$"))

# str_count() for numeric vs. T/F output
x <- c("apple", "banana", "pear")
str_count(x, "a")    # 1   3   1
# onv avg. how many vowels/word?
mean(str_count(words, "[aeiou]"))   # 1.99 on avg.
mean(str_count(words, "[^aeiou]"))   # 3.24 on avg.

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

# Matches NEVER OVERLAP.
str_count("abababa", "aba")      # 2!
str_view_all("abababa", "aba")   # 2!

# Exercises
# Find all words that start or end with x 
df %>% 
  mutate(
    start_x = str_detect(word, "^x"),
    end_x = str_detect(word, "x$")
  )
# in one regex
words[str_detect(words, "^x|x$")]

# Find all words start with vowel and end with consonant
str_detect(words, "^[aeiou]")
str_detect(words, "[^aeiou]$")

df <- df %>% 
  mutate(
    start_vowel = str_detect(words, "^[aeiou]"),
    end_conso = str_detect(words, "[^aeiou]$"))

df %>% 
  filter(start_vowel == TRUE & end_conso == TRUE) %>% 
  count()   # 122, or head() to see.

# Any words contain at least one of each different vowel? 

# Word with highest # of vowels?
# Word with highest proportion of vowels?
str_count(words, "[aeiou]")


# Extract matches: 
# extract text with str_extract()
stringr::sentences
length(sentences)  # 720 sentences
head(sentences)
# find all sentences with color
colors <- c("red", "orange", "blue", "black", "green", "purple", "grey", "white")
color_match <- str_c(colors, collapse = "|")
color_match # single regex of color names
has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)
# select sentences with more than one match
more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)  # 5 sentences with x > 1 color in string

# all matches
str_extract_all(more, color_match, simplify = TRUE)    # simplify = TRUE: return matrix 

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

# Exercises: 
# previous include flickeRED, coveRED, spatteRED... modify regex to fix problem. 
# use \b for boundary between words
color_match2 <- str_c("\\b(", str_c(colors, collapse = "|"), ")\\b")
color_match2
has_color2 <- str_subset(sentences, color_match2)
matches2 <- str_extract(has_color2, color_match2)
head(matches2)
more2 <- sentences[str_count(sentences, color_match2) > 1]
str_view_all(more2, color_match2)  # 2 

# extract first word from each sentence
str_extract(sentences, "[a-zA-Z]+") %>% head()
str_count(sentences, "[a-zA-Z]+") %>% head()

# extract words ending in "ing"
pattern <- "\\b[a-zA-Z]+ing\\b"  # use \\b rather than $ as extract from sentences.
sentences_with_ing <- str_detect(sentences, pattern)
unique(unlist(str_extract_all(sentences[sentences_with_ing], pattern))) %>% head()

# plurals: assume any word ending with 's' and longer than 3 characters
unique(unlist(str_extract_all(sentences, "\\b[A-Za-z]{3,}s\\b"))) %>% head()

## Grouped matches
# parentheses to extract parts of complex match 
noun <- "(a|the) ([^ ]+)"                          # a or the followed by any # of letters
has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun)
# str_extract() = complete match, str_match() = individual component, returns matrix
has_noun %>% 
  str_match(noun)
# detects adjectives as well.....
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, 
    c("article", "noun"), 
    "(a|the) ([^ ]+)",
    remove = FALSE
  )

# Exercises
# find all words after a number word
numword <- "(one|two|three|four|five|six|seven|eight|nine|ten) +(\\S+)"
sentences[str_detect(sentences, numword)] %>% 
  str_extract(numword)
# "+(\\S+)")   parentesized back=reference, extract matched sub=string from input string

# find all contractions 
contraction <- "([A-Za-z]+)'([A-Za-z]+)"
sentences %>% 
  `[`(str_detect(sentences, contraction)) %>% 
  str_extract(contraction)

## Replacing matches
# str_replace(), str_replace_all()
x <- c("apple", "pear", "banana")

str_replace(x, "[aeiou]", "-")
# "-pple"   "p-ar", "b-nana"
# multiple replacements with str_replace_all()
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))
# "one house"     "two cars"     "three people"

# replace using backreferences to insert components into match 
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% head(6)
# swap order of 3rd and 2nd word

# Exercises: 
# replace all forward slashes with backslashes
strang <- "\\/ blahblah \\/ yoyo\\/ har"
strang %>% str_replace("\\/", "\\\\")


## Splitting 
































