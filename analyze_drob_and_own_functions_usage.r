library(tidyverse)
library(tidyquant)
library(tibbletime)
library(rvest)
library(broom)


# http://www.business-science.io/learning-r/2018/03/03/how_to_learn_R_pt1.html

# count_to_pct(): convert counts ot percentages

count_to_pct <- function(data, ..., col = n ) {
  
  grouping_vars_expr <- quos(...) 
  col_expr <- enquo(col)
  
  data %>% 
    group_by(!!! grouping_vars_expr) %>% 
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>% 
    ungroup()
  
}

# Usage:
# Retrieve counts 
# count_to_pct() to get percentages within groups

mpg %>% 
  count(manufacturer) %>% 
  count_to_pct() %>% 
  top_n(5) %>% 
  arrange(desc(n))

# Parse function names: takes text and returns function names
parse_function_names <- function(text, stop_words = c("")) {
  
  parser <- function(text, stop_words) {
    
    ret <- text %>%
      str_c(collapse = " ") %>%
      str_split("\\(") %>%
      set_names("text") %>%
      as.tibble() %>%
      slice(-n()) %>%
      mutate(str_split = map(text, str_split, " ")) %>%
      select(-text) %>%
      unnest() %>%
      mutate(function_name = map_chr(str_split, ~ purrr::pluck(last(.x)))) %>%
      select(function_name) %>%
      separate(function_name, into = c("discard", "function_name"), 
               sep = "(:::|::|\n)", fill = "left") %>%
      select(-discard) %>%
      mutate(function_name = str_replace_all(function_name, 
                                             pattern = "[^[:alnum:]_\\.]", "")) %>%
      filter(!(function_name %in% stop_words)) 
    
    return(ret)
  }
  
  safe_parser <- possibly(parser, otherwise = NA)
  
  safe_parser(text, stop_words)
}

# Usage: parses the function names preceding "(" >>> returns tibble
test_text <- "my_mean <- mean(1:10) blahblah blah glarb base::sum(my_mean)"

parse_function_names(test_text)


# find functions in package: takes library/package name >>> returns all funs inside

find_functions_in_package <- function(package) {
  
  pkg_text <- paste0("package:", package)
  
  safe_ls <- possibly(ls, otherwise = NA)
  
  package_functions <- safe_ls(pkg_text)
  
  if (is.na(package_functions[[1]])) return(package_functions)
  
  ret <- package_functions %>% 
    as.tibble() %>% 
    rename(function_name = value) 
  
  return(ret)
  
}


# usage: takes in package loaded via library(____) >>> returns tibble of all funs in pkg

find_functions_in_package("dplyr") %>% glimpse()

# find loaded packages: detects which packages laoded in R system

find_loaded_packages <- function() {
  
  ret <- search() %>% 
    list() %>% 
    set_names("search") %>% 
    as.tibble() %>% 
    separate(search, into = c("discard", "keep"), sep = ":", fill = "right") %>% 
    select(keep) %>% 
    filter(!is.na(keep)) %>% 
    rename(package = keep) %>% 
    arrange(package)
  
  return(ret)
  
}

# usage: returns tibble of loaded packgages

find_loaded_packages()


# map found functions to respective package

map_loaded_package_functions <- function(data, col) {
  
  col_expr <- enquo(col)
  
  data %>% 
    mutate(function_name = map(!! col_expr, find_functions_in_package)) %>% 
    mutate(is_logical = map_dbl(function_name, is.logical)) %>% 
    filter(is_logical != 1) %>% 
    select(-is_logical) %>% 
    unnest()
  
}

# usage: use with find_loaded_packages(), build corpus of package and function combinations for every package 
# when loaded >>> returns tibble of packages and associated functions 

find_loaded_packages() %>% 
  map_loaded_package_functions(package) %>% 
  glimpse()

## web-scraping VE blog

# 1. get PATH for one of articles
# 2. create corpus of all packages loaded in R Sessions >>> 
# find_loaded_packages + map_loaded_package_functions = loaded_functions_tbl
# 3. rvest >>> read_html() >>> html_text()
# 4. parse_function_names() >>> left_join() with loaded_functions_tbl

path <- "http://varianceexplained.org/r/mixture-models-baseball/"

loaded_functions_tbl <- find_loaded_packages() %>% 
  map_loaded_package_functions(package)

html_code_text <- path %>% 
  read_html() %>% 
  html_nodes("code") %>% 
  html_text()

# replace any missing packages == "Unknown"
# stats::filter and dplyr::filter conflict!

mixture_models_code_tbl <- html_code_text %>% 
  parse_function_names() %>% 
  left_join(loaded_functions_tbl) %>% 
  filter(!(function_name == "filter" & !(package == "dplyr"))) %>% 
  mutate(package = case_when(is.na(package) ~ "Unknown", TRUE ~ package))

mixture_models_code_tbl %>% glimpse()
# 131 functions used in article!

mixture_models_code_tbl %>% 
  count(package, function_name) %>% 
  count_to_pct() %>% 
  arrange(desc(n)) %>% 
  top_n(5) %>% 
  knitr::kable()


## Scale up to ALL blog posts!

# 1. web scrape post title/dates/paths with rvest
# 2. scale to all posts with purrr and custom funs

# titles = article a
# dates = article p.datetime
# paths/href = article a >>> href

posts_path <- "http://varianceexplained.org/posts/"

titles_vec <- posts_path %>% 
  read_html() %>% 
  html_node("#main") %>% 
  html_nodes("article") %>% 
  html_nodes("a") %>% 
  html_text(trim = TRUE)

dates_vec <- posts_path %>% 
  read_html() %>% 
  html_node("#main") %>% 
  html_nodes("article") %>% 
  html_nodes("p.dateline") %>% 
  html_text(trim = TRUE) %>% 
  mdy()      # lubridate >>> transforms dates as chr into POXIT/lubridate objects!

hrefs_vec <- posts_path %>% 
  read_html() %>% 
  html_node("#main") %>% 
  html_nodes("article") %>% 
  html_nodes("a") %>% 
  html_attr("href")

variance_explained_tbl <- bind_cols(
  title = titles_vec,
  date = dates_vec, 
  href = hrefs_vec)

variance_explained_tbl %>% head() %>% glimpse()


# combine rvest ops for scrape and create tbl of function names

build_function_names_tbl_from_url_path <- function(path, loaded_functions_tbl) {
  
  builder <- function(path, loaded_functions_tbl) {
    
    path %>% 
      read_html() %>% 
      html_nodes("code") %>% 
      html_text() %>% 
      parse_function_names() %>% 
      left_join(loaded_functions_tbl) %>% 
      filter(
        !(function_name == "filter" & !(package == "dplyr"))
      ) %>% 
      mutate(package = ifelse(is.na(package), "Unknown", package))
    
  }
  
  safe_builder <- possibly(builder, otherwise = NA)
  
  safe_builder(path, loaded_functions_tbl)
  
}

# Usage:

path <- "http://varianceexplained.org/r/mixture-models-baseball/"
build_function_names_tbl_from_url_path(path, loaded_functions_tbl) %>%
  glimpse()


# scale to ALL posts
# some posts have NO code >>> nested NA values
# filter out by mapping "is.logical"


variance_explained_tbl <- bind_cols(
    title = titles_vec, 
    date = dates_vec,
    href = hrefs_vec) %>% 
  mutate(
    function_name = map(href, build_function_names_tbl_from_url_path, loaded_functions_tbl),
    is_logical = map_dbl(function_name, is.logical)
  ) %>% 
  filter(is_logical == 0) %>% 
  select(-is_logical) %>% 
  unnest()

# 2314 functions across 58 articles!

# Which funs are most frequently used???

ve_functions_top_20_tbl <- variance_explained_tbl %>% 
  count(package, function_name) %>% 
  count_to_pct() %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  mutate(function_name = as_factor(function_name) %>% fct_reorder(n)) %>% 
  arrange(desc(function_name)) %>% 
  mutate(package = as_factor(package))

ve_functions_top_20_tbl %>% glimpse()

# visualize with lolipop-charts:

ve_functions_top_20_tbl %>%
  ggplot(aes(x = n, y = function_name, color = package)) +
  geom_segment(aes(xend = 0, yend = function_name), size = 2, 
               arrow = arrow(length = unit(0.03, "npc"))) +   # add arrow for the lolz
  geom_point(size = 4) +
  geom_label(aes(label = paste0(function_name, "(), ", package, ", ", scales::percent(pct))), 
             hjust = "inward", size = 3.5) +  # inward = toward center, outward = away from center
  expand_limits(x = 0) +
  labs(
    title = "Which Functions Are Most Frequently Used by DRob?",
    subtitle = "Variance Explained Blog",
    x = "Function Count (n)", y = "Count of R Functions (n)") +
  scale_color_tq() +
  theme_tq() + # theme from tidyquant
  theme(legend.position = "none")


# Which packages used most frequently:
# time-series >>> trends with packages over time
# aggregate/group by 6-month intervals
# sort packages into 6 categories based on top 5 packages + extra of "Other"

ve_package_frequency_tbl <- variance_explained_tbl %>% 
  select(date, package, function_name) %>% 
  mutate(package = as.factor(package) %>%  fct_lump(n = 5, other_level = "Other")) %>% # VERY COOL
  arrange(date) %>% 
  as_tbl_time(index = date) %>% collapse_by(period = "6 m", clean = TRUE) %>% 
  count(date, package) %>% 
  count_to_pct(date) %>% 
  mutate(biannual = paste0("H", semester(date)))

ve_package_frequency_tbl %>% glimpse()

# in some 6-month intervals less blog posts than others...
# mitigate by normalizing >>> percentage of total functions by half_year

# pure total counts
ve_package_frequency_tbl %>% 
  ggplot(aes(date, n, fill = package)) +
  geom_col() +
  geom_text(aes(x = date, y = n, label = biannual), 
            vjust = -1, color = palette_light()[[1]], size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ package, ncol = 3) +
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Which Packages Are Most Frequently Used by DRob?",
    subtitle = "Variance Explained Blog",
    x = "Date (Bi-Annual Aggregation)", y = "Count of R Functions (n)"
  )

# percentage of total functions by half_year
ve_package_frequency_tbl %>% 
  ggplot(aes(date, pct, fill = package)) +
  geom_col() +
  geom_text(aes(x = date, y = pct, label = biannual), 
            vjust = -1, color = palette_light()[[1]], size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ package, ncol = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Which Packages Are Most Frequently Used by DRob?",
    subtitle = "Variance Explained Blog",
    x = "Date (Bi-Annual Aggregation)", y = "% of Total R Functions [n / sum(n)]"
  )


# overall by uncounting >> recounting by package >>> cumulative percentage col

ve_package_frequency_tbl %>% 
  uncount(weights = n) %>% 
  count(package) %>% 
  count_to_pct() %>% 
  arrange(desc(n)) %>% # see pct of each of top 5 pacakges
  mutate(pct_cum = cumsum(pct)) # create cum perc column!


# Usage of "tidyverse" packages?

tidyverse_packages(include_self = F)

ve_tidiness_tbl <- variance_explained_tbl %>% 
  select(date, function_name, package) %>% 
  mutate(tidy_function = case_when(
    package %in% tidyverse_packages() ~ "Yes",
    TRUE ~ "No"
  ))

# could also use inner_join, anti_join if create separate tbl of only tidyverse packages...

ve_tidiness_tbl %>% glimpse()

ve_tidiness_tbl %>% 
  count(tidy_function) %>% 
  count_to_pct() %>% 
  arrange(desc(n))

# change to tidyverse over time??

ve_tidiness_over_time_tbl <- ve_tidiness_tbl %>% 
  select(date, tidy_function, function_name, package) %>% 
  arrange(date) %>% 
  as_tbl_time(index = date) %>% 
  collapse_by(period = "6 m", clean = TRUE) %>% # clean == round up to next interval!
  count(date, tidy_function) %>% 
  count_to_pct(date) %>% 
  filter(tidy_function == "Yes") %>% 
  mutate(biannual = paste0("H", semester(date)))

ve_tidiness_over_time_tbl %>% glimpse()

ve_tidiness_over_time_tbl %>% 
  ggplot(aes(date, pct)) +
  geom_col(fill = palette_light()[[1]], color = "white") +
  geom_text(aes(x = date, y = pct, label = biannual),
            vjust = -1, color = palette_light()[[1]], size = 3) +
  geom_text(aes(x = date, y = pct, label = scales::percent(pct)),
            vjust = 2, color = "white", size = 3) +
  geom_smooth(method = "lm", se = F) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = 'How "Tidy" Is DRobs Code?',
    subtitle = "Variance Explained Blog",
    x = "Date (Bi-Annual Aggregation)", y = "% of Total R Functions [n / sum(n)]"
  ) +
  expand_limits(y = 1)   # to show up to 100% !! necessary to not fudge scale of bars
  

# 80/20 rule

ve_eighty_twenty_tbl <- variance_explained_tbl %>% 
  count(package, function_name) %>% 
  count_to_pct() %>% 
  arrange(desc(pct)) %>% 
  mutate(
    pct_cum = cumsum(pct),
    high_usage = case_when(
      pct_cum <= 0.8 ~ "Yes",
      TRUE ~ "No"
    )
  )

ve_eighty_twenty_tbl %>% glimpse()

ve_eighty_twenty_tbl %>% 
  count(high_usage) %>% 
  count_to_pct(col = nn)


ve_eighty_twenty_tbl %>% 
  filter(high_usage == "Yes") %>% 
  split(.$package)                     # split high_usage funs by package!






# analyze your own code! --------------------------------------------------

library(fs)


dir_path <- "C:/Users/Ryo Nakagawara/Documents/R_materials"

dir_info(dir_path, recursive = TRUE) %>% head()

rmd_or_r_file_paths_tbl <- dir_info(dir_path, recursive = TRUE) %>% 
  mutate(file_name = path_file(path)) %>% 
  select(file_name, path) %>% 
  filter(str_detect(file_name, "(\\.R|\\.Rmd)$"))

rmd_or_r_file_paths_tbl %>% glimpse()

# build_function_names_from_file_path >>> instead of from HTML!

build_function_names_tbl_from_file_path <- function(path, loaded_functions_tbl) {
  
  builder <- function(path, loaded_functions_tbl) {
    
    readLines(path) %>% 
      parse_function_names() %>% 
      left_join(loaded_functions_tbl) %>% 
      filter(
        !(function_name == "filter" & !(package == "dplyr"))
      ) %>% 
      mutate(package = ifelse(is.na(package), "Unknown", package))
    
  }
  
  safe_builder <- possibly(builder, otherwise = NA)
  
  safe_builder(path, loaded_functions_tbl)
  
}


# test

file_path_1 <- rmd_or_r_file_paths_tbl$path[[1]]

file_path_1

build_function_names_tbl_from_file_path(file_path_1, loaded_functions_tbl) %>% 
  glimpse()

# scale to ALL file paths

local_function_names_tbl <- rmd_or_r_file_paths_tbl %>% 
  mutate(
    function_name = map(path, build_function_names_tbl_from_file_path, loaded_functions_tbl), 
    is_logical    = map_dbl(function_name, is.logical)
  ) %>% 
  filter(is_logical != 1) %>% 
  select(file_name, function_name) %>% 
  unnest() %>% 
  left_join(loaded_functions_tbl)


local_function_names_tbl %>% glimpse()

local_functions_top_20_tbl <- local_function_names_tbl %>% 
  count(package, function_name) %>% 
  count_to_pct() %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  rowid_to_column(var = "rank")    # create rank var from ROW ID!

local_functions_top_20_tbl %>% glimpse()

local_functions_top_20_tbl %>%
  ggplot(aes(x = n, y = fct_reorder(function_name, n), color = package)) +
  geom_segment(aes(xend = 0, yend = function_name), size = 2) +
  geom_point(size = 4) +
  geom_label(aes(label = paste0(function_name, "(), ", package, ", ", scales::percent(pct))), 
             hjust = "inward", size = 3.5) + 
  expand_limits(x = 0) +
  theme_tq() + scale_color_tq() + theme(legend.position = "none") +
  labs(
    title = "Which Functions Are Most Frequently Used by Ryo Nakagawara?",
    subtitle = "R_by_Ryo blog",
    x = "Function Count (n)", y = "Count of R Functions (n)")

# similarities with DRob
local_functions_top_20_tbl %>% 
  filter(function_name %in% ve_functions_top_20_tbl$function_name)

# differences with DRob
local_functions_top_20_tbl %>% 
  filter(!function_name %in% ve_functions_top_20_tbl$function_name)


ve_functions_top_20_tbl %>% 
  rowid_to_column(var = "rank") %>% 
  filter(!function_name %in% local_functions_top_20_tbl$function_name)







