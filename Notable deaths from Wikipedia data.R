# Notable deaths from WIKIPEDIA.

library("rvest")
library("dplyr")
library("purrr")
library("tidyr")
library("lazyeval")
library("tibble")
library("fuzzyjoin")
library("stringr")
months <- c("January", "February",
            "March", "April",
            "May", "June",
            "July", "August",
            "September", "October",
            "November", "December")
years <- 2004:2016
pages_content <- map(months, paste, years, sep = "_") %>%
  unlist() %>%
  # read page with monthly deaths
  map(function(x){
    read_html(paste0("https://en.wikipedia.org/wiki/Deaths_in_", x))})

transform_day <- function(day_deaths, day_in_month){
  # filter only those that have the format of lines presenting deaths
  day_deaths <- str_replace_all(day_deaths$days, "<ul><li>", "")
  day_deaths <- str_split(day_deaths, "<li>", simplify = TRUE)
  day_deaths <- day_deaths[!str_detect(day_deaths, "Death")]
  day_deaths <- day_deaths[!str_detect(day_deaths, "Category")]
  
  # Erases the end of each line
  day_deaths <- str_replace_all(day_deaths, "\\(<i>.*", "")
  
  # Create a table
  day_deaths <- tibble_(list(line = ~day_deaths))
  
  # Variable for grouping by row 
  day_deaths <- mutate_(day_deaths, row = interp(~1:nrow(day_deaths)))
  day_deaths <- group_by_(day_deaths, "row")
  
  # separate by the word "title", first part is a link, second part description
  day_deaths <- mutate_(day_deaths, line = interp(~str_split(line, "title")))
  day_deaths <- mutate_(day_deaths, wiki_link = interp(~str_replace_all(line[[1]][1], "<a href=\\\"/wiki/", "")))
  
  # get Wikipedia link or better said the thing to paste to Wikipedia address
  day_deaths <- mutate_(day_deaths, 
                        wiki_link = interp(~str_replace_all(wiki_link, "\\\\", "")))
  day_deaths <- mutate_(day_deaths, 
                        wiki_link = interp(~str_replace_all(wiki_link, "\"", "")))
  
  
  # now transform the description into several columns
  # the format of the end of the description is variable so
  # I only keep the beginning of the reason for notoriety i.e. country of origin
  # and a role anyway cause of death is not written for all and I don't want to 
  # use many details
  day_deaths <- mutate_(day_deaths, content = interp(~line[[1]][2]))
  day_deaths <- mutate_(day_deaths,
                        content = interp(~str_replace_all(content, '<a.*a>', "")))
  day_deaths <- separate_(day_deaths, "content", 
                          into = c("name", "age", "country_role"),
                          sep = ",")
  
  # when no age
  day_deaths <- mutate_(day_deaths, country_role = interp(~ifelse(is.na(country_role),
                                                                  age,
                                                                  country_role)))
  day_deaths <- mutate_(day_deaths, age = interp(~ as.numeric(age)))
  
  # improves the name
  day_deaths <- mutate_(day_deaths, 
                        name = interp(~ str_replace_all(name, "\">.*", "")))
  day_deaths <- mutate_(day_deaths, 
                        name = interp(~ str_replace_all(name, "=\"", "")))
  
  # improves the country_role
  day_deaths <- mutate_(day_deaths, 
                        country_role = interp(~ str_replace_all(country_role, "</li>", "")))
  day_deaths <- mutate_(day_deaths, 
                        country_role = interp(~ str_replace_all
                                              (country_role, "</ul>", "")))
  
  # get rid of original line
  day_deaths <- select_(day_deaths, quote(- line))
  
  # get rid of grouping
  day_deaths <- ungroup(day_deaths)
  day_deaths <- select_(day_deaths, quote(- row))
  return(day_deaths)
}

parse_month_deaths <- function(month_deaths){
  
  # remember month and year
  title <- stringr::str_extract(toString(html_nodes(month_deaths, "title")), 
                                "Deaths in .* -")
  
  title <- str_replace_all(title, "Deaths in ", "")
  title <- str_replace_all(title, " -", "")
  title <- str_replace_all(title, "January", "01")
  title <- str_replace_all(title, "February", "02")
  title <- str_replace_all(title, "March", "03")
  title <- str_replace_all(title, "April", "04")
  title <- str_replace_all(title, "May", "05")
  title <- str_replace_all(title, "June", "06")
  title <- str_replace_all(title, "July", "07")
  title <- str_replace_all(title, "August", "08")
  title <- str_replace_all(title, "September", "09")
  title <- str_replace_all(title, "October", "10")
  title <- str_replace_all(title, "November", "11")
  title <- str_replace_all(title, "December", "12")
  
  # find days with deaths
  content <- toString(month_deaths)
  content <- str_split(content, "\n", simplify = TRUE)
  days <- which(str_detect(content, "h3"))
  
  paragraphs <- which(str_detect(content, "<ul>"))
  
  last_good <- max(which(str_detect(unlist(lapply(html_nodes(month_deaths, "h3"), 
                                                  toString)),
                                    pattern = "title=Deaths_in_.*_..")))
  
  possible_days <- 1:last_good
  
  possible_days <- possible_days[diff(c(days[possible_days], 999999)) > 1]
  # read only lines
  month_deaths <- html_nodes(month_deaths, "ul")
  first_not_good <- max(which(str_detect(month_deaths, "Template")))
  first_good <- min(which(str_detect(month_deaths, "wiki")))
  paragraphs <- paragraphs[first_good:(first_not_good - 1)]
  
  if(length(paragraphs) > length(possible_days)){
    jours <- NULL
    for(paragraph in paragraphs){
      x <- days[days < paragraph]
      jours <- c(jours,
                 possible_days[which(abs(x-paragraph)==min(abs(x-paragraph)))])
    }
    possible_days <- jours
  }
  
  month_deaths <- month_deaths[first_good:(first_not_good - 1)]
  month_deaths <- unlist(map(month_deaths, toString))
  
  
  
  # transform for getting the different columns
  month_deaths_table <- data.frame(days = month_deaths,
                                   day_in_month = possible_days,
                                   title = title) %>%
    by_row(transform_day, .to = "all_deaths") %>%
    unnest_("all_deaths") %>%
    group_by_("wiki_link") %>%
    mutate_(index = ~ 1:n()) %>%
    filter_(~index == 1) %>%
    select_(quote(-index)) %>%
    group_by_("days") %>%
    mutate_(date = interp(~ lubridate::dmy(paste(day_in_month, title)))) %>%
    select_(quote(- day_in_month)) %>%
    select_(quote(- title)) %>%
    ungroup() %>%
    select_(quote(- days)) 
  
  month_deaths_table
  
}

# Map to 'deaths' wikipedia pages.
deaths_2004_2016 <- pages_content %>%  
  map(parse_month_deaths) %>%
  dplyr::bind_rows()
knitr::kable(head(deaths_2004_2016))


# Get the demonyms table from Wikipedia

# I discovered Wikipedia has a table (a table! a table!) of adjectivals 
# for many countries and nations. The only thing I changed was getting one 
# line by adjectivals when there were several ones by country or nation. 
# I also calculated the number of words in this adjectival in order to be 
# able to easily remove it from the "country_role" column and thus get the 
# role on its own.

demonyms_page <- read_html("https://en.m.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations")

demonyms_table <- html_nodes(demonyms_page, "table")[1]
demonyms_table <- html_table(demonyms_table, fill = TRUE)[[1]]
names(demonyms_table) <- c("country", "adjectivals", "demonyms", "colloquial_demonyms")
demonyms_table <- tibble::as_tibble(demonyms_table)
demonyms_table <- by_row(demonyms_table,
                         function(df){
                           adjectivals <- tibble_(list(goodadjectivals = interp(~str_split(df$adjectivals, " or "))))
                         }, .collate = "list", .to = "good_adjectivals") %>%
  unnest_("good_adjectivals") %>%
  unnest_("goodadjectivals")
demonyms_table <- select_(demonyms_table, quote(- adjectivals))
demonyms_table <- rename_(demonyms_table, "adjectivals" = "goodadjectivals" )
demonyms_table <- by_row(demonyms_table,
                         function(df){
                           adjectivals <- tibble_(list(goodadjectivals = interp(~str_split(df$adjectivals, ","))))
                         }, .collate = "list", .to = "good_adjectivals") %>%
  unnest_("good_adjectivals") %>%
  unnest_("goodadjectivals")

demonyms_table <- select_(demonyms_table, quote(- adjectivals))
demonyms_table <- rename_(demonyms_table, "adjectivals" = "goodadjectivals" )

demonyms_table <- mutate(demonyms_table, adjectivals = trimws(adjectivals ))
demonyms_table <- by_row(demonyms_table,
                         function(df){
                           length(str_split(df$adjectivals, " ")[[1]])},
                         .to = "adj_length", .collate = "cols")



# Using adjectivals to split deaths' country and role

# For finding which country/nation to add to a line I used
# fuzzyjoin::regex_left_join() which worked well but a bit slowly 
# given the number of lines.

deaths <- deaths_2004_2016
demonyms <- demonyms_table
deaths <- mutate(deaths, country_role = str_replace(country_role,
                                                    "<.*", ""))

demonyms <- mutate(demonyms, adjectivals = strsplit(adjectivals, ","))
demonyms <- unnest(demonyms, adjectivals)
demonyms <- mutate(demonyms, 
                   adjectivals = paste0(adjectivals, " .*"))
deaths <- regex_left_join(deaths, 
                          demonyms, by = c("country_role" = "adjectivals"))

deaths <- mutate(deaths,
                 country = ifelse(str_detect(country_role, "American"),
                                  "United States",
                                  country))

deaths <- mutate(deaths,
                 adjectivals = ifelse(country == "United States",
                                      "American", adjectivals))
deaths <- mutate(deaths,
                 adj_length = ifelse(country == "United States",
                                     1, adj_length))
# keep one country only
deaths <- group_by(deaths, wiki_link) %>%
  mutate(index = 1:n()) %>%
  filter(index == 1)

deaths <- by_row(deaths,
                 function(df){
                   if(!is.na(df$adj_length)) {
                     country_role <- trimws(df$country_role)
                     splitted <- str_split(country_role, " ")[[1]]
                     role <- toString(splitted[(df$adj_length + 1):length(splitted)])
                     str_replace_all(role, ",", "")
                   }else{
                     df$country_role
                   }
                   
                 }, .to = "occupation", .collate = "cols")

deaths <- mutate(deaths, 
                 occupation = str_replace_all(occupation, "\\r", ""))
deaths <- mutate(deaths, 
                 occupation = str_replace_all(occupation, "\\n", ""))
deaths <- mutate(deaths, 
                 occupation = str_replace_all(occupation, "\\.", ""))

deaths <- select(deaths, - demonyms, - colloquial_demonyms,
                 - index)
readr::write_csv(deaths, path = "data/deaths_with_demonyms.csv")
knitr::kable(head(deaths))


# In the table I have information about 56303 notable deaths. I know the 
# age of 97% of them, a country or nation for 96.2% of them





# Who + age of notable dead from wikipedia data?
library("ggplot2")
library("viridis")
library("broom")
library("dplyr")
library("lubridate")
library("tidytext")
library("rcorpora")
deaths <- readr::read_csv("data/deaths_with_demonyms.csv")

ggplot(deaths) +
  geom_histogram(aes(age)) +
  ggtitle("Age at death of Wikipedia notable dead")


tidy(summary(deaths$age))

##   minimum q1 median  mean q3 maximum   na
## 1       1 68     80 75.94 88     176 1677

# ^high maximal age.

arrange(deaths, desc(age)) %>%
  head(n = 10) %>%
  knitr::kable()

# Oldest = tortoise + tree... not very relevant lmao

deaths <- filter(deaths, age < 125)

# What about the deaths at the youngest ages?

arrange(deaths, age) %>%
  head(n = 10) %>%
  knitr::kable()

# Age distribution change over time?

deaths <- mutate(deaths, death_year = as.factor(year(date)))
ggplot(deaths) +
  geom_boxplot(aes(death_year, age, fill = death_year)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none")

# Increasing trend? 
# Usage Mann-Kendall Test! 
# Age @ death monotonically increase over time?

library("trend")
library("lubridate")
weekly_median_age <- deaths %>% 
  filter(!is.na(age)) %>%
  group_by(wiki_link) %>%
  mutate(week = paste(year(date), week(date))) %>%
  group_by(week) %>%
  summarize(age = median(age)) %>% .$age
weekly_median_age <- as.ts(weekly_median_age)
plot(weekly_median_age)



res <- mk.test(weekly_median_age)
summary.trend.test(res)

## Mann-Kendall Test
##  
## two-sided homogeinity test
## H0: S = 0 (no trend)
## HA: S != 0 (monotonic trend)
##  
## Statistics for total series
##       S     varS    Z   tau     pvalue
## 1 79925 36111378 13.3 0.337 < 2.22e-16


# Sen's method to compute slope...
sens <- sens.slope(weekly_median_age, level = 0.95)
sens

##  
## Sen's slope and intercept
##  
##  
## slope:  0.0065
## 95 percent confidence intervall for slope
## 0.0074 0.0056
##  
## intercept: 77.1831
## nr. of observations: 689

# With such a slope in one year one gains 0.34 years. 
# Will we soon have humans as old as Harriet the tortoise?


# WHERE did notable dead come from?
deaths %>% group_by(country) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(n = 10) %>%
  knitr::kable()

# Occupations of notable dead?
stopwords <- corpora("words/stopwords/en")$stopWords

deaths_words <- deaths %>%
  unnest_tokens(word, occupation) %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% stopwords)


head(deaths_words, n = 10) %>%
  knitr::kable()


# Time series of notable deaths from wikipedia data
library("ggplot2")
library("viridis")
library("dplyr")
library("lubridate")
deaths <- readr::read_csv("data/deaths_with_demonyms.csv")
deaths %>%
  group_by(date) %>%
  summarize(n_deaths = n()) %>%
  ggplot(aes(x = date, xend = date,
             y = 0, yend = n_deaths)) +
  geom_segment() +
  xlab("Time (days)") + ylab("Number of reported deaths")

# Monthly counts



library("tscount")
monthly_deaths <- deaths %>%
  group_by(wiki_link) %>%
  mutate(month = update(date, day = 1)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarize(n = n()) 

haiti_earthquake <- update(ymd("2010-01-12"), day = 1)
aircrash1 <- update(ymd("2010-01-12"), day = 1)
aircrash2 <- update(ymd("2011-09-07"), day = 1)

monthly_deaths <- mutate(monthly_deaths, n = ifelse(month == haiti_earthquake, n - 10, n))
monthly_deaths <- mutate(monthly_deaths, n = ifelse(month == aircrash1, n - 96, n))
monthly_deaths <- mutate(monthly_deaths, n = ifelse(month == aircrash2, n - 44, n))

ggplot(monthly_deaths) +
  geom_segment(aes(x = month, xend = month,
                   y = 0, yend = n)) +
  ylab("Number of reported deaths") +
  xlab("Time (months)")

# Time series (ts) object

ts_deaths <- xts::xts(monthly_deaths$n[1:144], monthly_deaths$month[1:144])

ts_deaths = ts(ts_deaths, freq=12, start=c(2004, 1))
plot(ts_deaths)



time <- log(1:nrow(monthly_deaths))
fit_pois <- tsglm(ts_deaths[1:144], model = list(past_obs = 1, past_mean = 13), xreg = time[1:144], distr = "poisson")
fit_nb <- tsglm(ts_deaths[1:144], model = list(past_obs = 1, past_mean = 13), xreg = time[1:144], distr = "nbinom")



rbind(Poisson = scoring(fit_pois), NegBin = scoring(fit_nb))

##         logarithmic    quadratic  spherical rankprob    dawseb     normsq
## Poisson    10.23428  0.003338134 -0.0495852 40.83497 17.633417 11.8105665
## NegBin      5.68638 -0.004157600 -0.0655695 35.61633  9.417219  0.9722222
##          sqerror
## Poisson 3835.296
## NegBin  3835.296

# Predict 2016 values
set.seed(1)
pred2016 <- predict(fit_nb, n.ahead = 12, level = 0.9, global = TRUE, B = 3000, newxreg = time[145:156])
monthly_deaths <- mutate(monthly_deaths, lower = NA,
                         upper = NA, pred = NA)
monthly_deaths$lower[145:156] <- pred2016$interval[,1]
monthly_deaths$upper[145:156] <- pred2016$interval[,2]
monthly_deaths$pred[145:156] <- pred2016$pred

ggplot(monthly_deaths) +
  geom_segment(aes(x = month, xend = month,
                   y = 0, yend = n)) +
  ylab("Number of reported deaths") +
  xlab("Time (months)") +
  geom_line(aes(x = month, y = pred), col = "blue") +
  geom_ribbon(aes(x = month, ymin = lower, ymax = upper),
              alpha = 0.5)





