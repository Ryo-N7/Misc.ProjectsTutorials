# World Pop
library(ggplot2)
library(ggthemes)
library(scales)
library(wpp2015)
library(tidyverse)
library(ggjoy)
library(forcats)
library(viridis)
library(extrafont)


data("UNlocations")

countries <- UNlocations %>% 
  filter(location_type == 4) %>% 
  transmute(name = name %>% paste()) %>% 
  as_vector()  # turn into vector format

data("e0M")

str(countries)

DataData <- e0M %>% filter(country %in% countries) %>% 
  select(-last.observed) %>% 
  gather(period, value, 3:15) 


DataData %>% ggplot(aes(x = value, y = period %>% fct_rev())) + 
  geom_joy(aes(fill = period)) +   # dont forget aes()
  scale_fill_viridis(discrete = T, option = "B", direction = -1,
                     begin = .1, end = .9) +
  labs(x = "Male life expectancy at birth",
       y = "Period",
       title = "The world convergence in male life expectancy at birth since 1950",
       subtitle = "Data: UNPD World Population Prospects 2015 Revision",
       caption = "ikashnitsky.github.io") +
  theme_minimal(base_family =  "Roboto Condensed", base_size = 15)+
  theme(legend.position = "none")
 
  as.vector()


str(countries)

??as_vector()
??transmute()
??paste()




AllPop <- popM %>%  select(-country_code) %>% 
  gather(`1950`:`2015`, key = "year", value = "population")

# make list of belonging continent/region for each country and join into dataset...
# comparison within region NOT just region/continent total.
# or just combine with UNlocations data..... GODDAMITTT

AllPop$continent <- NA
AllPop$region <- NA

url <- "http://en.wikipedia.org/wiki/List_of_sovereign_states_and_dependent_territories_in_Africa"

AfricaCountry <- url %>% 
                     read_html() %>% 
                     html_nodes("table.wikitable:nth-child(12)") %>% 
                     .[[1]] %>% 
                     html_table()
AfricaCountry

AfricaCountry <- AfricaCountry %>% select(contains("English"))


AllPop %>% select(country) %>% unique()


AllPop %>% filter(country == c("Algeria", "Angola", 
                               "Benin", "Botswana", "Burkina Faso", "Burundi",
                               "Cape Verde", "Cameroon", "Central African Republic", "Congo", "Comoros", "Cote d'Ivoire",
                               "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia",
                               "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                               "Kenya", "Lesotho", "Liberia", "Libya",
                               "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique",
                               "Namibia", "Niger", "Nigeria", "Rwanda",
                               "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland",
                               "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")) %>% 
  mutate(continent = "Africa")

AllPop %>% filter(country == c("Afghanistan", "Armenia", "Azerbaijan", 
                               "Bahrain", "Bangladesh", "Bhutan", "Brunei Darussalam", 
                               "Cambodia", "China", "Cyprus", "Georgia",
                               "India", "Indonesia", "Iran", "Iraq", "Israel",
                               "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgzstan",
                               "Laos", "Lebanon",
                               "Malaysia", "Maldives", "Mongolia", "Myanmar", 
                               "Nepal", "North Korea", "Oman",
                               "Pakistan", "Palestine", "Philippines", "Qatar", "Russia",
                               "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria",
                               "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan",
                               "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")) %>% 
  mutate(continent = "Asia")

AllPop %>% filter(country == c("Albania", "Andorra", "Austria", 
                               "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                               "Croatia", "Czech Republic", "Denmark", "Estonia",
                               "Finland", "France", "Germany", "Greece",
                               "Hungary", "Iceland", "Ireland", "Italy", 
                               "Latvia", "Liechtenstein", "Lithuania", "Luxembourg",
                               "Macedonia", "Malta", "Moldova", "Montenegro", "Netherlands", "Norway",
                               "Poland", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
                               "Turkey", "Ukraine", "United Kingdom")) %>% 
  mutate(continent = "Europe")

AllPop %>% filter(country == c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                               "Ecuador", "French Guiana", "Guyana", "Paraguay", "Peru",
                               "Suriname", "Uruguay", "Venezuela")) %>% 
  mutate(continent = "South America")

AllPop %>% filter(country == c("Antigua and Barbuda", "The Bahamas", "Barbados", "Belize", 
                               "Canada", "Costa Rica", "Cuba", "Curacao", 
                               "Dominica", "Dominican Republic", "El Salvador",
                               "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica",
                               "Mexico", "Nicaragua", "Panama", "Puerto Rico", 
                               "Saint Lucia", "Trinidad and Tobago", "United States of America", "United States Virgin Islands")) %>% 
  mutate(continent = "North America")


AllPop %>% filter(country == c("Australia", "Micronesia", "Fiji", "Kiribati", "Marshall Islands",
                               "Nauru", "New Zealand", "Palau", "Papua New Guinea",
                               "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")) %>% 
  mutate(continent = "Oceania")




WorldPop <- popM %>% filter(country == "World") %>% select(-country_code)
View(WorldPop)

plot(WorldPop)
WorldPop <- popM %>% filter(country == "World") %>% select(-country_code, -country)
?gather()

WorldPop <- popM %>% 
  filter(country == "World") %>% 
  select(-country_code, -country) %>% 
  gather(`1950`:`2015`, key = "year", value = "population")


WorldPop %>% 
  ggplot(aes(x = year, y = population)) +     # simple plot...
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_y_continuous(breaks = pretty_breaks(n = 10))

WorldPop %>% 
  ggplot(aes(x = year, y = population)) +     # simple plot...
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  facet_grid(~ age)


WorldPop$age.div <- factor(WorldPop$age, levels = c('0-4', '5-9', '10-14', '15-19'))

WorldPop %>% 
  ggplot(aes(x = year, y = population)) +     # simple plot...
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  facet_grid(~ age.div)



# JAPAN:
JapanPop <- popM %>% 
  filter(country == "Japan") %>% 
  select(-country, -country_code) %>% 
  gather(`1950`:`2015`, key = "year", value = "population")

JapanPop %>% ggplot(aes(x = year, y = population)) +
  geom_bar(stat = "identity") +
  theme_economist() +
  scale_y_continuous(breaks = pretty_breaks(n = 10))



