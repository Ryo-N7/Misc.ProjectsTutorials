# Web-scraping wikipedia tables. 
# USA Obesity data.


library(rvest)
library(ggplot2)
library(scales)
library(ggthemes)
library(tidyverse)


obesity <- read_html("https://en.wikipedia.org/wiki/Obesity_in_the_United_States")

obesitytable <- obesity %>% 
              html_nodes(".wikitable") %>% 
              .[[1]] %>% 
              html_table(fill = TRUE)

obesitytable2 <- obesity %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% 
  .[[1]] %>% 
  html_table(fill = TRUE)

str(obesitytable)
str(obesitytable2)

head(obesitytable)
head(obesitytable2)

# need to convert percentage values from chr to numeric and take out '%'.

obesitytable[, 2] <-  as.numeric(gsub("%", "", obesitytable[, 2]))

obesitytable[, 3] <- as.numeric(gsub("%", "", obesitytable[,3]))

obesitytable[, 4] <- gsub("%", "", obesitytable[,4]) %>% as.numeric()

str(obesitytable)



# OR USE CUSTOM FUNCTION:
for(i in 2:4){
  obesitytable2[,i] = gsub("%", "", obesitytable2[,i])
  obesitytable2[,i] = as.numeric(obesitytable2[,i])
}

str(obesitytable2)

# fix names of columns using make.names():
names(obesitytable)
?make.names()

names(obesitytable) <- make.names(names(obesitytable))
names(obesitytable)  # spaces removed, sytntactically valid!

# Prep map data for USA:

?map_data()

states <- map_data("state")
# Create region variable within obesitytable:
obesitytable$region <- tolower(obesitytable$State.and.District.of.Columbia)

states <- merge(states, obesitytable, by = "region", all.x = T)
str(states)


statenames = states %>% 
  group_by(region) %>%
  summarise(
    long = mean(range(long)), 
    lat = mean(range(lat)), 
    group = mean(group), 
    Obese.adults = mean(Obese.adults), 
    Obese.children.and.adolescents = mean(Obese.children.and.adolescents)
  )

# top 10 obese states (adults)
topstate = states %>% 
  group_by(region) %>%
  summarise(
    Obese.adults = mean(Obese.adults))%>%
  arrange(desc(Obese.adults)) %>%
  top_n(10)

topstateKIDS <- states %>% 
  group_by(region) %>% 
  summarise(
    Obese.KIDS = mean(Obese.children.and.adolescents)) %>% 
  arrange(desc(Obese.KIDS)) %>% 
  top_n(10)

# Adults
ggplot(aes(x = reorder(region,Obese.adults), y = Obese.adults),data = topstate) + 
  geom_col(color = "black", fill = "#1EDBC2", alpha = 0.6) +
  labs(y = "Percentage of Obese Adults", x = "Top 10 States") +
  coord_flip()

# Kids
topstateKIDS %>% 
  ggplot(aes(reorder(region, Obese.KIDS), Obese.KIDS)) + 
  geom_col(color = "blue", fill = "#FCCB85", alpha = 0.8) +
  labs(x = "Top 10 States", y = "Percentage of Obese Kids") +
  coord_flip()


# On the MAP:
states %>% ggplot(aes(long, lat, group = group, fill = Obese.adults)) +
  geom_polygon(color = "#6EE543", show.legend = TRUE) +
  scale_fill_gradient(name = "Percent", low = "#FAB8D2", high = "#F91C74",
                      guide = "colorbar", na.value = "black",
                      breaks = pretty_breaks(n = 5)) +
  labs(title = "Obesity in Adults",
       x = "Longitude", y = "Latitude") +
  geom_text(data=statenames,    # add state names to map!
            aes(x = long, y = lat, label = region), size = 3.5,
            fontface = "italic") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())



states %>% ggplot(aes(long , lat, group = group, fill = Obese.children.and.adolescents)) + 
  geom_polygon(color = "white", show.legend = T) +
  scale_fill_gradient(name = "Percent", low = "#132B43", high = "#56B1F7",
                      guide = "colorbar", na.value = "green", 
                      breaks = pretty_breaks(n = 5)) +
  labs(title = "Obesity in Kids") +
  geom_text(data = statenames, 
            aes(x = long, y = lat, label = region), size = 3,
            fontface = "bold") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())



