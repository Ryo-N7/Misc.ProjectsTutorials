library(ggplot2)
rm(list = ls())

df.facet_data <- read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/12/facet_dummy_data.csv"))

df.facet_data$month <- factor(df.facet_data$month, levels=month.abb)

ggplot(data=df.facet_data, aes(x=df.facet_data$month,y=sales, group=region)) +
  geom_line()
# Diffcult to interpret............

# Separate lines into different regions facets
ggplot(data=df.facet_data, aes(x=df.facet_data$month,y=sales, group=region)) +
  geom_line() +
  facet_grid(region ~ .) +
  geom_rect(aes(fill= region, size = "size"),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha = 0.000002, colour="black",show.legend = F) +
  theme(strip.text.x = element_text(face="bold", size=12)) +
  theme(strip.text.y = element_text(face="bold", size=12))
?geom_rect()
# use box drawn around each location to cleanly separate facets + suppress guide
# optional changes in strip
#+ theme(strip.text.x = element_text(face="bold", size=12, colour="white")) +
#  theme(strip.text.y = element_text(face="bold", size=12, color="white")) +
#  theme(strip.background = element_rect(fill="black"))


# Inside of the facet_grid() call, the "~" character means "by", and the "." character indicates 
# that we want to make a 1-dimensional grid (i.e., no column facet in our grid layout).
# So we can read the line of code facet_grid(region ~ .) as:
#   - create a small multiples chart
#   - with 1 small multiple chart for each region
#   - and lay out one small multiple chart on a separate row.

# Multiple charts as a ribbon: facet_wrap 
ggplot(data = df.facet_data, aes(x = region,y = sales)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ month, ncol = 6) +
  ggtitle("Small Multiples in R") +
  theme(plot.title = element_text(family="Arial", face="bold", 
                                  size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90), axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  labs(x = "Region", y = "Sales") +
  geom_rect(aes(fill = region, size = "size"),xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, alpha = 0.002, colour = "red",show.legend = F)


# Example with 'diamonds' dataset
ggplot(data=diamonds, aes(x=price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 5)


??geom_rect()


library(reshape2)

tips %>% head()

sp <- tips %>% ggplot(aes(total_bill, tip/total_bill)) + geom_point(shape = 4)
sp

sp + facet_grid(sex ~ .)     # horizontal

sp + facet_grid(. ~ sex)     # vertical

sp + facet_grid(sex ~ day)
sp + facet_grid(day ~ sex)

sp + facet_wrap(~ day, ncol = 2)

sp + facet_grid(sex ~ day) +
  theme(strip.text.x = element_text(size = 10, angle = 45),
        strip.text.y = element_text(size = 5, angle = 150),
        strip.background = element_rect(color = "blue", fill = "red"))

diamonds %>% head()

df_d <- diamonds %>% filter(cut == "Premium") %>% select(color, carat, price, clarity)

df_d %>% ggplot(aes(x = color, y = price, group = clarity)) + geom_point(aes(color = clarity), position = "jitter") + facet_wrap( ~ clarity)


































