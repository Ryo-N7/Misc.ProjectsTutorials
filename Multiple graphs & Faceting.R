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
  facet_grid(region ~ .)
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
  labs(x = "Region", y = "Sales")

# Example with 'diamonds' dataset
ggplot(data=diamonds, aes(x=price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 5)




