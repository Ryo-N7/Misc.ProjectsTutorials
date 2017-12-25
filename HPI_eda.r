library(dplyr)
library(plotly)
library(stringr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(NbClust)

# Happy Planet Index:

# index of human well-being and environmental impact >> NEF think tank

# HPI = (LifeExpectancy * ExperiencedWellBeing) * InequalityOfOutcomes  / EcologicalFootprint
# Correlations among happiness, wealth, life expectancy, ecological footprint, etc.
# Cluster all 140 countries in dataset

library(xlsx)

HPI <- read.xlsx("~/R_materials/HPI-data-2016.xlsx", sheetIndex = 5, header = TRUE)

HPI <- HPI %>% slice(-c(1:5, 146:163))

HPI <- HPI %>% select(3:14)

str(HPI)

names(HPI)

names(HPI) <- c('country', 'region', 'life_expectancy', 'wellbeing', 'happy_years', 
                'footprint', 'inequality_outcomes', 'adj_life_expectancy', 'adj_wellbeing',
                'HPI_index', 'gdp', 'population')
names(HPI)

# fix data type:
glimpse(HPI)

summary(HPI)
# everything is as a factor >>> need to change to proper data type!
# leave out `country` and `region`, those should be turned into chr later instead of numeric
col <- c('HPI_index', 'life_expectancy', 'happy_years', 
         'footprint', 'gdp', 'inequality_outcomes', 'wellbeing', 
         'adj_life_expectancy', 'adj_wellbeing', 'population')

for(i in col) {
  
  HPI[, i] <- as.numeric(as.character(unlist(HPI[,i])))  # use unlist() !!

}


glimpse(HPI)
# change the two other ones to chr...
HPI$country <- as.character(HPI$country)
HPI$region <- as.character(HPI$region)

glimpse(HPI)

summary(HPI[-c(1:2)])

library(scales)
HPI %>% 
  ggplot(aes(gdp, life_expectancy)) +
  geom_point(aes(size = population, color = region, alpha = 0.6)) +
  scale_size_continuous(range = c(1, 13), breaks = pretty_breaks())

# add log10 transformation

HPI %>% 
  ggplot(aes(gdp, life_expectancy)) +
  geom_point(aes(size = population, color = region, alpha = 0.6)) +
  coord_trans(x = "log10") +
  geom_smooth(method = "loess") +
  scale_size_continuous(range = c(1, 13), breaks = pretty_breaks()) +
  theme_bw()

# can see a positive relationship...
cor.test(HPI$gdp, HPI$life_expectancy)
# 0.62 

HPI %>% 
  ggplot(aes(life_expectancy, HPI_index)) +
  geom_point(aes(size = population, color = region)) + 
  geom_smooth(method = "loess") +
  scale_size_continuous(breaks = pretty_breaks())

# many europe countries + highly devloped america/asia countries = medium HPI
# >>> due to carbon footprint weighing score down!

HPI %>% 
  ggplot(aes(gdp, HPI_index)) +
  geom_point(aes(size = population, color = region)) +
  coord_trans(x = "log10") +
  geom_smooth(method = "loess") +
  scale_size_continuous(breaks = pretty_breaks())

cor.test(HPI$gdp, HPI$HPI_index)
# 0.11    LOW correlation


# Clustering ####

# necessity for standardizing all the variables 
# mean = 0, sd = 1

HPI_standardized <- HPI
HPI_standardized <- HPI_standardized %>% select(-country, -region) %>% scale() %>% as.data.frame()
HPI_standardized <- HPI_standardized %>% bind_cols(HPI %>% select(country, region))

HPI_standardized %>% select(-country, -region) %>% summary()

# Correlation heatmap:

melt(cor(HPI[, 3:12], use = "p")) # p for probability

qplot(x = Var1, y = Var2, data = melt(cor(HPI[, 3:12], use = "p")), fill = value, geom = "tile") +
  scale_fill_gradient2(limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Heatmap of correlation matrix", 
       x = NULL, y = NULL)

library(tidyr)
HPI %>% 
  select(-country, -region) %>% 
  cor(use = "p") %>% 
  gather(key = Var1, value = life_expectancy:population)

qplot(x=Var1, y=Var2, data=melt(cor(HPI[, 3:12], use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Heatmap of Correlation Matrix", 
       x=NULL, y=NULL)

library(corrr)
HPI %>% 
  select(-country, -region) %>% 
  correlate() %>% 
  rplot()

# PCA ####

# explain largest amount of variance with smallest number of variables 
# >>> uncorrelated components

HPI_PCA <- HPI %>% select(-country, - region) %>% PCA(graph = FALSE)

HPI_PCA

eigen_values <- HPI_PCA$eig

head(eigen_values)
# Component 1: 66.67% of variance explained
# Component 2: 13.116% of variance explained

# Use scree plot to visualize the "eigen-values > 1" rule
fviz_screeplot(HPI_PCA, addlabels = TRUE, ylim = c(0, 65))

head(HPI_PCA$var$contrib)
# Component 1: spread evenly throughout life_exp, wellbeing, happyyears,footprint, ineq, etc.
# COmponent 2: largely from footprint (24.7%), then life_exp (2.29%)!

fviz_pca_var(HPI_PCA, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# NbClust for determining BEST # of clusters:

num_clust <- HPI %>% 
  select(-country, -region) %>% 
  NbClust(distance = "euclidean", min.nc = 1, max.nc = 15,
          method = "ward.D", index = "all")                  # duda insteald of all...

num_clust

NbClust(HPI[, 3:12], distance = "euclidean", min.nc = 2, max.nc = 15,
        method = "ward.D", index = "alllong",                  # duda insteald of all...
        alphaBeale = 0.1)


number <- NbClust(HPI[, 3:12], distance="euclidean",
                  min.nc=2, max.nc=15, method='ward.D', index = "dunn", alphaBeale = 0.1)
number
# dunn gives two


bc <- NbClust(HPI[, 3:12], distance="manhattan", 
              min.nc=2, max.nc=30, method="ward.D", index='all')






# partitioning around medoids >>> clustering into k-clusters (more robust than k-means)
set.seed(2017)
pam <- HPI %>% 
  select(-country, -region) %>% 
  pam(diss = FALSE, 3, keep.data = TRUE)

fviz_silhouette(pam)

HPI$country[pam$id.med]

fviz_cluster(pam, stand = FALSE, geom = "point", ellipse.type = "norm")


pam <- pam(HPI[, 3:12], diss=FALSE, 3, keep.data=TRUE)


HPI['cluster'] <- as.factor(pam$clustering)

map <- map_data("world")
map <- left_join(map, HPI, by = c('region' = 'country'))

ggplot() + 
  geom_polygon(data = map, 
               aes(x = long, y = lat, group = group, 
                   fill = cluster, color = cluster)) +
  labs(title = "Clustering Happy Planet Index", 
       subtitle = "Based on data from:http://happyplanetindex.org/", 
       x = NULL, y = NULL) + 
  theme_minimal()








