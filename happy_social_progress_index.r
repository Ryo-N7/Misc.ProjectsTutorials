library(rvest)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(useful)


# World Happiness Report (2017)
# Social Progres Index (2015)
# join by "Country" column
# >>> CPA pre-processing, then clustering
# similarities in countries for grouping?

# Import WHR (2017)
url_1 <- "https://en.wikipedia.org/wiki/World_Happiness_Report"
happy <- read_html(url_1) %>% 
  html_nodes("table") %>% 
  extract2(1) %>% 
  html_table(fill = TRUE)

str(happy)

# Exclude columns with rank and scores
happy <- happy[c(3, 6:11)]

colnames(happy) <- gsub(" ", "_", colnames(happy), perl = TRUE)

names(happy)

happy$Country <- as.character(plyr::mapvalues(happy$Country, 
                                     from = c("United States", "Congo (Kinshasa)", 
                                                 "Congo (Brazzaville)", "Trinidad and Tobago"), 
                                     to = c("USA","Democratic Republic of the Congo", 
                                               "Democratic Republic of the Congo", "Trinidad")))

glimpse(happy)

happy[, 2:7] <- happy[, 2:7] %>% 
  lapply(., as.numeric)

glimpse(happy)

# Import SPI (2015)
url_2 <- "https://en.wikipedia.org/wiki/List_of_countries_by_Social_Progress_Index"

social <- read_html(url_2) %>% 
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_table(fill = TRUE)

glimpse(social)

social <- social %>% select(Country, `Basic Human Needs`, 
                            `Foundations of Well-being`, Opportunity)

glimpse(social)

names(social) <- c("Country", "Basic_human_needs", "Foundations_well_being", "Opportunity")

glimpse(social)

social$Country <- as.character(plyr::mapvalues(social$Country, 
                                         from = c("United States", "Côte d'Ivoire",
                                                  "Democratic Republic of Congo", 
                                                  "Congo", 
                                                  "Trinidad and Tobago"),
                                         to=c("USA", "Ivory Cost",
                                              "Democratic Republic of the Congo", 
                                              "Democratic Republic of the Congo", 
                                              "Trinidad")))

glimpse(social)

social[, 2:4] <- social[, 2:4] %>% 
  lapply(., as.numeric)

glimpse(social)

# Join two indexes

social_happy_index <- left_join(happy, social, by = c("Country" = "Country"))

glimpse(social_happy_index)

mean(is.na(social_happy_index[, 2:10]))
# NA values from joining data set.

summary(social_happy_index %>% select(-Country))

# Data imputation algorithms for populating missing values:
# Use: median imputation strategy >>> replace NA with median of each variable column

for(i in 1:ncol(social_happy_index[, 2:10])) {
  social_happy_index[, 2:10][is.na(social_happy_index[, 2:10][,i]), i] <- median(
                                                              social_happy_index[, 2:10][,i], 
                                                              na.rm = TRUE)
}

summary(social_happy_index %>% select(-Country))
# no more NAs! replaced by median values of each column

# Standardize!

## Scaling variables to a mean of 0 and standard deviation of 1
sd_scale <- function(x) {
  (x - mean(x))/sd(x)
}

# or just use scale() function:

social_happy_index[, 2:10] <- social_happy_index %>% 
  select(-Country) %>% 
  scale() %>% 
  as_tibble() 

summary(social_happy_index %>% select(-Country))

# Simple correlation analysis:

corr <- cor(social_happy_index %>% select(-Country), method = "pearson")
corr

# correlation matrix 
corrplot::corrplot.mixed(corr, upper = "ellipse", tl.pos = "lt")

# heatmap correlation matrix
ggplot(melt(corr, varnames = c("x", "y"), value.name = "correlation"), 
       aes(x = x, y = y)) +
  geom_tile(aes(fill = correlation)) +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red",
                       guide=guide_colorbar(ticks = FALSE, barheight = 5),
                       limits = c(-1,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Heatmap of Correlation Matrix", 
       x = NULL, y = NULL)

# Principal Component Analysis:

social_happy_index_PCA <- social_happy_index %>% 
                            select(-Country) %>% 
                            PCA(graph = FALSE)
  
social_happy_index_PCA

fviz_screeplot(social_happy_index_PCA, addlabels = TRUE, ylim = c(0, 65))
# first component explains 56.3% of the total variance in the data!
# second component explains ~16% of the total variance in the data!
# choose number of components using the eigenvalues >= 1 rule

social_happy_index_PCA$eig
# Component 1: eigen = 5.0699
# Component 2: eigen = 1.4359

# Contribution of the chosen principal components:
social_happy_index_PCA$var$contrib[, 1:2]
# Component 1: represents GPD/capita, Healthy_life_exp, Opportunity, Foundations, Basic_H_N
# Component 2: represents social support, generosity

# Applied clustering:
library(NbClust)

cluster <- social_happy_index %>% select(-Country) %>% NbClust(distance = "manhattan",
                                           min.nc = 2, max.nc = 30,
                                           method = "ward.D", index = "all")
# Of all the indices, 9 proposed 3 clusters!
# Best number of clusters: THREE!

# Apply 3 clusters to PAM
set.seed(4653)

pam_k3 <- social_happy_index %>% 
  select(-Country) %>% 
  pam(diss = FALSE, k = 3, keep.data = TRUE)

fviz_silhouette(pam_k3)
# number of countries assigned to EACH cluster:
# C1: 28
# C2: 85
# C3: 46

# Most typical country of each cluster:
social_happy_index$Country[pam_k3$id.med]
# C1: Germany
# C2: World              <<<< need to remove...
# C3: Burkina Faso

# append cluster labels to the combined dataset:
social_happy_index$cluster <- as.factor(pam_k3$clustering)

social_happy_index['cluster'] <- as.factor(pam_k3$clustering)

glimpse(social_happy_index)

# Visualize the clusters!

fviz_pca_ind(social_happy_index_PCA,
        label = "none",
        habillage = social_happy_index$cluster,
        palete = "jco",
        addEllipses = TRUE)

# Visualize clusters on World Map!
map_world <- map_data("world")

map_world_index <- left_join(map_world, social_happy_index, by = c("region" = "Country"))

glimpse(map_world_index)

ggplot() +
  geom_polygon(data = map_world_index, 
               aes(x = long, y = lat, group = group, 
                   fill = cluster, color = cluster)) +
  labs(title = "Applied Clustering World Happiness and Social Progress Index",
       subtitle = "Based on data from:https://en.wikipedia.org/wiki/World_Happiness_Report and\n 
       https://en.wikipedia.org/wiki/List_of_countries_by_Social_Progress_Index", 
       x = NULL, y = NULL) +
  coord_equal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())


























































































































