# Measuring associations between categorical variables:

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

# Import data from UCI ML repo
file.url<- "http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"

# Explicitly adding the column headers from the data dictionary
mushroom.data<- read.csv(file = file.url, header = FALSE, sep = ",",strip.white = TRUE,
                           stringsAsFactors = TRUE, 
                           col.names = c("class","cap-shape","cap-surface","cap-color","bruises",
                                         "odor","gill-attachment","gill-spacing","gill-size",
                                         "gill-color","stalk-shape","stalk-root","stalk-surface-above-ring",
                                         "stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring",
                                         "veil-type","veil-color","ring-number","ring-type","spore-print-color",
                                         "population","habitat"))

# summarize dataset:
mushroom.data.levels <- cbind.data.frame(Variable = names(mushroom.data), 
                                         Total_Levels = sapply(mushroom.data, function(x) {as.numeric(length(levels(x)))}))
print(mushroom.data.levels)

# Recode levels with meaningful names via data dictionary (online):
# ex. Class: from 'e' and 'p' to 'edible' and 'poisonous'....etc.
levels(mushroom.data$class) <- c("edible", "poisonous")
levels(mushroom.data$cap.shape)<-c("bell","conical","flat","knobbed","sunken","convex") 
levels(mushroom.data$cap.surface)<- c("fibrous","grooves","smooth","scaly")
levels(mushroom.data$cap.color)<- c("buff","cinnamon","red","gray","brown","pink","green","purple","white","yellow")
levels(mushroom.data$bruises)<- c("bruisesno","bruisesyes")
levels(mushroom.data$odor)<-c("almond","creosote","foul","anise","musty","nosmell","pungent","spicy","fishy")
levels(mushroom.data$gill.attachment)<- c("attached","free")
levels(mushroom.data$gill.spacing)<- c("close","crowded")
levels(mushroom.data$gill.size)<-c("broad","narrow")
levels(mushroom.data$gill.color)<- c("buff","red","gray","chocolate","black","brown","orange","pink","green","purple","white","yellow")
levels(mushroom.data$stalk.shape)<- c("enlarging","tapering")

# table(mushroom.data$stalk.root) # has a missing level coded as ?
# ?    b    c    e    r 
# 2480 3776  556 1120  192 

levels(mushroom.data$stalk.root)<- c("missing","bulbous","club","equal","rooted")
levels(mushroom.data$stalk.surface.above.ring)<-c("fibrous","silky","smooth","scaly")
levels(mushroom.data$stalk.surface.below.ring)<-c("fibrous","silky","smooth","scaly")
levels(mushroom.data$stalk.color.above.ring)<- c("buff","cinnamon","red","gray","brown",                "orange","pink","white","yellow")
levels(mushroom.data$stalk.color.below.ring)<- c("buff","cinnamon","red","gray","brown",      "orange","pink","white","yellow")
levels(mushroom.data$veil.type)<-c("partial")
levels(mushroom.data$veil.color)<- c("brown","orange","white","yellow")
levels(mushroom.data$ring.number)<-c("none","one","two")
levels(mushroom.data$ring.type)<- c("evanescent","flaring","large","none","pendant")
levels(mushroom.data$spore.print.color)<- c("buff","chocolate","black","brown","orange","green","purple","white","yellow")
levels(mushroom.data$population)<- c("abundant","clustered","numerous","scattered","several","solitary")
levels(mushroom.data$habitat)<-c("woods","grasses","leaves","meadows","paths","urban","waste")

# Data visualization:
# Relationship between 'cap-surface' and 'cap-shape' of mushroom?
p <- mushroom.data %>% ggplot(aes(cap.shape, cap.surface, color = class))
p + geom_jitter(alpha = 0.3) + scale_color_manual(breaks = c('edible', 'poisonous'), values = c('darkgreen', 'red'))

# most cap.shape(flat) + cap.surface(scaly,smooth, fibroous) = poisonous!
# most cap.shape(bell, knob, sunken) + cap.shape(fibrous, smooth, scaly) = edible!

# Relationshpi between 'habitat' and 'population' of mushroom?
p <- mushroom.data %>% ggplot(aes(habitat, population, color = class))
p + geom_jitter(alpha = 0.3) + scale_color_manual(breaks = c('edible', 'poisonous'), values = c('green', 'darkred'))

# ALL habitat(woods) + population(scattered, clustered) = poisonous!
# ALL habitat(grasses) + population(abundant, numerous) = edible!

# Relationship between 'living condition' and 'color' of mushroom?
p <- mushroom.data %>% ggplot(aes(habitat, odor, color = class))
p + geom_jitter(alpha = 0.3) + scale_color_manual(breaks = c('edible', 'poisonous'), values = c('darkgreen', 'red'))

# ALL habitat(paths, urban, waste) + odor(no smell) = edible!
# ALL habitat(woods, leaves, paths) + odor(fishy, spicy) = poisonous!
# ALL odor(foul) + any habitat = POISONOUS!!!! #dangar

# Correlation detection for categorical predictors
# Levels of factors = unordered >>> 'nominal variables'
# Ordered factors >>> 'ordinal variables'
# "Interval > Ordinal > Nominal, statistical methods for one variable type usage with variable type of HIGHER levels but NOT lower levels."
# Cheatsheet: https://stats.idre.ucla.edu/other/mult-pkg/whatstat/
# Categorical variables: correlation as significance test + effect size (strength of association)
# ex. Pearson's Chi-Squared, test statistical dependence between "X" and "Y". H(0): X, Y = independent, H(1): X, Y = dependent.

chisq.test(mushroom.data$cap.shape, mushroom.data$cap.surface, correct = F)    # alpha = 0.05
# p < 2.2e^-16, REJECT NULL, cap.shape + cap.surface = DEPENDENT

chisq.test(mushroom.data$habitat, mushroom.data$odor, correct = F)    # alpha - 0.05
# p < 2.2e^-16, REJECT NULL, habitat + odor = DEPENDENT

# Effect size: strength of association relationship
# Goodman & Kruskal's tau for 'nominal categorical predictors':
install.packages("GoodmanKruskal")
library(GoodmanKruskal)

varset1 <- c("cap.shape", "cap.surface", "habitat", "odor", "class")
mushroomFrame1 <- subset(mushroom.data, select = varset1)

GKmatrix1 <- GKtauDataframe(mushroomFrame1)
plot(GKmatrix1, corrColors = "red")

# K = unique levels for each variable, Numerical values = association measure of X, Y.
# odor - class = 0.94! Interpretation: odor is highly predictive of class ('edible' or 'poisonous'), association = STRONG.
# class - odor = 0.34. Interpretation: class of mushroom highly predictive of odor, association = strong.

# From chi.sq, cap.shape + cap.surface = dependent.
# both forward and reverse association is weak (cap.shape - cap.surface = 0.03, cap.surface - cap.shape = 0.01)
# significant dependency but difficultly in predicting one from other variable.















