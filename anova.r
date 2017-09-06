# ANOVA 
plant.df <- PlantGrowth
plant.df$group <- factor(plant.df$group, 
                   labels = c("Control", "Treatment 1", "Treatment 2"))

library(ggplot2)

ggplot(plant.df, aes(x = group, y = weight)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + 
  xlab("Treatment Group") +
  ylab("Dried weight of plants")

# one-way ANOVA 
plant.mod1 = lm(weight ~ group, data = plant.df)
summary(plant.mod1)

anova(plant.mod1)
confint(plant.mod1)

plant.mod = data.frame(Fitted = fitted(plant.mod1),
                       Residuals = resid(plant.mod1), 
                       Treatment = plant.df$group)

ggplot(plant.mod, aes(Fitted, Residuals, colour = Treatment)) + 
  geom_point()



# t.test
library(tidyverse)

data(chickwts)
chick_tibble <- as_tibble(chickwts)

casein <- subset(chickwts, feed == "casein")

sunflower <- subset(chick_tibble, feed == "sunflower")

t.test(sunflower$weight, casein$weight) ## this works
t.test(as.data.frame(sunflower[, 1]), as.data.frame(casein[, 1])) ## this works too
t.test(sunflower[, 1], casein[, 1]) ## this doesn't



