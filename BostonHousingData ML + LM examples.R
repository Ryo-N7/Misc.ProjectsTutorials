# Regression with Boston Dataset:
library(MASS)
library(tidyverse)
library(corrplot)


str(Boston)
head(Boston)

hist(medv, data = Boston)

qqnorm(Boston$medv)
qqline(Boston$medv) 
# right skew/positive tail

corrplot(Boston)
cor(Boston)

Model.1A <- lm(medv ~ ., data = Boston)
summary(Model.1A)












# ML and LM with Boston housing data:
install.packages("ISLR")
install.packages("car")

library(ISLR)
library(car)
library(MASS)

fix(Boston)
str(Boston)
# Target: medv (median house value) using 13 predictors.

# predict with lstat (% lower status of the population)
lm.fit <- lm(medv ~ lstat, data = Boston)

lm.fit  # intercept: 34.55    slope: -0.95
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit) # intercept: 33.448 ~ 35.65922      slope: -1.026 ~ -0.8739

# predict() function to produce confidence intervals + prediction intervals for medv for value of lstat:
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")



















