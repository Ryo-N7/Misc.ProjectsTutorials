rm(list = ls ())
dim(iris)
names(iris)
str(iris)
attributes(iris)
head(iris)
# values of single column
iris[15:26, "Sepal.Width"]

summary(iris)
?quantile
quantile(iris$Sepal.Length)
var(iris$Sepal.Length)
library(ggplot2)
qplot(Sepal.Length, data = iris, geom = "histogram")
qplot(Sepal.Length, data = iris, geom = "histogram", binwidth = 0.1)
qplot(Sepal.Length, data = iris, geom = "histogram", color = I("blue"))
qplot(Sepal.Length, data = iris, geom = "histogram", color = I("red"), bins = 8)
qplot(Sepal.Length, data = iris, geom = "histogram", color = I("green"), binwidth = 0.25)
hist(iris$Sepal.Length)
qplot(Sepal.Length, data = iris, geom = "histogram", color = I("grey"), bins = 8)

qplot(Sepal.Length, data = iris, geom = "density")
plot(density(iris$Sepal.Length))
table(iris$Species)
pie(table(iris$Species))
barplot((table(iris$Species)))
qplot(Species, data = iris, geom = "bar")
qplot(Sepal.Length, data = iris, geom = "bar")

cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[, 1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
?cov
cor(iris[, 1:4])

# Sepal.Length for EACH species >>> aggregate()
aggregate(Sepal.Length ~ Species, summary, data = iris)
?aggregate
aggregate(Sepal.Length ~ Species, mean, data = iris)
# Boxplot aggregated data
boxplot(Sepal.Length ~ Species, data = iris)
qplot(Species, Sepal.Length, data = iris, geom = "boxplot")

# Scatterplot
# with() = no need 'dataset'$ for variable naming.
with(iris, plot(Sepal.Length, Sepal.Width, col = Species, pch = as.numeric(Species)))
qplot(Sepal.Length, Sepal.Width, data = iris, color = Species, shape = Species,
      xlab = "Length of Sepal", ylab = "Width of Sepal")

library(ggthemes)
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(color = Species, shape = Species)) +
  ylab("Width Of Sepal") + xlab("Length Of Sepal") + theme_economist() +
  ggtitle("LOL WTF IS A SEPAL ANYWAY??")

# Jitter 
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))
?jitter
# Matrix of scatterplots
pairs(iris)


install.packages("scatterplot3d")
install.packages("rgl")
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)
library(rgl)
plot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

distMatrix <- as.matrix(dist(iris[, 1:4]))
heatmap(distMatrix)
library(lattice)
levelplot(Petal.Width ~ Sepal.Length*Sepal.Width, iris, cuts = 9, 
          col.regions = rainbow(10)[10:1])
?levelplot

# Facets, split based on Species
qplot(Sepal.Length, Sepal.Width, data = iris, facets = Species ~ .)

pdf("mysepalplot.pdf")
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(color = Species, shape = Species)) +
  ylab("Width Of Sepal") + xlab("Length Of Sepal") + theme_economist() +
  ggtitle("LOLWTF IS A SEPAL ANYWAY??") +
  facet_wrap(~Species, nrow = 3)
# save as pdf.

graphics.off()
# Ex. 2
pdf("plotplot.pdf")
x <- 1:50
plot(x, log(x))
graphics.off()


# Decision trees and Random Forest
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainData <- iris[ind == 1,]
testData <- iris[ind == 2,]
install.packages("party")
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = trainData)
# Prediction
table(predict(iris_ctree), trainData$Species)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type = "simple")
# Test tree with test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)
