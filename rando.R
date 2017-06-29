attitude
Orange
coplot(circumference ~ age | Tree, data = Orange, show.given = FALSE)
fm1 <- nls(circumference ~ SSlogis(age, Asym, xmid, scal),
           data = Orange, subset = Tree == 3)
plot(circumference ~ age, data = Orange, subset = Tree == 3,
     xlab = "Tree age (days since 1968/12/31)",
     ylab = "Tree circumference (mm)", las = 1,
     main = "Orange tree data and fitted model (Tree 3 only)")

age <- seq(0, 1600, length.out = 101)
lines(age, predict(fm1, list(age = age)))


BJsales
BJsales.lead
str(BJsales)

ability.cov
require(stats)
(ability.FA <- factanal(factors = 1, covmat = ability.cov))
update(ability.FA, factors = 2)
## The signs of factors and hence the signs of correlations are
## arbitrary with promax rotation.
update(ability.FA, factors = 2, rotation = "promax")

women
plot(women, xlab = "Height (in)", ylab = "Weight (lb)",
     main = "women data: American women aged 30-39")


install.packages("devtools")
library(devtools)
install_github("Displayr/flipChartBasics")
install_github("Displayr/flipData")
install_github("Displayr/flipDimensionReduction")
install_github("Displayr/flipExampleData")
install_github("Displayr/flipFormat")
install_github("Displayr/flipImputation")
install_github("Displayr/flipPlots")
install_github("Displayr/flipStatistics")
install_github("Displayr/flipTransformations")
install.packages("GPArotation")
install.packages("plotly")
install.packages("psych")
install_github("Displayr/rhtmlLabeledScatter")
install_github("Displayr/rhtmlMoonPlot")

data(csd.perceptions, package = "flipExampleData")
csd = csd.perceptions * 100

library(flipDimensionReduction)
CorrespondenceAnalysis(csd,
                       output = "Scatterplot",
                       logos = c("https://vignette3.wikia.nocookie.net/gundam/images/5/52/Katejina_Loos.jpg",
                                 "http://docs.displayr.com/images/7/7c/V.jpg",
                                 "http://docs.displayr.com/images/8/82/RedBull.jpg", 
                                 "http://docs.displayr.com/images/d/dc/LifePlus.jpg",
                                 "http://docs.displayr.com/images/0/09/DietCoke.png",
                                 "http://docs.displayr.com/images/d/da/Fanta.jpg",
                                 "http://docs.displayr.com/images/e/e8/Lift.png",
                                 "http://docs.displayr.com/images/5/5e/Pepsi.jpg"))


