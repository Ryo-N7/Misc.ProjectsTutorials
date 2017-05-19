rm(list = ls())
training.data.raw <- read.csv("~/R materials/train.csv")
?sapply
sapply(training.data.raw, function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
install.packages("Amelia")
# View missing data using 'Amelia' missmap()
library(Amelia)
missmap(training.data.raw, main = "Missing values vs. Observed values")
# Cabin = too many missing, tickets = irrelevant, PassengerID = irrelevant
data <- subset(training.data.raw, select = c(2,3,5,6,7,8,10,12))
# Replace missing NA data with avg/median/mode values. 
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)
?na.rm
??na.rm
# Categorical variables = factors? Check.
is.factor(data$Sex)
is.factor(data$Embarked)
# How categorical variables contrast coded? Check.
contrasts(data$Sex)
contrasts(data$Embarked)

# Discard missing data in Embarked
data <- data[!is.na(data$Embarked), ]
rownames(data) <- NULL

# Split into Training set + Testing set. 
train <- data[1:800,]
test <- data[801:889,]

model <- glm(Survived ~ . , family = binomial(link = 'logit'), data = train)
summary(model)
# Male = reduce log odds of Survival by 2.75, +Age = reduce log odds of Survival by 0.037 

# Analyze variance (ANOVA)
anova(model, test = "Chisq")
# Difference between Null Deviance and Residual Deviance = whether current model improves
# null model (only intercept), ^wide gap = better performance of current model > null model
# Assess model fit through McFadden R^2
install.packages("pscl")
library(pscl)
pR2(model)
# McFadden = 0.335169

# Predictive ability of model: 
# Output of probabilities as P(y = 1|X). Decision boundary = 0.5
# IF P(y = 1|X) > 0.5, y = 1, ELSE y = 0. 
fitted.results <- predict(model, newdata = subset(test, select = c(2,3,4,5,6,7,8)), 
                          type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy', 1-misClasificError))
# Model accuracy = 84.269%

# K-fold crossvalidation... 

# Performance measurements for binary classifiers: ROC curve, calculate AUC
# ROC: curve from plot true positive rate (TPR) against false positive rate (FPR) at 
# various threshold settings 
# AUC: area under ROC curve
# Models with good predictive ability: AUC closer to 1 vs. 0.5
install.packages("ROCR")
library(ROCR)
p <- predict(model, newdata = subset(test, select = c(2,3,4,5,6,7,8)), type = "response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



# Binomial logistical regression

dat <- read.csv("~/R materials/train.csv", header = TRUE, sep = ",", na.strings = c(" "))
sapply(dat, function(x) {sum(is.na(x))})
dat[is.na(dat$Age),][6]<- mean(dat$Age,na.rm = TRUE)
dat <- dat[,-11]

set.seed(30)
indices<- rnorm(nrow(dat))>0
traindat<- dat[indices,]
testdat<-dat[!indices,]
dim(traindat)
dim(testdat)
?set.seed

str(testdat$Pclass)
testdat$Pclass<- factor(testdat$Pclass)
testdat$SibSp <- factor(testdat$SibSp)
testdat$Parch <- factor(testdat$Parch)
testdat$Survived<-factor(testdat$Survived)
traindat$Pclass<- factor(traindat$Pclass)
traindat$SibSp <- factor(traindat$SibSp)
traindat$Parch <- factor(traindat$Parch)
traindat$Survived<-factor(traindat$Survived)

mod <- glm(Survived ~ Pclass + Sex + Age + SibSp+ Parch + Embarked + Fare ,
           family=binomial(link='logit'),data=traindat)

require(car)
Anova(mod)
# Significance found for Pclass, Sex, Age, Sibsp

mod2 <- glm(Survived ~ Pclass + Sex + Age + SibSp ,
            family=binomial(link='logit'),data=traindat)
Anova(mod2)

plot(mod2)


?histogram
histogram(Pclass, mod2)
library(ggplot2)
ggplot(dat, aes(x = Pclass, y = Pclass, group = Pclass)) + 
  geom_boxplot() + geom_point()

