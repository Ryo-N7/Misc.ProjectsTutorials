# Likert scales

# Set up data:
set.seed(1234)
library(e1071)

probs <- cbind(c(.4,.2/3,.2/3,.2/3,.4),c(.1/4,.1/4,.9,.1/4,.1/4),c(.2,.2,.2,.2,.2))
my.n <- 100

my.len <- ncol(probs)*my.n
raw. <- matrix(NA, nrow = my.len, ncol = 2)
raw <- NULL

for(i in 1:ncol(probs)){
  raw <- rbind(raw, cbind(i,rdiscrete(my.n,probs=probs[,i],values=1:5)))
}
raw <- data.frame(raw)
names(raw) <- c("group","value")
raw$group <- as.factor(raw$group)
raw.1.2 <- subset(raw, raw$group %in% c(1,2))

# T.Test

# Assumptions:
# Z follows standard normal distribution
# Variance follows Chi-Sq. distribution
# Variance from two populations should be equal
# Two populations = INDEPENDENT
t.test(raw.1.2$value ~ raw.1.2$group, var.equal=TRUE)
# p = 0.01114, significant. Mean of two populations is signif. different.



# Chi-Square Test:
# Handling categorical frequency data & test association between 2 variables.
# IF sample size = small, use Fisher's Exact Test.

(c.test <- chisq.test(raw$group, raw$value))
# small cells... chi-sq NOT work? Try Fisher's Exact...

sim.table <- table(raw$group, raw$value)
sim.table
fisher.test(sim.table, simulate.p.value = TRUE, B = 1e6)  # B: # of replicates simulated MC
?fisher.test


# Wilcox Signed Test:
# For when related sample and from same population. --- Matched Pairs sample
wilcox.test(raw.1.2$value[raw.1.2$group == 1], raw.1.2$value[raw.1.2$group == 2], paired = TRUE)

# p-value: 0.02067
# H(1): true location shift NOT = 0.

# Mann-Whitney Test:
# for test 2 independent samples are same.
# same as above without paired sample specification.

# Kruskal-Wallis Test:
# Analysis of Variance for categorical data.
# Assumption: independent populations.
kruskal.test(raw$value ~ raw$group)
# p-value = 0.0009536, Chi-sq: 13.91, df = 2.

# Comparison with ANOVA
fit <- lm(raw$value ~ raw$group)
anova(fit)
# p-value 0.01878, df(2,297), Sum Squares: 14.91.


























