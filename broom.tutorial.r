# BROOM tutorial:

fit <- lm(mpg ~ wt + qsec, mtcars)
summary(fit)
library(dplyr)
library(broom)
library(ggplot2)

tidy(fit)
#          term  estimate   std.error  statistic      p.value
# 1 (Intercept) 19.746223   5.2520617   3.759709  7.650466e-04
# 2          wt -5.047982   0.4839974 -10.429771  2.518948e-11
# 3        qsec  0.929198   0.2650173   3.506179  1.499883e-03

# rownames as under 'term' column
# columns renamed for ease of use

# Per-observation information (fitted values, residuals, etc...):
augment(fit)
augment(fit) %>% head()

#          .rownames  mpg   wt  qsec  .fitted   .se.fit      .resid       .hat   .sigma      .cooksd  .std.resid
# 1         Mazda RX4 21.0 2.620 16.46 21.81511 0.6832424 -0.81510855 0.06925986 2.637300 2.627038e-03 -0.32543724
# 2     Mazda RX4 Wag 21.0 2.875 17.02 21.04822 0.5468271 -0.04822401 0.04436414 2.642112 5.587076e-06 -0.01900129
# 3        Datsun 710 22.8 2.320 18.61 25.32728 0.6397681 -2.52727880 0.06072636 2.595763 2.174253e-02 -1.00443793
# 4    Hornet 4 Drive 21.4 3.215 19.44 21.58057 0.6231436 -0.18056924 0.05761138 2.641895 1.046036e-04 -0.07164647
# 5 Hornet Sportabout 18.7 3.440 17.02 18.19611 0.5120709  0.50388581 0.03890382 2.640343 5.288512e-04  0.19797699
# 6           Valiant 18.1 3.460 20.22 21.06859 0.8032106 -2.96858808 0.09571739 2.575422 5.101445e-02 -1.20244126


# Per-model statistics (R-squared, AIC, BIC....):
glance(fit)

# r.squared     adj.r.squared    sigma    statistic      p.value     df    logLik      AIC      BIC       deviance   df.residual
# 1 0.8264161     0.8144448      2.596175  69.03311     9.394765e-12  3   -74.36025   156.7205  162.5834   195.4636          29

# construct coefficient plots with ggplot2:
td <- tidy(fit, conf.int = T)
ggplot(td, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0)

# Usage with group_by() and do() for within-groups regressions
# Ex. within automatic cars, manual cars:
mtcars %>% group_by(am) %>% do(tidy(lm(mpg ~ wt, .)))
# Ex. within different cylinders:
mtcars %>% group_by(cyl) %>% do(tidy(lm(mpg ~ wt, .)))

# Ex. LASSO regression
set.seed(03-19-2015)
# geenrate data with 5 real variables and 45 null, on 100 obsv.:
nobs <- 100
nvar <- 50
real <- 5
x <- matrix(rnorm(nobs * nvar), nobs)
beta <- c(rnorm(real, 0, 1), rep(0, nvar - real))
y <- c(t(beta) %*% t(x)) + rnorm(nvar, sd =3)

glmnet_fit <- cv.glmnet(x,y)
# tidy with broom and plot with ggplot2:
tidied_cv <- tidy(glmnet_fit)
glance_cv <- glance(glmnet_fit)

tidied_cv %>% ggplot(aes(lambda, estimate)) +
  geom_line(color = "darkred") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_x_log10() +
  geom_vline(xintercept = glance_cv$lambda.min) +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2)


# Survival analysis:
surv_fit <- survfit(coxph(Surv(time, status) ~ age + sex, lung))
td <- tidy(surv_fit)

td %>% ggplot(aes(time, estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)



# Tutorial #2: broom & dplyr
library(broom)
library(dplyr)

data("Orange")
dim(Orange)
str(Orange)

glimpse(Orange)
head(Orange)    # Tree (factor: 5 levels), age, circumference

Orange %>% summarize(orange_cor = cor.test(age, circumference)$estimate,
                     orange_df = cor.test(age, circumference)$parameter)
# 0.9135, df = 33
# OR for just the correlation coefficient:
Orange %>% do(tidy(cor(.$age, .$circumference)))

library(ggplot2)
Orange %>% ggplot(aes(age, circumference, color = Tree)) +
                  geom_line()

# Test correlations individually WITHIN each tree >>> group_by():

Orange %>% group_by(Tree) %>% summarise(correlation = cor(age, circumference))
#     Tree     correlation
#     <ord>     <dbl>
# 1     3     0.9881766
# 2     1     0.9854675
# 3     5     0.9877376
# 4     2     0.9873624
# 5     4     0.9844610

# higher than aggregate (0.9135), similar across trees.

cor.test(Orange$age, Orange$circumference)
# OR JUST DO THIS:
Orange %>% group_by(Tree) %>% do(tidy(cor.test(.$age, .$circumference)))

# Regressions: +1 row of output within each model
Orange %>% group_by(Tree) %>% do(tidy(lm(age ~ circumference, data = .)))

# Tree          term     estimate      std.error    statistic      p.value
# <ord>         <chr>        <dbl>      <dbl>        <dbl>        <dbl>
# 1     3   (Intercept)  -209.512321  85.2682904   -2.4570954   5.743323e-02
# 2     3 circumference   12.038885   0.8353445   14.4118806    2.901046e-05
# 3     1   (Intercept)  -264.673437  98.6205569   -2.6837553   4.362351e-02
# 4     1 circumference   11.919245   0.9188029   12.9725813    4.851902e-05
# 5     5   (Intercept)  -54.484097   76.8862788   -0.7086323   5.102144e-01
# 6     5 circumference    8.787132   0.6211365   14.1468610    3.177093e-05
# 7     2   (Intercept) -132.439725   83.1314146   -1.5931369   1.720092e-01
# 8     2 circumference    7.795225   0.5595479   13.9312907    3.425041e-05
# 9     4   (Intercept)  -76.513671   88.2943757   -0.8665747   4.257969e-01
# 10    4 circumference    7.169842   0.5719516   12.5357484    5.733090e-05


# ML within each group
data("mtcars")
head(mtcars)

mtcars %>% group_by(am) %>% do(tidy(lm(wt ~ mpg + qsec + gear, .)))

# am           term    estimate  std.error   statistic      p.value
# <dbl>       <chr>       <dbl>      <dbl>       <dbl>        <dbl>
# 1     0 (Intercept)  4.91754623 1.39665675  3.52094116 0.0030879519
# 2     0         mpg -0.19188914 0.04428329 -4.33321746 0.0005909953
# 3     0        qsec  0.09191361 0.09832067  0.93483509 0.3646797728
# 4     0        gear  0.14653754 0.36819363  0.39799041 0.6962441554
# 5     1 (Intercept)  4.28307028 3.45859958  1.23838281 0.2469014834
# 6     1         mpg -0.10098320 0.02943409 -3.43082488 0.0074984578
# 7     1        qsec  0.03983165 0.15112135  0.26357393 0.7980436972
# 8     1        gear -0.02288330 0.34878226 -0.06560912 0.9491232955

# also for augment() and glance() outputs for regressions:
regressions <- mtcars %>% group_by(cyl) %>% 
  do(fit = lm(wt ~ mpg + qsec + gear, .))

regressions
#   cyl      fit                            creates rowwise dataframe.
# *<dbl>   <list>
#   1     4 <S3: lm>
#   2     6 <S3: lm>
#   3     8 <S3: lm>

regressions$fit  # each of the regression outputs for 4, 6, 8 cyl

regressions %>% tidy(fit)

regressions %>% augment(fit)

regressions %>% glance(fit)

# combine estimates + p-values, etc. across ALL groups into dataframe for regressions:
# - allow for sorting by p-value, estimate, etc. to find most significant terms across all tests
# - p-value histograms
# - volcano plots comparing p-values to effect size estimates



