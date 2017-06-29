library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales)
library(fields)

set.seed(170513)
n <- 200
d <- data.frame(a = rnorm(n))
d$b <- 0.4 * (d$a + rnorm(n))
head(d)


d %>% ggplot(aes(a, b)) + 
  geom_point() +
  theme_minimal()

# Change size + points.
d %>% ggplot(aes(a, b)) + 
  geom_point(shape = 16, size = 3) +
  theme_minimal()

# Color
d %>% ggplot(aes(a, b, color = a)) + 
  geom_point(shape = 16, size = 3, show.legend = FALSE) +
  theme_minimal()
# Color change from left -> right. Rather for direction of correlation (diagonally).
# Colors points by First Principal Component. Add as variable 'pc' for usage as color in aes()
d$pc <- predict(prcomp(~a + b, d))[, 1]

d %>% ggplot(aes(a, b, color = pc)) + 
  geom_point(shape = 16, size = 3, show.legend = FALSE) +
  theme_minimal()

# Use scale_color_gradient to customize select colors:
d %>% ggplot(aes(a, b, color = pc)) + 
  geom_point(shape = 16, size = 10, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

# Points mushed up. Adjust transparency with 'alpha' in aes()
d %>% ggplot(aes(a, b, color = pc)) + 
  geom_point(shape = 16, size = 10, show.legend = FALSE, alpha = 0.4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

# What if ^^^ # of points? Change size or...?
set.seed(170513)
n <- 5000
d <- data.frame(a = rnorm(n))
d$b <- 0.4 * (d$a + rnorm(n))
d$pc <- predict(prcomp(~a + b, d))[,1]

d %>% ggplot(aes(a, b, color = pc)) + 
  geom_point(shape = 16, size = 10, show.legend = FALSE, alpha = 0.4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")
# If use alpha...
d %>% ggplot(aes(a, b, color = pc)) + 
  geom_point(shape = 16, size = 10, show.legend = FALSE, alpha = 0.05) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")
# Inability see extreme points or outliers.
# Usage bivariate density to inverse point density on 'alpha', plot as 1/density:
d$density <- fields::interp.surface(MASS::kde2d(d$a, d$b), d[, c("a", "b")])
1/d$density

d %>% ggplot(aes(a, b, color = pc, alpha = 1/density)) + 
  geom_point(shape = 16, size = 10, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

# Extreme points too bright. Tweak alpha range with scale_alpha()
d %>% ggplot(aes(a, b, color = pc, alpha = 1/density)) + 
  geom_point(shape = 16, size = 10, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  scale_alpha(range = c(0.05, 0.5))


# All together:
# Simulate data
set.seed(170513)
n <- 2000
d <- data.frame(a = rnorm(n))
d$b <- -(d$a + rnorm(n, sd = 2))

# Add first principal component
d$pc <- predict(prcomp(~a+b, d))[,1]

# Add density for each point
d$density <- fields::interp.surface(
  MASS::kde2d(d$a, d$b), d[,c("a", "b")])

# Plot
ggplot(d, aes(a, b, color = pc, alpha = 1/density)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#32aeff", high = "#f2aeff") +
  scale_alpha(range = c(.25, .6))














