# Internal consistency
# using Big 5 personality dataset.

temp <- tempfile()
download.file("http://personality-testing.info/_rawdata/BIG5.zip", temp, mode = "wb")

d <- read.table(unz(temp, "BIG5/data.csv"), header = T, sep = "\t")
unlink(temp)
rm(temp)

# n = ~1900, demographics, 50 items (10 for each of Big 5).
# shorten for this analysis:
d <- d[1:500, paste0("E", 1:10)]
str(d)

# 5-point Likert scale, anchored by 1 = Disagree, 5 = Agree
# EXTRAVERSION Questions:
# E1 I am the life of the party.
# E2 I don’t talk a lot.     # reverse: for Extraversion 1:Disagree should be highest for E.
# E3 I feel comfortable around people.
# E4 I keep in the background.
# E5 I start conversations.
# E6 I have little to say.
# E7 I talk to a lot of different people at parties.
# E8 I don’t like to draw attention to myself.
# E9 I don’t mind being the center of attention.
# E10 I am quiet around strangers.

# E2, E4, E6, E8, E10 need reverse scoring.
d[, paste0("E", c(2, 4, 6, 8, 10))] <- 6 - d[, paste0("E", c(2, 4, 6, 8, 10))]

str(d)

# Avg. inter-item correlation: 
# 1. Correlation between all items. 2. average correlations.
library(corrr)
d %>% correlate()
# Obtain avg. correlation for each item with other by compute mean of each column (excl. rowname)

inter_item <- d %>% correlate() %>% select(-rowname) %>% colMeans(na.rm = T)
inter_item
# E1        E2        E3        E4        E5        E6        E7        E8        E9       E10 
# 0.4983678 0.4827948 0.4866458 0.4991764 0.5334524 0.4090418 0.5133091 0.4270190 0.4731896 0.4814496 
# most correlate around 0.4~0.5     E5 and E7 highest, E8 lowest.

mean(inter_item)
# 0.4804446

library(ggplot2)
library(ggthemes)
library(scales)

data.frame(inter_item) %>% ggplot(aes(x = inter_item)) + 
  geom_histogram(bins = 10, alpha = 0.8) +
  geom_vline(xintercept = mean(inter_item), color = "darkred") +
  labs(x = "Mean inter-item correlation") +
  theme_minimal()

# Average item-total correlation:
d$score <- rowMeans(d)
d$score

item_total <- d %>% correlate() %>% focus(score)
item_total
# rowname     score
# 1      E1 0.7520849
# 2      E2 0.7297506
# 3      E3 0.7350644
# 4      E4 0.7485092
# 5      E5 0.7945943
# 6      E6 0.6367799
# 7      E7 0.7768180
# 8      E8 0.6640914
# 9      E9 0.7273987
# 10     E10 0.7306038

mean(item_total$score)
# 0.7295695

item_total %>% ggplot(aes(x = score)) +
  geom_histogram(bins = 10, alpha = 0.3) +
  geom_vline(xintercept = mean(item_total$score), color = "blue") +
  labs(x = "Mean item-total correlation") +
  theme_classic()



# Cronbach's Alpha.
# use psych::alpha()
# df or matrix >>> each column = test/item, row = observation
d$score <- NULL

psych::alpha(d)
# standardised alpha based upon the correlations:
psych::alpha(d)$total$std.alpha
# std. alpha: 0.9 (very high...)
# average inter-item correlation:
# average_r: 0.48            matches with mean(inter_item)
# correlation of each item with total score:
# raw.r: 0.66 ~ 0.78         matches with item_total

# Composite reliability:


