# P-values and significance levels to ggplots:
# Use ggpubr >>>
library(ggpubr)
library(tidyverse)
library(scales)
library(ggplot2)
args(compare_means)
args(stat_compare_means)   # for adding p-values to ggplots

View(ToothGrowth)

ToothGrowth %>% compare_means(len ~ supp, data = .)  # default: wilcoxon test
ToothGrowth %>% compare_means(len ~ supp, method = "t.test", data = .)

p <- ToothGrowth %>% ggboxplot(x = "supp", y = "len", 
                               color = "supp", palette = "jco",
                               add = "jitter")
p + stat_compare_means()
p + stat_compare_means(method = "t.test")
p + stat_compare_means(aes(label = ..p.signif..), 
                       label.x = 2.2, label.y = 30)
p + stat_compare_means(aes(label = paste0(..method.., "\n", "p = ", ..p.format..)))
p + stat_compare_means(label = "p.signif")

# Compare 2-paired samples:
ToothGrowth %>% compare_means(len ~ supp, paired = TRUE, data = .)

ToothGrowth %>% ggpaired(x = "supp", y = "len", color = "supp",
                         line.color = "gray", line.size = 0.4, palette = "jco") +
  stat_compare_means(paired = T)


# Compare more than 2 groups:
compare_means(len ~ dose, data = ToothGrowth, method = "anova")

ToothGrowth %>% ggboxplot(x = "dose", y = "len", color = "dose", palette = "jco") +
  stat_compare_means()     # default for multiple groups: Kruskal-Wallis test

ToothGrowth %>% ggboxplot(x = "dose", y = "len", color = "dose", palette = "jco") +
  stat_compare_means(method = "anova") 

# Pair-wise comparisons:
ToothGrowth %>% compare_means(len ~ dose, data = .) # default: wilcox test

my_comparisons <- list(c("0.5", "1"), c("1", "2"), c("0.5", "2"))
ToothGrowth %>% ggboxplot(x = "dose", y = "len", color = "dose", palette = "jco") +
  stat_compare_means(comparisons = my_comparisons) +
  stat_compare_means(label.y = 50)

# Pair-wise vs. reference
ToothGrowth %>% compare_means(len ~ dose, data = ., ref.group = "0.5", method = "t.test")

ToothGrowth %>% ggboxplot(x = "dose", y = "len", color = "dose", palette = "jco") +
  stat_compare_means(method = "anova", label.y = 42.5) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "0.5")

# Pair-wise against base-mean:
ToothGrowth %>% compare_means(len ~ dose, data = ., ref.group = ".all.", method = "t.test")

ToothGrowth %>% ggboxplot(x = "dose", y = "len", color = "dose", palette = "jco") +
  stat_compare_means(method = "anova", label.y = 41.2) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")

# When multiple groups, performing pairwise comparison between all is difficult to interpret
# and very inefficient. Comparison with base-mean allow conclusion that variable is 
# significantly OVER or UNDER in group compared to all others.

# Ex. myeloma data
if(!require(survminer)) install.packages("survminer")
data("myeloma", package = "survminer")

compare_means(DEPDC1 ~ molecular_group, data = myeloma, ref.group = ".all.", method = "t.test")

myeloma %>% ggboxplot(x = "molecular_group", y = "DEPDC1", 
                      color = "molecular_group", add = "jitter", legend = "none") +
  rotate_x_text(angle = 35) +
  geom_hline(yintercept = mean(myeloma$DEPDC1), linetype = 2) +   # line for base-mean
  stat_compare_means(method = "anova", label.y = 1600) +
  stat_compare_means(label = "p.signif", method = "t.test", 
                     ref.group = ".all.", hide.ns = T)   # hide not significant symbol
# Significantly overexpressed in proliferation group.
# Significantly udnerexpressed in Hyperdiploid and LB disease groups


# Multiple grouping variables: 
compare_means(len ~ supp, data = ToothGrowth, group.by = "dose")

p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
               color = "supp", palette = "jco",
               add = "jitter",
               facet.by = "dose", short.panel.labs = F)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format")

# Without facet panels:
p <- ggboxplot(ToothGrowth, x = "dose", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = supp))
p + stat_compare_means(aes(group = supp), label = "p.format") # show only p-value
p + stat_compare_means(aes(group = supp), label = "p.signif") # show signif. symbol


# Paired sample comparisons after grouping data by another variable:
compare_means(len ~ supp, data = ToothGrowth, 
              group.by = "dose", paired = TRUE)

p <- ggpaired(ToothGrowth, x = "supp", y = "len",
              color = "supp", palette = "jco", 
              line.color = "gray", line.size = 0.4,
              facet.by = "dose", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", paired = TRUE)


# Bar and line plots with 1 grouping variable:
# Bar plot of mean +/-se
ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se")+
  stat_compare_means() +                                         # Global p-value
  stat_compare_means(ref.group = "0.5", label = "p.signif",
                     label.y = c(22, 29))                   # compare to ref.group

# Line plot of mean +/-se
ggline(ToothGrowth, x = "dose", y = "len", add = "mean_se")+
  stat_compare_means() +                                         # Global p-value
  stat_compare_means(ref.group = "0.5", label = "p.signif",
                     label.y = c(22, 29))  

# Bar + line plots with 2 grouping variables:
ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se",
          color = "supp", palette = "jco", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)

ggline(ToothGrowth, x = "dose", y = "len", add = "mean_se",
       color = "supp", palette = "jco")+
  stat_compare_means(aes(group = supp), label = "p.signif", 
                     label.y = c(16, 25, 29))

