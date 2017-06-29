# Bar charts +
library(ggpubr)

df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))

p <- df %>% ggbarplot(x = "dose", y = "len", color = "blue", fill = "grey")
p 

# Horizontal bars
p + rotate()

# Colors by groups:
df %>% ggbarplot(x = "dose", y = "len", color = "dose", fill = "dose", palette = "jco")

# Multiple grouping variables:
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
# Labels inside bars, grouped by second grouping variable:
df2 %>% ggbarplot(x = "dose", y = "len", color = "supp", fill = "supp", 
                  palette = c("grey", "black"), 
                  label = TRUE, lab.col = "red", lab.pos = "in")

# Dodged positions for each second group variable:
df2 %>% ggbarplot(x = "dose", y = "len", color = "supp", fill = "supp", 
                  palette = c("grey", "black"), 
                  position = position_dodge(0.9))


# Ordered bar plots:
data("mtcars")
dfm <- mtcars
dfm$cyl <- as.factor(dfm$cyl)
dfm$name <- rownames(dfm)        # add name as own column

head(dfm[ , c("name", "wt", "mpg", "cyl")])
head(dfm[ , c(12, 6, 1, 2)])
head(dfm[ , c(1:4, 8:10)])

dfm %>% ggbarplot(x = "name", y = "mpg", fill = "cyl", color = "white", palette = "jco",
                  sort.val = "desc",   # sort value in descending order 
                  sort.by.groups = F,  # don't sort inside each group
                  x.text.angle = 30    # rotate axis text 
                  )
# Sort bys inside EACH group, use sort.by.groups = TRUE:
dfm %>% ggbarplot(x = "name", y = "mpg", fill = "cyl", color = "white", palette = "jco",
                  sort.val = "asc",   # sort value in ascending order 
                  sort.by.groups = T,  # don't sort inside each group
                  x.text.angle = 90    # rotate axis text 
)

# Deviation graphs:
# Deviation of quantitative values to a reference value.
# Z-scores:
dfm$mpg_z <- (dfm$mpg - mean(dfm$mpg))/sd(dfm$mpg)
dfm$mpg_grp <- factor(ifelse(dfm$mpg_z < 0, "low", "high"), 
                      levels = c("low", "high"))
head(dfm[, 12:14])

# Ordered bar plot, colored to level of mpg:
dfm %>% ggbarplot("name", "mpg_z", fill = "mpg_grp", color = "white", palette = "jco",
                  sort.val = "asc", sort.by.groups = F, x.text.angle = 90,
                  ylab = "MPG z-score", xlab = FALSE, 
                  legend.title = "MPG Group")
# Rotate axes, sort descending order:
dfm %>% ggbarplot("name", "mpg_z", fill = "mpg_grp", color = "white", palette = "jco",
                  sort.val = "desc", sort.by.groups = F,
                  ylab = "MPG z-score", xlab = FALSE, rotate = T,
                  legend.title = "MPG Group")

# Lollipop chart: large sets of values for visualization
dfm %>% ggdotchart(x = "name", y = "mpg", color = "cyl", 
                   palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   sorting = "asc", 
                   add = "segments", # add segments down from point to y-int
                   ggtheme = theme_pubr())
# bigger dots, larger labels, sort inside each group, vertical plot, sort (desc):
dfm %>% ggdotchart(x = "name", y = "mpg", color = "cyl", 
                   palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   sorting = "asc", group = "cyl",
                   sort.by.groups = T, sort.val = "desc",
                   add = "segments", # add segments down from point to y-int
                   dot.size = 6, label = round(dfm$mpg), 
                   font.label = list(color = "white", size = 9, vjust = 0.5),
                   ggtheme = theme_pubr(), 
                   rotate = T)

# With deviation z-scores
dfm %>% ggdotchart(x = "name", y = "mpg_z", color = "cyl", 
                   palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   sorting = "desc", add = "segments", 
                   add.params = list(color = "lightgray", size = 2),
                   group = "cyl", dot.size = 6, 
                   label = round(dfm$mpg_z, 1), 
                   font.label = list(color = "white", size = 9, vjust = 0.5), 
                   ggtheme = theme_pubr()) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")

# Cleveland's dot plot:
dfm %>% ggdotchart(x = "name", y = "mpg", color = "cyl", dot.size = 2,
                   palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   sorting = "desc", rotate = T,
                   y.text.col = T, ggtheme = theme_pubr()) +
  theme_cleveland()   # dashed grids











