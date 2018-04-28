# https://github.com/apreshill/ohsu-biodatavis/blob/master/slides.Rmd

library(tidyverse)

set.seed(1000)


asdpop <- tibble(
  time1 = sample(1:100, 100, replace = F), 
  time2 = time1) %>% 
  gather(x, y , time1:time2, factor_key = TRUE)

asdpop

asdpop <- asdpop %>% 
  mutate(services = as.factor(case_when(
    x == "time1" & y <= 30 ~ 1,
    x == "time1" & y > 30 ~ 0, 
    x == "time2" & y <= 60 ~ 1,
    TRUE ~ 0
  )))

asdpop

bar1 <- ggplot(asdpop, aes(x, fill = services))
bar1

bar1 <- bar1 + geom_bar(width = 0.6)
bar1

library(wesanderson)
ff <- wes_palette("FantasticFox")[c(2:3)]
ff

bar2 <- bar1 + scale_fill_manual(values = ff)
bar2

bar3 <- bar2 +scale_x_discrete(name = "", labels = c("Time 1", "Time 2"))
bar3
bar3 <- bar3 + scale_y_continuous(expand = c(0.02, 0), 
                                  name = "ASD Cases per 10,000")
bar3
bar3 <- bar3 + 
  theme_bw(base_family = "Lato") +
  theme(axis.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank())

bar3

bar4 <- bar3 + 
  annotate("text", label = "Acessing \nServices", 
           x = 2, y = 30, size = 4, color = "white", 
           fontface = "bold", family = "Lato") +
  annotate("text", label = "Not \nAcessing \nServices", 
           x = 2, y = 80, size = 4, color = "white", 
           fontface = "bold", family = "Lato") +
  guides(fill = FALSE)

bar4

# add horizontal line for population prevalance:
bar5 <- bar4 + 
  geom_segment(aes(x = 0.6, xend = 2.45, y = 100, yend = 100), 
               lty = 3, lwd = 0.3, color = "black")
bar5

bar6 <- bar5 +
  coord_cartesian(ylim = c(0, 102), xlim = c(1, 3.2))
bar6

bar6 <- bar6 +
  annotate("text", x = 2.5, y = 97, size = 4, just = 0, 
           family = "Lato", 
           label = "Estimates of prevalence based\non population sampling will remain\nstable over time if true prevalence\nis stable.")

bar6

# add segments to track sample prevalence

bar7 <- bar6 +
  geom_segment(aes(x = 0.6, xend = 1.3, y = 30, yend = 30),
               lty = 3, lwd = 0.5, color = ff[2])
bar7

bar7 <- bar7 +
  geom_segment(aes(x = 1.3, xend = 1.7, y = 30, yend = 60),
               lty = 3, lwd = 0.5, color = ff[2])
bar7

bar7 <- bar7 +
  geom_segment(aes(x = 1.7, xend = 2.45, y = 60, yend = 60), 
               lty = 3, lwd = 0.5, color = ff[2])
bar7

bar8 <- bar7 + 
  annotate("text", 
           x = 2.5, y = 60, size = 4, hjust = 0, 
           family = "Lato", 
           label = "Estimates of prevalence based\non individuals accessing services\ncan create an illusion of an\nincrease in prevalence over time,\nyet still underestimate prevalence\nat both time points.")
bar8






