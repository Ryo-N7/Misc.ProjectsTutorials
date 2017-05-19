rm(list = ls())
# Simplified: 1 Choice, 50 Spaces

set.seed(2017-02-13)
cumsum(sample(6, 20, replace = TRUE))
?sample
?replicate
?cumsum
num_rolls <- 50
max_position <- 50
trials <- 200000
positions <- replicate(trials, cumsum(sample(6, num_rolls, replace = TRUE)))
positions[1:6, 1:6]
?tabulate
tabulate(c(2,3,5))
tabulate(c(3,4,5))
count_per_position <- tabulate(positions, max_position)
count_per_position
install.packages('dplyr')
library(dplyr)
position_prob <- data.frame(positions = seq_len(max_position), 
                            probability = count_per_position/trials)
probability <- count_per_position/trials
position_prob

library(ggplot2)
library(ggthemes)
ggplot(position_prob, aes(positions, probability)) + geom_line() + theme_classic() + 
  labs(x = 'Posiciones', y = 'Probability') + 
  geom_vline(aes(xintercept = 6, col = "red", 
                 linetype = "longdash")) + 
  geom_hline(aes(yintercept = mean(probability), col = "blue", 
                 linetype = "longdash")) 

arrange(position_prob, desc(probability))

