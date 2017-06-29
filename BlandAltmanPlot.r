# Bland-Altman Mean Difference Plots (Using ggplot2)
library(dplyr)
library(ggplot2)

pine_df <- Loblolly     # dataset: growht of loblolly pine trees
glimpse(pine_df)

# random split data into 2 samples (equal length)
sample_df <- data.frame(sample_n(pine_df, size = nrow(pine_df) * 0.5) %>%
                          select(height) %>% 
                          arrange(desc(height)), 
                        sample_n(pine_df, size = nrow(pine_df) * 0.5) %>%
                          select(height) %>%
                          arrange(desc(height)))   # demonstration: NOT actually measured using different instruments...

names(sample_df) <- c("Sample_1", "Sample_2")
# B-A Plot x-axis: avg. of measurement pairs, y-axis: differences between measures in each pair
sample_df$Avg <- (sample_df$Sample_1 + sample_df$Sample_2) / 2
sample_df$Dif <- sample_df$Sample_1 - sample_df$Sample_2

# Plot + add mean difference (blue line) + 95% confint (red lines) for predictions of mean difference (prediction interval: 1.96 * SD)
sample_df %>% ggplot(aes(Avg, Dif)) + 
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, size = 0.5, lty = "dashed") +
  geom_hline(yintercept = mean(sample_df$Dif), color = "blue", size = 1.7, alpha = 0.5) + 
  geom_hline(yintercept = mean(sample_df$Dif) - (1.96 * sd(sample_df$Dif)), color = "red", size = 0.8) +
  geom_hline(yintercept = mean(sample_df$Dif) + (1.96 * sd(sample_df$Dif)), color = "red", size = 0.8) +
  ylab("Diff. Between Measures") +
  xlab("Average Measure") 


hist(sample_df$Dif)    # LEFT SKEW!!   NOT normal distribution.... necessity transformation : log?
sample_df$Dif <- log(sample_df$Sample_1 - sample_df$Sample_2)
