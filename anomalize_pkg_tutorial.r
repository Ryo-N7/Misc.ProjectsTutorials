library(tidyverse)
library(anomalize)

# https://business-science.github.io/anomalize/


tidyverse_cran_downloads %>% 
  ggplot(aes(date, count)) +
  geom_point(color = "#2c3e50", alpha = 0.25) + 
  facet_wrap(~ package, scale = "free_y", ncol = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
tidyverse_cran_downloads %>% 
  ggplot(aes(date, count)) +
  geom_point(color = "#2c3e50", alpha = 0.25) + 
  facet_grid(~ package, scale = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

tidyverse_cran_downloads %>% 
  time_decompose(count, method = "stl") %>% 
  anomalize(remainder, method = "iqr") %>% 
  time_recompose() %>% 
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Tidyverse anomalies")




