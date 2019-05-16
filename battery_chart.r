library(dplyr)
library(ggplot2)
# https://github.com/neilcollins2018/battery-and-gauge-plots/blob/master/battery%20chart.R

###################Battery plot
# example data: 
df <- tibble(player = c("Player1", "Player2", "Player3", "Player4"),
             value =  sample(2500:5000, 4),
             max = sample(5000:5500, 4))

# scale data: 
df <- df %>% 
  mutate(value_scale = (value / max)*100,
         aim = rep(100, nrow(.)))


###function for colour code
colour_func_battery <- function(x){ifelse(x$value_scale > 90, '#800000', 
                                          ifelse(x$value_scale < 60, '#000080', '#008000'))
}

ggplot(df, aes(x = player)) + 
  geom_bar(aes(y = aim), width = 0.85, stat = "identity", colour = "grey68") +
  geom_bar(width = 0.85, stat = "identity", 
           aes(y = value_scale, fill = player), 
           colour = "black") +
  geom_text(aes(x = player, y = 100), label = paste0(round(df$value_scale,0),'%'),
            colour = "azure4", vjust = -1) +
  scale_fill_manual(values = colour_func_battery(df)) +
  labs(title ='Indication of Distance Compared to Highest',
       subtitle = 'Blue under 60%, Green 60-90%, Red over 90%') +
  xlab("") + ylab("") +
  bbplot::bbc_style()


###################Gauge style plot
# example data: 
df <- tibble(player = c("Player1", "Player2", "Player3", "Player4"),
             value =  sample(2500:5000, 4),
             max = sample(5000:5500, 4))

# scale data: 
df <- df %>% 
  add_row(player = "A", `value` = 0, max=0,.before = 1) %>%
  mutate(value_scale = (value / max)*100,
         value_sc_half = value_scale/2,
         aim = rep(50, nrow(.)))

df_sub <- filter(df, player != "A")

###function for colour code
colour_func_gauge <- function(x){ifelse(x$value_sc_half > 45, '#800000', 
                                        ifelse(x$value_sc_half < 30, '#000080', '#008000'))
}

ggplot(df, aes(x = player)) + 
  geom_bar(width = 0.85, stat="identity", aes(y = value_scale), 
           colour = "white", fill = "white") +
  geom_bar(data=df_sub, aes(y = aim), width = 0.85, stat = "identity", colour = "grey68") +
  geom_bar(data=df_sub, width = 0.85, stat = "identity", 
           aes(y = value_sc_half, fill = player), 
           colour = "black") +
  geom_text(data=df_sub, aes(x = player, y = 50), label = paste0(round(df_sub$value_scale,0),'%'),
            colour = "azure4", vjust = -1) +
  scale_fill_manual(values = colour_func_gauge(df_sub)) +
  labs(title ='Indication of Distance Compared to Highest',
       subtitle = 'Blue under 60%, Green 60-90%, Red over 90%') +
  xlab("") + ylab("") +
  geom_text(data=df_sub, hjust = 1.02, size = 4, 
            aes(x = player, y = 0, label = player), angle = 70) +
  theme_classic()+
  coord_polar(theta = "y",start=-pi/2) +
  ylim(0,100) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "top",
        legend.text.align = 0,
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size=18,
                                            color="#222222"))