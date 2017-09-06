
troops <- read.table("~/R materials/Misc.ProjectsTutorials/minard/troops.txt", header=T)
cities <- read.table("~/R materials/Misc.ProjectsTutorials/minard/cities.txt", header=T)
temps <- read.table("~/R materials/Misc.ProjectsTutorials/minard/temps.txt", header=T)
temps$date <- as.Date(strptime(temps$date,"%d%b%Y"))

library(maps)
borders <- data.frame(map("world", xlim=c(10,50), ylim=c(40, 80), plot=F)[c("x","y")])

xlim <- scale_x_continuous(limits = c(24, 39))

library(ggplot2)
library(ggmap)

minard <- c(left = 10, bottom = 40, right = 50, top = 80)


minard.map <- get_stamenmap(minard, zoom = 3, 
                            maptype = "terrain-background", where = "cache", )

ggmap(minard.map) + 
  geom_path(aes(size = survivors, colour = direction, group = group), lineend = "round", data = troops) + 
  geom_point(cities, aes(x = long, y = lat)) + 
  geom_text(aes(label = city), hjust = 0, vjust = 1, size = 4) + 
  scale_size(range = c(0.5, 15)) + 
  scale_colour_manual(values = c("grey50","red")) + # + xlim(?)
  theme(legend.position = "none") +
  map("world", xlim=c(10,50), ylim=c(40, 80), plot = T)


ggplot(cities, aes(x = long, y = lat)) + 
  geom_path(aes(size = survivors, colour = direction, group = group), lineend = "round", data = troops) + 
  geom_point() + 
  geom_text(aes(label = city), hjust = 0, vjust = 1, size = 4) + 
  scale_size(range = c(0.5, 15)) + 
  scale_colour_manual(values = c("grey50","red")) + # + xlim(?)
  theme(legend.position = "none") +
  map("world", xlim=c(10,50), ylim=c(40, 80), plot = T)

ggsave(file = "march.pdf", width=16, height=4)

qplot(long, temp, data=temps, geom="line") + 
geom_text(aes(label = paste(day, month)), vjust=1) + xlim

ggsave(file = "temps.pdf", width=16, height=4)