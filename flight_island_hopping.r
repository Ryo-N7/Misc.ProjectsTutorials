library(tidyverse)
library(ggmap)
library(ggrepel)
library(geosphere)


# Part 1 ------------------------------------------------------------------

# Read flight list
flights <- read.csv("flights.csv", stringsAsFactors = FALSE)

# Lookup coordinates
library(ggmap)
airports <- unique(c(flights$From, flights$To))
coords <- geocode(airports)
airports <- data.frame(airport=airports, coords)

aero <- airports %>% filter(airport != c("Midway Atoll", "Wake Island"))


# Add coordinates to flight list
flights <- merge(flights, aero, by.x="To", by.y="airport")
flights <- merge(flights, aero, by.x="From", by.y="airport")

# Plot flight routes
library(ggplot2)
library(ggrepel)
worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders

ggplot() + worldmap + 
  geom_curve(data=flights, 
             aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y), 
             col = "#b29e7d", size = 1, curvature = .2) + 
  geom_point(data=airports, 
             aes(x = lon, y = lat), 
             col = "#970027") + 
  geom_text_repel(data=airports, 
                  aes(x = lon, y = lat, label = airport), 
                  col = "black", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )



# Trying on Part 2's Pacific-centric map ----------------------------------

# Pacific centric
flights$lon.x[flights$lon.x < 0] <- flights$lon.x[flights$lon.x < 0] + 360
flights$lon.y[flights$lon.y < 0] <- flights$lon.y[flights$lon.y < 0] + 360
aero$lon[aero$lon < 0] <- aero$lon[aero$lon < 0] + 360

# Plot flight routes
worldmap <- borders("world2", colour="#252525", fill="#BD3B3B")   # #efede1

ggplot() + worldmap + 
  geom_point(data = aero, aes(x = lon, y = lat), col = "#970027") + 
  geom_text_repel(data = aero, aes(x = lon, y = lat, label = airport), 
                  col = "black", size = 3, segment.color = NA) + 
  geom_curve(data=flights, aes(x = lon.x, y = lat.x, xend = lon.y, 
                               yend = lat.y), size = .4, curvature = .2) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) + 
  xlim(100, 280) + ylim(-40,40)

?borders()

ggplot() + borders("world2", colour = "red") + xlim(100, 280) + ylim(-40, 40)



# Part 2 ------------------------------------------------------------------

# Read flight list and airport list
flights <- read.csv("PacificFlights.csv", stringsAsFactors = FALSE)
f <- "airports.csv"

if (file.exists(f)) {
  airports <- read.csv(f)
} else airports <- data.frame(airport = NA, lat = NA, lon = NA)

# Lookup coordinates for new airports
all_airports <- unique(c(flights$From, flights$To))

new_airports <- all_airports[!(all_airports %in% airports$airport)]

if (length(new_airports) != 0) {
  coords <- geocode(new_airports)
  new_airports <- data.frame(airport = new_airports, coords)
  airports <- rbind(airports, new_airports)
  airports <- subset(airports, !is.na(airport))
  write.csv(airports, "Geography/airports.csv", row.names = FALSE)
}

# Add coordinates to flight list
flights <- merge(flights, airports, by.x="From", by.y="airport")
flights <- merge(flights, airports, by.x="To", by.y="airport")


# Pacific centric
flights$lon.x[flights$lon.x < 0] <- flights$lon.x[flights$lon.x < 0] + 360
flights$lon.y[flights$lon.y < 0] <- flights$lon.y[flights$lon.y < 0] + 360
airports$lon[airports$lon < 0] <- airports$lon[airports$lon < 0] + 360

# Plot flight routes
worldmap <- borders("world2", colour="#efede1", fill="#efede1")

ggplot() + worldmap + 
  geom_point(data=airports, aes(x = lon, y = lat), col = "#970027") + 
  geom_text_repel(data=airports, aes(x = lon, y = lat, label = airport), 
                  col = "black", size = 2, segment.color = NA) + 
  geom_curve(data=flights, aes(x = lon.x, y = lat.x, xend = lon.y, 
                               yend = lat.y, col = Airline), size = .4, curvature = .2) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) + 
  xlim(100, 300) + ylim(-40,40)




# Network analysis --------------------------------------------------------

library(igraph)
g <- graph_from_edgelist(as.matrix(flights[,1:2]), directed = FALSE)
par(mar = rep(0, 4))
plot(g, layout = layout.fruchterman.reingold, vertex.size=0)
V(g)
shortest_paths(g, "Auckland", "Saipan")

