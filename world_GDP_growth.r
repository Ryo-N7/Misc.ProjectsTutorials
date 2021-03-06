# https://github.com/bilevg/SimpleGrowthVisualization/blob/master/Growthviz.R

library(cshapes)
library(tidyverse)
library(wbstats)
library(scales)
library(gpclib)
library(grid)
library(gridExtra)
library(magick)


gpclibPermit()
options(digits = 8, max.print = 1000, scipen = 13)


start <- 1961
end <- 2016

# truncate:
maximum <- 6

#color gradients: 
levels <- 5

# colors 
ncolors <-  maximum
increment <- (1/levels)

x <- seq(0, 1, length.out = ncolors * levels + 1)

topcolor <- "red4"
midcolor <- "yellow"
botcolor <- "green4"

colors <- c(seq_gradient_pal(botcolor, midcolor)(x), seq_gradient_pal(midcolor, botcolor)(x))
colors <- unique(colors)


# data prep

truncate_top <- function(x, top) {ifelse(x > top, top, x)}
truncate_bottom <- function(x, bottom) {ifelse(x < bottom, bottom , x)}

wb_df <- wb(indicator = c("NY.GDP.MKTP.KD.ZG", "SP.POP.TOTL"),
            country = "countries_only", start = start, end = end)

glimpse(wb_df)

wb_df <- wb_df %>% 
  select(country, date, iso3c, value, indicator) %>% 
  spread(key = indicator, value = value) %>% 
  mutate(date = as.numeric(date)) %>% 
  rename(Population = "Population, total",
         ISO1AL3 = iso3c,
         GDPgrowth = "GDP growth (annual %)") %>% 
  mutate(GDPgrowth_trunc = truncate_top(GDPgrowth, maximum), 
         GDPgrowth_trunc = truncate_bottom(GDPgrowth_trunc, -maximum)) %>% 
  select(-Population)


glimpse(wb_df)


## data for timeline plot
wb_world <- wb(indicator=c('NY.GDP.MKTP.KD.ZG'),
              country='aggregates', start=start, end=end)

wb_world <- wb_world %>%
  filter(country == 'World') %>%
  select(country, date, value, indicator) %>%
  spread(key = indicator, value=value) %>%
  mutate(date = as.numeric(date)) %>%
  rename(GDPgrowth = 'GDP growth (annual %)') %>%
  ## for colors, truncated growth
  mutate(GDPgrowth.trunc = truncate_top(GDPgrowth, maximum)) %>%
  mutate(GDPgrowth.trunc = truncate_bottom(GDPgrowth.trunc, -maximum)) %>%
  ## discretize growth values and turn into colors
  mutate(col = colors[cut(GDPgrowth.trunc,
                          breaks = seq(-maximum, maximum, length.out = length(GDPgrowth.trunc)),
                          include.lowest=TRUE)])







# shape files:

## customize the built-in cshp function to include several African states later start dates
my_cshp <- function (date = NA)
{
  path <- paste(system.file(package = 'cshapes'), 'shp/cshapes.shp',
                sep = '/')
  cshp.full <- readShapePoly(path, proj4string = CRS('+proj=longlat +ellps=WGS84'),
                             IDvar = 'FEATUREID')
  ## fix for several missing states (late start)
  missing <- c('Angola', 'Zambia', 'Mozambique', 'Botswana',
               'Tanzania', 'Kenya', 'Zimbabwe', 'Uganda',
               'Algeria', 'Rwanda', 'Burundi', 'Malawi',
               'Swaziland', 'Lesotho', 'Sierra Leone',
               'Equatorial Guinea')
  for(state in missing){
    cshp.full$GWSYEAR[cshp.full$CNTRY_NAME == state] <- 1960
  }
  ## manual fix for Western Sahara (part of Morocco)
  cshp.full$GWSYEAR[cshp.full$FEATUREID == 243] <- 1960
  cshp.full$GWSYEAR[cshp.full$FEATUREID == 219] <- 2017
  cshp.full$GWSYEAR[cshp.full$FEATUREID == 220] <- 2017
  ## original function from cshp package, selects states active at that time
  cshp.full <- cshp.full[cshp.full$GWCODE >= 0, ]
  startdate <- as.Date(paste(cshp.full$GWSYEAR, cshp.full$GWSMONTH,
                             cshp.full$GWSDAY, sep = '-'))
  enddate <- as.Date(paste(cshp.full$GWEYEAR, cshp.full$GWEMONTH,
                           cshp.full$GWEDAY, sep = '-'))
  cshp.part <- cshp.full[startdate <= date & enddate >=
                           date, ]
  ## ## slightly thin (simplify) the polygons for faster and neater plotting
  ## cshp.part <- thinnedSpatialPoly(cshp.part, tolerance=.1, minarea=100)
  cshp.part
}


## options for map plot
themeops <- theme(plot.background = element_rect(fill = 'white'),
                  plot.margin = margin(t = 0, r=0, b=-.3, l=-.2, unit='in'),
                  panel.background = element_rect(fill = 'white'),
                  axis.line=element_blank(),
                  axis.text=element_blank(),
                  axis.title.x=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.y=element_blank(),
                  legend.direction='vertical',
                  panel.grid=element_blank(),
                  legend.position='right',
                  legend.key.height=unit(2,'line'),
                  legend.margin=margin(t=0, r=.1, b=0, l=-.4, unit='in'),
                  legend.text=element_text(face='bold', size=11),
                  plot.title=element_text(hjust=.5, size = 15))


img <- image_graph(1024, 731, res = 150)

for (year in start:end) {
  
  day <- as.Date(paste(year, "6-30", sep = "-"))
  
  map_year <- my_cshp(day)
  
  map_year@data <- map_year@data %>% select(CNTRY_NAME, ISO1AL3)  # ISO 3 letter code is the common element in map_year and wb_year!
  
  map_year$ISO1AL3 <- as.character(map_year$ISO1AL3)
  
  wb_year <- wb_df %>% filter(date == year)  # for each i in year
  
  map_year$id <- map_year$CNTRY_NAME
  
  # polygons
  mapofyear <- fortify(model = map_year, region = "CNTRY_NAME")
  map_df <- mapofyear %>% join(map_year@data, by = "id")
  
  # breaks and labels
  breaks <- seq(-maximum, maximum, 2)
  labels <- breaks 
  labels[1] <- paste("u2264", labels[1])
  labels[length(labels)] <- paste("\u2265", labels[length(labels)])
  
  # map plot
  
  map <- map_df %>% 
    ggplot(aes(long, lat, group = group, fill = GDPgrowth)) +
    geom_polygon() +
    geom_path(color = "black", lwd = 0.1) +
    coord_fixed(1.2) +
    xlim(-170, 185) +
    ylim(-54, 78) +
    scale_fill_gradient2(mid = midcolor,
                        low = botcolor,
                        high = topcolor,
                        breaks = breaks,
                        limits = c(-maximum, maximum),
                        labels = labels) +
    guides(fill = guide_colorbar(title.position = "top", 
                                 title = NULL,
                                 direction = "vertical")) +
    annotate("text", x = -170, y = -48,
             label = "Source: World Bank\nCode: https://asljflsdjf",
             size = 3, hjust = 0) +
    themeops +
    ggtitle("Econ growht country/global", subtitle = "1961-2016")
  
  
  # timeline
  global <- wb_world[1:match(year, wb_world$date)] 
  
  timeline <- global %>% 
    ggplot(aes(date, GDPgrowth, group = country)) +
    annotate("rect", 
             ymin = -Inf, ymax = Inf,
             xmin = global$date - 0.5, xmax = global$date + 0.5,
             fill = global$col) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1965, 2016, 5),
                       limits = c(1960.5, 2016.5),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(-3, 7.5),
                       breaks = seq(-2, maximum, 2), 
                       expand = c(0, 0)) +
    expand_limits(x = 2016) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(face = "bold", size = 12))
    
  grid.arrange(map, timeline, ncol = 2, heights = c(2, 1))
  
  
}


animation <- image_animate(img, loop = 1, fps = 1)

image_write(animation, "growth.gif")




wb_df %>% filter(date == 1962)





global <- wb_world[1:match(1975, wb_world$date), ]
global <- wb_world[1:match(1962, wb_world$date), ]


global %>% glimpse()
global %>% filter(date == c(1961:1975))




# straight copy -----------------------------------------------------------

for (year in start:end){
  day = as.Date(paste(year, '6-30', sep='-'))
  map.year = my_cshp(day)
  ## drop unnecessary variables from cshape data
  map.year@data <- map.year@data %>% select(CNTRY_NAME, ISO1AL3)
  map.year$ISO1AL3 <- as.character(map.year$ISO1AL3)
  wb.year <- wb_df[wb_df$date == year, ]
  ## merge the two
  map.year@data <- join(map.year@data, wb.year, by='ISO1AL3', type='left')
  map.year$id <- map.year$CNTRY_NAME
  ## prepare polygons for plotting
  mapofyear <- fortify(model=map.year, region = 'CNTRY_NAME')
  map.df <- join(mapofyear, map.year@data, by='id')
  ## prepare breaks and labels for map plot
  breaks <- seq(-maximum, maximum, 2)
  labels <- breaks
  labels[1] <- paste('\u2264',labels[1])
  labels[length(labels)] <- paste('\u2265', labels[length(labels)])
  ## map plot
  map <- ggplot(map.df) +
    aes(long, lat, group=group, fill=GDPgrowth) +
    geom_polygon() +
    geom_path(color='black', lwd=.1) +
    coord_fixed(1.2) +
    xlim(-170, 185) +
    ylim(-54, 78) +
    scale_fill_gradient2(mid = midcolor,
                         low = botcolor,
                         high = topcolor,
                         breaks= breaks,
                         limits=c(-maximum, maximum),
                         labels= labels) +
    guides(fill=guide_colorbar(title.position='top',
                               title = NULL,
                               direction = 'vertical')) +
    annotate('text', x=-170, y=-48,
             label='Source: World Bank\nCode: https://git.io/vpYYG',
             size=3, hjust=0) +
    themeops +
    ggtitle('Economic Growth by Country and Global (GDPpc), 1961-2016')
  ## timeline plot
  global <- wb_world[1:match(year, wb_world$date), ]
  timeline <- ggplot(global,
                     aes(x=date, y=GDPgrowth, group=country)) +
    annotate('rect',
             ymin = -Inf,
             ymax = Inf,
             xmin = global$date - .5,
             xmax = global$date + .5,
             fill = global$col) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1965, 2016, 5),
                       limits=c(1960.5, 2016.5),
                       expand=c(0,0)) +
    scale_y_continuous(limits=c(-3, 7.5),
                       breaks=seq(-2, maximum, 2),
                       expand=c(0,0)) +
    expand_limits(x=2016) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text = element_text(face='bold', size=12))
  ## combine the two plots into one and print
  grid.arrange(map, timeline, nrow=2, heights=c(2,1))
}

animation <- image_animate(img, loop = 1, fps = 1)

image_write(animation, "growth.gif")
