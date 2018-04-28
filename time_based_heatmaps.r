library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)

# https://www.littlemissdata.com/blog/heatmaps
incidents <- read.table("../R_materials/Misc.ProjectsTutorials/Seattle_Police_Department_911_Incident_Response.csv",
                        fill = TRUE, header = TRUE, sep = ",", stringsAsFactors = FALSE)

col1 <- "#d8e1cf"
col2 = "#438484"


glimpse(incidents)
attach(incidents)


incidents$ymd <- mdy_hms(Event.Clearance.Date)
incidents$month <- month(incidents$ymd, label = TRUE)
incidents$year <- year(incidents$ymd)
incidents$wday <- wday(incidents$ymd, label = TRUE)
incidents$hour <- hour(incidents$ymd)

attach(incidents)

head(incidents)
glimpse(incidents)

## HEATMAP: Day/Hour

# create summary table for info in viz
# display number of incidents by hour + day of the week

dayHour <- ddply(incidents, c("hour", "wday"), summarize, N = length(ymd))

dayHour <- incidents %>% group_by(hour, wday) %>% summarize(N = length(ymd))

glimpse(dayHour)

# reverse order of months for easier graphing
dayHour$wday <- factor(dayHour$wday, levels = rev(levels(dayHour$wday)))
attach(dayHour)

glimpse(dayHour)

## Use geom_tile() for heatmap

dayHour %>% 
  ggplot(aes(hour, wday)) +
  geom_tile(aes(fill = N), color = "white", na.rm = TRUE) + 
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill = guide_legend(title = "Total Incidents")) +
  theme_bw() + theme_minimal() +
  labs(title = "Histogram of Seattle Incidents by Day of Week and Hour",
       x = "Incidents Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Year/Month

yearMonth <- ddply(incidents, c("year", "month"), summarize, N = length(ymd))

yearMonth <- incidents %>% 
  group_by(year, month) %>% 
  summarize(N = length(ymd)) 

yearMonth$month   # going from Jan to Dec

yearMonth$month <- factor(yearMonth$month, levels = rev(levels(yearMonth$month)))
attach(yearMonth)

glimpse(yearMonth)

yearMonth %>% 
  ggplot(aes(year, month)) +
  geom_tile(aes(fill = N), color = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  labs(title = "Histogram of Seattle Incidents by Month and Year",
       x = "Year", y = "Month") +
  guides(fill = guide_legend(title = "Total Incidents")) +
  theme_bw() + theme_minimal() 


## Group/Hour 

groupSummary <- incidents %>% group_by(Event.Clearance.Group, hour) %>% summarize(N = length(ymd))

glimpse(groupSummary)

groupSummary %>% 
  ggplot(aes(hour, Event.Clearance.Group)) +
  geom_tile(aes(fill = N), color = "white") +
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill = guide_legend(title = "Total Incidents")) +
  labs(title = "Histogram of Seattle Incidents by Event and Hour",
       x = "Hour", y = "Event") +
  theme_bw() + theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
















