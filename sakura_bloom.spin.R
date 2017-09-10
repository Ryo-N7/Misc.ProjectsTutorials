# Sakura data
library(tidyverse)
library(stringr)
library(scales)

# Load data ---------------------------------------------------------------

sakura <- read.csv("~/R_materials/Kyoto_Flowers.csv")
View(sakura)

# Tidy and Clean ----------------------------------------------------------

# First data point doesn't show until 812 AD... just skip ahead to that.
sakura <- sakura %>% filter(AD %in% 812:2015)

# look at column names
colnames(sakura)

# colnames don't look very neat and tidy...
colnames(sakura) <- sakura %>% 
  colnames() %>% 
  str_to_lower() %>%                  # to lower case letters
  str_replace_all("\\.", "_")         # replace . with _

colnames(sakura)

# manually rename two of the columns...
colnames(sakura)[1] <- "year"
colnames(sakura)[2] <- "full_flowering_day_of_year"

# remove rows without flowering date
sakura <- sakura %>% filter(!is.na(full_flowering_date))

# turn three digit number into month and day values.
date_sep <- as.character(sakura$full_flowering_date) %>% 
  str_replace_all("(.{1})(.*)", "\\1.\\2") %>%            # split into two backreferences on the first digit, then place a .
  as.data.frame()

colnames(date_sep)[1] <- "date_fl"                        # properly name column
colnames(date_sep)

date_sep <- date_sep %>% separate(date_fl, c("month", "day"), "\\.")    # separate into 'month' and 'day' columns on .

sakura <- bind_cols(date_sep, sakura)   # combine date_sep into sakura
sakura <- sakura %>% select(-full_flowering_date, -full_flowering_day_of_year, -x, -data_type_code, -reference_name, -source_code)  # remove extraneous columns

library(lubridate)
# ?make_date()
# ?format()
# use make_date function to create separate variable in full date format
sakura <- sakura %>% 
  mutate(bloom = make_date(year, month, day))

# Reformat date variables into specific date formats:
sakura$Day_Of_Year <- as.numeric(format(sakura$bloom, "%j"))   #  %j: decimal day of the year
sakura$Year <- format(sakura$bloom, "%Y")                      #  %Y: 4 digit year
sakura$Month <- format(sakura$bloom, "%b")                     #  %b: abbreviated month
sakura$Day <- format(sakura$bloom, "%d")                       #  %d: decimal date

glimpse(sakura)
# date format are all in <chr>
# for plotting need to convert with as.numeric() for axes!
sakura$Year %>% as.numeric() %>% glimpse()

# Plotting ----------------------------------------------------------------

ggplot(sakura, aes(x = as.numeric(Year), y = Day_Of_Year)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))
# does not look very clear...

ggplot(sakura, aes(x = year, y = Day_Of_Year)) +  # or just use original 'year' variable...
  geom_point() +
  geom_smooth(span = 0.2, size = 3) +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b-%d"),
                     limits = c(84, 125))
# Could we make it more... sakura-y?

ggplot(sakura, aes(x = year, y = Day_Of_Year)) +  # or just use original 'year' variable...
  geom_point(shape = 8, size = 5, color = "pink") +
  geom_smooth(span = 0.2, color = "#dd1c77", fill = "red", size = 3) +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b-%d"),
                     limits = c(84, 125))
# Better! but does not look good on a drab grey background...

#### With background image!
library(jpeg)
library(grid)
library(gridExtra)
library(cowplot)

sakura_r <- function(df = sakura, xvar = 'as.numeric(Year)', yvar = 'Day_Of_Year') {
  img_url <- 'https://i.imgur.com/CgwU1zb.jpg'
  tmp_file <- tempfile()
  download.file(img_url, tmp_file, mode = "wb")
  img <- readJPEG(tmp_file)
  file.remove(tmp_file)
  
  rstr <- rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE)
  
  g <- ggplot(data = df)  + annotation_custom(rstr, -Inf, Inf, -Inf, Inf)
  g <- g + geom_point(aes_string(x = xvar, y = yvar), alpha = 0.8, color = "pink", shape = 8)
  g <- g + geom_smooth(aes_string(x = xvar, y = yvar), color = "#dd1c77", span = 0.2, size = 2.5, fill = "#f768a1", alpha = 0.7)
  g <- g + scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))
  g <- g + scale_x_continuous(limits = c(800, 2020), breaks = seq(800, 2000, 200))
  g <- g + labs(x = "Year", y = "Date of peak sakura bloom")
  g <- g + ggtitle("Sakura blooming", subtitle = "Date of sakura blossoming in Kyoto (800-2015 CE)")
  g <- g + theme(legend.position = "top", legend.background = element_rect(color = "black"),
                 axis.text.x = element_text(angle = 45, hjust = 1))
  return (g)
}
sakura_r()

