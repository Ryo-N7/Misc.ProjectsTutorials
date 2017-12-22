library(OECD)
library(dplyr)
library(ggplot2)
library(scales)

# http://stats.oecd.org
# look for dataset
search_dataset("health")

dataset <- "HEALTH_STAT"
filter <- list(c("EVIE+EVIETOTA.EVIDUREV.AUS+AUT+BELF/all?startTime=1960&endTime=2016"))

Life_Exp_OECD <- get_dataset("HEALTH_STAT",
                   filter = list(c("FRA", "DEU"),
                                 c("EVIE", "EVIETOTA.EVIDUREV")),
                   start_time = 2000, end_time = 2010)


get_dataset("HEALTH_STAT",
            filter = "EVIE+EVIEFE00+EVIEFE40+EVIEFE60+EVIEFE65+EVIEFE80+EVIEHO00+EVIEHO40+EVIEHO60+EVIEHO65+EVIEHO80+EVIETOTA.EVIDUREV+EVIFHOEV+EVIHFEEV.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LTU+RUS+ZAF/all?startTime=1960&endTime=2016",
            pre_formatted = TRUE,
            start_time = 2010, end_time = 2011)





df <- get_dataset("EPL_OV",
                  filter = list(c("DEU", "FRA"),
                                c("EPRC_V1", "EPRC_V2")),
                  start_time = 2008, end_time = 2010)

"EVIE+EVIEFE00+EVIEFE40+EVIEFE60+EVIEFE65+EVIEFE80+EVIEHO00+EVIEHO40+EVIEHO60+EVIEHO65+EVIEHO80+EVIETOTA.EVIDUREV+EVIFHOEV+EVIHFEEV.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LTU+RUS+ZAF/all?startTime=1960&endTime=2016"


get_dataset("EPL_OV",
            filter = list(c("JPN", "KOR"),
                          c("EPRC_V1", "EPRC_V2")),
            start_time = 2005, end_time = 2010)










