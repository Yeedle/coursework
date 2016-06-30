library(dplyr)
library(lubridate)

source("feature_functions.R")
load("model.Rdata")

#############################
# predict for 2015
#############################

# define a function to turn strings into datetimes (written by Jake Hofman)
parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}

# read the csv
weather <- read.table('weather_2015.csv', header=T, sep=',')

# extract just a few columns, lowercase column names, and parse dates
weather <- select(weather, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather) <- tolower(names(weather))
weather <- mutate(weather,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather <- tbl_df(weather)


# add columns needed for prediction
weather <- weather %>% mutate(wday = wday(ymd, label = T), 
         weekend = is_weekend(wday(ymd)), 
         holiday = ymd %in% holidays14, 
         tavg = mean(c(tmin, tmax)))


# predict...
weather$predicted <- predict(model, weather)

