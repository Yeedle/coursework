library(dplyr)
library(lubridate)
load("trips15.RData")
source("feature_functions.R")
load("model.Rdata")

#############################
# predict for 2015
#############################

# define a function to turn strings into datetimes (written by Jake Hofman)


# add columns needed for prediction

trips_per_day15 <- trips15 %>% 
  group_by(ymd) %>% 
  summarize(count = n()) %>% 
  inner_join(weather15, by="ymd") %>% 
  mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)), holiday = ymd %in% holidays15, tavg = (tmin+tmax)/2)

df <- weather15 %>% mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)), holiday = ymd %in% holidays15, tavg = (tmin+tmax)/2)
 

# predict...
df$predicted <- predict(model, df)
sqrt(mean((df$count-df$predicted)^2))
ggplot(df, aes(ymd, predicted)) + geom_point()
ggplot(trips_per_day15, aes(predicted, count)) + geom_point()
