########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(tripduration)) + geom_histogram() + xlim(0, 3600)
# plot the distribution of trip times by rider type
ggplot(trips, aes(tripduration, color=usertype), position="identity") + geom_density() + xlim(0, 3600) 
# plot the number of trips over each day
ggplot(trips, aes(ymd)) + geom_bar()
# plot the number of trips by gender and age
trips_groups <- trips %>% group_by(gender, birth_year) %>% summarize(count = n())
ggplot(trips_groups, aes(birth_year, count, color=as.factor(gender))) + geom_point() + xlim(1940, 2000)

# plot the ratio of male to female trips by age
# hint: use the spread() function to reshape things to make it easier to compute this ratio
trips_per_gender <- trips %>% group_by(birth_year, gender) %>% summarize(count = n()) %>% spread(gender, count)
male_to_female <- trips_per_gender %>% mutate(ratio = Male/Female)
ggplot(male_to_female, aes(birth_year, ratio)) + geom_line() + xlim(c(1950, 2000)) + ylim(c(0, 10))
########################################
# plot weather data
########################################
# plot the minimum temperature over each day
ggplot(weather, aes(ymd, tmin)) + geom_point()
# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting
weather_minmax <- gather(weather, "key", "val", tmin, tmax)
ggplot(weather_minmax, aes(ymd,val)) + geom_point()
ggplot(weather_minmax, aes(ymd,val)) + geom_ribbon(aes(ymin= val -10, ymax= val +10))
########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")
# plot the minimum temperature over each day

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
df <- group_by(trips_with_weather, ymd, tmin) %>% summarize(count = n()) 
ggplot(df, aes(tmin, count)) + geom_point()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
df <- df %>% mutate(rainy = prcp > 50) 
ggplot(df, aes(tmin, count, color=rainy)) + geom_point() 
# add a smoothed fit on top of the previous plot, using geom_smooth
ggplot(df, aes(tmin, count, color=rainy)) + geom_point() +geom_smooth()
# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
library(lubridate)
trip_hours <- trips %>% mutate(hour = hour(starttime)) %>% group_by(ymd, hour) %>% summarize(num_trip=n()) %>% group_by(hour) %>% summarize(mean_trips=mean(num_trip), sd = sd(num_trip))
# plot the above
ggplot(trip_hours, aes(hour, mean_trips)) + geom_point() + geom_errorbar(aes(ymin=mean_trips-sd, ymax =mean_trips+sd))
# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
