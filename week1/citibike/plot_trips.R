########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(tripduration)) + geom_histogram() + xlim(0, 3600*3)
# plot the distribution of trip times by rider type
ggplot(trips, aes(tripduration, color=usertype), position="identity") + geom_density() + xlim(0, 3600) 
# plot the number of trips over each day
ggplot(trips, aes(ymd)) + geom_bar()
# plot the number of trips by gender and age
trips_groups <- trips %>% group_by(gender, birth_year) %>% summarize(count = n())
ggplot(trips_groups, aes(birth_year, count, color=as.factor(gender))) + geom_point() + xlim(1940, 2000)
########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the minimum temperature over each day
ggplot(weather, aes(ymd, tmin)) + geom_point()
# plot the number of trips as a function of the minimum temperature, where each point represents a day
df <- group_by(trips_with_weather, ymd, tmin) %>% summarize(count = n()) 
ggplot(df, aes(tmin, count)) + geom_point()
# you'll need to summarize the trips and join to the weather data to do this

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# add a smoothed fit on top of the previous plot, using geom_smooth