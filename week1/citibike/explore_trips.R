library(dplyr)

load('trips.RData')

# count the number of trips (= rows in the data frame)
trips %>% summarize(count = n())
summarize(trips, n() )

# find the earliest and latest birth years (see help for max and min to deal with NAs)
trips %>% summarize(max_birth_year = max(birth_year, na.rm = T), min_birth_year = min(birth_year, na.rm = T))
summarize(trips, max = max(birth_year, na.rm = TRUE), min = min(birth_year, na.rm = TRUE))
# use filter and grepl to find all trips that either start or end on broadway
trips %>% filter(grepl("Broadway", start_station_name) | grepl("Broadway", end_station_name))
View(filter(trips, grepl("Broadway", start_station_name) | grepl("Broadway",end_station_name)))
# do the same, but find all trips that both start and end on broadway
trips %>% filter(grepl("Broadway", start_station_name) & grepl("Broadway", end_station_name))
View(filter(trips, grepl("Broadway", start_station_name) & grepl("Broadway",end_station_name)))
# use filter, select, and distinct to find all unique station names
trips %>% select(end_station_name) %>% distinct() %>% View()
View(distinct(select(trips,end_station_name)))
# count trips by gender
trips %>% group_by(gender) %>% summarize(count = n()) %>% View()

# find the 10 most frequent station-to-station trips
trips %>% group_by(start_station_id, end_station_id) %>% summarize(count = n()) %>% ungroup() %>% arrange(desc(count)) %>% head(n=10) %>% View()
#count all trips that start and end on broadway
trips %>% filter(grepl("Broadway", start_station_name) & grepl("Broadway", end_station_name)) %>% summarize(count = n())
