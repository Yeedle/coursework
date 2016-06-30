library(dplyr)
library(lubridate)

load("trips15.RData")
source("feature_functions.R")
load("model.Rdata")

weather <- weather15 %>% mutate(wday = wday(ymd, label = T), 
         weekend = is_weekend(wday(ymd)), 
         holiday = ymd %in% holidays14, 
         tavg = mean(c(tmin, tmax)))


weather$predicted <- predict(model, weather)

