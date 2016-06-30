library(dplyr)
library(ggplot2)
library(glmnet)
library(lubridate)

load("../week1/citibike/trips.RData")
source("feature_functions.R")

trips_per_day <- trips %>% 
  group_by(ymd) %>% 
  summarize(count = n()) %>% 
  inner_join(weather, by="ymd") %>% 
  mutate(wday = wday(ymd, label = T), 
         weekend = is_weekend(wday(ymd)), 
         holiday = ymd %in% holidays14, 
         tavg = (tmin+tmax)/2)

formula <- as.formula(count ~ poly(tavg, 4)  + snwd + holiday + weekend + log(prcp + 0.01) + holiday:log(prcp+0.01))    

model <- lm(formula, trips_per_day)
save(model, file="model.Rdata")
