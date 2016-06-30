library(dplyr)
library(ggplot2)
library(glmnet)
library(lubridate)

load("../week1/citibike/trips.RData")
load("trips15.RData")
source("feature_functions.R")


trips_per_day14 <- trips %>% 
  group_by(ymd) %>% summarize(count = n()) %>% 
  inner_join(weather, by="ymd") %>% 
  mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)), holiday = ymd %in% holidays14, tavg = (tmin+tmax)/2, level = weather_level(tmax))


trips_per_day15 <- trips15 %>% 
  group_by(ymd) %>% summarize(count = n()) %>% 
  inner_join(weather15, by="ymd") %>% 
  mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)), holiday = ymd %in% holidays15, tavg = (tmin+tmax)/2, level = weather_level(tmax))


#split the data into testing and training
tests <- sample(1:nrow(trips_per_day14), size=0.2*nrow(trips_per_day14))
trains <- -tests

formula <- as.formula(count ~ poly(tavg, 4)  + log(snwd + 0.01) + log(snow + 0.01) + snow:weekend + snwd:weekend + log(prcp + 0.01) + weekend + holiday + weekend:log(prcp + 0.01) + holiday:log(prcp + 0.01) + tavg:log(prcp + 0.01))

model <- lm(formula, trips_per_day14[trains, ])
trips_per_day14$predicted <- predict(model, trips_per_day14)
r2(model, trips_per_day14)
RMSE(model, trips_per_day14[tests, ])
trips_per_day15  <- predict(model, trips_per_day15)

## glmnet



x <- model.matrix(formula, trips_per_day14[trains, ])
y <- trips_per_day14[trains,]$count

model <- cv.glmnet(x, y)
X_plot <- model.matrix(formula, trips_per_day14[tests, ])
trips_per_day14$predicted <- as.numeric(predict(model, newx=X_plot, s="lambda.min"))

sqrt(mean((trips_per_day14$count-trips_per_day14$predicted)^2))
