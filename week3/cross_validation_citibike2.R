library(dplyr)
library(ggplot2)
library(glmnet)
library(lubridate)

load("../week1/citibike/trips.RData")
load("trips15.RData")

is_weekend <- function(day)
{
  if (day == 1 | day == 7) TRUE
  else FALSE
}

weather_level <-function(temp)
{
  if (temp < 4.5)
    "cold"
  else if (temp <= 7)
    "fine"
  else
    "hot"
}

is_weekend <- Vectorize(is_weekend)
weather_level <- Vectorize(weather_level)

# calculates r^2 of a model predicted on a dataframe. Dataframe must contain column named count
r2 <- function(model, data)
{
  data$predicted <- predict(model, data)
  r2 <- cor(data$predicted, data$count)^2
  return(r2)
}


# calculates RMSE of a model predicted on a dataframe. Dataframe must contain column named count
RMSE <- function(model, data)
{
  data$predicted <- predict(model, data)
  RMSE <- sqrt(mean((data$count-data$predicted)^2))
  return(RMSE)
}

holidays14 = as.Date(c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25"))
holidays15 = as.Date(c("2015-01-01", "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07", "2015-10-12", "2015-11-11", "2015-11-26", "2015-12-25"))

trips_per_day14 <- trips %>% 
  group_by(ymd) %>% summarize(count = n()) %>% 
  inner_join(weather, by="ymd") %>% 
  mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)), holiday = ymd %in% holidays14, tavg = (tmin+tmax)/2, level = weather_level(tmax))


trips_per_day15 <- trips15 %>% 
  group_by(ymd) %>% summarize(count = n()) %>% 
  inner_join(weather15, by="ymd") %>% 
  mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)), holiday = ymd %in% holidays15, tavg = (tmin+tmax)/2, level = weather_level(tmax))
formula <- count ~ poly(tavg, 4)  + snwd+ log(snow + 0.01) + snow:weekend + snwd:weekend + log(prcp + 0.01) + weekend + holiday + weekend:log(prcp + 0.01) + holiday:log(prcp + 0.01) + tavg:log(prcp + 0.01)

model <- lm(formula, trips_per_day14)
trips_per_day14$predicted <- predict(model, trips_per_day14)
r2(model, trips_per_day14)
RMSE(model, trips_per_day14)
trips_per_day15  <- predict(model, trips_per_day15)

## glmnet
#split the data into testing and training
tests <- sample(1:nrow(trips_per_day14), size=0.2*nrow(trips_per_day14))
trains <- -tests


x <- model.matrix(formula, trips_per_day14[trains, ])
y <- trips_per_day14[trains,]$count

model <- cv.glmnet(x, y)
X_plot <- model.matrix(formula, trips_per_day14[tests, ])
trips_per_day14$predicted <- as.numeric(predict(model, newx=X_plot, s="lambda.min"))

sqrt(mean((trips_per_day14$count-trips_per_day14$predicted)^2))
