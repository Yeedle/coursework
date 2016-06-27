library(dplyr)
library(ggplot2)
library(lubridate)

load("../week1/citibike/trips.RData")
# In this assignment we'll predict number of trips per day as a function of the weather on that day.
# 1) Create a data frame with one row for each day, the number of trips taken on that day, and the minimum temperature on that day.
is_weekend <- function(day)
{
  if (day == 1 | day == 7) TRUE
  else FALSE
}
is_weekend(c(1,2,3))
is_weekend <- Vectorize(is_weekend)
trips_per_day <- trips %>% group_by(ymd) %>% summarize(count = n()) %>% inner_join(weather, by="ymd") %>% mutate(wday = wday(ymd, label = T), weekend = is_weekend(wday(ymd)))

#2) Split the data into a randomly selected training and test set, as in the above exercise, with 80% of the data for training the model and 20% for testing.
indexes <- sample(1:nrow(trips_per_day), size=0.2*nrow(trips_per_day))
trips_test <- trips_per_day[indexes, ]
trips_train <- trips_per_day[-indexes, ]
# 3) Fit a model to predict the number of trips as a (linear) function of the minimum temperature, and evaluate the fit on the training and testing data sets. 
model <- lm(count ~ tmin, trips_train)
summary(model)
trips_train$predicted_count <-fitted(model)
cor(trips_train$predicted_count, trips_train$count)^2
trips_test$predicted_count <- predict(model, trips_test)
cor(trips_test$predicted_count, trips_test$count)^2
#Do this first visually by plotting the predicted and actual values as a function of the minimum temperature. Then do this with R^2, as above. You'll want to use the predict and cor functions for this.
ggplot(trips_train, aes(tmin, count)) + geom_point() + geom_line(aes(tmin, predicted_count)) 
ggplot(trips_test, aes(tmin, count)) + geom_point() + geom_line(aes(tmin, predicted_count))
#   Repeat this procedure, but add a quadratic term to your model (e.g., + tmin^2, or equivalently + poly(k,2)). How does the model change, and how do the fits between the linear and quadratic models compare?
model <- lm(count ~ poly(tmin, 2), trips_train)
summary(model)
trips_train$predicted_count <-fitted(model)
cor(trips_train$predicted_count, trips_train$count)^2
trips_test$predicted_count <- predict(model, trips_test)
cor(trips_test$predicted_count, trips_test$count)^2
ggplot(trips_train) + geom_point(aes(tmin, count), color="red") + geom_point(data=trips_test,aes(tmin,count)) +geom_line(data=trips_train, aes(tmin, predicted_count)) 
ggplot(trips_test, aes(tmin, count)) + geom_point() + geom_line(aes(tmin, predicted_count))
#  Now automate this, extending the model to higher-order polynomials with a for loop over the degree k. 
#For each value of k, fit a model to the training data and save the R^2 on the training data to one vector and test vector to another. 
#Then plot the training and test R^2 as a function of k. What value of k has the best performance?
test_cor <- vector()
train_cor <- vector()
for (k in 1:10)
{
  model <- lm(count ~ poly(tmin, k), trips_train)
  trips_train$predicted_count <-fitted(model) 
  trips_test$predicted_count <- predict(model, trips_test)
  train_cor[k] <- cor(trips_train$predicted_count, trips_train$count) 
  test_cor[k] <- cor(trips_test$predicted_count, trips_test$count) 
}
df <- data_frame(k=1:10, train = train_cor^2, test = test_cor^2)
ggplot(df, aes(as.factor(k), train)) + geom_point(color="red") + geom_line(aes(k, train), color="red") + geom_point(aes(y=test), color="blue") + geom_line(aes(k, test), color="blue")
# Finally, fit one model for the value of k with the best performance in 6), and plot the actual and predicted values for this model.
model <- lm(count ~ poly(tmin, 3), trips_train)
summary(model)
trips_train$predicted_count <-fitted(model)
cor(trips_train$predicted_count, trips_train$count)^2
trips_test$predicted_count <- predict(model, trips_test)
cor(trips_test$predicted_count, trips_test$count)^2
ggplot(trips_train) + geom_point(aes(tmin, count), color="red") + geom_point(data=trips_test,aes(tmin,count), color="blue") +geom_line(data=trips_train, aes(tmin, predicted_count)) 
ggplot(trips_test, aes(tmin, count)) + geom_point() + geom_line(aes(tmin, predicted_count))

## DAY 5: ##
r2andRmse <- function(train, test, model)
{
  trips_train$predicted <- fitted(model)
  trips_test$predicted <- predict(model, trips_test)
  cat(paste("r^2 for train data:\t" , cor(train$predicted, train$count)^2),
      paste("r^2 for test data:\t" , cor(test$predicted, test$count)^2),
      paste("RMSE for train data:\t", sqrt(mean((train$count-train$predicted)^2))),
      paste("RMSE for test data:\t", sqrt(mean((test$count-test$predicted)^2))),
      sep = "\n")
}

plotmodel <- function(data, model)
{
  data$predicted <- predict(model, data)
  ggplot(data, aes(predicted, count)) + geom_point() + geom_line(aes(predicted, predicted))
}

plotyear <- function(data, model)
{
  data$predicted <- predict(model, data)
  ggplot(data, aes(ymd, count)) + geom_point() + geom_line(aes(ymd, predicted)) 
}
# You can use any features you like that are available prior to the day in question, ranging from the weather, to the time of year and day of week, to activity in previous days or weeks, but don't cheat and use features from the future (e.g., the next day's trips).
# As usual, split your data into training and testing subsets and evaluate performance on each.
# Quantify your performance in two ways: R^2 (or the square of the correlation coefficient), as we've been doing, and with root mean-squared error.
#     Report the model with the best performance on the test data. Watch out for overfitting.
#     Plot your final best fit model in two different ways. First with the date on the x-axis and the number of trips on the y-axis, showing the actual values as points and predicted values as a line. 
#     Second as a plot where the x-axis is the predicted value and the y-axis is the actual value, with each point representing one day.
#     Inspect the model when you're done to figure out what the highly predictive features are, and see if you can prune away any negligble features that don't matter much.

# number of rides on tmax
model <- lm(count ~ tmax, trips_train)
r2andRmse(trips_train, trips_test, model)
plotyear(trips_n_weather, model)
plotmodel(trips_n_weather, model)

#number of rides on tmin
model <- lm(count ~ tmin, trips_train)
r2andRmse(trips_train, trips_test, model)
plotyear(trips_n_weather, model)
plotmodel(trips_n_weather, model)

# number of rides by day of year
test_cor <- vector()
train_cor <- vector()
for (k in 1:20)
{
  model <- lm(count ~ poly(ymd, k), trips_train)
  trips_train$predicted_count <-fitted(model) 
  trips_test$predicted_count <- predict(model, trips_test)
  train_cor[k] <- cor(trips_train$predicted_count, trips_train$count) 
  test_cor[k] <- cor(trips_test$predicted_count, trips_test$count) 
}
df <- data_frame(k=1:20, train = train_cor^2, test = test_cor^2)
ggplot(df, aes(as.factor(k), train)) + geom_point(color="red") + geom_line(aes(k, train), color="red") + geom_point(aes(y=test), color="blue") + geom_line(aes(k, test), color="blue")
r2andRmse(trips_train, trips_test, model)
plotyear(trips_per_day, model)
plotmodel(trips_per_day, model)


#number of rides on percipitation
model <- lm(count ~ prcp, trips_train)
r2andRmse(trips_train, trips_test, model)
#number of rides on tmin+prcp
model <- lm(count ~ tmin*prcp, trips_train)
r2andRmse(trips_train, trips_test, model)

# number of rides on tmax*prcp
model <- lm(count ~ tmax*prcp, trips_train)
r2andRmse(trips_train, trips_test, model)

model <- lm(count ~ weekend, trips_train)
r2andRmse(trips_train, trips_test, model)
plotyear()