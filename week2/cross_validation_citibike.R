library(dplyr)
library(ggplot2)

load("../week1/citibike/trips.RData")

# In this assignment we'll predict number of trips per day as a function of the weather on that day.
# 1) Create a data frame with one row for each day, the number of trips taken on that day, and the minimum temperature on that day.

trips_per_day <- trips %>% group_by(ymd) %>% summarize(count = n())
trips_n_weather <- inner_join(trips_per_day, weather, by="ymd")
#2) Split the data into a randomly selected training and test set, as in the above exercise, with 80% of the data for training the model and 20% for testing.
indexes <- sample(1:nrow(trips_n_weather), size=0.2*nrow(trips_n_weather))
trips_test <- trips_n_weather[indexes, ]
trips_train <- trips_n_weather[-indexes, ]
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
