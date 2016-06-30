
is_weekend <- function(day)
{
  if (day == 1 | day == 7) TRUE
  else FALSE
}

is_weekend <- Vectorize(is_weekend)

# calculates r^2 of a model predicted on a dataframe. Dataframe must contain column named count
r2 <- function(model, data)
{
  data$predicted <- predict(model, data)
  r2 <- cor(data$predicted, data$count)^2
  return(r2)
}


# calculates RMSE of a model predicted on a dataframe. Dataframe must contain column named count and predicted
RMSE <- function(model, data)
{
  data$predicted <- predict(model, data)
  RMSE <- sqrt(mean((data$count-data$predicted)^2))
  return(RMSE)
}

holidays14 <- as.Date(c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25"))
holidays15  <- as.Date(c("2015-01-01", "2015-01-19", "2015-02-16", "2015-05-25", "2015-07-03", "2015-09-07", "2015-10-12", "2015-11-11", "2015-11-26", "2015-12-25"))
