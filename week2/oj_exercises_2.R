library(readr)
library(ggplot2)
library(dplyr)

oj <- read_csv("oj.csv")
oj <- mutate(oj, sales = exp(1)^logmove)
#Let’s return to the orange juice assignment and investigate how store demographics are related to demand.
#Let’s start with the following model: logmove ~ log(price)*brand*feat and add in the store demographics as linear features (e.g., + AGE60 + EDUC + ETHNIC + INCOME). 
#Try them individually and then all together.
model <- lm(logmove ~ log(price)*brand*feat+ AGE60 +  HHLARGE+WORKWOM+HVAL150+SSTRDIST+SSTRVOL+CPDIST5   +CPWVOL5, oj)
summary(model)
oj$predicted <- fitted(model)

#What demographics are significantly (t > 2 standard deviations) related to demand?
#How much did the adjusted R-squared improve with the addition of these variables?
#Let’s focus on two variables HHLARGE ("fraction of households that are large") and EDUC ("fraction of shoppers with advanced education").
model <- lm(logmove ~ log(price)*HHLARGE + log(price)*EDUC , oj)
summary(model)
oj$predicted <- fitted(model)
#What are the means and percentiles of each of these variables?
mean(oj$HHLARGE)
quantile(oj$HHLARGE, c(0.25, .5, .75))
mean(oj$EDUC)
quantile(oj$EDUC, c(.25, .5, .75))
#Using your coefficient estimates from the regression in 1b:
#If we move from the median value of HHLARGE to the 75th percentile (3rd quartile), how much does logmove change each week on average? You can estimate this visually if you plot the fitted model, or you can compare the predicted values for rows that have the median and 75th percentiles for HHLARGE.
filtered<- filter(oj, HHLARGE == median(HHLARGE))
hhlarge_median_mean_predicted <- mean(filtered$predicted)

filtered<- filter(oj, HHLARGE == quantile(HHLARGE, .75))
hhlarge_75th_mean_predicted <- mean(filtered$predicted)
#If we move from the median value of EDUC to the 75th percentile (3rd quartile), how much does logmove change each week on average?
filtered <- filter(oj, EDUC == median(EDUC))
educ_median_mean_predicted <- mean(filtered$predicted)

filtered <- filter(oj, EDUC == quantile(EDUC, .75))
educ_75th_mean_predicted <- mean(filtered$predicted)
#Based on this analysis, which is the more important predictor of demand?
hhlarge_75th_mean_predicted - hhlarge_median_mean_predicted
educ_75th_mean_predicted - educ_median_mean_predicted
#Now let’s see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the model to test this.
model <- lm(logmove ~ log(price)*HHLARGE + log(price)*EDUC, oj)
summary(model)
#What are the coefficients on the interaction terms?
coef(model)
#Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. Do your estimates make sense based on your intuition?
#What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?
#Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity change? Based on this, which is more important to price sensitivity?
#You should notice that the coefficients on EDUC and HHLARGE have flipped sign once we include interaction terms with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures what is going on.


#Let’s split our data into a training set and a test set. 
#An easy way to do this is with the sample command. 
#The following will randomly select 20% of the rows in our data frame: 
indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
#Now let’s use this index to create a training and a test set, try: 
OJtest <- oj[indexes, ]
Ojtrain <- oj[-indexes, ]
#What did this do? How many rows does the test set have? 
#How many rows does the training set have?
#Now let’s run the very simple model logmove ~ log(price) + brand on the training data.
model <- lm(logmove ~ log(price) + brand, Ojtrain)
summary(model)
#Use LM on this model and report the R-squared. A: 0.3967
#Use predict(model, Ojtest) to predict log sales for the test set.
OJtest$predicted <- predict(model, OJtest)
#Compute cor(predicted_sales,logmove)^2 on the test set. This is our "honest R-squared". 
cor(OJtest$predicted, OJtest$logmove)^2
#How does it compare to the value in (a)?
#A: 0.3836859
#Now let’s run better models.
#Run our "previous favorite" logmove ~ brand*log(price)*feat on the training data. 
#Use LM to get regular R-squared. 
model <- lm(logmove ~ brand*log(price)*feat, Ojtrain)
summary(model)
# Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?
OJtest$predicted <- predict(model, OJtest)
cor(OJtest$predicted, OJtest$logmove)^2
#Now add in all the demographics. What is the regular R-squared on training data? A: 0.6402
#What is the honest R-squared on the test set? A: 0.6098247
model <- lm(logmove ~ log(price)*brand*feat*AGE60*EDUC*ETHNIC*INCOME*HHLARGE, Ojtrain)
OJtest$predicted <- predict(model, OJtest)
cor(OJtest$predicted, OJtest$logmove)^2
