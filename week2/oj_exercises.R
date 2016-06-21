library(readr)
library(ggplot2)
library(dplyr)

# 1) load the data
oj <- read_csv("oj.csv")

### 2) Visualizing price ###
# i) Make a plot of the distribution of prices.
ggplot(oj, aes(price)) + geom_histogram()

# ii) Make a plot of the distribution of prices using a log10 scale
ggplot(oj, aes(price)) + geom_histogram() + scale_x_log10()

# iii) Repeat i), faceted by brand.
ggplot(oj, aes(price)) + geom_histogram() + facet_wrap(~ brand)
# iv) Repeat ii) , faceted by brand
ggplot(oj, aes(price)) + geom_histogram() + scale_x_log10() + facet_wrap(~ brand)
# v)  What do these graphs tell you about the variation in price? 
#     Why do the log plots look different? 
#     Do you find them more/less informative?

### 3) Visualizing the price/quantity relationship ###
# i) Plot logmove (the log of quantity sold) vs. log price. 
oj <-mutate(oj, logprice = log(price))
ggplot(oj, aes(logprice, logmove)) + geom_point()
# ii) Color each point by brand.
ggplot(oj, aes(logprice, logmove, color=brand)) + geom_point()
#     What do insights can you derive that were not apparent before?

### 4) Estimating the relationship
# i)  Do a regression of logmove on log price. 
#     How well does the model fit? 
#     What is the elasticity (the coefficient on log price), and does it make sense?
model <- lm(oj$logmove ~ oj$logprice)
summary(model)

# ii) Now add in an intercept term for each brand (by adding brand to the regression formula). 
#     How do the results change? 
#     How should we interpret these coefficients?
model <- lm(oj$logmove ~ oj$logprice + oj$brand)
summary(model)

# iii)  Now add interaction terms to allow the elasticities to differ by brand, by including a brand:log price term in the regression formula. 
#       Note the estimate coefficients will "offset" the base estimates. 
#       What is the insights we get from this regression? 
#       What is the elasticity for each firm? 
#       Do the elasticities make sense?
model <- lm(oj$logmove ~ oj$logprice + oj$brand + oj$brand:oj$logprice)
summary(model)

### 5) Impact of "featuring in store"
# i) Which brand is featured the most? Make a plot to show this.
oj_by_brand <- oj %>% filter(feat ==1) %>% group_by(brand)
ggplot(oj_by_brand, aes(brand)) + geom_bar()
# ii) How should we incorporate the "featured in store" variable into our regression? 
#     Start with an additive formulation (e.g. feature impacts sales, but not through price).
model <- lm(oj$logmove ~ oj$feat)
summary(model)
ggplot(oj, aes(logprice, logmove))
# iii) Now run a model where features can impact sales and price sensitivity.
model <- lm(oj$logmove ~ oj$logprice + oj$feat)
summary(model)

# iv) Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. 
model <- lm(oj$logmove ~ oj$brand:oj$logprice + oj$brand:oj$feat)
summary(model)

# ..  Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).
