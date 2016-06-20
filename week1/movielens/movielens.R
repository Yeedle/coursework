# note: if working in RStudio, making sure to set working directory
# with Session -> Set working directory -> To source file location

library(dplyr)    # install.packages('dplyr')
library(ggplot2)  # install.packages('ggplot2')
library(scales)   # install.packages('scales')
library(readr)    # install.packages('readr')

# set plot theme
theme_set(theme_bw())

# read ratings from csv file
ratings <- read_csv('ratings.csv', col_names=FALSE)
colnames(ratings) <- c("user_id", "movie_id", "rating", "timestamp")
# for reference: same thing, using base R functions and explicitly setting column information
#   ratings <- read.delim('ratings.csv',
#                         sep=',',
#                         header=F,
#                         col.names=c('user_id','movie_id','rating','timestamp'),
#                         colClasses=c('integer','integer','numeric','integer'))

print(object.size(ratings), units="Mb")

####################
# brief look at data
####################

head(ratings)
nrow(ratings)
str(ratings)
summary(ratings)

####################
# aggregate stats
####################

# plot distribution of rating values (slide 13)

ratings %>% ggplot(aes(rating)) + geom_histogram()

####################
# per-movie stats
####################

# aggregate ratings by movie, computing mean and number of ratings
# hint: use the n() function for easy counting within a group
ratings_by_movie <- ratings %>% group_by(movie_id) %>% mutate(avg = mean(rating), count = n())
# plot distribution of movie popularity (= number of ratings the movie received)
# hint: try scale_x_log10() for a logarithmic x axis
movies_by_count <- ratings %>% group_by(movie_id) %>% summarize(count = n()) %>% arrange(desc(count))
ggplot(movies_by_count, aes(count)) + geom_histogram() 
# plot distribution of mean ratings by movie (slide 15)
# hint: try geom_histogram and geom_density

# rank movies by popularity and compute cdf (slide 17)
# hint: use dplyr's rank and arrange functions, and the base R sum and cumsum functions

# plot the CDF of movie popularity

####################
# per-user stats
####################

# aggregate ratings by user, computing mean and number of ratings

# plot distribution of user activity (= number of ratings the user made)
# hint: try a log scale here
