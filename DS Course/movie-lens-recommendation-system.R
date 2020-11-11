##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines("DS Course/capstone/ml-10m/ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines("DS Course/capstone/ml-10m/ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
 #                                          genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# rm(dl, ratings, movies, test_index, temp, movielens, removed)
rm(ratings, movies, test_index, temp, movielens, removed)

edx %>% filter(rating == 3) %>% count()

# No of Users and No of Movies
edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId))

edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
edx %>% filter(grepl("Drama", genres)) %>% count()
edx %>% filter(grepl("Comedy", genres)) %>% count()
edx %>% filter(grepl("Thriller", genres)) %>% count()
edx %>% filter(grepl("Romance", genres)) %>% count()

edx %>% group_by(title) %>% summarise(n = n()) %>% arrange(-n)
edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(-n)

# Create Training set and Test set from edx
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Remove entrices from test_set that do not appear in test_set by semi_join
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Define function calculating the RMSE 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Build the prelim model with average rating of a movie from a user
mu_hat <- mean(train_set$rating)

# RMSE of the first model
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# Create a result tables for rmse alongside the improvement of the model 
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Modeling movie effects - movie bias
# fit <- lm(rating ~ as.factor(movieId), data = movielens)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings <- mu_hat + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings, test_set$rating)

rmse_results <- rmse_results %>% add_row(method = "Added Movie effects", RMSE = RMSE(predicted_ratings, test_set$rating))

# User effects - user bias
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)

rmse_results <- rmse_results %>% add_row(method = "Added User effects", RMSE = RMSE(predicted_ratings, test_set$rating))
rmse_results
