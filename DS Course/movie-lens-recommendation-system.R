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

# Regularization to penalize the large estimate on movie effects with a small sample size -> penalize least square
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

movie_titles <- edx %>% select(movieId, title) %>% distinct()

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)
rmse_results <- rmse_results %>% add_row(method = "Regularized Movie effects",RMSE= RMSE(predicted_ratings, test_set$rating))
rmse_results

# Cross validation on lamda - the penalize terms
lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
#> `summarise()` ungrouping output (override with `.groups` argument)

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

# Redo the regularization with the best fit lambda

lamda <- 1.5
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

user_avgs_after_reg_avg <- train_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_avgs_after_reg_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)
rmse_results <- rmse_results %>% add_row(method = "Regularized Movie effects with cross validation with User effects updated",RMSE= RMSE(predicted_ratings, test_set$rating))
rmse_results


# Matrix Factorization for group of movie effects and group of user pattern
train_small <- train_set %>% 
  group_by(movieId) %>%
  filter(n() >= 150 | movieId == 3252) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 150) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# Removing colmean and rowmean -> convert them into residuals
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))

m_1 <- "Toy Story (1995)"
m_2 <- "Jumanji (1995)"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Toy Story (1995)"
m_3 <- "Grumpier Old Men (1995)"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "Toy Story (1995)" 
m_5 <- "Magnificent Seven, The (1960)" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

gridExtra::grid.arrange(p1, p2 ,p3, ncol = 3)

rmse_results

# Time effect

library(lubridate)

train_set <- train_set %>% mutate(date = as_datetime(timestamp))

train_set %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

test_set <- test_set %>% mutate(date = as_datetime(timestamp))

time_effect_wk <- train_set %>% mutate(date = round_date(date, unit = "week")) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_avgs_after_reg_avg, by='userId') %>%
  group_by(date) %>% 
  summarize(f_t = mean(rating - mu_hat - b_i - b_u))
  
time_effect_wk

train_set %>%  pull(date)

predicted_ratings <- test_set %>% mutate(date = round_date(date, unit ="week")) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(user_avgs_after_reg_avg, by='userId') %>%
  left_join(time_effect_wk, by='date') %>%
  mutate(pred = mu + b_i + b_u + f_t) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)
rmse_results <- rmse_results %>% add_row(method = "Added Weekly Time effects",RMSE= RMSE(predicted_ratings, test_set$rating))
rmse_results


# Genre Effect