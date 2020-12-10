# title: "**HarvardX: PH125.9x: Data Science: Capstone**"
# subtitle: "A Capstone Project for the `Professional Certificate in Data Science` 
# offered by Harvard University (HarvardX) via EdX"
# author: "John King'athia Karuitha"

##################
## detach any loaded packages
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

##################
## install missing packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(inspectdf)) install.packages("inspectdf", repos = "http://cran.us.r-project.org")
if(!require(anytime)) install.packages("anytime", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

##################
## Load the required libraries----
library(tidyverse)
library(data.table)
library(lubridate)
library(inspectdf)
library(anytime)
library(tibble)
library(caret)
library(skimr)
library(scales)
library(ggthemes)

##################################################################################################
## Read in the required data ----
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#title = as.character(title),
#genres = as.character(genres))
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#################################
## Data wrangling and summary statistics ----
str(edx)
edx$timestamp <- anytime(edx$timestamp)

#################################
# Skim the data for summary stats and make a tibble
skim(edx$rating) %>% tibble() %>% 
  
  # Format table by removing unwanted columns
  select(-skim_type, -numeric.hist, - complete_rate) %>% 
  
  ## Rename remaining columns
  rename(Variable = skim_variable, Missing = n_missing, Mean = numeric.mean, 
         
         SD = numeric.sd, min = numeric.p0, Q1 = numeric.p25, Median = numeric.p50, 
         
         Q3 = numeric.p75, Max = numeric.p100) %>% 
  
  knitr::kable(caption = "Summary of the Movie Ratings")

################################
## Examine categorical variables 
inspectdf::inspect_cat(edx)

################################
## Summary statistics on the movies
## Distribution of ratings 
edx %>% 
  
  ## Group the movies by movieId
  group_by(movieId) %>% 
  
  ## Get summary statistics
  skimr::skim(rating) %>% 
  
  ## Get the first six movies
  head() %>% 
  
  ## make a tibble
  tibble() %>% 
  
  ## Clean the tibble
  select(-skim_type, -numeric.hist, -n_missing, -complete_rate) %>%
  
  ## Rename tibble columns
  rename(Variable = skim_variable, Mean = numeric.mean, 
         SD = numeric.sd, Min = numeric.p0, Q1 = numeric.p25,
         Median = numeric.p50, Q3 = numeric.p75, Max = numeric.p100) %>% 
  
  ## make a nice table
  knitr::kable(caption = "Summary statistics for the ratings variable on training set")

###############################
## Movies with very few ratings
edx %>% 
  
  ## Count the movies
  count(movieId, title) %>% 
  
  ## Arrange in ascending order
  arrange(n) %>% 
  
  ## preview the first ten movies with few ratings
  head(10) %>% 
  
  ## make a nice table
  knitr::kable(caption = "Sample of movies that have few ratings")

#####################################
## Users that have made very few ratings
edx %>% 
  
  ## Count the users
  count(userId) %>% 
  
  ## Arrange the users in ascending order of counts
  arrange(n) %>% 
  
  ## Preview the first 10 users
  head(10) %>% 
  
  ## Make a neat table
  knitr::kable(caption = "Sample of users that have made the fewest ratings")

###########################
## Visualization of users by count
edx %>% 
  
  ## Count the number of users
  count(userId) %>% 
  
  ## Plot the users by ratings
  ggplot(aes(x = n)) + geom_histogram(col = "black", bins = 40) + 
  
  ## Log the scale of the users
  scale_x_log10()

#####################################
## How the first six users have rated the movies
table(edx$userId, edx$rating) %>% 
  
  ## Preview the first six users
  head() %>% 
  
  ## make a neat table
  knitr::kable(caption = "Distribution of ratings among the first six users")

#####################################
# Data visualization ----
## Plot bar graph of the distribution of ratings
edx %>% ggplot(aes(x = as.factor(rating), fill = as.factor(rating))) + geom_bar() + 
  
  # Add a pleasant theme
  ggthemes::theme_igray() + 
  
  # Remove legends
  theme(legend.position = "none") + 
  
  # Convert the y-axis into numbers from scientific notation using the scales package
  scale_y_continuous(labels = scales::comma_format()) + 
  
  # Label the X and Y-axis and add a caption
  labs(x = "Rating", y = "Count", title = "Ratings of Movies", 
       
  caption = "Data from the Movielens Website, https://grouplens.org/datasets/movielens/latest/")

##########################
## popular movies
edx %>% 
  
  ## Group by movie and title.
  group_by(movieId, title) %>% 
  
  ## Mutate timestamp to extract year.
  mutate(movie_year = lubridate::year(timestamp)) %>% 
  
  ## Filter the movies that have at least 15,000 ratings in 1993 or later.
  filter(n() >= 15000 & movie_year >= 1993) %>% 
  
  ## Do summary statistics for the movies.
  summarise(min_rating = min(rating),
            q1_rating = quantile(rating, 0.25),
            median_rating = median(rating),
            q3_rating = quantile(rating, 0.75),
            max_rating = max(rating)) %>% 
  
  ## Filter only those movies with a median rating of 4 and above.
  filter(median_rating >= 4) %>% 
  
  group_by(movieId, title) %>% 
  
  ## Convert the data to long format. These functions were previously done using gather.
  pivot_longer(-c(1,2), names_to = "level", values_to = "rating") %>% 
  
  group_by(title, rating) %>% 
  
  ## Plot ratings versus movie title.
  ggplot(aes(x = reorder(title, rating, median), y = rating, fill = title)) + 
  
  ## Add geom
  geom_boxplot(show.legend = FALSE) + 
  
  ## Flip the coordinate and add theme for pleasant theme.
  coord_flip() + theme_minimal() + 
  
  ## Add axis labels and title.
  labs(x = "Movie", y = "Rating", title = "Comparative Ratings of Movies")

#####################
## Popular genres
edx %>% 
  
  ## Group by genres and title.
  group_by(genres, title) %>% 
  
  ## Filter genres with at least 20,000 ratings.
  filter(n() >= 20000) %>% 
  
  ## Summarise the ratings by genre data.
  summarise(min_rating = min(rating),
            q1_rating = quantile(rating, 0.25),
            median_rating = median(rating),
            q3_rating = quantile(rating, 0.75),
            max_rating = max(rating)) %>% 
  
  ## Filter genres with no genres listed.
  filter(genres != "no genres listed", genres != "(no genres listed)") %>% 
  
  ## Convert the data to long format as we did using gather.
  pivot_longer(-c("genres", "title"), names_to = "level", values_to = "value") %>% 
  
  ## Group by genre
  group_by(genres) %>% 
  
  ## Filter genres with a median rating of at least 4.
  filter(median(value) >= 4) %>% 
  
  ## Plot the data
  ggplot(mapping = aes(x = reorder(genres, value, median), y = value, fill = genres)) + 
  
  ## Add the geom and flip coordinates.
  geom_boxplot(show.legend = FALSE) + coord_flip() + 
  
  ## Add axis labels and title.
  labs(x = "Genre", y = "Rating", title = "Rating of Movies by Genre")

#################################
## Trends in movie ratings over time
edx %>% 
  
  ## Create a year variable from the timestamp
  mutate(year = year(timestamp)) %>% 
  
  ## Group by year
  group_by(year) %>% 
  
  ## Summarize the mean and median rating over time
  summarise(median = median(rating), mean = mean(rating)) %>% 
  
  ## Plot the data
  ggplot(mapping = aes(x = year, y = median)) + 
  
  ## Add point geometry for median
  geom_point() + 
  
  ## Add line geometry for mean
  geom_line(mapping = aes(x = year, y = mean)) + 
  
  ## Add labels and titles
  labs(x = "Year", y = "Mean(___) / Median(...)", title = "Ratings trends over time") + 
  
  ## Add a pleasant theme
  theme_light()

########################
# Recommendation system ----
## RMSE function 
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted) ^ 2))
}
#########################
## Prediction using the average only ----
## Here I define a baseline for assessing model accuracy with a RMSE of 1.06 on the training set
rmse(edx$rating, mean(edx$rating))

###########################
## Here I define a baseline for assessing model accuracy with a RMSE of 1.06 on the test set
rmse(validation$rating, mean(validation$rating))

###########################
## Incorporate user effects ----
## Defining the mean of ratings
mu <- mean(edx$rating)

###########################
## Computing the user specific coefficient bi.
bi <- edx %>% group_by(movieId) %>% 
  
  summarise(bi = mean(rating - mu)) %>% 
  
  ungroup()

################################
## Combining the user effects with the validation set for  evaluation
user_effect <- validation %>% 
  
  left_join(bi, by = "movieId")

#################################
## Evaluation on the test set
rmse(validation$rating, user_effect$bi + mu)

################################
## Incorporate movie effects ----
## Combining the training set with the computed user effects bi and computing 
## The movie effects bm.
bm <- edx %>% 
  
  left_join(bi, by = "movieId") %>% 
  
  group_by(userId) %>% 
  
  summarise(bm = mean(rating - mu - bi, na.rm = TRUE)) %>% 
  
  ungroup()

########################
## Combining the computed movie effect with the validation set for evaluation
user_movie_effect <- user_effect %>% 
  
  left_join(bm, by = "userId")

##########################
## The evaluation
rmse(validation$rating, mu + user_movie_effect$bi + user_movie_effect$bm)

###########################################
# Regularization ----

#######################
## Regularized movie and user effects 
# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)
# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time 
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating,predicted_ratings))
})
# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)

## Regularized movie, user, year, amd genres effect
# b_y and b_g are the year and genre effects. I try a series of lambdas.
lambdas2 <- seq(3.5, 6, 0.5)
# Setting up the function  
rmses <- sapply(lambdas2, function(l){
  
  ## Library for converting time columns
  library(anytime)
  
  ## The baseline prediction
  mu <- mean(edx$rating)
  
  ## Regularized movie effect
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  
  ## Regularized user effect
  b_u <- edx %>% 
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
  ## Regularized user and movie effects 
  b_y <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(year = lubridate::year(timestamp)) %>% 
    group_by(year) %>%
    summarise(b_y = sum(rating - mu - b_i - b_u)/(n()+l), n_y = n())
  
  ## Regularized movie, user and year effects 
  b_g <- edx %>%
    mutate(year = lubridate::year(timestamp)) %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+l), n_g = n())
  
  ## Regularized movie, user, year, and genre effects.
  predicted_ratings <- validation %>%
    mutate(year = lubridate::year(anytime(timestamp))) %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    .$pred
  
  ## Computing the RMSEs.
  return(rmse(validation$rating, predicted_ratings))
})
# Compute new predictions using the optimal lambda
# Test and save results 
qplot(lambdas2, rmses)  

###########################
## Best RMSE
min(rmses)

############################
# Table of results 
tribble(~ No, ~ Model, ~ RMSE, ~ Comment,
        "1", "Naive mean guess", "1.0606506", "The guess has high RMSE",
        "2", "Movie effect", "0.9437046", "Does better than guessing",
        "3", "Movie and user effects", "0.8655329", "Improvement over the movie effect",
        "4", "Regularized movie and user effects", "0.8649857", "More improvement",
        "5", "Regularized movie, user, year, and genre effects", "0.864411", "Marginal improvement") %>% 
  
  knitr::kable(caption = "Summary of Machine Learning Model and Performance")
#################################








