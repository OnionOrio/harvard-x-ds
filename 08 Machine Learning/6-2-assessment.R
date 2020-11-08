library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

colnames(movielens)

# Q1 Year with Highest median number of rating

median_ratings <- movielens %>% group_by(movieId, year) %>% summarise(count = n())
median_ratings
plot(median_ratings$year, sqrt(median_ratings$count))

median_year <- median_ratings %>% group_by(year) %>% summarise(median = median(count))

which.max(median_year$median)
median_year$year[82]

movie_1993 <- movielens %>% filter(year>= 1993, year <= 2018) %>% group_by(movieId) %>% summarise(avg_per_year = n()/26)

movie_1993 <- data.frame(movie_1993)

head(movie_1993)

shawshank <- movielens %>% filter(year>= 1993, year <= 2018) %>% filter(str_detect(title, "Shawshank Redemption"))

forrest <- movielens %>% filter(year>= 1993, year <= 2018) %>% filter(str_detect(title, "Forrest Gump"))

movie_1993 %>% filter(movieId == 356)

mean(shawshank$rating)

length(forrest$movieId)/25
movie_1993


movielens %>% 
  filter(year >= 1993) %>% 
  group_by(title) %>% 
  summarize(n_of_ratings = n(), year = year, yeardiff = 2018 - year) %>% 
  group_by(title, year) %>% 
  summarize(mean = mean(n_of_ratings/yeardiff)) %>% 
  group_by(title) %>% 
  arrange(desc(mean)) %>%
  top_n(25)

colnames(movielens)

movie_1993 <- movielens %>% 
  filter(year >= 1993) %>% 
  group_by(title) %>% 
  summarize(avg = sum(rating/n()), n_of_ratings = n(), year = year, yeardiff = 2018 - year, sum = sum(rating)) %>% 
  group_by(title, year) %>% 
  summarize(mean = mean(n_of_ratings/yeardiff)) %>% 
  group_by(title) %>% 
  arrange(desc(mean))

movie_avg_rating <- movielens %>% filter(year >= 1993) %>% 
  group_by(title) %>% 
  summarize(n_of_ratings = n(), avg = sum(rating/n())) %>% arrange(desc(avg))
movie_avg_rating

q3_df <- left_join(movie_1993, movie_avg_rating)

data <- q3_df %>% mutate(strata = round(mean)) %>% group_by(strata)
data
plot(q3_df$mean, q3_df$avg)

q3_df

movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens

q6 <- movielens %>% mutate(week = round_date(date, unit = "week")) %>% group_by(week) %>% summarize(avg = sum(rating)/n())
q6
plot(q6)

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

genres <- movielens %>% group_by(genres) %>% filter(n()>1000) %>% summarize(avg = mean(rating), sd = sd(rating))

genres %>% ggplot(aes(genres, avg, ymin = avg - 2*sd, ymax = avg + 2*sd)) + geom_errorbar()
