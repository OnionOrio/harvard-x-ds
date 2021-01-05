library(tidyverse)
library(caret)
library(data.table)
library(tidytext)
library(textdata)
url <- "https://raw.githubusercontent.com/OnionOrio/harvard-x-ds/master/DS%20Course/capstone2/tripadvisor_hotel_reviews.csv"

dl <- tempfile()
download.file(url, dl)

reviews <- read_csv(dl)
summary(reviews)

reviews %>% group_by(Rating) %>% summarise(n = n()) %>% ggplot(aes(Rating, n)) + geom_bar(stat="identity")

# We will take a look at the 100th review as example

i <- 100

# wrap review into paragraphs 

reviews$Review[i] %>%  str_wrap(width = 65) %>%  cat()

reviews[i,] %>%  unnest_tokens(sentence, Review, token = "regex", pattern = ",+.")

# convert the review into words

reviews[i,] %>% 
  unnest_tokens(word, Review) %>% 
  pull(word)

reviews[i,] %>% 
  unnest_tokens(sentence, Review, token="sentences") %>% 
  pull(sentence)

# convert all reviews in word data
reviews <- reviews %>% mutate(Review = tolower(Review))
reviews <- reviews %>% mutate(Review = str_replace_all(tolower(Review), "not |n't ", "not_"))

reviews[100,] %>%  str_wrap(width = 65) %>%  cat()

review_words <- reviews %>% 
  unnest_tokens(word, Review)

# We try to concate the "not pattern" with the next word, eg. not/n't nice -> not_nice. So this word can be joined by the not_affin set


review_words %>% filter(str_detect(word, "not_")) %>% head()

word_summaries <- review_words %>% group_by(word) %>% summarise(n = n(), avg = mean(Rating))

word_summaries %>% filter(avg>=4.5) %>%  arrange(desc(n))

word_summaries %>% filter(avg<=2) %>%  arrange(desc(n))

word_summaries %>% arrange(desc(n))

# we have a lot of words that are neutral that are not contributing to the rating. Thus, we need to filter out these not informative words. One of the ways is apply the stop words database from the tidytext package.

stop_words

filtered_words <- review_words %>% filter(!word %in% stop_words$word & !str_detect(word, "^\\d+$"))

filtered_summaries <- filtered_words %>% group_by(word) %>% summarise(n = n(), avg = mean(Rating))

filtered_summaries %>% filter(avg<=2) %>%  arrange(desc(n))

filtered_summaries %>% filter(avg>=4.5) %>%  arrange(desc(n))

filtered_summaries %>% arrange(desc(n))

# one of the popular text mining process is the sentiment analysis which provide the affinity of a word. The choice of words reflect the emotion of a person which will be definitely useful in predicting the rating if we can know a piece of text is postive or not.

# we will use the tidytext and textdata package to conduct our sentiment analysis
get_sentiments("afinn")

affin <- get_sentiments("afinn") %>%
  select(word, sentiments=value)

# We carefully inspect our datasource and the affin set. There exists a pattern of negation, eg. not recommended, but it will be catched as positive sentiment under our current processing approach. To better analyse these pattern, we add negation affin by changing the sign of the sentiments score.

not_affin <- affin %>% mutate(word = paste("not", word, sep="_"), sentiments = sentiments * -1)
head(not_affin)
naffin <- rbind(affin, not_affin)
tail(naffin)

# Catching negative prefixes, eg. a-, dis-, in-, un-, non-, il-, ir-, im-
neg_prefix <- function(prefix){
  neg_affin <- affin %>% mutate(word = paste(prefix, word, sep=""), sentiments = sentiments * -1)
  neg_affin
}
prefixes <- c("a", "dis", "in", "un", "non", "il", "ir", "im")
neg_affin <- do.call(rbind, lapply(prefixes, neg_prefix))
tail(neg_affin)
head(neg_affin)
naffin <- rbind(naffin, neg_affin)
naffin %>% filter(str_detect(word, "not_")) %>% head

filtered_summaries <- filtered_summaries %>% inner_join(naffin, by = "word")
filtered_summaries %>%  sample_n(5)

filtered_summaries %>% filter(avg<=2) %>%  arrange(desc(n))

filtered_summaries %>% filter(avg>=4.5) %>%  arrange(desc(n))  

filtered_summaries <- filtered_summaries %>% mutate(sscore = (sentiments+5)/2)

filtered_summaries %>% filter(sentiments==-5)

filtered_summaries %>% mutate(sscore = as.factor(sscore)) %>% filter(n >= 5) %>%  ggplot(aes(x = sscore, y = avg)) + geom_boxplot()

# Now, we will try to incorporate the word affinity score into the each review

# First, we add an ID for each review for identification. It helps to join tables of data later.

reviews <- reviews %>% mutate(id = row_number())
reviews

compile_sscore <- function(text){
  words <- text %>% unnest_tokens(word, sentence) %>% 
    filter(!word %in% stop_words$word & !str_detect(word, "^\\d+$")) %>% 
    inner_join(naffin, by = "word") %>% 
    group_by(sentiments) %>%
    summarise(n = n())
  
  sscore <- words %>% summarise(score = sum(sentiments*n)/sum(n)) %>% pull(score)
  if(is.nan(sscore))
    0
  else
    sscore
}

# We will use the 100th as an example
word_count <- reviews[i,] %>%  unnest_tokens(word, Review) %>% nrow()

review_score <- reviews[i,] %>%  unnest_tokens(word, Review) %>% 
  filter(!word %in% stop_words$word & !str_detect(word, "^\\d+$")) %>% 
  inner_join(naffin, by = "word") %>%  
  mutate(sscore = (sentiments+5)/2) %>% 
  group_by(sentiments) %>% 
  summarise(n = n())

nword <- review_score %>% summarise(n = sum(n)) %>% pull(n)

negative_score <- review_score %>% filter(sentiments<0) %>% summarise(score = sum(sentiments*n)/sum(n), n = sum(n))

positive_score <- review_score %>% filter(sentiments>0) %>% summarise(score = sum(sentiments*n)/sum(n), n = sum(n), weighted = sum(n)/nword)

neutral_score <- review_score %>% filter(sentiments==0) %>% summarise(score = 0, n = sum(n), weighted = sum(n)/nword)

nsentence <- reviews[i,] %>%  unnest_tokens(sentence, Review, token = "regex", pattern = ",+.") %>% nrow()

sentence <- reviews[i,] %>%  unnest_tokens(sentence, Review, token = "regex", pattern = ",+.")

first_few_sentence_score <- (compile_sscore(sentence[1,]) + compile_sscore(sentence[2,]) + compile_sscore(sentence[3,]) + compile_sscore(sentence[4,]) + compile_sscore(sentence[5,]))/5

#Now a function to extract the following data for each reviews
sentiments_analysis <- function(i){
  print(i)
  
  word_count <- reviews[i,] %>%  unnest_tokens(word, Review) %>% nrow()
  
  review_score <- reviews[i,] %>%  unnest_tokens(word, Review) %>% 
    filter(!word %in% stop_words$word & !str_detect(word, "^\\d+$")) %>% 
    inner_join(naffin, by = "word") %>%  
    mutate(sscore = (sentiments+5)/2) %>% 
    group_by(sentiments) %>% 
    summarise(n = n())
  
  nword <- review_score %>% summarise(n = sum(n)) %>% pull(n)
  
  negative_score <- review_score %>% filter(sentiments<0) %>% summarise(neg_score = sum(sentiments*n)/sum(n), neg_nword = sum(n), neg_weighted = sum(n)/nword)
  
  positive_score <- review_score %>% filter(sentiments>0) %>% summarise(plus_score = sum(sentiments*n)/sum(n), plus_nword = sum(n), plus_weighted = sum(n)/nword)
  
  neutral_score <- review_score %>% filter(sentiments==0) %>% summarise(neu_score = 0, neu_nword = sum(n), neu_weighted = sum(n)/nword)
  
  nsentence <- reviews[i,] %>%  unnest_tokens(sentence, Review, token = "regex", pattern = ",+.") %>% nrow()
  
  sentence <- reviews[i,] %>%  unnest_tokens(sentence, Review, token = "regex", pattern = ",+.")
  
  first_few_sentence_score <- (compile_sscore(sentence[1,]) + compile_sscore(sentence[2,]) + compile_sscore(sentence[3,]) + compile_sscore(sentence[4,]) + compile_sscore(sentence[5,]))/5
  
  analysis_data <- bind_cols(data.frame(id=i, wordc = word_count, nword = nword, nsentence = nsentence, first_paragraph_score = first_few_sentence_score), negative_score, positive_score, neutral_score)
  
  analysis_data
}

# Adding sentimental analysis data to Raw data.
n <- reviews %>% nrow()
n
i <- seq(1:n)

options(dplyr.summarise.inform = FALSE)

sentiments_analysis_data <- do.call(rbind, lapply(i, sentiments_analysis))
# sentiments_analysis_data_old <- sentiments_analysis_data
head(sentiments_analysis_data)

df <- sentiments_analysis_data %>% mutate(id = row_number())
df <- reviews %>% left_join(df, by="id")

df %>% pull(neg_score)
#Fill NA
df[is.na(df)] <- 0

df %>% pull(neg_score)

# df_old <- df

# Create Training set and Test set from df
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = df$Rating, times = 1, p = 0.1, list = FALSE)
train_set <- df[-test_index,]
test_set <- df[test_index,]

train_set

test_set

# Define function calculating the RMSE 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Build the first model with average rating of a movie from a user
mu <- mean(train_set$Rating)

# RMSE of the first model
naive_rmse <- RMSE(test_set$Rating, mu)
naive_rmse

# Create a result tables for rmse alongside the improvement of the model 
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

# If string detect find negative word, rating are below average significantly
review_words %>% filter(str_detect(word, "not_suggest")) %>% summarize(avg = mean(Rating)) %>% pull(avg)
review_words %>% filter(str_detect(word, "not_recommend")) %>% summarize(avg = mean(Rating)) %>% pull(avg)
review_words %>% filter(str_detect(word, "horrible")) %>% summarize(avg = mean(Rating)) %>% pull(avg)

# If string detect find positive word, rating are above average significantly
review_words %>% filter(str_detect(word, "nice")) %>% summarize(avg = mean(Rating)) %>% pull(avg)
review_words %>% filter(str_detect(word, "good")) %>% summarize(avg = mean(Rating)) %>% pull(avg)
review_words %>% filter(str_detect(word, "decent")) %>% summarize(avg = mean(Rating)) %>% pull(avg)

# Weighted Negative word effect
fit <- lm(Rating ~ neg_score*neg_weighted, data = train_set)
fit
predict_ratings <- predict(fit, test_set)
RMSE(predict_ratings, test_set$Rating)
rmse_results <- rmse_results %>% add_row(method = "Added Weighted Negative word effects", RMSE = RMSE(test_set$Rating, predict_ratings))

# Weighted Positive word effect
fit <- lm(Rating ~ plus_score*plus_weighted, data = train_set)
fit
predict_ratings <- predict(fit, test_set)
RMSE(test_set$Rating, predict_ratings)
rmse_results <- rmse_results %>% add_row(method = "Added Weighted Positive word effects", RMSE = RMSE(test_set$Rating, predict_ratings))
rmse_results

# Weighted Negative and Positive word effect
fit <- lm(Rating ~ (neg_score*neg_weighted) + (plus_score*plus_weighted), data = train_set)
fit
predict_ratings <- predict(fit, test_set)
RMSE(test_set$Rating, predict_ratings)
rmse_results <- rmse_results %>% add_row(method = "Added Weighted +/- word effects", RMSE = RMSE(test_set$Rating, predict_ratings))

# first_paragraph_score effect
fit <- lm(Rating ~ first_paragraph_score, data = train_set)
predict_ratings <- predict(fit, test_set)
RMSE(test_set$Rating, predict_ratings)
rmse_results <- rmse_results %>% add_row(method = "Added Weighted 1st paragraph effects", RMSE = RMSE(test_set$Rating, predict_ratings))


# Weighted Negative and Positive word plus first_para_score effect
fit <- lm(Rating ~ (neg_score*neg_weighted) + (plus_score*plus_weighted) + first_paragraph_score, data = train_set)
fit
predict_ratings <- predict(fit, test_set)
RMSE(test_set$Rating, predict_ratings)
rmse_results <- rmse_results %>% add_row(method = "Added Weighted =/- & 1st para effects", RMSE = RMSE(test_set$Rating, predict_ratings))
rmse_results

# Mark 5 for prediction > 5 and 1 for prediction < 1
regulate_boundaries <- function(ratings){
  index_5 <- ratings > 5
  ratings[index_5] = 5
  index_1 <- ratings < 1
  ratings[index_1] = 1
  ratings
}

predict_ratings <- regulate_boundaries(predict_ratings)

rmse_results <- rmse_results %>% add_row(method = "Added coerce boundaries", RMSE = RMSE(test_set$Rating, predict_ratings))
rmse_results

# Mini Decision Tree, if the nword is lower than N, we do not apply linear regression and using the mean as prediction instead
prediction <- function(n){
  data <- test_set %>% mutate(predict_ratings = predict_ratings)
  index <- data$nword <= n
  data$predict_ratings[index] = mu
  results <- data$predict_ratings
  RMSE(test_set$Rating, results)
}

n <- seq(0,10,1)
n

rmse_list <- sapply(n, prediction)
rmse_list

# Ultimate N is 1
data <- test_set %>% mutate(predict_ratings = predict_ratings)
index <- data$nword <= 1
data$predict_ratings[index] = mu
predict_ratings <- data$predict_ratings

RMSE(test_set$Rating, predict_ratings)
rmse_results <- rmse_results %>% add_row(method = "Replace mu for those with a small sample size of words from sentiment analysis", RMSE = RMSE(test_set$Rating, predict_ratings))
rmse_results


# Negative and Positive Dominance

sum(predict_ratings <= 2)
sum(test_set$Rating <= 2)

sum(predict_ratings >= 4)
sum(test_set$Rating >= 4)


# Select the best fit on weight W
weights <- seq(0.5, 1.0, 0.025)
weights

rmse_dominance <- sapply(weights, function(weight){
  neg_index <- train_set$neg_weighted >= weight
  train_neg <- train_set[neg_index, ]
  train_temp <- train_set[!neg_index,]
  plus_index <- train_temp$plus_weighted >= weight
  train_plus <- train_temp[plus_index, ]
  train_rest <- train_temp[!plus_index, ]
  
  fit_neg <- lm(Rating ~ neg_score, data=train_neg)
  fit_plus <- lm(Rating ~ plus_score, data=train_plus)
  fit_rest <- lm(Rating ~ (neg_score*neg_weighted) + (plus_score*plus_weighted) + first_paragraph_score, data = train_rest)
  
  neg_index <- test_set$neg_weighted >= weight
  test_neg <- test_set[neg_index, ]
  test_temp <- test_set[!neg_index,]
  plus_index <- test_temp$plus_weighted >= weight
  test_plus <- test_temp[plus_index, ]
  test_rest <- test_temp[!plus_index, ]
  
  test_neg <- test_neg %>% mutate(predict_ratings = predict(fit_neg, test_neg))
  test_plus <- test_plus %>% mutate(predict_ratings = predict(fit_plus, test_plus))
  test_rest <- test_rest %>% mutate(predict_ratings = predict(fit_rest, test_rest))
  result <- rbind(test_neg, test_plus, test_rest) %>% select(id, predict_ratings)
  result <- test_set %>% left_join(result, by="id")
  
  predict_ratings_dominance <- result$predict_ratings
  
  predict_ratings_dominance <- regulate_boundaries(predict_ratings_dominance)
  
  # Ultimate N is 1
  data <- test_set %>% mutate(predict_ratings = predict_ratings_dominance)
  index <- data$nword <= 1
  data$predict_ratings[index] = mu
  predict_ratings_dominance <- data$predict_ratings
  
  RMSE(result$Rating, predict_ratings_dominance)
})

plot(weights,rmse_dominance)
weights[which.min(rmse_dominance)]

weight <- 0.875
neg_index <- train_set$neg_weighted >= weight
train_neg <- train_set[neg_index, ]
train_temp <- train_set[!neg_index,]
plus_index <- train_temp$plus_weighted >= weight
train_plus <- train_temp[plus_index, ]
train_rest <- train_temp[!plus_index, ]

fit_neg <- lm(Rating ~ neg_score, data=train_neg)
fit_plus <- lm(Rating ~ plus_score, data=train_plus)
fit_rest <- lm(Rating ~ (neg_score*neg_weighted) + (plus_score*plus_weighted) + first_paragraph_score, data = train_rest)

neg_index <- test_set$neg_weighted >= weight
test_neg <- test_set[neg_index, ]
test_temp <- test_set[!neg_index,]
plus_index <- test_temp$plus_weighted >= weight
test_plus <- test_temp[plus_index, ]
test_rest <- test_temp[!plus_index, ]

test_neg <- test_neg %>% mutate(predict_ratings = predict(fit_neg, test_neg))
test_neg

test_plus <- test_plus %>% mutate(predict_ratings = predict(fit_plus, test_plus))
test_plus

test_rest <- test_rest %>% mutate(predict_ratings = predict(fit_rest, test_rest))
test_rest

test_set

result <- rbind(test_neg, test_plus, test_rest) %>% select(id, predict_ratings)
result <- test_set %>% left_join(result, by="id")
result

RMSE(result$Rating, result$predict_ratings)

predict_ratings_dominance <- result$predict_ratings

predict_ratings_dominance <- regulate_boundaries(predict_ratings_dominance)

# Ultimate N is 1
data <- test_set %>% mutate(predict_ratings = predict_ratings_dominance)
index <- data$nword <= 1
data$predict_ratings[index] = mu
predict_ratings_dominance <- data$predict_ratings

RMSE(result$Rating, predict_ratings_dominance)
rmse_results <- rmse_results %>% add_row(method = "+/- dominance effects", RMSE = RMSE(test_set$Rating, predict_ratings_dominance))
rmse_results
