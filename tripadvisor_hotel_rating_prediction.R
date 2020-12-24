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

affin

# We carefully inspect our datasource and the affin set. There exists a pattern of negation, eg. not recommended, but it will be catched as positive sentiment under our current processing approach. To better analyse these pattern, we add negation affin by changing the sign of the sentiments score.

not_affin <- affin %>% mutate(word = paste("not", word, sep="_"), sentiments = sentiments * -1)
not_affin


naffin <- rbind(affin, not_affin)
tail(naffin)

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
  
  # analysis_data <- data.frame(id=i, wordc = word_count, nword = nword, nsentence = nsentence, first_paragraph_score = first_few_sentence_score)
  
  analysis_data
}

# Define function calculating the RMSE 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Adding sentimental analysis data to Raw data.
n <- reviews %>% nrow()
n
i <- seq(1:n)

options(dplyr.summarise.inform = FALSE)

j <- seq(1:3)
#sentiments_analysis_data_t <- do.call(rbind, lapply(j, sentiments_analysis))
head(sentiments_analysis_data_t)
sentiments_analysis_data_t[1,]

sentiments_analysis_data <- do.call(rbind, lapply(i, sentiments_analysis))
head(sentiments_analysis_data)
sentiments_analysis_data[1,]
class(sentiments_analysis_data[1,])
sentiments_analysis_data <- t(sentiments_analysis_data)
sentiments_analysis_data[1,][1]
df <- rbind(sentiments_analysis_data)
df <- sentiments_analysis_data %>% mutate(id = row_number())
head(df)

final_data <- reviews %>% left_join(df, by="id")

head(final_data) 
final_data[1,]

# Create Training set and Test set from edx
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = reviews$Rating, times = 1, p = 0.1, list = FALSE)
train_set <- reviews[-test_index,]
test_set <- reviews[test_index,]

train_set

test_set

# Build the first model with average rating of a movie from a user
mu <- mean(train_set$Rating)

# RMSE of the first model
naive_rmse <- RMSE(test_set$Rating, mu)
naive_rmse

# If string detect find not_recommend, put rating as 1
