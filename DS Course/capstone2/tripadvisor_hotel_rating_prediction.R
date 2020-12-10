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

review_words <- reviews %>% 
  unnest_tokens(word, Review)

word_summaries <- review_words %>% group_by(word) %>% summarise(n = n(), avg = mean(Rating))

word_summaries %>% filter(avg>=4.5) %>%  arrange(desc(n))

word_summaries %>% filter(avg<=2) %>%  arrange(desc(n))

word_summaries %>% arrange(desc(n))

# we have a lot of words that are neutral that are not contributing to the rating. Thus, we need to filter out these not informative words. One of the ways is apply the stop words database from the tidytext package.

stop_words

filtered_words <- review_words %>% filter(!word %in% stop_words$word & !str_detect(word, "^\\d+$") & word != "n't")

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

filtered_summaries <- filtered_summaries %>% inner_join(affin, by = "word")
filtered_summaries %>%  sample_n(5)

filtered_summaries %>% filter(avg<=2) %>%  arrange(desc(n))

filtered_summaries %>% filter(avg>=4.5) %>%  arrange(desc(n))

filtered_summaries %>% mutate(sscore = (sentiments+5)/2) %>% ggplot(aes(sscore, avg)) + geom_point()

# Now, we will try to incorporate the word affinity score into the each review

# First, we add an ID for each review for identification. It helps to join tables of data later.

reviews <- reviews %>% mutate(id = row_number())
reviews

# Test function

library(janeaustenr)

d <- tibble(txt = prideprejudice)
d

d %>%
  unnest_tokens(word, txt)

d %>%
  unnest_tokens(sentence, txt, token = "sentences")

d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)

d %>%
  unnest_tokens(chapter, txt, token = "regex", pattern = "Chapter [\\\\d]")

d %>%
  unnest_tokens(shingle, txt, token = "character_shingles", n = 4)
