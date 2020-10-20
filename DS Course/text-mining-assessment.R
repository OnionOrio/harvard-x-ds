library(dslabs)
library(lubridate)
library(tidyverse)
data("brexit_polls")

brexit_polls %>% filter(month(startdate) == 4) %>%  count

brexit_polls %>% filter(round_date(enddate, unit="week") == '2016-06-12') %>%  count

brexit_polls %>% mutate(weekday = weekdays(enddate)) %>% group_by(weekday) %>% count(weekday)

data("movielens")

movielens %>% max(count(hour(as_datetime(timestamp))))


library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata
gutenberg_metadata %>% filter(str_detect(gutenberg_metadata$title, "Pride and Prejudice")) %>%  gutenberg_works() %>%  head

gutenberg_works(only_languages = TRUE, title == "Pride and Prejudice") %>%  head

Location <- "J:\\Documents\\DS Course\\gutenberg.txt"

content <- readLines(Location)

head(content)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

content_x <- str_replace_all(content, pattern, " ")

head(content_x)

content %>% setNames("text")

head(content)

library(dplyr)
library(janeaustenr)
d <- tibble(txt = prideprejudice)
d

words <- unnest_tokens(d, word, txt)
words
count(words)

words <- words %>% filter(!word %in% stop_words$word)
count(words)

words <- words %>% filter(!str_detect(word, "^\\d+$"))
count(words)

words <- words %>% filter(!str_detect(word, "\\d"))
count(words)

words_count <- words %>% group_by(word) %>% mutate(n = n()) %>%  select(word, n)
words_count

afinn_sentiments <- get_sentiments("afinn")
head(afinn_sentiments)

sentiment_counts <- words %>% left_join(afinn_sentiments, by="word")

sentiment_counts %>% filter(!is.na(value)) %>% count

afinn_sentiments <- sentiment_counts %>%  filter(!is.na(value))

afinn_sentiments %>% filter(value == 4) %>% count
3414/6065
