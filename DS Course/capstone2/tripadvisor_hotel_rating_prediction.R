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
