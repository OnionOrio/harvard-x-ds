library(dslabs)
library(dplyr)
library(lubridate)
library(tidyverse)
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times=1, p=0.5, list= FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

train

summary(train)


# examine the accuracy of 10 cutoffs
cutoff <- seq(5, 7.9, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(iris$Species))
  mean(y_hat == test$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

best_accuracy <- function(list, t){
  cutoff <- seq(min(list), max(list), 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(list > x, "virginica", "versicolor") %>% 
      factor(levels = levels(iris$Species))
    mean(y_hat == test$Species)
  })
  
  best_cutoff <- cutoff[which.max(accuracy)]
  best_cutoff
}

best_accuracy(train$Sepal.Length, test$Sepal.Length)
best_accuracy(train$Sepal.Width, test$Sepal.Width)
best_accuracy(train$Petal.Width, test$Petal.Width)
best_accuracy(train$Petal.Length, test$Petal.Length)

y_hat <- ifelse((test$Petal.Length > 4.7|test$Petal.Width > 1.5), "virginica", "versicolor") %>% 
  factor(levels = levels(iris$Species))
y_hat
mean(y_hat == test$Species)

plot(iris,pch=21,bg=iris$Species)
