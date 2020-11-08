models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
fits
names(fits)

prediction <- sapply(fits, function(fit){
  predict(fit, mnist_27$test)
})

prediction <- as.data.frame(prediction)

prediction$glm

prediction_t <- as.data.frame(t(as.matrix(prediction)))

accuracy <- sapply(prediction, function(y_hat){
      #print(y_hat)
      mean(mnist_27$test$y == y_hat)
})
accuracy

mean(accuracy)

prediction_t

score_7 <- sapply(prediction_t, function(p){
  sum(p == 7)
})
score_7
y_ensemble <- ifelse(score_7 > 5, 7, 2)
y_ensemble
mean(y_ensemble == mnist_27$test$y)

mini_accuracy <- sapply(fits, function(fit){
  min(fit$results$Accuracy)
})

f <- fits$adaboost
min(f$results$Accuracy)

mean(mini_accuracy)
mini_accuracy
prediction_s <- prediction %>% select(glm, naive_bayes, knn, gamLoess, qda, rf)
prediction_st <- as.data.frame(t(as.matrix(prediction_s))) 

prediction_st

score_7s <- sapply(prediction_st, function(p){
  sum(p == 7)
})
score_7s
y_ensemble_s <- ifelse(score_7s > 3, 7, 2)
y_ensemble_s
mean(y_ensemble_s == mnist_27$test$y)
