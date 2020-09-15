library(dslabs)
library(caret)
library(tidyverse)

data(heights)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list= FALSE)

train_set <- heights[-test_index,]
test_set <- heights[test_index,]

x <- train_set$height
y <- train_set$sex


ks <- seq(1, 101, 3)

f1_score <- function(k)
{
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(test_set$sex))
}

F_1 <- sapply(ks, f1_score)

max(F_1)
which.max(F_1)  

length(F_1)

ks[16]


data("tissue_gene_expression")

set.seed(1, sample.kind = "Rounding")
k = seq(1, 11, 2)
k

df <- data.frame(tissue_gene_expression)

df$y

test_index <- createDataPartition(df$y, times=1, p = 0.5, list = FALSE)

train_set <- df[-test_index,]
test_set <- df[test_index,]

accuracy <- function(k){
  fit <- knn3(y ~ ., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(train_set$y))
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$y)
  cm_test$overall["Accuracy"]
}

ac <- sapply(k, accuracy)
ac


fit <- knn3(y ~ ., data = train_set, k = 3)
y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(train_set$y))
mean(y_hat == test_set$y)
