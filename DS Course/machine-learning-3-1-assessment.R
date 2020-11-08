library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding") # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)

Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

y2 <- dat$y

set.seed(1, sample.kind="Rounding")

prediction <- replicate(100, {
  test_index <- createDataPartition(y2, times = 1, p=0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set <- dat[test_index, ]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

c(mean(prediction), sd(prediction))


rsme<- function(n){
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  y2 <- dat$y
  
  prediction <- replicate(100, {
    test_index <- createDataPartition(y2, times = 1, p=0.5, list = FALSE)
    train_set <- dat[-test_index, ]
    test_set <- dat[test_index, ]
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  
  c(mean(prediction), sd(prediction))
}

predict_rsme <- sapply(n, rsme)
predict_rsme[2, 1]/sqrt(100)

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

prediction <- replicate(100, {
  test_index <- createDataPartition(y2, times = 1, p=0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set <- dat[test_index, ]
  fit <- lm(y ~ x_1+x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

set.seed(1)
prediction <- replicate(100, {
  test_index <- createDataPartition(y2, times = 1, p=0.5, list = FALSE)
  train_set <- dat[-test_index, ]
  test_set <- dat[test_index, ]
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

y2 <- dat$y

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y2, times = 1, p=0.5, list = FALSE)
train_set <- dat[-test_index, ]
test_set <- dat[test_index, ]
fit <- lm(y ~ x_1+x_2, data = train_set)
fit2 <- lm(y ~ x_1, data = train_set)
fit3 <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
y_hat2 <- predict(fit2, test_set)
y_hat3 <- predict(fit3, test_set)
sqrt(mean((y_hat - test_set$y)^2))
sqrt(mean((y_hat2 - test_set$y)^2))
sqrt(mean((y_hat3 - test_set$y)^2))


mean(prediction)
sd(prediction)
