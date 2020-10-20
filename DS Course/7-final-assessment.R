options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

y <- brca$y
y

x <- brca$x
x
dim(x)


# Q1 examine data : dimensions and properties
mean(y=='M')
which.min(colSds(x))


# Q2 scaling data
x <- with(brca, sweep(brca$x, 1, colMeans(brca$x)))

x <- sweep(brca$x, 2, colMeans(brca$x), "-")

x <- sweep(x, 2, colSds(brca$x), "/")

colMedians(x)
colSds(x)

# Q3 Calculating distance between data
d <- dist(x)
plot(d)
dim(x)
dim(d)
class(d)
d_mat <- as.matrix(d)
dim(d_mat)

# Q4 Heatmap
d_features <- dist(t(x))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# Q5 Hcluster
h <- hclust(d_features, 5)
plot(h)

# Q6, 7, 8 PCA
pca <- prcomp(x)
pca
summary(pca)
pca$x[,1:2]
df <- data.frame(pc1 = pca$x[,1], pc2 = pca$x[,2], y = y)
df %>% ggplot(aes(pc1, pc2, color = y)) +geom_point()

df_q8 <- data.frame(pc_10 = pca$x[,1:10], y = y)
  df_q8 %>%  ggplot(aes(pc_10.PC10, y, color = y)) + geom_boxplot()

# q9 test, train set
# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x[test_index,]
test_y <- brca$y[test_index]
train_x <- x[-test_index,]
train_y <- brca$y[-test_index]

mean(test_y=='B')
mean(train_y=='B')


# Q10 kmeans clustering
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

set.seed(3, sample.kind = "Rounding") 
k <- kmeans(train_x, centers = 2)

y_0 <- predict_kmeans(test_x, k)
y_0
y_0_f <- factor(y_0, levels= c(1, 2), labels = c('B','M'))
test_y
?factor
mean(y_0_f[test_y=='B']=='B')
mean(y_0_f[test_y=='M']=='M')

train <- data.frame(x = train_x, y = train_y)
test <- data.frame(x = test_x, y = test_y)
set.seed(5, sample.kind = "Rounding")
fit_lm <- caret::train(y ~ ., model="glm", data = train)
y0_lm <- predict(fit_lm, test)
y0_lm
mean(y0_lm == test_y)
confusionMatrix(y0_lm, test_y)$overall[["Accuracy"]]
train

fit_lda <- caret::train(y ~ ., method="lda", data = train)
y_hat_lda <- predict(fit_lda, test, type = "raw")
confusionMatrix(y_hat_lda, test_y)$overall[["Accuracy"]]

fit_qda <- caret::train(y ~ ., method="qda", data = train)
y_hat_qda <- predict(fit_qda, test, type = "raw")
confusionMatrix(y_hat_qda, test_y)$overall[["Accuracy"]]

library(gam)
set.seed(5, sample.kind = "Rounding")
fit_losses <- caret::train(train_x, train_y, method="gamLoess")
set.seed(5, sample.kind = "Rounding")
y_hat_ls <- predict(fit_losses, test_x)
confusionMatrix(y_hat_ls, test_y)$overall[["Accuracy"]]

train_glm <- train(train_x, train_y,
                   method = "glm")
pred <- predict(train_glm, test_x)
confusionMatrix(pred, test_y)

set.seed(7, sample.kind = "Rounding")
train_knn <- train(y ~ ., method = "knn", 
                   data = train,
                   tuneGrid = data.frame(k = seq(3, 21, 1)))
train_knn
y_hat_knn <- predict(train_knn, test)
confusionMatrix(y_hat_knn, test_y)$overall[["Accuracy"]]

set.seed(9, sample.kind = "Rounding")
train_rf <- train(y ~ ., method = "rf", tuneGrid = data.frame(mtry =c(3, 5, 7, 9)), importance = TRUE, data = train)
train_rf

y_hat_rf <- predict(train_rf, test)
confusionMatrix(y_hat_rf, test_y)$overall[["Accuracy"]]
imp <- varImp(train_rf)
imp

y_ensem <- (y_hat_rf == 'B') + (y_hat_rf =='B') + (y_hat_ls == 'B') + (pred == 'B') + (y_0_f == 'B') + (y_hat_lda == 'B') + (y_hat_qda == 'B')
y_hat_ensem <- ifelse(y_ensem>3, 'B', 'M')
confusionMatrix(factor(y_hat_ensem, levels= c('B', 'M'), labels = c('B','M')), test_y)$overall[["Accuracy"]]
