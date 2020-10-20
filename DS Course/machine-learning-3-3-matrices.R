library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

my_vector <- 1:15

# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat

# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[, 28:1])

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

sums
avg

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)
avgs
x

sds

summary(x)

apply(mat, 1, mean)
mat

library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#extract columns and rows
x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60]
dim(new_x)
class(x[,1])
dim(x[1,])

#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

dim(x)

?dim

(x - rowMeans(x)) / rowSds(x)

t(t(X) - colMeans(X))

X_mean_0 <- sweep(x, 2, colMeans(x))

#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))

#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

t(x) %*% x
crossprod(x)
solve(x)

x <- matrix(rnorm(100*10), 100, 10)

dim(x)
dim(x)[1]

1:nrow(x)

x

x + seq(nrow(x))

if(!exists("mnist")) mnist <- read_mnist()

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

#binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1

train_set <- mnist$train

class(train_set)
class(train_set[1]$image)
train_set <- train_set$image
train_set
bin_train_set <- train_set
bin_train_set[(bin_train_set > 50 & bin_train_set < 205)] <- 1
55>50 && 55 < 205
bin_train_set[!(bin_train_set > 50 & bin_train_set < 205)] <- 0

bin_train_set
mean(bin_train_set)
