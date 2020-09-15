library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

class(indexes)

indexes$Resample01

sum(indexes$Resample01 == 3)

no_3 <- function(x)
{
  sum(x == 3)
}

list_3 <- sapply(indexes, no_3)

sum(list_3)

y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding") 
B <- 10^5
M <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

sd(M)
0.6654465

set.seed(1, sample.kind="Rounding") 
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10^5)
q_75 <- function(ind){
  sample <- y[ind]
  quantile(sample, 0.75)
}

M_star <- sapply(indexes, q_75)

mean(M_star)
sd(M_star)
