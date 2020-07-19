library(tidyverse)

p_win <- 5/38

p_loss <- 1-p_win

win <- 6

loss <- -1

n <- 500

e_X <- win*p_win + loss*p_loss

e_X

sd_X <- abs(loss-win)*sqrt(p_win*p_loss)

sd_X

X <- sample(c(win,loss), n, prob=c(p_win,p_loss), replace=TRUE)

sd_X*sqrt(n)

pnorm(0, e_X*n, sd_X*sqrt(n))

