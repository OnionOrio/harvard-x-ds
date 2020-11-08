library(tidyverse)
options(digits = 3)


library(dslabs)
data("brexit_polls")

p <- 0.481
d <- 2*p-1

N <- 1500
E_x <- p*N
E_x

sd <- sqrt(p*(1-p)/N)
sd

sqrt(p*(1-p)*N)

brexit_polls <- brexit_polls %>% mutate(x_hat = remain/(remain+leave), spread = remain-leave)

head(brexit_polls)

mean(brexit_polls$spread)

sd(brexit_polls$spread)

sample_x <- mean(brexit_polls$x_hat)

sample_sd <- 0.00294

sample_sd_f <- sd(brexit_polls$x_hat)/sqrt(127)

count(brexit_polls)

sample_sd

qnorm(0.025, sample_x, sample_sd)

qnorm(0.975, sample_x, sample_sd_f)

sample_x+qnorm(0.975)*sample_sd_f

qnorm(0.975)
