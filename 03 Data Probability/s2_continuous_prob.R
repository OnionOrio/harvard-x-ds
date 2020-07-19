library(tidyverse)

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

set.seed(16, sample.kind="Rounding")

act_scores <- rnorm(10000, 20.9, 5.7)

mean(act_scores)
sd(act_scores)

sum(act_scores>=36)

mean(act_scores<10)

x <- 1:36

f_x <- dnorm(x, 20.9, 5.7)

plot(x, f_x)

x_i <- function(z){
  z*sd(act_scores)+mean(act_scores)
}

mean(act_scores>x_i(2))

x_i(2)

qnorm(0.975, mean(act_scores), sd(act_scores))

prob <- function(x){
    pnorm(x, mean(act_scores), sd(act_scores))
}

prob_list <- prob(1:36)

plot(1:36, prob_list)

prob(31)

qnorm(0.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)

sample_quantile <- quantile(act_scores, p)

theoretical_quantiles<- qnorm(p, 20.9, 5.7)

qqplot(sample_quantile, theoretical_quantiles)