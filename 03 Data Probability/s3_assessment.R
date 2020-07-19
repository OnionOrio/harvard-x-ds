library(tidyverse)

p_guess <- 0.25

penalty <- 0

p_guess_wrong <- 1 - p_guess

e_X <- p_guess + p_guess_wrong*(penalty)

44*e_X

sd_X <- abs(penalty-1)*sqrt(p_guess*p_guess_wrong)

sd_X*sqrt(44)

X <- sample(c(1,penalty), 44, prob=c(p_guess, p_guess_wrong), replace=TRUE)

1-pnorm(8,e_X,sd_X*sqrt(44))

set.seed(21, sample.kind = "Rounding")

B <- 10000

S <- replicate(B, {Y <- sum(sample(c(1,penalty), 44, prob=c(p_guess, p_guess_wrong), replace=TRUE))})

mean(S >= 8)

p <- seq(0.25, 0.95, 0.05)

score_35 <- function(skill){
  S <- replicate(10000, {Y <- sum(sample(c(1,0), 44, prob=c(skill, 1-skill), replace=TRUE))})
  mean(S > 35)
}

plot(p, sapply(p, score_35))
