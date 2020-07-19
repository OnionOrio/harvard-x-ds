library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

p <- death_prob %>% filter(age==50, sex=="Female") %>% .$prob

loss <- - 150000
premium <- 1150

expected <- p*loss + (1-p)*premium


sd <- abs(loss-premium)*sqrt(p*(1-p))

expected*1000

abs(loss-premium)*sqrt(1000*p*(1-p))

X <- sample(c(loss, premium), 1000, prob=c(p, 1-p), replace=TRUE)

mean(X)

sd(X)

pnorm(0,expected*1000,abs(loss-premium)*sqrt(1000*p*(1-p)))

death_prob <- death_prob %>% filter(age==50, sex=="Male") %>% .$prob

b <-(700 - -150000*death_prob)/(1-death_prob) 

sd <- abs(-150000-b)*sqrt(1000*death_prob*(1-death_prob))
sd

pnorm(0, 700000, sd)

p <- 0.015
exp <- 1000*(p*(-150000)+(1-p)*1150)
exp
sd <- abs(-150000-1150)*sqrt(1000*p*(1-p))
sd
pnorm(-10^6,exp,sd)

p <-seq(.01, .03, .0025)
exp <- 1000*(p*(-150000)+(1-p)*1150)
sd <- abs(-150000-1150)*sqrt(1000*p*(1-p))

prob_l <- pnorm(-10^6, exp, sd)

plot(p, prob_l)

p_loss <- 0.015
b <- -150000
a <- 1150
set.seed(29, sample.kind = "Rounding")
S <- replicate(10000, {X <- sum(sample(c(b, a), 1000, prob=c(p_loss, 1-p_loss), replace=TRUE))})
S
mean(S <= -10^6)

p <- 0.015
b <- -150000
n<- 1000
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
a <- x
exp <- n*(l*p+x*(1-p))
exp/1000
set.seed(29, sample.kind = "Rounding")

S <- replicate(10000, {p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  z <- qnorm(0.05)
  X <- sum(sample(c(b, x), 1000, prob=c(p, 1-p), replace=TRUE))
  X})
mean(S)
mean(S<0)
mean(S < -10^6)
