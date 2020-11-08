library(dslabs)
library(tidyverse)
library(caret)

# set.seed(1986) # if using R 3.5 or earlier
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

topten <- schools %>% top_n(10, score) %>% arrange(desc(score))

schools %>%
  
  select(id,size,score)%>% 
  
  top_n(10, score) %>% 
  
  arrange(desc(score))

median(topten$size)

median(schools$size)

bottomten <- schools %>% top_n(-10, score) %>% arrange((score))



bottomten

median(bottomten$size)

plot(schools$size, schools$score)

overall <- mean(sapply(scores, mean))

above_average <- schools %>% mutate(reg_score = overall + size*(score - overall)/(size + 25)) %>%  filter(reg_score > overall) %>% top_n(10, reg_score) %>% arrange(desc(reg_score))
above_average

alpha <- seq(10, 250)

reg_score <- schools %>% mutate(reg_score = overall + size*(score - overall)/(size + 25), diff_sq = (score - reg_score)^2)

reg_score

rmse_alpha <- function(alp){
  df <- schools %>% mutate(reg_score = overall + size*(score - overall)/(size + alp), diff_sq = (quality - reg_score)^2)
  sqrt(sum(df$diff_sq))
}

rmse <- sapply(alpha, rmse_alpha)
rmse
which.min(rmse)
alpha[126]

above_average <- schools %>% mutate(reg_score = overall + size*(score - overall)/(size + 135)) %>%  filter(reg_score > overall) %>% top_n(10, reg_score) %>% arrange(desc(reg_score))
above_average

rmse_alpha_q8 <- function(alp){
  df <- schools %>% mutate(reg_score = size*(score)/(size + alp), diff_sq = (quality - reg_score)^2)
  sqrt(sum(df$diff_sq))
}

rmse_q8 <- sapply(alpha, rmse_alpha_q8)
which.min(rmse_q8)
alpha[241]
