# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

head(brexit_polls)

june_polls <- brexit_polls %>% filter(enddate >= '2016-06-01')

june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), se_d = 2*se_x_hat, upper = qnorm(0.975, spread, se_d), lower = qnorm(0.025, spread, se_d), hit = ifelse(upper>= -0.038 & -0.038 >=lower, 1, 0))

head(june_polls)

mean(june_polls$upper>=0 & june_polls$lower <= 0)
mean(june_polls$lower>0)
mean(june_polls$hit)

june_polls %>% group_by(pollster) %>% summarise(avg_hit = mean(hit), n = n()) %>% arrange(avg_hit)

boxplot(june_polls$spread, june_polls$poll_type)

head(june_polls)

(june_polls)
