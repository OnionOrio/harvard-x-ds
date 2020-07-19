library(tidyverse)
library(dslabs)
library(dplyr)
data(murders)
sort(murders$total)
murder_rate <- murders$total/murders$population*100000

index <- murder_rate <= 0.71
murders$state[index]

sum(index)

safe <- murder_rate <=1
place <- murders$region == "West"

index <- safe & place

index

murders$state[index]

index <- which(murders$state == "Massachusetts")

murder_rate[index]



rate <- murders$total/murders$population*100000
murders <- mutate(murders, rate = rate, rank=rank(-rate))
murders