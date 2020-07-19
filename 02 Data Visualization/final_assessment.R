library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)

names(stars)

avg <- mean(stars$magnitude)

sd <- sd(stars$magnitude)

sd

p <- stars %>% ggplot(aes(x = magnitude))

p + geom_density()

p <- stars %>% ggplot(aes(x = temp))

p + geom_density()

p <- stars %>% ggplot(aes(x = temp,y = magnitude, color=type))

p + geom_point() + scale_y_reverse() + scale_x_continuous(trans="log10") + scale_x_reverse()


