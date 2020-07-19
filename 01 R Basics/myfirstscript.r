library(tidyverse)
library(dslabs)
data(murders)

murders %>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

x1 <- (-b + sqrt(b^2-4*a*c))/(2*a)
x2 <- (-b - sqrt(b^2-4*a*c))/(2*a)
