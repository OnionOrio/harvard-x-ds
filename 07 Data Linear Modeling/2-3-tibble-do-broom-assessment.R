library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton %>% group_by(pair) %>% summarize(n = n(), cor(parentHeight, childHeight))

library(broom)

result <- galton %>% group_by(pair) %>% do(tidy(lm(childHeight~parentHeight, data = .), conf.int=TRUE))

as.numeric(result$p.value)

1.46e-07
