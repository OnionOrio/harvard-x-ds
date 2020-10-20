library("tidyverse")
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_m <- mean(female_heights$mother)
sd_m <- sd(female_heights$mother)
mu_d <- mean(female_heights$daughter)
sd_d <- sd(female_heights$daughter)

r <- female_heights %>% summarize(r = cor(mother, daughter)) %>% pull(r)

m <- r*(sd_d/sd_m)
b <- mu_d - m*mu_m

r^2

b+m*60
