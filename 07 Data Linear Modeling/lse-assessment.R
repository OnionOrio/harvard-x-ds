library(Lahman)
library(tidyverse)
library(HistData)
data("GaltonFamilies")

head(Teams)
t_data <- Teams %>% filter(yearID >= '1961', yearID <= '2001')

head(t_data)
summary(t_data)


t_data <- t_data %>% mutate(BB_per_game = BB / G, HR_per_game = HR / G, R_per_game = R/G)

lm(R_per_game ~ BB_per_game+HR_per_game, data = t_data)

?lm

set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

summary(lse)

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

lm(mother~daughter, data = female_heights)

head(female_heights)

44.18+0.31*69

model<-lm(mother~daughter, data = female_heights, x=69)

model

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  group_by(playerID) %>% summarize(mean_singles = mean(singles), mean_bb = mean(bb)) 

bat %>% filter(mean_singles>0.2) %>% count
bat %>% filter(mean_bb>0.2) %>% count

bat_new <- inner_join(bat, bat_02)

bat_new %>% summarize(r = cor(singles, mean_singles)) %>%  pull(r)

bat_new %>% summarize(r = cor(bb, mean_bb)) %>%  pull(r)

bat_new %>% ggplot(aes(mean_singles, singles)) + geom_point()

bat_new %>% ggplot(aes(mean_bb, bb)) + geom_point()

lm(singles~mean_singles, data=bat_new)

lm(bb~mean_bb, data=bat_new)
