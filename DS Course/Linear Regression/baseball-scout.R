library("Lahman")
library("tidyverse")
library("dslabs")
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>% 
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>%  filter(yearID %in% 1961:2001) %>% 
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>% 
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>%  filter(yearID %in% 1961:2001) %>% 
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>% 
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

?Teams

Teams %>%  filter(yearID %in% 1961:2001) %>% 
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>% 
  ggplot(aes(X3B_per_game, X2B_per_game)) +
  geom_point(alpha = 0.5)

Teams %>%  filter(yearID %in% 1961:2001) %>%
  summarize(r = cor(X3B/G, X2B/G)) %>% pull(r)

t <- Teams %>%  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, HR_per_game = HR / G)
  lm(BB_per_game ~ HR_per_game, data = t)
