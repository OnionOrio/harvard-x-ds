library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>% mutate(R_per_game = R/G) %>% 
  lm(avg_attendance~R_per_game, data=.)

Teams_small %>% mutate(HR_per_game = HR/G) %>% 
  lm(avg_attendance~HR_per_game, data=.)

Teams_small %>% mutate(W_per_game = W/G) %>% 
  lm(avg_attendance~W, data=.)

Teams_small %>% lm(avg_attendance~yearID, data=.)

Teams_small %>% mutate(R_per_game = R/G) %>% summarize(r = cor(W, R_per_game)) %>%  pull(r)

Teams_small %>% mutate(HR_per_game = HR/G) %>% summarize(r = cor(W, HR_per_game)) %>% pull(r)

Teams_strata <- Teams_small %>% mutate(win_strata = round(W/10, 0)) %>% 
  group_by(win_strata) %>% 
  filter(win_strata %in% 5:10, n() >= 20)

Teams_strata %>% filter(win_strata == 8) %>% count()

Teams_strata %>% mutate(R_per_game = R/G) %>%  do(tidy(lm(avg_attendance~R_per_game, data = .)))

Teams_strata %>% mutate(HR_per_game = HR/G) %>%  do(tidy(lm(avg_attendance~HR_per_game, data = .)))

fit <-Teams_small %>% mutate(R_per_game = R/G, HR_per_game = HR/G) %>% 
  lm(avg_attendance~R_per_game+HR_per_game+W+yearID, data =.)

f <- function(R, HR, W, Y){
  -456674+322*R+HR*1798+117*W+230*Y
}

f(5,1.2,80,1960)

predict(fit, newdata = data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))

Teams %>% filter(yearID == 2002) %>% 
  mutate(R_per_game = R/G, HR_per_game = HR/G) %>% 
  mutate(predicted_attendance = predict(fit, newdata = .), avg_attendance = attendance / G) %>% 
  summarize(r = cor(predicted_attendance, avg_attendance)) %>% pull(r)
