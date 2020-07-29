library(dslabs)
data("research_funding_rates")
research_funding_rates

r <- research_funding_rates

men_awarded <- sum(r$awards_men)
men_awarded_no <- sum(r$applications_men) - men_awarded
women_awarded <- sum(r$awards_women)
women_awarded_no <- sum(r$applications_women) - women_awarded

t <- data.frame(awarded = c("no", "yes"),
                men = c(men_awarded_no, men_awarded),
                women = c(women_awarded_no, women_awarded))
t

290/(1345+290)

177/(177+1011)

chisq_test <- t %>% select(-awarded) %>% chisq.test()
chisq_test

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% ggplot(aes(discipline, success, color=gender, size= applications)) + geom_point()
