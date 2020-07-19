# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

june_polls <- brexit_polls %>% filter(enddate >= '2016-06-01')

june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), se_d = 2*se_x_hat, upper = qnorm(0.975, spread, se_d), lower = qnorm(0.025, spread, se_d), hit = ifelse(upper>= -0.038 & -0.038 >= lower, 1,0))

head(june_polls)

mean(june_polls$lower>0)

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type

se_online <- 2*sqrt(0.496*(1-0.496)/46711)
se_offline <- 2*sqrt(0.506*(1-0.506)/13490)
qnorm(0.975, -0.00741, se_online)
qnorm(0.025, -0.00741, se_online)

qnorm(0.975, 0.0127, se_offline)
qnorm(0.025, 0.0127, se_offline)


brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit
count(brexit_hit%>% filter(poll_type=='Online'& ! hit))

two_by_two <- data.frame(
                          hit = c("no", "yes"),
                         online = c(
                           37,
                           48
                           ),
                         tele = c(
                           32,
                           10
                           )
                         )

two_by_two
chisq_test<- two_by_two %>% select(-hit) %>% chisq.test()
chisq_test

48/37

10/32

(48/37)/(10/32)

brexit_polls %>% ggplot(aes(enddate, spread, color=poll_type)) + geom_smooth(method="loess", span = 0.4) + geom_line(y=-0.038) + geom_point()

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate, proportion, color=vote)) + geom_smooth(method="loess", span = 0.3) + geom_point()
                        