options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% ggplot(aes(Age, color = Sex)) + geom_density()

titanic %>% top_n(5, Age)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p <- titanic %>% ggplot(aes(sample=Age)) + geom_qq(dparams = params) +
  geom_abline()

p

titanic %>% ggplot(aes(Survived, color = Sex)) + geom_bar()

titanic  %>% ggplot(aes(Age, color = Survived)) + geom_density(alpha = 0.2)

titanic %>% filter(Fare > 0) %>% ggplot(aes(Fare, Survived)) + geom_boxplot() + scale_x_continuous(trans="log2") + geom_jitter()

titanic %>% ggplot(aes(Pclass, color = Survived)) + geom_bar()

titanic %>% ggplot(aes(Pclass, color = Survived)) + geom_bar(position = position_fill())

titanic %>% ggplot(aes(Survived, color = Pclass)) + geom_bar(position = position_fill())

# define plots p1, p2, p3
# p <- titanic %>% filter(Sex == "male")
# p1 <- p %>% ggplot(aes(x = Age, color = (Survived))) +  geom_density()
# p2 <- p %>% ggplot(aes(x = Age, color = (Pclass))) + geom_density()
# p3 <- p %>% ggplot(aes(x = Survived, color = (Pclass))) + geom_density()
# 
# p <- titanic %>% filter(Sex == "female")
# p4 <- p %>% ggplot(aes(x = Age, color = (Survived))) +  geom_density()
# p5 <- p %>% ggplot(aes(x = Age, color = (Pclass))) + geom_density()
# p6 <- p %>% ggplot(aes(x = Survived, color = (Pclass))) + geom_density()
# 
# 
# # arrange plots next to each other in 1 row, 3 columns
# library(gridExtra)
# grid.arrange(p1, p2, p4, p5, p6, nrow = 3)

p <- titanic %>% ggplot(aes(Age, color = (Survived))) + geom_density() + facet_grid(Sex ~ Pclass)

p

p <- titanic %>% ggplot(aes(Age, color = (Pclass))) + geom_density()

p

p <- titanic %>% ggplot(aes(Age, color = (Pclass))) + geom_bar()

p

