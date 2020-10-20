library("tidyverse")
library("dslabs")

head(co2)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

head(co2_wide)

co2_tidy <- gather(co2_wide, month, co2, -year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

data(admissions)
dat <- admissions %>% select(-applicants)

head(dat)
tidy_data <- dat %>% spread(gender, admitted)
tidy_data

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
