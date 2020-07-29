library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

names(polls)

polls$Remain

patterns <- "%"
patterns

ind <- str_detect(polls$Remain, "\\d%")
ind

polls <- polls %>% 
  filter(str_detect(Remain, "%")) %>% 
  setNames(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
polls %>%  head

count(polls)
