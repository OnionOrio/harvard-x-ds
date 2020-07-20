library(rvest)
library(tidyverse)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

html_text(nodes[[8]])
html_table(nodes[[8]])

html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

length(nodes)

html_table(nodes[[21]])
html_table(nodes[[20]])
html_table(nodes[[19]])

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
head(nodes[[1]])

tab_1
tab_2

new_tab_1 <- tab_1[2:30,2:4]
new_tab_1 
new_tab_2 <- tab_2[2:30,]
new_tab_2
full_join(new_tab_1, new_tab_2)

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- h %>% html_nodes("table")
length(tab)

colnames(html_table(tab[[5]], fill=T))
