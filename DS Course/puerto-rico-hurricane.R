library("tidyverse")
library("pdftools")
options(digits = 3)

fn <- system.file("J://Document//DS Course//RD-Mortality-Report_2015-18-180531.pdf")

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

txt <- pdf_text(fn)

x <- txt[9] %>% str_split("\n")
class(x)

s <- x[[1]]
class(s)
length(s)

s <- str_trim(s)
s[1]

str_which(s, "2015")
s[2]
s[24]

header_index_str <- str_split(s[2], "\\s+", simplify = TRUE)
header_index_str
month = header_index_str[1]
header = header_index_str[2:5]

str_which(s, "Total")
s[35]

s_count <- str_count(s, pattern="\\d+")
ind <- s_count[3:34] != 1

ind

s_edit <- s[3:34]

s_edit <- s_edit[ind]

s_edit

s <- str_remove_all(s_edit, "[^\\d\\s]")
s

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s

class(s)
names(s)

colnames(s) <- c("day", header)

tab <- map_df(tab, as.numeric)
tab
mean(tab$X2017[20:30])

tab <- tab %>% gather(year, deaths, -day)
tab

tab %>% ggplot(aes(day, deaths, color=year)) + geom_smooth()


