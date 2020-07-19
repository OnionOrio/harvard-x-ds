library(dslabs)
library(tidyverse)
library(ggthemes)
library(ggrepel)
data(murders)

p <- ggplot(data = murders, aes(population/10^6, total, label=abb)) 
class(p)

# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

p <- p + geom_point(aes(col=region), size = 3) +
geom_text_repel() +
  scale_x_log10() +
  scale_y_log10()+
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")+
  geom_abline(intercept = log10(r), lty= 2, color = "darkgrey")    # slope is default of 1

p <- p + scale_color_discrete(name = "Region")    # capitalize legend title

p <- p + theme_economist()

p