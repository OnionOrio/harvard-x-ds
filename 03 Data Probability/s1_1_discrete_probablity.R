beads <- rep( c("red", "blue"), times = c(2,3))
beads
sample(beads, 1)

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

events_2 <- sample(beads, B, replace = TRUE)
tab_2 <- table(events_2)
tab_2
prop.table((tab_2))

?set.seed

mean(beads=="blue")