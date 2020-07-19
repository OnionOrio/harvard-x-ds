runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

set.seed(1)

B <- 10000

results <- replicate(B, {l <- sample(runners, 3)
  check <- sum(l %in% "Jamaica")
  check == 3})

mean(results)