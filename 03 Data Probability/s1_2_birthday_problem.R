#  checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
bdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

meal_combinations <- function(n){
  6*choose(n,2)*3
}

meal_combinations(6)

table <- sapply(2:12, meal_combinations)
table