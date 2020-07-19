library(dslabs)
data(heights)

sex_n <- ifelse(heights$sex == "Female", 1, 2)
sum(sex_n)

higher_72 <- ifelse(heights$height > 72, heights$height, 0)
mean(higher_72)

inches_to_ft <- function(x){
  x/12
}

inches_to_ft(144)

sum(inches_to_ft(heights$height)<5)

