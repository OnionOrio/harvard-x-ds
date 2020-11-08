library("tidyverse")
library("dslabs")

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = T)

head(new_tidy_data)

class(new_tidy_data$year)


# plotting tidy data is simple
new_tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#convert tidy_data back into the wide format
new_wide_data <- new_tidy_data %>% spread(year, fertility)

select(new_wide_data, country, `1960`:`1967`)

