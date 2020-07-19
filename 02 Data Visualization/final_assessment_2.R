library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max()

temp_carbon %>% .$year %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max()

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year)

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% min()

temp_carbon %>% filter(year == 2018, !is.na(temp_anomaly)) %>%  pull(temp_anomaly) %>% min()

temp_carbon %>% filter(year == 1880, !is.na(temp_anomaly)) %>%  pull(temp_anomaly) %>% min()

p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(x = year, y= temp_anomaly))

p + geom_point() + geom_hline(aes(yintercept=0), col="blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th Century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col ="blue") +
  geom_line(aes(x = year, y= ocean_anomaly), col ="green") +
  geom_line(aes(x=year, y=land_anomaly),col = "red")
  
p <- greenhouse_gases %>% ggplot(aes(x=year, y=concentration)) +
  geom_line() +
  facet_grid(gas ~., scales = "free")+
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0 - 2000")

p<- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(x = year, y= carbon_emissions))

p + geom_line()

names(historic_co2)

co2_time <- historic_co2 %>% ggplot(aes(x=year, y=co2, col=source)) + geom_line()

co2_time + xlim(min=-3000, max=2018)
