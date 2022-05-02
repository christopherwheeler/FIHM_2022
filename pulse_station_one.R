# Exploration of Konza Pulse station 1 data

rm(list=ls())

library(tidyverse)
library(cowplot)
library(patchwork)
library(dygraphs)
library(janitor)

# bring in station 1 data

station_1 <- read_csv("station_1.csv")

names(station_1)

station_1 <- station_1 %>% 
  mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
  clean_names() %>% 
  select(- x1)
  

# cross plot for inital exploration
# can't do all vars so select 10

names(station_1)

cross_plot <- station_1 %>% 
  select(datetime, precipitation, atmospheric_pressure, vpd, max_precip_rate,
         storage_50_cm, vapor_pressure, logger_temperature, vapor_pressure_deficit, 
         water_content_5_cm, water_content_40_cm)

# plot(cross_plot)

par(new = TRUE)
par(mfrow = c(2, 2))

plot(cross_plot$logger_temperature, cross_plot$vapor_pressure_deficit,
     xlab = "logger temperature", ylab = "vapor pressure deficit")


plot(cross_plot$water_content_5_cm, cross_plot$water_content_40_cm,
     xlab = "water content 5cm", ylab = "water content 40cm")

plot(cross_plot$datetime, cross_plot$precipitation,
     xlab = "datetime", ylab = "precipitattion")

plot(cross_plot$datetime, cross_plot$logger_temperature,
     xlab = "datetime", ylab = "logger temperature")

# now some ggplots

ggplot(cross_plot, aes(x = datetime, y = logger_temperature)) + 
  geom_path() + 
  geom_smooth(color = "blue") + 
  theme_cowplot() + 
  ylab("logger temperature")

plot.new()

plot(cross_plot$datetime, cross_plot$logger_temperature,
     xlab = "datetime", ylab = "logger temperature", type = "l", lty = 1)


# Random forest ?
