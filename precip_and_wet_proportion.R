# precip and wet percent (FIHM expolration)

library(tidyverse)
library(fs)
library(Rcpp)
library(sp)
library(raster)
library(rgdal)
library(rasterVis)
library(sf)
library(dataRetrieval)
library(cowplot)
library(reshape2)
library(mapview)
library(readxl)
library(janitor)
library(patchwork)


rm(list=ls()) # removes all objects from workspace when you use list=ls() as base. basically it is like restarting R session



#### bring it in

rm(station_1)

station_1 <- read_csv("data/station_1.csv")


station_1 <- station_1 %>% 
  mutate(Date_Time = lubridate::mdy_hm(datetime))

station_1 <- station_1 %>% 
  mutate(Date = lubridate::floor_date(Date_Time, "day"))




ggplot(station_1, aes(x = Date_Time, y = Water_Content_5cm)) + 
  geom_point(aes(x = Date_Time, y = Water_Content_40cm), color = "red") +
  geom_point(aes(x = Date_Time, y = Water_Content_20cm), color = "blue") +
  geom_point() + 
  geom_path() +
  theme_classic()


ggplot(station_1, aes(x = Date_Time, y = Air_Temperature)) + 
  geom_path() +
  geom_smooth() +
  theme_classic()

library(dataRetrieval)
# Kings Creek near Manhattan, KS
siteNumber <- "06879650" 
KingsInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
rawDailyData <- readNWISdv(siteNumber,parameterCd,
                           "2021-07-01","2021-12-21")


##### Join Konza Pulse and USGS Kings Creek

station_1 <- station_1 %>% 
  mutate(Date = lubridate::floor_date(Date_Time, "day"))

names(discharge_pulse)

discharge_pulse <- left_join(rawDailyData, station_1, by = "Date") %>% 
  clean_names() %>% 
  rename(discharge = "x_00060_00003" ) 

ggplot(discharge_pulse, aes(x = storage_50cm, y = discharge)) + 
  geom_point()

#===============================================================================
# Now bring in Wet Network Proportion

wet_prop <- read_csv("konza_stic_round1_wet_prop.csv") %>% 
  rename(date_time = datetime)

df <- left_join(wet_prop, discharge_pulse, by = "date_time") %>% 
  drop_na()

a <- ggplot(df, aes(x = date_time, y = percent)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_cowplot() + 
  xlab("datetime") + 
  ylab("wet proportion") +
  theme(axis.title.x=element_blank())


a

b <- ggplot(df, aes(x = date_time, y = storage_50cm)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_cowplot() +
  xlab("datetime") + 
  ylab("storage 50 cm") +
  theme(axis.title.x=element_blank())

b


a/b

c <- ggplot(df, aes(x = date_time, y =  precipitation)) +
  geom_line(size = 1, color = "steelblue") + 
  theme_cowplot() +
  xlab("datetime") + 
  ylab("precip")

c

# three panel fig
a/b/c


#================================================================================================
#================================================================================================


