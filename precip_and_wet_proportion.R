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

# Konza Pulse Maps

rm(list=ls())

library(tidyverse)
library(patchwork)
library(janitor)
library(fs)
library(Rcpp)
library(sp)
library(raster)
library(rgdal)
library(rasterVis)
library(sf)
library(dataRetrieval)
library(cowplot)
library(mapview)
library(readxl)
library(patchwork)
library(ggthemes)
library(gganimate)

# bring in data from all stations and  row bind

station_1 <- read_csv("pulse_data/s_1.csv")
station_2 <- read_csv("pulse_data/s_2.csv")
station_3 <- read_csv("pulse_data/s_3.csv")
station_4 <- read_csv("pulse_data/s_4.csv")
station_5 <- read_csv("pulse_data/s_5.csv")
station_6 <- read_csv("pulse_data/s_6.csv")
station_7 <- read_csv("pulse_data/s_7.csv")
station_8 <- read_csv("pulse_data/s_8.csv")
station_9 <- read_csv("pulse_data/s_9.csv")
station_10 <- read_csv("pulse_data/s_10.csv")
station_11 <- read_csv("pulse_data/s_11.csv")
station_12 <- read_csv("pulse_data/s_12.csv")
station_13 <- read_csv("pulse_data/s_13.csv")
station_14 <- read_csv("pulse_data/s_14.csv")
station_15 <- read_csv("pulse_data/s_15.csv")
station_16 <- read_csv("pulse_data/s_16.csv")

station_1$id <- "01"
station_2$id <- "02"
station_3$id <- "03"
station_4$id <- "04"
station_5$id <- "05"
station_6$id <- "06"
station_7$id <- "07"
station_8$id <- "08"
station_9$id <- "09"
station_10$id <- "10"
station_11$id <- "11"
station_12$id <- "12"
station_13$id <- "13"
station_14$id <- "14"
station_15$id <- "15"
station_16$id <- "16"

df <- bind_rows(station_1, station_2, station_3, station_4, station_5, station_6, 
                 station_7, station_8, station_9, station_10, station_11, station_12,
                 station_13, station_14, station_15, station_16)


# clean up the data frame

df <- df %>% 
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
  janitor::clean_names() %>% 
  dplyr::select(- x1) %>% 
  dplyr::mutate(id = as.numeric(id)) %>% 
  dplyr::mutate(id_f = as_factor(id))


ggplot(df, aes(x = datetime, y = storage_50_cm)) + 
  geom_line(color = "steelblue", size = 1) + 
  facet_wrap(~ id) + 
  theme_cowplot()

ggplot(df, aes(x = datetime, y = storage_50_cm, color = id_f)) + 
  geom_line(size = 1) + 
  theme_cowplot()



