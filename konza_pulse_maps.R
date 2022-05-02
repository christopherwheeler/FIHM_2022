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

k_p <- bind_rows(station_1, station_2, station_3, station_4, station_5, station_6, 
                 station_7, station_8, station_9, station_10, station_11, station_12,
                 station_13, station_14, station_15, station_16)


# clean up the data frame

k_p <- k_p %>% 
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
  janitor::clean_names() %>% 
  dplyr::select(- x1) %>% 
  dplyr::mutate(id = as.numeric(id))

# bring in coords

pulse_coords <- read_csv("pulse_coords.csv", 
                         col_types = cols(id = col_number()))


pulse_map <- dplyr::left_join(pulse_coords, k_p, by = "id")

### Make Duration Map


pulse_locs <- st_as_sf(pulse_map,coords = c("long", "lat"), crs = 4326)

konza_streams <- st_read(
  "GIS210/GIS210.shp")

konza_boundary <- st_read(
  "GIS002/GIS002.shp")

ggplot() + 
  geom_sf(data = konza_boundary) +
  geom_sf(data = konza_streams) + 
  geom_sf(data = pulse_locs, size = 4) +
  scale_color_viridis_c() +
  theme_cowplot() +
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15)) + 
  theme(plot.title = element_text(size=23)) + 
  xlab("Longitude") + 
  ylab("Latitude")


 #  coord_sf(xlim = c(708000.9  , 710500.3 ), ylim = c(4327200.8  , 4330000.0 ), expand = FALSE) +


# now animted map

pulse_anim <- ggplot() + 
  geom_sf(data = konza_boundary) +
  geom_sf(data = konza_streams) + 
  geom_sf(data = pulse_locs, size = 4, aes(color = storage_50_cm)) +
  scale_color_viridis_c() +
  theme_cowplot() +
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15)) + 
  theme(plot.title = element_text(size=23)) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  transition_manual(datetime) + 
  labs(title = 'datetime: {current_frame}')


animate(pulse_anim)

anim_save("pulse_anim.gif")

animate(pulse_anim, fps = 5)

anim_save("youngmeyer_anim_v7.gif")


# lets just do a still map with average storage 


mean_kp <- pulse_locs %>% 
  group_by(id) %>% 
  summarize(storage_50cm = mean(storage_50_cm, na.rm = TRUE))


ggplot() + 
  geom_sf(data = konza_boundary) +
  geom_sf(data = konza_streams) + 
  geom_sf(data = mean_kp, size = 4, aes(color = storage_50cm)) +
  scale_color_viridis_c(direction = -1, name = "mean storage 50 cm") +
  theme_cowplot() +
  xlab("Longitude") + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Konza Pulse (July 2021 - April 2022)") + 
  theme(plot.title = element_text(hjust = 0.5))

# now doing mean temp


mean_lt <- pulse_locs %>% 
  group_by(id) %>% 
  summarize(logger_temperature = mean(logger_temperature, na.rm = TRUE))

mean_temp <- mean_lt %>%
  filter(!row_number() %in% 5)

ggplot() + 
  geom_sf(data = konza_boundary) +
  geom_sf(data = konza_streams) + 
  geom_sf(data = mean_temp, size = 4, aes(color = logger_temperature)) +
  scale_color_viridis_c() +
  theme_cowplot() +
  xlab("Longitude") + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Konza Pulse (July 2021 - April 2022)") + 
  theme(plot.title = element_text(hjust = 0.5))



