# STIC wrangling (first two rounds at Konza

#### CTW - Feb 2022
#### Importing multiple "Clean" STIC files from the same folder and binding
### Creating binary wet/dry dataset from relative conductivity
### Determining basic flow metrics


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


rm(list=ls()) #clearing workspace

### loading in processed stics 
### NOTE: for this script to work you need to have a single folder
### in your working directory with all the STIC files in it 

data_dir <- "konza_stics_round_1"

fs::dir_ls(data_dir)

length(data_dir)

stic_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")

### using the map_dfr function from tidyverse to loop in individual csvs and bind rows
stic_files <- stic_files %>% 
  map_dfr(read_csv)

### Creating binary wet/dry column based of relative cond value of 1000
stic_files <- stic_files %>% 
  dplyr::mutate(wetdry = if_else(conductivity >= 1000, "wet", "dry" ))

### Making another datetime column in tidy date format 
stic_files <- stic_files %>% 
  mutate(date = lubridate::date(datetime))

### NOTE: this command is just trimming the date range to the time when all the loggers were active
### It is not something that is always neccessary
stic_files <- stic_files%>% 
  dplyr::filter(datetime >= "2021-05-23")

### using lubridate to separate out the individual days 
stic_files <- stic_files %>% 
  mutate(day = lubridate::yday(date))


### Now calculating the percentage of wet STICs at each time step

### Calculate wet network proportion

stic_wet_prop_r1 <- stic_files %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry == "wet"), n_sensors = n() ) %>% 
  mutate(percent = n_wet/n_sensors)


#========================================================================================================================
# now wet network proportion for second round


data_dir <- "konza_stics_round_2"

fs::dir_ls(data_dir)

length(data_dir)

stic_files_r2 <- fs::dir_ls(data_dir, regexp = "\\.csv$")

### using the map_dfr function from tidyverse to loop in individual csvs and bind rows
stic_files_r2 <- stic_files_r2 %>% 
  map_dfr(read_csv)

### Creating binary wet/dry column based of relative cond value of 1000
stic_files_r2 <- stic_files_r2 %>% 
  dplyr::mutate(wetdry = if_else(conductivity >= 1000, "wet", "dry" ))

### Making another datetime column in tidy date format 
stic_files_r2 <- stic_files_r2 %>% 
  mutate(date = lubridate::date(datetime))

### NOTE: this command is just trimming the date range to the time when all the loggers were active
### It is not something that is always neccessary
stic_files_r2 <- stic_files_r2%>% 
  dplyr::filter(datetime >= "2021-09-30" & datetime <= "2022-01-12")

### using lubridate to separate out the individual days 
stic_files_r2 <- stic_files_r2 %>% 
  dplyr::mutate(day = lubridate::yday(date))


### Now calculating the percentage of wet STICs at each time step

### Calculate wet network proportion

stic_wet_prop_r2 <- stic_files_r2 %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry == "wet"), n_sensors = n() ) %>% 
  mutate(percent = n_wet/n_sensors) 

full_wet_prop <- bind_rows(stic_wet_prop_r1, stic_wet_prop_r2)

full_wet_prop <- full_wet_prop %>% 
  drop_na()


ggplot(full_wet_prop, aes(x = datetime, y = percent)) + 
  geom_line() + 
  theme_cowplot()


#========================================================================================================================
# now combine with pulse data to make three panel

station_1 <- read_csv("data/s_1.csv") %>% 
  clean_names()

station_1 <- station_1 %>% 
  dplyr::mutate(datetime = lubridate::ymd_hms(datetime))

df <- left_join(full_wet_prop, station_1, by = "datetime") %>% 
  drop_na()

a <- ggplot(df, aes(x = datetime, y = percent)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_cowplot() + 
  xlab("datetime") + 
  ylab("wet proportion") +
  theme(axis.title.x=element_blank())


a

b <- ggplot(df, aes(x = datetime, y = storage_50_cm)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_cowplot() +
  xlab("datetime") + 
  ylab("storage 50 cm") +
  theme(axis.title.x=element_blank())

b


a/b

c <- ggplot(df, aes(x = datetime, y =  precipitation)) +
  geom_line(size = 1.3, color = "steelblue") + 
  theme_cowplot() +
  xlab("datetime") + 
  ylab("precip")

c

# three panel fig
a/b/c

#================================================================================================================
# Now try analysis based on outcropping units: compare to flow duration
# first get just n04d durations




