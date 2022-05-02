#### Compare STIC data from N04D and Konza pulse station 3

rm(list=ls()) # removes all objects from workspace when you use list=ls() as base. basically it is like restarting R session


library(tidyverse)
library(dataRetrieval)
library(readxl)
library(cowplot)

station_3 <- read_csv("station_3.csv")

station_3 <- station_3 %>% 
  mutate(DateTime = lubridate::ymd_hms(datetime))


station_3 <- station_3 %>% 
  rename(Storage50cm = 'Storage 50 cm') %>% 
  rename(AtmosphericPressure = `Atmospheric Pressure`)

ggplot(station_3, aes(x = Precipitation, y = AtmosphericPressure)) + 
  geom_point(size = 1) + 
  theme_bw()


rm(station_3)

### STIC stuff for 04M01_1



#### No second Dataset for 04M01_1
###### However, this is always wet, so trying 04M02_1 
stic_04M01_2_jan_2022 <- read_csv("04M01_2_Jan_2022.csv", 
                                  col_types = cols(datetime = col_datetime(format = "%m/%d/%Y %H:%M")))



ggplot(stic_04M01_2_jan_2022, aes(x = datetime, y = conductivity)) + 
  geom_line() = theme_bw()



ggplot(stic_04M01_2_jan_2022, aes(x = datetime, y = temperature)) + 
  geom_line() + 
  theme_bw()


#### trying 04M05_1 

stic_21064694_pt_1 <- read_csv("stic_21064694_pt_1.csv", 
                               col_types = cols(logger = col_character(), 
                                                datetime = col_datetime(format = "%m/%d/%Y %H:%M")))


stic_21064694_pt_2 <- read_csv("stic_21046494_pt_2.csv", 
                               col_types = cols(logger = col_character(), 
                                                datetime = col_datetime(format = "%m/%d/%Y %H:%M")))


full_04M05_1 <- bind_rows(stic_21064694_pt_1, stic_21064694_pt_2)



pulse_3_and_04M05_1 <- left_join(full_04M05_1, station_3, by = "datetime")



ggplot(pulse_3_and_04M05_1, aes(x = datetime, y = temperature)) + 
  geom_line(color = "steelblue") + 
  geom_line(data = pulse_3_and_04M05_1, aes(x = datetime, y = airtemp), color = "red", size = 1) + 
  theme_bw()



ggplot(pulse_3_and_04M05_1, aes(x = datetime, y = VPD)) + 
  geom_line(color = "steelblue")


station_3 <- station_3 %>% 
  rename(air_temp = 'Air Temperature')

station_3 <- station_3 %>% 
  mutate(temperature = air_temp)


ggplot(station_3, aes(x = datetime, y = air_temp)) + 
  geom_point()




pulse_3_and_04M05_1 <- pulse_3_and_04M05_1 %>% 
  rename(airtemp = 'Air Temperature')



ggplot(data = NULL, aes(x = datetime, y = temperature)) +
  geom_line(data = full_04M05_1, color = "grey20",size = 0.5) +
  geom_line(data = station_3, color = 'steelblue', size = 0.5) +
  theme_classic()


a <- ggplot(station_3, aes (x = datetime, y = Precipitation)) + 
  geom_line(color = 'steelblue', size = 1) + 
  theme_bw() + 
  ylim(0, 30)


ggplot(station_3, aes (x = datetime, y = Precipitation)) + 
  geom_line(color = 'steelblue', size = 1) + 
  ylim(0, 30) + 
  theme_bw()


############################################################################################################################################
############################################################################################################################################

### 01/19/22 stations 3 and 14 with STIC and piezo data 

rm(list=ls()) # removes all objects from workspace when you use list=ls() as base. basically it is like restarting R session

#### Bring in station 3, station 14, 04M05 STIC, and 04M05 piezo 

station_3 <- read_csv("C:/Users/cwhee/Desktop/R_Directory/station_3_072021_012022.csv", 
                      col_types = cols(datetime = col_character()))

station_14 <- read_csv("C:/Users/cwhee/Desktop/R_Directory/station_14_072021_01122022.csv", 
                       col_types = cols(datetime = col_character()))


station_3$station_id <- "Station_3"

station_14$station_id <- "Station_14"


pulse <- bind_rows(station_3, station_14)

pulse <- pulse %>% 
  mutate(datetime = lubridate::ymd_hms(datetime))


ggplot(pulse, aes(x = datetime, y = Precipitation, color = station_id)) + 
  geom_line(size = 1)


pulse <- pulse %>% 
  rename(airtemp = 'Air Temperature') %>% 
  rename(storage50cm = 'Storage 50 cm') %>% 
  rename(precip = Precipitation)







ggplot(pulse, aes(x = datetime, y = airtemp, color = station_id)) + 
  geom_line(size = 1, alpha = 0.5) + 
  theme_bw()

names(pulse)


ggplot(pulse, aes(x = datetime, y = storage50cm, color = station_id)) + 
  geom_line(size = 1) + 
  theme_bw()


################################################################################################################################################################################
################################################################################################################################################################################

### 04M05 has DTW measurements for both 

###bring in barologger

baro <- read_csv("raw_piezos/20962816_SFM01_Barologger_20211011.csv", 
                 col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))


### bring in 04M05 piezometer 

P_04M05 <- read_csv("raw_piezos/20962794_04M05_P_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

### bring in 04M05 stilling well

S_04M05 <- read_csv("raw_piezos/20962818_04M05_S_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

#### join all three 

baro_p_04M05 <- left_join(baro, P_04M05, by = "DateTime")

Full_04M05 <- left_join(baro_p_04M05, S_04M05)



### subtracting baro from absolute for both Piezo and SW and dividing by density * gravity of water

Full_04M05 <- Full_04M05 %>% 
  mutate(piezo_height = (piezo_press - Abs_Pres_kPa)/9.80665) %>% 
  mutate(sw_height = (sw_press - Abs_Pres_kPa)/9.80665)

ggplot(Full_04M05, aes(x = DateTime, y = piezo_height)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_height), color = "blue") + 
  theme_bw()


#### Using reference level to set datum and ground surface 

### piezo water relative to ground on 10/11/21 at 1:00 pm: 0.053 m
### SW water relative to ground on 10/11/21 at 1:00 pm: 0.124 m

### piezo height from transducer on 10/11/21 at 11:00 am: 0.5090423 m
### SW height from transducer on 10/11/21 at 11:00 am: 0.09299812 m


### piezo correction: - 0.4560423
### SW correction: + 0.03100188


Full_04M05 <- Full_04M05 %>% 
  mutate(piezo_level = piezo_height - 0.4560423) %>% 
  mutate(sw_level = sw_height + 0.03100188)


### test that levels are working 

ggplot(Full_04M05, aes(x = DateTime, y = piezo_level)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_level), color = "blue") + 
  theme_bw()




### Calculating vertical hydraulic gradient (VHG): delta h - delta l 
### delta h is the level in piezo minus level of in stream surface 
### delta l is depth from stream surface to top of screen 
### delta l = 0.277

Full_04M05 <- Full_04M05 %>% 
  mutate(vhg = (piezo_level - sw_level)/0.277)

ggplot(Full_04M05, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  scale_y_continuous(trans = "reverse") +
  theme_bw()

ggplot(Full_04M05, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  theme_bw()


################################################################################################################################################################################
################################################################################################################################################################################


### cutting off PT data to August 8th

full_04M05_cut <- Full_04M05 %>% 
  dplyr::filter(DateTime >= "2021-08-08")

full_04M05_cut <- full_04M05_cut %>% 
  rename(datetime = DateTime)



full_04M05_cut$station_id <- "paired_piezo"


pt_pulse <- left_join(pulse, full_04M05_cut, by = "datetime")


pt_pulse_cut <- pt_pulse %>% 
  dplyr::filter(datetime >= "2021-08-08")


################################################################################################################################################################################
################################################################################################################################################################################


### mutipanel graphs  



################################################################################################################################################################################
################################################################################################################################################################################

station_3 <- station_3 %>% 
  mutate(datetime = lubridate::ymd_hms(datetime))



station_3_pt <- left_join(station_3, full_04M05_cut, by = "datetime")



station_3_pt <- station_3_pt %>% 
  filter(datetime >= "2021-08-08", datetime <= "2021-10-11")

station_3_pt <- station_3_pt %>% 
  rename(storage50cm = 'Storage 50 cm')



a <- ggplot(station_3_pt, aes(x = datetime, y = storage50cm)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_classic()


b <- ggplot(station_3_pt, aes(x = datetime, y = sw_level)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_classic()


c <- ggplot(station_3_pt, aes(x = datetime, y = piezo_level)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_classic()

d <- ggplot(station_3_pt, aes(x = datetime, y = Precipitation)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_classic()


library(cowplot)

plot_grid(a, b, c, d, nrow = 4, ncol = 1, align = "v")


################################################################################################################################################################################
################################################################################################################################################################################

### add in 04M05 STIC


stic_21064694_pt_1 <- read_csv("stic_21064694_pt_1.csv", 
                               col_types = cols(logger = col_character(), 
                                                datetime = col_datetime(format = "%m/%d/%Y %H:%M")))


stic_21064694_pt_2 <- read_csv("stic_21046494_pt_2.csv", 
                               col_types = cols(logger = col_character(), 
                                                datetime = col_datetime(format = "%m/%d/%Y %H:%M")))


full_04M05_1 <- bind_rows(stic_21064694_pt_1, stic_21064694_pt_2)


full_04M05_1 <- full_04M05_1 %>% 
  filter(datetime >= "2021-08-08", datetime <= "2021-10-11")


station_3_pt_stic <- left_join(station_3_pt, full_04M05_1, by = "datetime")



e <- ggplot(station_3_pt_stic, aes(x = datetime, y = conductivity)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_classic()

plot_grid(a, b, c, d, e, nrow = 5, ncol = 1, align = "v")


################################################################################################################################################################################
###########################################################################################################################################################################



aaa <- ggplot(station_3_pt_stic, aes(x = storage50cm, y = piezo_level)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth()



bbb <- ggplot(station_3_pt_stic, aes(x = storage50cm, y = sw_level)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth()


plot_grid(aaa, bbb, nrow = 2, ncol = 1, align = "v")


model1 <- lm(piezo_level ~ storage50cm, station_3_pt_stic)

model2 <- lm(sw_level ~ storage50cm, station_3_pt_stic)




summary(model1)

summary(model2)



################################################################################################################################################################################
###########################################################################################################################################################################
#### Repeat same thing with station 14 and not station 3

station_14 <- station_14 %>% 
  mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
  rename(airtemp = 'Air Temperature') %>% 
  rename(storage50cm = 'Storage 50 cm')




full_04M05_stat_14 <- left_join(station_14, full_04M05_1, by = "datetime")


Full_04M05 <- Full_04M05 %>% 
  rename(datetime = DateTime)



stat14full <- left_join(full_04M05_stat_14, Full_04M05, by = "datetime")


ggplot(stat14full, aes(x = storage50cm, y = piezo_level)) + 
  geom_point() + 
  theme_bw()



ggplot(stat14full, aes(x = storage50cm, y = sw_level)) + 
  geom_point() + 
  theme_bw()


################################################################################################################################################
################################################################################################################################################

#### STIC total percent wet time series versus storage



### loading in processed stics and 

data_dir <- "processed_stic_names"

fs::dir_ls(data_dir)

length(data_dir)

stic_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")


stic_files_row <- stic_files %>% 
  map_dfr(read_csv)


### Calculating wet network proportion for all of them

### Make binary wet/dry column based on 50 conductivity 

stic_files_binary <- stic_files_row %>% 
  dplyr::mutate(wetdry = if_else(conductivity >= 1000, "wet", "dry" ))

### Calculate wet network proportion

stic_wet_prop <- stic_files_binary %>% 
  group_by(datetime) %>% 
  summarise(n_wet = sum(wetdry == "wet"), n_sensors = n() ) %>% 
  mutate(percent = n_wet/n_sensors)

wet_prop_cut_off <- stic_wet_prop %>% 
  dplyr::filter(datetime >= "2021-05-23")

#### Join data with pulse storage 

prop_wet_storage <- left_join(station_3, wet_prop_cut_off, by = "datetime")


prop_wet_storage <- prop_wet_storage %>% 
  filter(datetime <= "2021-09-15")


ggplot(prop_wet_storage, aes(x = storage50cm, y = percent)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() 

names(prop_wet_storage)


prop_wet_storage <- prop_wet_storage %>% 
  rename(storage50cm = 'Storage 50 cm')


###### Make time series with both values 

station_3$category <- "storage_50cm"

wet_prop_cut_off$category <- "proportion"


bindedrowstorepulse <- bind_rows(station_3, wet_prop_cut_off)


ggplot(bindedrowstorepulse)

#########################################################################################################################################

###Above is not working. Different approach to time series


z <- ggplot(prop_wet_storage, aes(x = datetime, y = percent)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_bw()

z


y <- ggplot(prop_wet_storage, aes(x = datetime, y = storage50cm)) + 
  geom_line(size = 1, color = "steelblue") + 
  theme_bw()

y



plot_grid(z, y, nrow = 2, ncol = 1, align = "v")




################################################################################################################################################
################################################################################################################################################

### Linear regressions for cut off data sets 

### Surface water level starting at 175

station_3_pt_stic_cut_175 <- station_3_pt_stic %>% 
  filter(storage50cm >= 175)



ggplot(station_3_pt_stic_cut_175, aes(x = storage50cm, y = sw_level)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_bw()


model3 <- lm(sw_level ~ storage50cm, station_3_pt_stic_cut_175)

summary(model3)


### Surface water level starting at 163

station_3_pt_stic_cut_163 <- station_3_pt_stic %>% 
  filter(storage50cm >= 163)



ggplot(station_3_pt_stic_cut_163, aes(x = storage50cm, y = piezo_level)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic()


model4 <- lm(piezo_level ~ storage50cm, station_3_pt_stic_cut_163)

summary(model3)


summary(model4)


################################################################################################################################################
################################################################################################################################################

### Find well located most closely to 04M05


AGW011 <- read_csv("AGW011.csv")

AGW012 <- read_csv("AGW012.csv")

AGW031 <- read_csv("AGW031.csv")


unique(AGW031$Wellname)


ow46mor <- AGW031 %>% 
  filter(Wellname == "OW4-6Mor")


ow35mor <- AGW031 %>% 
  filter(Wellname == "OW3-5Mor")


ow351mor <- AGW031 %>% 
  filter(Wellname == "OW3-5-1Mor")

### bring in piezo data 


################################################################################################################################################################################
################################################################################################################################################################################

### 04M05 has DTW measurements for both 

###bring in barologger

baro <- read_csv("raw_piezos/20962816_SFM01_Barologger_20211011.csv", 
                 col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))


### bring in 04M05 piezometer 

P_04M05 <- read_csv("raw_piezos/20962794_04M05_P_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

### bring in 04M05 stilling well

S_04M05 <- read_csv("raw_piezos/20962818_04M05_S_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

#### join all three 

baro_p_04M05 <- left_join(baro, P_04M05, by = "DateTime")

Full_04M05 <- left_join(baro_p_04M05, S_04M05)



### subtracting baro from absolute for both Piezo and SW and dividing by density * gravity of water

Full_04M05 <- Full_04M05 %>% 
  mutate(piezo_height = (piezo_press - Abs_Pres_kPa)/9.80665) %>% 
  mutate(sw_height = (sw_press - Abs_Pres_kPa)/9.80665)

ggplot(Full_04M05, aes(x = DateTime, y = piezo_height)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_height), color = "blue") + 
  theme_bw()


#### Using reference level to set datum and ground surface 

### piezo water relative to ground on 10/11/21 at 1:00 pm: 0.053 m
### SW water relative to ground on 10/11/21 at 1:00 pm: 0.124 m

### piezo height from transducer on 10/11/21 at 11:00 am: 0.5090423 m
### SW height from transducer on 10/11/21 at 11:00 am: 0.09299812 m


### piezo correction: - 0.4560423
### SW correction: + 0.03100188


Full_04M05 <- Full_04M05 %>% 
  mutate(piezo_level = piezo_height - 0.4560423) %>% 
  mutate(sw_level = sw_height + 0.03100188)

################################################################################################################################################################################
################################################################################################################################################################################


ow351mor <- ow351mor %>% 
  rename(DateTime = WLDate)

ow35mor <- ow35mor %>% 
  rename(DateTime = WLDate)

ow46mor <- ow46mor %>% 
  rename(DateTime = WLDate)



ow351mor <- ow351mor %>% 
  mutate(DateTime = lubridate::mdy_hms(DateTime))


ow35mor <- ow35mor %>% 
  mutate(DateTime = lubridate::mdy_hms(DateTime))



ow46mor <- ow46mor %>% 
  mutate(DateTime = lubridate::mdy_hms(DateTime))







test1 <- left_join(Full_04M05, ow351mor, by = "DateTime")



test2 <- left_join(Full_04M05, ow35mor, by = "DateTime")


test3 <- left_join(Full_04M05, ow46mor, by = "DateTime")


################################################################################################################################################################################
################################################################################################################################################################################

### Messing with new APT011

APT011 <- read_csv("APT011.csv", col_types = cols(RecDate = col_date(format = "%m/%d/%Y"), 
                                                  ppt = col_number()))

unique(APT011$watershed)


ggplot(APT011, aes(x = RecDate, y = ppt)) + 
  geom_line() + 
  facet_wrap(~ watershed, scales = "free") + 
  theme_cowplot()


hq <- APT011 %>% 
  filter(watershed == "HQ")


ggplot(hq, aes(x = RecDate, y = ppt)) + 
  geom_line() + 
  theme_cowplot()

ggplot(hq, aes(x = RecDate, y = ppt)) + 
  geom_line() + 
  theme_cowplot(12)

