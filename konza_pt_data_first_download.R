
### Paired Piezometer Conversions (Konza PT Data August through October 10th)

rm(list=ls()) # removes all objects from workspace when you use list=ls() as base. basically it is like restarting R session

library(tidyverse)

### SFM07

###bring in barologger

baro <- read_csv("raw_piezos/20962816_SFM01_Barologger_20211011.csv", 
                 col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))


### bring in SFM07 piezometer 

SFM07_P <- read_csv("raw_piezos/20962822_SFM07_P_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

SFM07_P <- SFM07_P %>% 
  rename(piezo_pres = Abs_Pres_kPa) %>% 
  rename(piezo_temp = Temp)


### bring in SFM07 stilling well

SFM07_S <- read_csv("raw_piezos/20962803_SFM07_S_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

SFM07_S <- SFM07_S %>% 
  rename(sw_pressure = Abs_Pres_kPa) %>% 
  rename(sw_temp = Temp)


#### join all three 

rm(SFM07)

baro_p <- left_join(baro, SFM07_P, by = "DateTime")

SFM07 <- left_join(baro_p, SFM07_S)

### subtracting baro from absolute for both Piezo and SW and dividing by density * gravity of water

SFM07 <- SFM07 %>% 
  mutate(piezo_height = (piezo_pres - Abs_Pres_kPa)/9.80665) %>% 
  mutate(sw_height = (sw_pressure - Abs_Pres_kPa)/9.80665)

ggplot(SFM07, aes(x = DateTime, y = piezo_height)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_height), color = "blue") + 
  theme_bw()


#### Using reference level to set datum and ground surface 
#### tricky because both P and SW were dry both times

### Correct SW by adding 0.03752556

### correct piezo by taking -0.03752556 + 0.03752556  (dry height value when first put in) minus
### 1.75 (total height) - 1.185 (above ground height) + 0.01 (sensor was 1cm above bottom)

1.75 - 1.185 + 0.01


SFM07 <- SFM07 %>% 
  mutate(piezo_level = piezo_height + 0.03752556 - 0.575) %>% 
  mutate(sw_level = sw_height + 0.03752556)


### test that levels are working 

ggplot(SFM07, aes(x = DateTime, y = piezo_level)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_level), color = "blue") + 
  theme_bw()


### Calculating vertical hydraulic gradient (VHG): delta h - delta l 
### delta h is the level in piezo minus level of in stream surface 
### delta l is depth from stream surface to top of screen 
### delta l = 0.315

SFM07 <- SFM07 %>% 
  mutate(vhg = (piezo_level - sw_level)/0.315)

ggplot(SFM07, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.5)


################################################################################################################################################################################
################################################################################################################################################################################

### 04M03 has DTW measurements for both 

###bring in barologger

baro <- read_csv("raw_piezos/20962816_SFM01_Barologger_20211011.csv", 
                 col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))


### bring in 04M03 piezometer 

P_04M03 <- read_csv("raw_piezos/20962767_04M03_P_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

### bring in 04M03 stilling well

S_04M03 <- read_csv("raw_piezos/20962775_04M03_S_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

#### join all three 

baro_p_04M03 <- left_join(baro, P_04M03, by = "DateTime")

Full_04M03 <- left_join(baro_p_04M03, S_04M03)

### subtracting baro from absolute for both Piezo and SW and dividing by density * gravity of water

Full_04M03 <- Full_04M03 %>% 
  mutate(piezo_height = (piezo_press - Abs_Pres_kPa)/9.80665) %>% 
  mutate(sw_height = (sw_press - Abs_Pres_kPa)/9.80665)

ggplot(Full_04M03, aes(x = DateTime, y = piezo_height)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_height), color = "blue") + 
  theme_bw()


#### Using reference level to set datum and ground surface 

### piezo water relative to ground on 10/11/21 at 1:00 pm: -0.237 m   
### SW water relative to ground on 10/11/21 at 1:00 pm: 0.058 m 

### piezo height from transducer on 10/11/21 at 11:00 am: 0.27134648
### SW height from transducer on 10/11/21 at 11:00 am: 0.01672335

### piezo correction: - 0.5083465
### SW correction: + 0.04127665


Full_04M03 <- Full_04M03 %>% 
  mutate(piezo_level = piezo_height - 0.5083465) %>% 
  mutate(sw_level = sw_height + 0.04127665)


### test that levels are working 

ggplot(Full_04M03, aes(x = DateTime, y = piezo_level)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_level), color = "blue") + 
  theme_bw()




### Calculating vertical hydraulic gradient (VHG): delta h - delta l 
### delta h is the level in piezo minus level of in stream surface 
### delta l is depth from stream surface to top of screen 
### delta l = 0.315

Full_04M03 <- Full_04M03 %>% 
  mutate(vhg = (piezo_level - sw_level)/0.315)

ggplot(Full_04M03, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  scale_y_continuous(trans = "reverse") +
  theme_bw()

ggplot(Full_04M03, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
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


### Correct SFM07
Full_SFM07 <- SFM07



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

ggplot(Full_04M03, aes(x = DateTime, y = piezo_level)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_level), color = "blue") + 
  theme_bw()




### Calculating vertical hydraulic gradient (VHG): delta h - delta l 
### delta h is the level in piezo minus level of in stream surface 
### delta l is depth from stream surface to top of screen 
### delta l = 0.277

Full_04M05 <- Full_04M05 %>% 
  mutate(vhg = (piezo_level - sw_level)/0.277)

ggplot(Full_04M03, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  scale_y_continuous(trans = "reverse") +
  theme_bw()

ggplot(Full_04M03, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  theme_bw()


################################################################################################################################################################################
################################################################################################################################################################################

### 04M09 has DTW measurements for both 

###bring in barologger

baro <- read_csv("raw_piezos/20962816_SFM01_Barologger_20211011.csv", 
                 col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))


### bring in 04M09 piezometer 

P_04M09 <- read_csv("raw_piezos/20962784_04M09_P_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

### bring in 04M09 stilling well

S_04M09 <- read_csv("raw_piezos/20962807_04M09_S_20211011.csv", 
                    col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

#### join all three 

baro_p_04M09 <- left_join(baro, P_04M09, by = "DateTime")

Full_04M09 <- left_join(baro_p_04M09, S_04M09)

### subtracting baro from absolute for both Piezo and SW and dividing by density * gravity of water

Full_04M09 <- Full_04M09 %>% 
  mutate(piezo_height = (piezo_press - Abs_Pres_kPa)/9.80665) %>% 
  mutate(sw_height = (sw_press - Abs_Pres_kPa)/9.80665)

ggplot(Full_04M09, aes(x = DateTime, y = piezo_height)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_height), color = "blue") + 
  theme_bw()


#### Using reference level to set datum and ground surface 

### piezo water relative to ground on 10/11/21 at 1:00 pm: 0.063 m
### SW water relative to ground on 10/11/21 at 1:00 pm: 0.104 m

### piezo height from transducer on 10/11/21 at 11:00 am: 0.5493211 m
### SW height from transducer on 10/11/21 at 11:00 am: 0.0271244513 m


### piezo correction: - 0.4863211
### SW correction: + 0.07687555


Full_04M09 <- Full_04M09 %>% 
  mutate(piezo_level = piezo_height - 0.4863211) %>% 
  mutate(sw_level = sw_height + 0.07687555)


### test that levels are working 

ggplot(Full_04M09, aes(x = DateTime, y = piezo_level)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = sw_level), color = "blue") + 
  theme_bw()




### Calculating vertical hydraulic gradient (VHG): delta h - delta l 
### delta h is the level in piezo minus level of in stream surface 
### delta l is depth from stream surface to top of screen 
### delta l = 0.317

Full_04M09 <- Full_04M09 %>% 
  mutate(vhg = (piezo_level - sw_level)/0.317)

ggplot(Full_04M09, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  scale_y_continuous(trans = "reverse") +
  theme_bw()

ggplot(Full_04M09, aes(x = DateTime, y = vhg)) + 
  geom_line() + 
  ylim(-2, 0.25) + 
  theme_bw()


################################################################################################################################################################################
################################################################################################################################################################################

#### Combine 04M03, 04M05, and 04M09

### make categorical column 

Full_04M03$site <- "site_04M03"

Full_04M05$site <- "site_04M05"

Full_04M09$site <- "site_04M09"


rbind_n04d <- bind_rows(Full_04M03, Full_04M05, Full_04M09)


ggplot(rbind_n04d, aes(x = DateTime, y = vhg, color = site)) + 
  geom_line(size = 1) + 
  theme_bw() +
  xlab("Date") + 
  ylab("Vertical Hydraulic Gradient") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))


ggplot(rbind_n04d, aes(x = site, y = vhg)) + 
  geom_boxplot() + 
  theme_classic() +
  xlab("Site") + 
  ylab("Vertical Hydraulic Gradient") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

################################################################################################################################################################################
################################################################################################################################################################################

### Bring in and join three STIC sites (high sites only)

stic_04M03_1 <- read_csv("processed_stic_names/04M03_1.csv")

stic_04M05_1 <- read_csv("processed_stic_names/04M05_1.csv")

stic_04M09_1 <- read_csv("processed_stic_names/04M09_1.csv")


stic_04M03_1 <- stic_04M03_1 %>% 
  rename(sn_04M03_1 = logger) %>% 
  rename(temp_04M03_1 = temperature) %>% 
  rename(cond_04M03_1 = conductivity) %>% 
  mutate(DateTime = lubridate::ymd_hms(datetime))

stic_04M05_1 <- stic_04M05_1 %>% 
  rename(sn_04M05_1 = logger) %>% 
  rename(temp_04M05_1 = temperature) %>% 
  rename(cond_04M05_1 = conductivity) %>% 
  mutate(DateTime = lubridate::ymd_hms(datetime))


stic_04M09_1 <- stic_04M09_1 %>% 
  rename(sn_04M09_1 = logger) %>% 
  rename(temp_04M09_1 = temperature) %>% 
  rename(cond_04M09_1 = conductivity) %>% 
  mutate(DateTime = lubridate::ymd_hms(datetime))


intermediate_1 <- left_join(rbind_n04d, stic_04M03_1, by = "DateTime")


intermediate_2 <- left_join(intermediate_1, stic_04M05_1, by = "DateTime")

pt_and_stic <- left_join(intermediate_2, stic_04M09_1, by = "DateTime")

################################################################################################################################################################################
################################################################################################################################################################################

#### generate the three binary data sets and graph stuff 
#### pt and stic didn't work, need to put back together again 


Full_04M03 <- Full_04M03 %>% 
  rename(vhg_04M03 = vhg)

Full_04M05 <- Full_04M05 %>% 
  rename(vhg_04M05 = vhg)

Full_04M09 <- Full_04M09 %>% 
  rename(vhg_04M09 = vhg)


int1 <- left_join(Full_04M03, Full_04M05, by = "DateTime")

joined_piezos <- left_join(int1, Full_04M09, by = "DateTime")


joined_vhg <- joined_piezos %>% 
  select(DateTime, vhg_04M03, vhg_04M05, vhg_04M09)


tin1 <- left_join(joined_piezos, stic_04M03_1, by = "DateTime")

tin2 <- left_join(tin1, stic_04M05_1, by = "DateTime")

sticandpt <- left_join(tin2, stic_04M09_1, by = "DateTime")

stic_and_pt <- sticandpt %>% 
  select(DateTime, vhg_04M03, vhg_04M05, vhg_04M09, cond_04M03_1, cond_04M05_1,
         cond_04M09_1, temp_04M03_1, temp_04M05_1, temp_04M09_1)

################################################################################################################################################################################
################################################################################################################################################################################
### BAD PLOTS

ggplot(stic_and_pt, aes(x = vhg_04M03, y = cond_04M03_1)) + 
  geom_point()


ggplot(stic_and_pt, aes(x = DateTime, y = cond_04M03_1)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = vhg_04M03), color = "blue")

################################################################################################################################################################################
################################################################################################################################################################################
###why does this not work 

ggplot(stic_and_pt, aes(x = DateTime, y = cond_04M03_1)) + 
  geom_line(color = "red") + 
  geom_line(aes(x = DateTime, y = vhg_04M03), color = "blue") + 
  scale_y_continuous(
    "04M03 Conductivity", 
    sec.axis = sec_axis(~ . * 0.0000000001, name = "VHG") ) 

################################################################################################################################################################################
################################################################################################################################################################################

stic_and_pt_cutoff <- stic_and_pt %>% 
  dplyr::filter(DateTime >= "2021-08-03 17:00" & DateTime <= "2021-09-17 11:00")



a <- ggplot(stic_and_pt, aes(x = DateTime, y = cond_04M03_1)) + 
  geom_line(size = 1) +
  theme_bw() +
  xlab("Date") + 
  ylab("04M03 Cond.") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

b <- ggplot(stic_and_pt, aes(x = DateTime, y = vhg_04M03)) + 
  geom_line(size = 1)+
  theme_bw() +
  xlab("Date") + 
  ylab("04M03 VHG") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))


g1 <- plot_grid(a, b, align = "v", ncol = 1, nrow = 2, scale = 1)

################################################################################################################################################################################
################################################################################################################################################################################


c <- ggplot(stic_and_pt, aes(x = DateTime, y = cond_04M05_1)) + 
  geom_line(size = 1) +
  theme_bw() +
  xlab("Date") + 
  ylab("04M05 Cond.") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

d <- ggplot(stic_and_pt, aes(x = DateTime, y = vhg_04M05)) + 
  geom_line(size = 1)+
  theme_bw() +
  xlab("Date") + 
  ylab("04M05 VHG") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))


g2 <- plot_grid(c, d, align = "v", ncol = 1, nrow = 2, scale = 1)


################################################################################################################################################################################
################################################################################################################################################################################


e <- ggplot(stic_and_pt, aes(x = DateTime, y = cond_04M09_1)) + 
  geom_line(size = 1) +
  theme_bw() +
  xlab("Date") + 
  ylab("04M09 Cond.") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

f <- ggplot(stic_and_pt, aes(x = DateTime, y = vhg_04M09)) + 
  geom_line(size = 1)+
  theme_bw() +
  xlab("Date") + 
  ylab("04M09 VHG") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))




################################################################################################################################################################################
################################################################################################################################################################################
#### SW and piezo for n04d


write_csv(Full_04M03, "Full_04M03.csv")

stacked_04M03 <- read_csv("stacked_04M03.csv", 
                          col_types = cols(DateTime = col_character()))



stacked_04M03 <- stacked_04M03 %>% 
  mutate(datetime = lubridate::ymd_hms(DateTime))


ggplot(stacked_04M03, aes(x = datetime, y = level_from_streambed, color = transducer)) + 
  geom_line(size = 1) + 
  theme_bw() +
  ggtitle("04M03 (0.0 is streambed)") + 
  theme(plot.title = element_text(hjust = 0.5))


################################################################################################################################################################################
################################################################################################################################################################################
#### looking at PT data and 50cm storage



