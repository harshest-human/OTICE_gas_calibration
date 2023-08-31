########### PACKAGES ######################
getwd()
library(tidyverse)
library(psych)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(writexl)
library(readr)
library(vroom)
library(lubridate)
library(purrr)
library(data.table)
#source("D:/Data Analysis/GCDP/GCDP_function_script.R")
#source("https://raw.githubusercontent.com/harshest-human/GCDP/main/GCDP_function_script.R")


########### DATA IMPORTING ###############
#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV") #check today's date

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

#FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV") #check today's date


########### DATA CLEANING (Time series) ###############
FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")
FTIR_1 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()

OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

OTICE_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-06-27 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-06-30 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 	

ggline(OTICE_1, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_1, x="Date.time", y="NH3.O", color = "Sampling.point")


OTICE_2 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-06 08:22:24", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-06 11:16:35", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_2, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_2, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_3 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-12 16:07:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_3, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_3, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_4 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-31 11:07:35", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-10 11:03:14", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_4, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_4, x="Date.time", y="NH3.O", color = "Sampling.point")


OTICE_5 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-10 15:07:08", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_5, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_5, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_6 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-10 15:07:08", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_6, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_6, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_7 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-17 11:02:32", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-22 06:59:59", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_7, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_7, x="Date.time", y="NH3.O", color = "Sampling.point")


#OTICE_1 <- OTICE_1 %>%  mutate(Date.time = round_date(Date.time, "1 minute")) %>% group_by(Date.time) %>% summarise_all(mean)


########### DATA VISUALIZATION ###############
#ggline(FTIR_1, x="Date.time", y="CH4.F")




########### MERGE ###############
# Merge data by rounding to nearest timestamp
#merged_data <- FTIR_1[, .(Date.time, Sampling.point.F, CO2.F, NH3.F, CH4.F, H2O.F)]
#merged_data <- OTICE_1[merged_data, on = "Date.time", roll = "nearest"]






