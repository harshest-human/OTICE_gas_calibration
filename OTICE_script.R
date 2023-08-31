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
source("https://raw.githubusercontent.com/harshest-human/GCDP/main/GCDP_function_script.R")

########### DATA IMPORTING ###############
ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV")

FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV") #check today's date


########### DATA CLEANING (Time series) ###############
FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")
FTIR_1 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()

OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")
OTICE_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 


########### CONVERT OTICE Rs/Ro values to PPM ###############
OTICE_1$NH3.O <- 0.5471 * OTICE_1$NH3^(-0.463)
OTICE_1$CH4.O <- 67.652 * OTICE_1$CH4^(-0.518)

OTICE_1 <- OTICE_1 %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>% #Round off by 1 minute to match FTIR frequency
  group_by(Sampling.point) %>% 
  summarise_all(mean)



########### DATA VISUALIZATION ###############
ggline(FTIR_1, x="Date.time", y="CH4.F")
ggline(OTICE_1, x="Date.time", y="CH4.O", color = "Sampling.point")


########### MERGE ###############
# Merge data by rounding to nearest timestamp
#merged_data <- FTIR_1[, .(Date.time, Sampling.point.F, CO2.F, NH3.F, CH4.F, H2O.F)]
#merged_data <- OTICE_1[merged_data, on = "Date.time", roll = "nearest"]






