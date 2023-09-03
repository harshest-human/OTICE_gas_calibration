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


########### DATA IMPORTING ###############
#source("D:/Data Analysis/GCDP/GCDP_function_script.R")
#source("https://raw.githubusercontent.com/harshest-human/GCDP/main/GCDP_function_script.R")

#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV") #check today's date

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

#FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV") #check today's date


########### DATA VIZ OTICE ###############
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
         Date.time <= as.POSIXct("2023-07-20 13:20:38", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_3, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_3, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_4 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()

ggline(OTICE_4, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_4, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_5 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-31 11:07:35", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-10 11:03:14", format = "%Y-%m-%d %H:%M:%S")) %>%
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
  filter(Date.time >= as.POSIXct("2023-08-10 15:07:08", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 

ggline(OTICE_7, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_7, x="Date.time", y="NH3.O", color = "Sampling.point")

OTICE_8 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-17 11:02:32", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-22 06:59:59", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 


ggline(OTICE_8, x="Date.time", y="CH4.O", color = "Sampling.point")
ggline(OTICE_8, x="Date.time", y="NH3.O", color = "Sampling.point")

#OTICE_1 <- OTICE_1 %>%  mutate(Date.time = round_date(Date.time, "1 minute")) %>% group_by(Date.time) %>% summarise_all(mean)


########### DATA VIZ Boxplot OTICE ###############
#ggline(FTIR_1, x="Date.time", y="CH4.F")

#remove outlier 1.5 times IQR
source("D:/Data Analysis/OTICE_gas_calibration/remove_outliers_function.R")
OTICE_1$NH3.O <- remove_outliers(OTICE_1$NH3.O)
OTICE_1$CH4.O <- remove_outliers(OTICE_1$CH4.O)

ggplot(OTICE_1, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_1, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()

OTICE_2$NH3.O <- remove_outliers(OTICE_2$NH3.O)
OTICE_2$CH4.O <- remove_outliers(OTICE_2$CH4.O)

ggplot(OTICE_2, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_2, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()

OTICE_3$NH3.O <- remove_outliers(OTICE_3$NH3.O)
OTICE_3$CH4.O <- remove_outliers(OTICE_3$CH4.O)

ggplot(OTICE_3, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_3, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()

OTICE_4$NH3.O <- remove_outliers(OTICE_4$NH3.O)
OTICE_4$CH4.O <- remove_outliers(OTICE_4$CH4.O)

ggplot(OTICE_4, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_4, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()

OTICE_5$NH3.O <- remove_outliers(OTICE_5$NH3.O)
OTICE_5$CH4.O <- remove_outliers(OTICE_5$CH4.O)

ggplot(OTICE_5, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_5, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()

OTICE_6$NH3.O <- remove_outliers(OTICE_6$NH3.O)
OTICE_6$CH4.O <- remove_outliers(OTICE_6$CH4.O)

ggplot(OTICE_6, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_6, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()

OTICE_7$NH3.O <- remove_outliers(OTICE_7$NH3.O)
OTICE_7$CH4.O <- remove_outliers(OTICE_7$CH4.O)

ggplot(OTICE_7, aes(x = Sampling.point, y = CH4.O)) + geom_boxplot()
ggplot(OTICE_7, aes(x = Sampling.point, y = NH3.O)) + geom_boxplot()


########### MERGE ###############
# OTICE
OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")
OTICE_4 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:22:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>% na.omit()
OTICE_4 <- OTICE_4 %>%  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% summarise_all(mean)

# FTIR
FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")
FTIR_4 <- FTIR_data %>%
  filter(Date.time >= as.POSIXct("2023-07-21 13:22:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>% na.omit()
FTIR_4 <- FTIR_4 %>%  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  (Date.time) %>% summarise_all(mean)

# Merge data by rounding to nearest timestamp
merge_4 <- merge(
  FTIR_4,
  OTICE_4,
  by.x = "Date.time", 
  by.y = "Date.time", 
  all = TRUE,
  suffixes = c(".FTIR", ".OTICE")
)

########### MERGE VIZ ###############
ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = "CH4.O"), size = 1) +
  geom_line(aes(y = CH4.F, color = "CH4.F"), size = 1) +
  labs(
    x = "Date and Time",
    y = "CH4 Value"
  ) +
  scale_color_manual(
    values = c("CH4.O" = "blue", "CH4.F" = "red"),
    labels = c("CH4.O", "CH4.F")
  ) +
  theme_minimal()

ggline(merge_4, x="Date.time", y="CH4.O", color = "CH4.F")
ggline(merge_4, x="Date.time", y="NH3.O", color = "NH3.F")











########### TRIAL CONVERT OTICE Rs/Ro values to PPM

#x.NH3 = 0.456375

#NH3 <- 0.5471 * x.NH3 ^ (-0.463)

#x.CH4 = 3.78376

#CH4 <- 67.652 * x.CH4 ^ (-0.518)

