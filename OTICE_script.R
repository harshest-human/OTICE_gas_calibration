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
source("D:/Data Analysis/GCDP/GCDP_function_script.R")
#source("https://raw.githubusercontent.com/harshest-human/GCDP/main/GCDP_function_script.R")


########### DATA IMPORTING ###############
ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw/2023/2023.10.18", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20231024_ODP.CSV") #check today's date

#TRIAL CONVERT OTICE Rs/Ro values to PPM

#x.NH3 = 0.456375

#NH3 <- 0.5471 * x.NH3 ^ (-0.463)

#x.CH4 = 3.78376

#CH4 <- 67.652 * x.CH4 ^ (-0.518)

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

# Write clean table
OTICE_gas_data <- select(OTICE_data, Sampling.point.O, Date.time, temperature, relative.humidity, NH3, NH3.O, CH4, CH4.O, CO2)

write.csv(OTICE_gas_data, file = "OTICE_gas_data.csv", row.names = FALSE)

# Import FTIR Data
FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/20230_10_18_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20231024_FDP.CSV") #check today's date


########### Cleaning Time Series  ###############
OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

OTICE_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-10-18 09:27:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-19 11:20:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_2 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-10-19 11:20:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-20 11:20:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))


###### FTIR Synchronization ####
FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

FTIR_1 <- FTIR_data %>%
  filter(Date.time >= as.POSIXct("2023-10-18 09:27:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-19 11:20:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)


FTIR_2 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-10-19 11:20:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-20 11:20:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>%
  summarise_all(mean)


########### MERGE ###############
# Merge FTIR and OTICE
merge_1 <- left_join(OTICE_1, FTIR_1, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_1 <- select(merge_1 , Date.time, Sampling.point.O, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)

merge_2 <- left_join(OTICE_2, FTIR_2, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_2 <- select(merge_2 , Date.time, Sampling.point.O, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)

########### CH4 VIZ ###############
CH4_plot1 <- ggplot(merge_1, aes(x = Date.time)) +
  geom_line(aes(y = CH4, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_1$Date.time), max(merge_1$Date.time), by = "60 mins")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")

########### NH3 VIZ ###############
NH3_plot1 <- ggplot(merge_2, aes(x = Date.time)) +
  geom_line(aes(y = NH3, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_2$Date.time), max(merge_2$Date.time), by = "60 mins")) + 
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


###### Rs/Ro Viz ######
CH4_plot2 <- ggplot(merge_1, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_1$Date.time), max(merge_1$Date.time), by = "60 mins")) +
  labs(x = "Date and Time",
       y = "CH4 Rs/Ro",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot2 <- ggplot(merge_2, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_2$Date.time), max(merge_2$Date.time), by = "60 mins")) + 
  labs(x = "Date and Time",
       y = "NH3 Rs/Ro",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")

###### View plots ######
CH4_plot1
CH4_plot2

NH3_plot1
NH3_plot2

###### Save plots as a PDF ######
# Save each plot on a separate page in the PDF
ggsave("CH4_plot1.pdf", CH4_plot4, width = 8, height = 5, units = "in")
ggsave("CH4_plot2.pdf", CH4_plot5, width = 8, height = 5, units = "in")

ggsave("NH3_plot1.pdf", NH3_plot4, width = 8, height = 5, units = "in")
ggsave("NH3_plot2.pdf", NH3_plot5, width = 8, height = 5, units = "in")


