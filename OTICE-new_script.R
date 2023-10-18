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
#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw/2023/08", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20231018_ODP.CSV") #check today's date

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

OTICE_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-10-03 16:34:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-04 06:43:42", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023_10_04_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20231018_FDP.CSV") #check today's date

FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

FTIR_1 <- FTIR_data %>%
  filter(Date.time >= as.POSIXct("2023-10-03 16:34:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-04 06:43:42", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

merge_1 <- left_join(OTICE_1, FTIR_1, by = c("Date.time" = "Date.time")) %>% na.omit()


CH4_plot1 <- ggplot(merge_1, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_1$Date.time), max(merge_1$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR"), limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot2 <- ggplot(merge_1, aes(x = Date.time)) +
  geom_line(aes(y = CH4, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_1$Date.time), max(merge_1$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "CH4 Rs/Ro",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR"), limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot1 <- ggplot(merge_1, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_1$Date.time), max(merge_1$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR"), limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot2 <- ggplot(merge_1, aes(x = Date.time)) +
  geom_line(aes(y = NH3, color = Sampling.point.O), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_1$Date.time), max(merge_1$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "NH3 Rs/Ro",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR"), limits = c(0, NA)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


###### Save plots as a PDF ######

# Save each plot on a separate page in the PDF
ggsave("CH4_New_plot1_R.pdf", CH4_plot1, width = 8, height = 5, units = "in")
ggsave("CH4_New_plot2_R.pdf", CH4_plot2, width = 8, height = 5, units = "in")

ggsave("NH3_New_plot1_R.pdf", NH3_plot1, width = 8, height = 5, units = "in")
ggsave("NH3_New_plot2_R.pdf", NH3_plot2, width = 8, height = 5, units = "in")
