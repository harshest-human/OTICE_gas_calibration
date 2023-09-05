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
#source("D:/Data Analysis/GCDP/GCDP_function_script.R")
#source("https://raw.githubusercontent.com/harshest-human/GCDP/main/GCDP_function_script.R")

#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV") #check today's date

#TRIAL CONVERT OTICE Rs/Ro values to PPM

#x.NH3 = 0.456375

#NH3 <- 0.5471 * x.NH3 ^ (-0.463)

#x.CH4 = 3.78376

#CH4 <- 67.652 * x.CH4 ^ (-0.518)

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

#FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV") #check today's date


########### Cleaning Time Series  ###############
OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

OTICE_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-06-27 13:22:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-06-30 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_2 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-06 08:22:24", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-06 11:16:35", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()%>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_3 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-12 16:07:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-20 13:20:38", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()%>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_4 <- OTICE_data %>%
  filter(Date.time >= as.POSIXct("2023-07-21 13:26:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:46:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_5 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-31 12:53:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-31 16:42:39", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_6 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-10 18:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-11 18:02:45", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_7 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-11 18:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-15 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_8 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-16 05:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 05:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

OTICE_9 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-17 11:02:32", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-22 06:59:59", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

# FTIR
FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

FTIR_4 <- FTIR_data %>%
  filter(Date.time >= as.POSIXct("2023-07-21 13:26:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:46:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)


FTIR_5 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-31 12:53:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-31 16:42:39", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>%
  summarise_all(mean)

FTIR_6 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-10 18:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-11 18:02:45", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

FTIR_7 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-11 18:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-15 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

FTIR_8 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-16 05:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 05:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

########### MERGE ###############
# Merge FTIR and OTICE
merge_4 <- left_join(OTICE_4, FTIR_4, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_4 <- select(merge_4 , Date.time, Sampling.point, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)

merge_5 <- left_join(OTICE_5, FTIR_5, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_5 <- select(merge_5 , Date.time, Sampling.point, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)

merge_6 <- left_join(OTICE_6, FTIR_6, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_6 <- select(merge_6 , Date.time, Sampling.point, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)

merge_7 <- left_join(OTICE_7, FTIR_7, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_7 <- select(merge_7 , Date.time, Sampling.point, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)

merge_8 <- left_join(OTICE_8, FTIR_8, by = c("Date.time" = "Date.time")) %>% na.omit()
merge_8 <- select(merge_8 , Date.time, Sampling.point, NH3.O, NH3.F, NH3, CH4.O, CH4.F, CH4, CO2.F, CO2)


########### CH4 VIZ ###############
CH4_plot4 <- ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_4$Date.time), max(merge_4$Date.time), by = "5 mins")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot5 <- ggplot(merge_5, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_5$Date.time), max(merge_5$Date.time), by = "1 hour")) + 
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot6 <- ggplot(merge_6, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_6$Date.time), max(merge_6$Date.time), by = "6 hours")) + 
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot7 <- ggplot(merge_7, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_7$Date.time), max(merge_7$Date.time), by = "12 hours")) + 
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot8 <- ggplot(merge_8, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_8$Date.time), max(merge_8$Date.time), by = "6 hours")) + 
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")

########### NH3 VIZ ###############
NH3_plot4 <- ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_4$Date.time), max(merge_4$Date.time), by = "5 mins")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot5 <- ggplot(merge_5, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_5$Date.time), max(merge_5$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot6 <- ggplot(merge_6, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_6$Date.time), max(merge_6$Date.time), by = "6 hours")) + 
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot7 <- ggplot(merge_7, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_7$Date.time), max(merge_7$Date.time), by = "12 hours")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")

NH3_plot8 <- ggplot(merge_8, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_8$Date.time), max(merge_8$Date.time), by = "6 hours")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "FTIR")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


###### View plots ######
CH4_plot4
CH4_plot5
CH4_plot6
CH4_plot7
CH4_plot8

NH3_plot4
NH3_plot5
NH3_plot6
NH3_plot7
NH3_plot8

###### Save plots as a PDF ######
# Save each plot on a separate page in the PDF
ggsave("CH4_plot4.pdf", CH4_plot4, width = 8, height = 5, units = "in")
ggsave("CH4_plot5.pdf", CH4_plot5, width = 8, height = 5, units = "in")
ggsave("CH4_plot6.pdf", CH4_plot6, width = 8, height = 5, units = "in")
ggsave("CH4_plot7.pdf", CH4_plot7, width = 8, height = 5, units = "in")
ggsave("CH4_plot8.pdf", CH4_plot8, width = 8, height = 5, units = "in")

ggsave("NH3_plot4.pdf", NH3_plot4, width = 8, height = 5, units = "in")
ggsave("NH3_plot5.pdf", NH3_plot5, width = 8, height = 5, units = "in")
ggsave("NH3_plot6.pdf", NH3_plot6, width = 8, height = 5, units = "in")
ggsave("NH3_plot7.pdf", NH3_plot7, width = 8, height = 5, units = "in")
ggsave("NH3_plot8.pdf", NH3_plot8, width = 8, height = 5, units = "in")


###### Rs/Ro Viz ######
CH4_plot4 <- ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = CH4, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_4$Date.time), max(merge_4$Date.time), by = "5 mins")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot5 <- ggplot(merge_5, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = CH4, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_5$Date.time), max(merge_5$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot6 <- ggplot(merge_6, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = CH4, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_6$Date.time), max(merge_6$Date.time), by = "6 hours")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot7 <- ggplot(merge_7, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = CH4, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_7$Date.time), max(merge_7$Date.time), by = "12 hours")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


CH4_plot8 <- ggplot(merge_8, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = CH4.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = CH4, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_8$Date.time), max(merge_8$Date.time), by = "6 hours")) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


# NH3
NH3_plot4 <- ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = NH3, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_4$Date.time), max(merge_4$Date.time), by = "5 mins")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot5 <- ggplot(merge_5, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = NH3, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_5$Date.time), max(merge_5$Date.time), by = "1 hour")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot6 <- ggplot(merge_6, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = NH3, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_6$Date.time), max(merge_6$Date.time), by = "6 hours")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot7 <- ggplot(merge_7, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = NH3, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_7$Date.time), max(merge_7$Date.time), by = "12 hours")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")


NH3_plot8 <- ggplot(merge_8, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point), size = 1) +
  geom_line(aes(y = NH3.F, color = "FTIR"), size = 1, color = "black") +
  geom_line(aes(y = NH3, color = Sampling.point), size = 1, linetype = "dashed") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", 
                   breaks = seq(min(merge_8$Date.time), max(merge_8$Date.time), by = "6 hours")) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Rs/Ro", breaks = NULL)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("Comparing OTICE nodes with a reference FTIR inside a calibration chamber")

###### View plots ######
CH4_plot4
CH4_plot5
CH4_plot6
CH4_plot7
CH4_plot8

NH3_plot4
NH3_plot5
NH3_plot6
NH3_plot7
NH3_plot8

###### Save plots as a PDF ######
# Save each plot on a separate page in the PDF
ggsave("CH4_plot4_R.pdf", CH4_plot4, width = 8, height = 5, units = "in")
ggsave("CH4_plot5_R.pdf", CH4_plot5, width = 8, height = 5, units = "in")
ggsave("CH4_plot6_R.pdf", CH4_plot6, width = 8, height = 5, units = "in")
ggsave("CH4_plot7_R.pdf", CH4_plot7, width = 8, height = 5, units = "in")
ggsave("CH4_plot8_R.pdf", CH4_plot8, width = 8, height = 5, units = "in")

ggsave("NH3_plot4_R.pdf", NH3_plot4, width = 8, height = 5, units = "in")
ggsave("NH3_plot5_R.pdf", NH3_plot5, width = 8, height = 5, units = "in")
ggsave("NH3_plot6_R.pdf", NH3_plot6, width = 8, height = 5, units = "in")
ggsave("NH3_plot7_R.pdf", NH3_plot7, width = 8, height = 5, units = "in")
ggsave("NH3_plot8_R.pdf", NH3_plot8, width = 8, height = 5, units = "in")



