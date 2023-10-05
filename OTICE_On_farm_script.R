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
#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw/2023/08", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20231005_ODP.CSV") #check today's date

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

########### Cleaning Time Series  ###############
OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

OTICE_AUG_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-03 12:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-09 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))


OTICE_AUG_2 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-17 12:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-23 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))


OTICE_AUG_3 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-31 12:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-09-06 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))


OTICE_SEP_1 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-09-13 12:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-09-19 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))


OTICE_SEP_2 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-09-25 12:00:00", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "minute")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))


########### CH4 VIZ ###############
CH4_plot_AUG_1 <- ggplot(OTICE_AUG_1, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")

CH4_plot_AUG_2 <- ggplot(OTICE_AUG_2, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")

CH4_plot_AUG_3 <- ggplot(OTICE_AUG_3, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")


CH4_plot_SEP_1 <- ggplot(OTICE_SEP_1, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")

CH4_plot_SEP_2 <- ggplot(OTICE_SEP_2, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "CH4 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")


########### NH3 VIZ ###############
NH3_plot_AUG_1 <- ggplot(OTICE_AUG_1, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")

NH3_plot_AUG_2 <- ggplot(OTICE_AUG_2, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")

NH3_plot_AUG_3 <- ggplot(OTICE_AUG_3, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")


NH3_plot_SEP_1 <- ggplot(OTICE_SEP_1, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")

NH3_plot_SEP_2 <- ggplot(OTICE_SEP_2, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = Sampling.point.O), size = 1) +
  labs(x = "Date and Time",
       y = "NH3 PPM",
       color = "Sensor Nodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  ggtitle("On-farm measured values") +
  scale_x_datetime(breaks = "12 hours", date_labels = "%Y-%m-%d %H:%M")


###### View plots ######
CH4_plot_AUG_1
CH4_plot_AUG_2
CH4_plot_AUG_3
CH4_plot_SEP_1
CH4_plot_SEP_2

NH3_plot_AUG_1
NH3_plot_AUG_2
NH3_plot_AUG_3
NH3_plot_SEP_1
NH3_plot_SEP_2

###### Save plots as a PDF ######
# Define the output PDF file
output_file <- "Plots_On_farm_results_AUG_SEP_2023.pdf"

# Create a PDF device
pdf(output_file, width = 8, height = 5)

# List of plots
plots <- list(
  CH4_plot_AUG_1, CH4_plot_AUG_2, CH4_plot_AUG_3, CH4_plot_SEP_1, CH4_plot_SEP_2,
  NH3_plot_AUG_1, NH3_plot_AUG_2, NH3_plot_AUG_3, NH3_plot_SEP_1, NH3_plot_SEP_2
)

# Loop through the list and print each plot
for (plot in plots) {
  print(plot)
}

# Close the PDF device
dev.off()


