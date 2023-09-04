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
  filter(Date.time >= as.POSIXct("2023-06-27 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-06-30 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_2 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-06 08:22:24", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-06 11:16:35", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_3 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-12 16:07:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-20 13:20:38", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_4 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:22:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_5 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-31 12:53:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-31 16:42:39", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_6 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-10 16:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-11 16:02:45", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_7 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-11 16:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

OTICE_8 <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-17 11:02:32", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-22 06:59:59", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)


# FTIR
FTIR_data$Date.time <- as.POSIXct(FTIR_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

FTIR_4 <- FTIR_data %>%
  filter(Date.time >= as.POSIXct("2023-07-21 13:22:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
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
  filter(Date.time >= as.POSIXct("2023-08-10 16:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-11 16:02:45", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

FTIR_7 <- FTIR_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-11 16:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() %>%
  mutate(Date.time = round_date(Date.time, "1 minute")) %>%
  group_by(Date.time) %>% 
  summarise_all(mean)

########### MERGE ###############

# Create a function to find the nearest timestamp
find_nearest_timestamp <- function(x, timestamps){timestamps[which.min(abs(timestamps - x))]}

# Merge FTIR_4 and OTICE_4 by finding the nearest timestamp
merge_4 <- left_join(OTICE_4, FTIR_4, by = c("Date.time" = "Date.time"))
merge_4 <- select(merge_4, Date.time, NH3.O, CH4.O, NH3.F, CH4.F)
#write.csv(merge_4, file = "merge_4.csv",row.names = FALSE)

merge_5 <- left_join(OTICE_5, FTIR_5, by = c("Date.time" = "Date.time"))
merge_5 <- select(merge_5 , Date.time, NH3.O, CH4.O, NH3.F, CH4.F)

merge_6 <- left_join(OTICE_6, FTIR_6, by = c("Date.time" = "Date.time"))
merge_6 <- select(merge_6 , Date.time, NH3.O, CH4.O, NH3.F, CH4.F)

merge_7 <- left_join(OTICE_7, FTIR_7, by = c("Date.time" = "Date.time"))
merge_7 <- select(merge_7 , Date.time, NH3.O, CH4.O, NH3.F, CH4.F, CO2.F, CO2)

########### MERGE VIZ ###############
# CH4
g1 <- ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = "CH4.O"), size = 1) +
  geom_line(aes(y = CH4.F, color = "CH4.F"), size = 1) +
  labs(
    x = "Date and Time (2023-07-21)",
    y = "CH4 Value"
  ) +
  scale_color_manual(
    values = c("CH4.O" = "blue", "CH4.F" = "red"),
    breaks = c("CH4.O", "CH4.F"),
    labels = c("CH4.O", "CH4.F")
  ) +
  theme_minimal()

g2 <- ggplot(merge_5, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = "CH4.O"), size = 1) +
  geom_line(aes(y = CH4.F, color = "CH4.F"), size = 1) +
  labs(
    x = "Date and Time (2023-07-31)",
    y = "CH4 Value"
  ) +
  scale_color_manual(
    values = c("CH4.O" = "blue", "CH4.F" = "red"),
    breaks = c("CH4.O", "CH4.F"),
    labels = c("CH4.O", "CH4.F")
  ) +
  theme_minimal()

g3 <- ggplot(merge_6, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = "CH4.O"), size = 1) +
  geom_line(aes(y = CH4.F, color = "CH4.F"), size = 1) +
  labs(
    x = "Date and Time (2023-08-10 to 2023-08-11)",
    y = "CH4 Value"
  ) +
  scale_color_manual(
    values = c("CH4.O" = "blue", "CH4.F" = "red"),
    breaks = c("CH4.O", "CH4.F"),
    labels = c("CH4.O", "CH4.F")
  ) +
  theme_minimal()

g4 <- ggplot(merge_7, aes(x = Date.time)) +
  geom_line(aes(y = CH4.O, color = "CH4.O"), size = 1) +
  geom_line(aes(y = CH4.F, color = "CH4.F"), size = 1) +
  labs(
    x = "Date and Time (2023-08-11 to 2023-08-17)",
    y = "CH4 Value"
  ) +
  scale_color_manual(
    values = c("CH4.O" = "blue", "CH4.F" = "red"),
    breaks = c("CH4.O", "CH4.F"),
    labels = c("CH4.O", "CH4.F")
  ) +
  theme_minimal()

# NH3
g5 <- ggplot(merge_4, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = "NH3.O"), size = 1) +
  geom_line(aes(y = NH3.F, color = "NH3.F"), size = 1) +
  labs(
    x = "Date and Time (2023-07-21)",
    y = "NH3 Value"
  ) +
  scale_color_manual(
    values = c("NH3.O" = "blue", "NH3.F" = "red"),
    breaks = c("NH3.O", "NH3.F"),
    labels = c("NH3.O", "NH3.F")
  ) +
  theme_minimal()

g6 <- ggplot(merge_5, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = "NH3.O"), size = 1) +
  geom_line(aes(y = NH3.F, color = "NH3.F"), size = 1) +
  labs(
    x = "Date and Time (2023-07-31)",
    y = "NH3 Value"
  ) +
  scale_color_manual(
    values = c("NH3.O" = "blue", "NH3.F" = "red"),
    breaks = c("NH3.O", "NH3.F"),
    labels = c("NH3.O", "NH3.F")
  ) +
  theme_minimal()

g7 <- ggplot(merge_6, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = "NH3.O"), size = 1) +
  geom_line(aes(y = NH3.F, color = "NH3.F"), size = 1) +
  labs(
    x = "Date and Time (2023-08-10 to 2023-08-11)",
    y = "NH3 Value"
  ) +
  scale_color_manual(
    values = c("NH3.O" = "blue", "NH3.F" = "red"),
    breaks = c("NH3.O", "NH3.F"),
    labels = c("NH3.O", "NH3.F")
  ) +
  theme_minimal()

g8 <- ggplot(merge_7, aes(x = Date.time)) +
  geom_line(aes(y = NH3.O, color = "NH3.O"), size = 1) +
  geom_line(aes(y = NH3.F, color = "NH3.F"), size = 1) +
  labs(
    x = "Date and Time (2023-08-11 to 2023-08-17)",
    y = "NH3 Value"
  ) +
  scale_color_manual(
    values = c("NH3.O" = "blue", "NH3.F" = "red"),
    breaks = c("NH3.O", "NH3.F"),
    labels = c("NH3.O", "NH3.F")
  ) +
  theme_minimal()

# Save each plot as a PDF
ggsave("plot1.pdf", g1, width = 8, height = 5, units = "in")
ggsave("plot2.pdf", g2, width = 8, height = 5, units = "in")
ggsave("plot3.pdf", g3, width = 8, height = 5, units = "in")
ggsave("plot4.pdf", g4, width = 8, height = 5, units = "in")
ggsave("plot5.pdf", g5, width = 8, height = 5, units = "in")
ggsave("plot6.pdf", g6, width = 8, height = 5, units = "in")
ggsave("plot7.pdf", g7, width = 8, height = 5, units = "in")
ggsave("plot8.pdf", g8, width = 8, height = 5, units = "in")


########### OTICE Sampling.point Viz ###############
#ggplot(FTIR_1, x="Date.time", y="CH4.F")
#remove outlier 1.5 times IQR
source("D:/Data Analysis/OTICE_gas_calibration/remove_outliers_function.R")

OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M:%S")

OTICE_1.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-06-27 13:20:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-06-30 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 
OTICE_1.sec$NH3.O <- remove_outliers(OTICE_1.sec$NH3.O)
OTICE_1.sec$CH4.O <- remove_outliers(OTICE_1.sec$CH4.O)


OTICE_2.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-06 08:22:24", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-06 11:16:35", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()
OTICE_2.sec$NH3.O <- remove_outliers(OTICE_2.sec$NH3.O)
OTICE_2.sec$CH4.O <- remove_outliers(OTICE_2.sec$CH4.O)


OTICE_3.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-12 16:07:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-20 13:20:38", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit() 
OTICE_3.sec$NH3.O <- remove_outliers(OTICE_3.sec$NH3.O)
OTICE_3.sec$CH4.O <- remove_outliers(OTICE_3.sec$CH4.O)


OTICE_4.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-21 13:22:38", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-21 13:48:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()
OTICE_4.sec$NH3.O <- remove_outliers(OTICE_4.sec$NH3.O)
OTICE_4.sec$CH4.O <- remove_outliers(OTICE_4.sec$CH4.O)


OTICE_5.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-07-31 12:53:59", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-07-31 16:42:39", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()
OTICE_5.sec$NH3.O <- remove_outliers(OTICE_5.sec$NH3.O)
OTICE_5.sec$CH4.O <- remove_outliers(OTICE_5.sec$CH4.O)


OTICE_6.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-10 16:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-11 16:02:45", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()
OTICE_6.sec$NH3.O <- remove_outliers(OTICE_6.sec$NH3.O)
OTICE_6.sec$CH4.O <- remove_outliers(OTICE_6.sec$CH4.O)


OTICE_7.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-11 16:02:45", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-17 08:15:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()
OTICE_7.sec$NH3.O <- remove_outliers(OTICE_7.sec$NH3.O)
OTICE_7.sec$CH4.O <- remove_outliers(OTICE_7.sec$CH4.O)


OTICE_8.sec <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-08-17 11:02:32", format = "%Y-%m-%d %H:%M:%S"),
         Date.time <= as.POSIXct("2023-08-22 06:59:59", format = "%Y-%m-%d %H:%M:%S")) %>%
  na.omit()
OTICE_8.sec$NH3.O <- remove_outliers(OTICE_8.sec$NH3.O)
OTICE_8.sec$CH4.O <- remove_outliers(OTICE_8.sec$CH4.O)


########### OTICE VIZ LINEPLOT ###############
ggplot(OTICE_1.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) +
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_1.sec, aes(x = Date.time, y = NH3.O, fill = Sampling.point)) +
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_2.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_2.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) +
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_3.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_3.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_4.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_4.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_5.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_5.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_6.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_6.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_7.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_7.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")

ggplot(OTICE_8.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
ggplot(OTICE_8.sec, aes(x = Date.time, y = CH4.O, fill = Sampling.point)) + 
  geom_boxplot() +  scale_fill_discrete(name = "Sampling.point")
