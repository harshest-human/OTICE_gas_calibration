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

########### OTICE DATA IMPORT ###############
ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw",
    clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")

OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230822_ODP.CSV")


########### FTIR DATA IMPORT ###############
FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/20230203_FTIR.TXT",
    clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")

FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230822_FDP.CSV")

# Convert DateTime columns to POSIXct
#FTIR_1$DateTime <- as.POSIXct(FTIR_1$DateTime, format = "%d/%m/%Y %H:%M:%S")
#OTICE_1$DateTime <- as.POSIXct(OTICE_1$DateTime, format = "%d/%m/%Y %H:%M:%S")

# Round DateTime to the nearest minute
#FTIR_1$DateTime <- round_date(FTIR_1$DateTime, "minute")
#OTICE_1$DateTime <- round_date(OTICE_1$DateTime, "minute")

# Merge data frames by DateTime
#merged_data <- merge(FTIR_1, OTICE_1, by = "DateTime", all = TRUE)

########### DATA VISUALIZATION ###############
FTIR_1 <- FTIR_data %>% 
  filter(DateTime >= "14/07/2023 14:02:22",
         DateTime <= "14/07/2023 14:22:22") %>% na.omit()

write.csv(FTIR_1, file = "FTIR_1.csv", row.names = FALSE)

ggline(FTIR_1, x="DateTime", y="CH4")


#OTICE
OTICE_1 <- OTICE_data %>% 
  filter(DateTime >= "14/07/2023 14:02:22",
         DateTime <= "14/07/2023 14:22:22") %>% na.omit()

write.csv(OTICE_1, file = "OTICE_1.csv", row.names = FALSE)

OTICE_1$CH4_ppm <- 0.5471 * OTICE_1$CH4^(-0.463)
OTICE_1$NH3_ppm <- 67.652 * OTICE_1$NH3^(-0.518)



ggline(OTICE_1, x="DateTime", y="CH4_ppm")
ggline(OTICE_1, x="DateTime", y="NH3_ppm")

y = 0.5471*x^(-0.463)
y

x = 5.2675
y <- 67.652*x^(-0.518)
y
y = 10.232
x = (67.652/y)^(-0.518)
x

y = 10.232
x = (67.652 / y)^(2.161)
x
