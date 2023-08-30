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

OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV")


########### FTIR DATA IMPORT ###############
FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT",
    clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")

FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV")

########### DATA VISUALIZATION ###############
FTIR_1 <- FTIR_data %>% 
  filter(Date.time.F >= "2023-07-21 13:20:38",
         Date.time.F <= "2023-07-21 13:48:00") %>% na.omit()

ggline(FTIR_1, x="Date.time.F", y="CH4.F")


#OTICE
OTICE_1 <- OTICE_data %>% 
  filter(Date.time.O >= "2023-07-21 13:20:38",
         Date.time.O <= "2023-07-21 13:48:00") %>% na.omit()

ggline(OTICE_1, x="Date.time.O", y="CH4.O")


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
