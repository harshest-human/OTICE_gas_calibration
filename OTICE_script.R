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

########### FTIR DATA ###############
#First manually remove unwanted columns from data in excel and check the starting point of data logging
#Import data
FTIR_data <- read.csv(("2023_FTIR_data/20230203.CSV"), header = T, fill = TRUE) %>%
  mutate(DateTime = as.POSIXct(paste(Datum, Zeit), format = "%m/%d/%Y %H:%M:%S")) %>%
  mutate(CO2 = as.numeric(CO2),
    NH3 = as.numeric(NH3),
    CH4 = as.numeric(CH4),
    H2O = as.numeric(H2O),
    Messstelle = as.numeric(Messstelle)) %>%
  select(-c(Datum, Zeit)) %>%
  relocate(DateTime)

########### OTICE DATA ###############
# Create an empty data frame
OTICE_data <- data.frame()

# Define the directory path where the CSV files are located
directory <- "D:/Data Analysis/OTICE_gas_calibration/2023_OTICE_data"

# Get a list of CSV files in the directory
csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)

# Iterate over each CSV file, read its contents, and append to the OTICE_data data frame
for (file in csv_files) {
  delimiter <- ifelse(grepl(";", readLines(file, n = 1)), ";", "\t")  # Determine the delimiter based on the first line of each file
  
  data <- read.table(file, header = TRUE, sep = delimiter)  # Adjust the sep parameter based on the delimiter
  
  # Select specific columns by index and rename column 1 to "DateTime"
  selected_data <- data[, c(1, 2, 3, 4, 6, 10, 13)]
  colnames(selected_data)[1] <- "DateTime"
  
  # Remove decimal points from seconds in the "DateTime" column
  selected_data$DateTime <- sub("\\.\\d+", "", selected_data$DateTime)
  
  # Convert the "DateTime" column to a POSIXct object
  selected_data$DateTime <- as.POSIXct(selected_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  # Adjust the time by adding 2 hours (120 minutes)
  selected_data$DateTime <- selected_data$DateTime + minutes(120)
  
  # Change the DateTime format to "DD/MM/YYYY HH:MM:SS"
  selected_data$DateTime <- format(selected_data$DateTime, format = "%d/%m/%Y %H:%M:%S")
  
  OTICE_data <- rbind(OTICE_data, selected_data)
}

# Save the combined data to a new CSV file
write.csv(OTICE_data, file = "OTICE_data.csv", row.names = FALSE)


########### DATA VISUALIZATION ###############
#FTIR
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

# Convert DateTime columns to POSIXct
#FTIR_1$DateTime <- as.POSIXct(FTIR_1$DateTime, format = "%d/%m/%Y %H:%M:%S")
#OTICE_1$DateTime <- as.POSIXct(OTICE_1$DateTime, format = "%d/%m/%Y %H:%M:%S")

# Round DateTime to the nearest minute
#FTIR_1$DateTime <- round_date(FTIR_1$DateTime, "minute")
#OTICE_1$DateTime <- round_date(OTICE_1$DateTime, "minute")

# Merge data frames by DateTime
#merged_data <- merge(FTIR_1, OTICE_1, by = "DateTime", all = TRUE)


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
