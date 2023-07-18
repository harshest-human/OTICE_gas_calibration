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
FTIR <- read.table(paste0("2023_FTIR_data/20230203.TXT"), header = T, fill = TRUE) %>%
  mutate(DateTime = paste(Datum, Zeit)) %>%
  select(-c(Datum, Zeit)) %>%
  relocate(DateTime)


FTIR$CO2 <- as.numeric(FTIR$CO2)
FTIR$NH3 <- as.numeric(FTIR$NH3) 
FTIR$CH4 <- as.numeric(FTIR$CH4) 
FTIR$Messstelle <- as.numeric(FTIR$Messstelle)


########### OTICE DATA ###############
# MERGE all csv data log files 
# Set the folder path
folder_path <- "2023_OTICE_data"

# List all CSV files in the folder
file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and merge CSV files
data <- purrr::map_dfr(file_list, read_csv)

# Output file path for the merged data
output_file <- file.path(folder_path, "merged_data.csv")

# Write the merged data to a CSV file
write_csv(data, output_file)

# Print a success message
cat("Merged data saved to:", output_file)

###### OTICE Import data ######
OTICE <- read.csv("2023_OTICE_data/merged_data.csv")%>%
  select()


x <- 100# Example input value
y <- 67.652*x^(-0.518)  # Calculating the output value using the equation

print(y)  
