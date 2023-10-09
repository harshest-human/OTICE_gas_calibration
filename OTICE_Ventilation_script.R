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
#source("https://raw.githubusercontent.com/harshest-human/GCDP/main/GCDP_function_script.R")

########### DATA IMPORTING ###############
#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw/2023/10", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20231006_ODP.CSV") #check today's date

# CONVERT OTICE Rs/Ro values to PPM
OTICE_data$NH3.O <- 0.5471 * OTICE_data$NH3^(-0.463)
OTICE_data$CH4.O <- 67.652 * OTICE_data$CH4^(-0.518)

# Average every two minutes
OTICE_data$Date.time <- as.POSIXct(OTICE_data$Date.time, format = "%Y-%m-%d %H:%M")

OTICE_OCT <- OTICE_data %>% 
  filter(Date.time >= as.POSIXct("2023-10-01 02:00", format = "%Y-%m-%d %H:%M"),
         Date.time <= as.POSIXct("2023-10-06 12:56", format = "%Y-%m-%d %H:%M")) %>%
  na.omit() %>%
  group_by(Sampling.point.O, Date.time = floor_date(Date.time, unit = "2 minutes")) %>%
  summarise(
    mean_temperature = mean(temperature),
    CH4.O = mean(CH4.O),
    CH4 = mean(CH4),
    NH3.O = mean(NH3.O),
    NH3 = mean(NH3),
    CO2 = mean(CO2))

####### DATA CLEANING ############

# Create separate OTICE_OCT frames for each OTICE sensor nodes
Node_178Q <- OTICE_OCT %>%
  filter(Sampling.point.O == '178Q') %>%
  select(Date.time, NH3, CO2, CH4)

Node_1791 <- OTICE_OCT %>%
  filter(Sampling.point.O == '1791') %>%
  select(Date.time, NH3, CO2, CH4)

Node_178X <- OTICE_OCT %>%
  filter(Sampling.point.O == '178X') %>%
  select(Date.time, NH3, CO2, CH4)

Node_178T <- OTICE_OCT %>%
  filter(Sampling.point.O == '178T') %>%
  select(Date.time, NH3, CO2, CH4)

Node_178F <- OTICE_OCT %>%
  filter(Sampling.point.O == '178F') %>%
  select(Date.time, NH3, CO2, CH4)

Node_178S <- OTICE_OCT %>%
  filter(Sampling.point.O == '178S') %>%
  select(Date.time, NH3, CO2, CH4)

Node_1792 <- OTICE_OCT %>%
  filter(Sampling.point.O == '1792') %>%
  select(Date.time, NH3, CO2, CH4)

Node_178V <- OTICE_OCT %>%
  filter(Sampling.point.O == '178V') %>%
  select(Date.time, NH3, CO2, CH4)

Node_178P <- OTICE_OCT %>%
  filter(Sampling.point.O == '178P') %>%
  select(Date.time, NH3, CO2, CH4)


########### VENTILATION DATA ###############
# Get a list of CSV files in the directory
csv_files <- list.files(path = "D:/Data Analysis/Ventilation_data", pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
Ventilation_data <- data.frame()

# Read and row-bind the CSV files
for (file in csv_files) {
  # Read the CSV file without headers
  data <- read.csv(file, header = TRUE, sep = ";")
  
  # If Ventilation_data is empty, set column names from the first file
  if (nrow(Ventilation_data) == 0) {
    colnames(data) <- colnames(data)[1:21]  # Assuming you have 20 columns of data
    Ventilation_data <- data
  } else {
    Ventilation_data <- rbind(Ventilation_data, data)
  }
}

# Sort the data by "Date/Time"
Ventilation_data$Date.time <- as.POSIXct(Ventilation_data$Date.Time, format = "%Y-%m-%d %H:%M")
Ventilation_data <- Ventilation_data[order(Ventilation_data$Date.time), ]


# Create separate data frames for each set of Puls counters
Vent.1 <- Ventilation_data[, c("Date.time", "Puls.counter.7")]%>% 
  filter(Date.time >= as.POSIXct("2023-10-01 02:00", format = "%Y-%m-%d %H:%M"),
         Date.time <= as.POSIXct("2023-10-06 12:56", format = "%Y-%m-%d %H:%M"))

Vent.2 <- Ventilation_data[, c("Date.time", "Puls.counter.8")]%>% 
  filter(Date.time >= as.POSIXct("2023-10-01 02:00", format = "%Y-%m-%d %H:%M"),
         Date.time <= as.POSIXct("2023-10-06 12:56", format = "%Y-%m-%d %H:%M"))

Vent.3 <- Ventilation_data[, c("Date.time", "Puls.counter.9")]%>% 
  filter(Date.time >= as.POSIXct("2023-10-01 02:00", format = "%Y-%m-%d %H:%M"),
         Date.time <= as.POSIXct("2023-10-06 12:56", format = "%Y-%m-%d %H:%M"))

Vent.4 <- Ventilation_data[, c("Date.time", "Puls.counter.10")]%>% 
  filter(Date.time >= as.POSIXct("2023-10-01 02:00", format = "%Y-%m-%d %H:%M"),
         Date.time <= as.POSIXct("2023-10-06 12:56", format = "%Y-%m-%d %H:%M"))

Vent.5 <- Ventilation_data[, c("Date.time", "Puls.counter.11")]%>% 
  filter(Date.time >= as.POSIXct("2023-10-01 02:00", format = "%Y-%m-%d %H:%M"),
         Date.time <= as.POSIXct("2023-10-06 12:56", format = "%Y-%m-%d %H:%M"))

####### MERGE #######
# Combine Vent.1 with 178S and 178Q
Vent_1_comb <- full_join(Vent.1, Node_178S, by = "Date.time") %>%
  full_join(Node_178Q, by = "Date.time") %>%
  mutate_at(vars(-Date.time), ~coalesce(., NA))

# Combine Vent.2 with 178X and 178T
Vent_2_comb <- full_join(Vent.2, Node_178X, by = "Date.time") %>%
  full_join(Node_178T, by = "Date.time") %>%
  mutate_at(vars(-Date.time), ~coalesce(., NA))

# Combine Vent.3 with 1791 and 1792
Vent_3_comb <- full_join(Vent.3, Node_1791, by = "Date.time") %>%
  full_join(Node_1792, by = "Date.time") %>%
  mutate_at(vars(-Date.time), ~coalesce(., NA))

# Combine Vent.4 with 178V and 178F
Vent_4_comb <- full_join(Vent.4, Node_178V, by = "Date.time") %>%
  full_join(Node_178F, by = "Date.time") %>%
  mutate_at(vars(-Date.time), ~coalesce(., NA))

# Combine Vent.5 with 178S
Vent_5_comb <- full_join(Vent.5, Node_178S, by = "Date.time") %>%
  mutate_at(vars(-Date.time), ~coalesce(., NA))


# Function to average data by hour
average_data_by_hour <- function(data) {
  data %>%
    mutate(Date.time = as.POSIXct(Date.time)) %>%
    group_by(hour = floor_date(Date.time, "hour")) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    mutate(Date.time = hour)
}

# Apply the function to each comb data frame
Vent_1_comb <- average_data_by_hour(Vent_1_comb)
Vent_2_comb <- average_data_by_hour(Vent_2_comb)
Vent_3_comb <- average_data_by_hour(Vent_3_comb)
Vent_4_comb <- average_data_by_hour(Vent_4_comb)
Vent_5_comb <- average_data_by_hour(Vent_5_comb)

######## CO2 DATA VIZ ########
# For Vent_1_comb
CO2plot_Vent_1 <- ggplot(Vent_1_comb, aes(x = Puls.counter.7)) +
  geom_line(aes(y = CO2.x, color = "CO2.x")) +
  geom_line(aes(y = CO2.y, color = "CO2.y")) +
  scale_color_manual(values = c("CO2.x" = "darkblue", "CO2.y" = "cyan4")) +
  labs(x = "Ventilation rate (%)", y = "CO2") +
  ggtitle("CO2 trends measured by 178S and 178Q at Ventilator:1")

# For Vent_2_comb
CO2plot_Vent_2 <- ggplot(Vent_2_comb, aes(x = Puls.counter.8)) +
  geom_line(aes(y = CO2.x, color = "CO2.x")) +
  geom_line(aes(y = CO2.y, color = "CO2.y")) +
  scale_color_manual(values = c("CO2.x" = "darkblue", "CO2.y" = "cyan4")) +
  labs(x = "Ventilation rate (%)", y = "CO2") +
  ggtitle("CO2 trends measured by 178X and 178T at Ventilator:2")

# For Vent_3_comb
CO2plot_Vent_3 <- ggplot(Vent_3_comb, aes(x = Puls.counter.9)) +
  geom_line(aes(y = CO2.x, color = "CO2.x")) +
  geom_line(aes(y = CO2.y, color = "CO2.y")) +
  scale_color_manual(values = c("CO2.x" = "darkblue", "CO2.y" = "cyan4")) +
  labs(x = "Ventilation rate (%)", y = "CO2") +
  ggtitle("CO2 trends measured by 1791 and 1792 at Ventilator:3")

# For Vent_4_comb
CO2plot_Vent_4 <- ggplot(Vent_4_comb, aes(x = Puls.counter.10)) +
  geom_line(aes(y = CO2.x, color = "CO2.x")) +
  geom_line(aes(y = CO2.y, color = "CO2.y")) +
  scale_color_manual(values = c("CO2.x" = "darkblue", "CO2.y" = "cyan4")) +
  labs(x = "Ventilation rate (%)", y = "CO2") +
  ggtitle("CO2 trends measured by 178V and 178F at Ventilator:4")

# For Vent_5_comb
CO2plot_Vent_5 <- ggplot(Vent_5_comb, aes(x = Puls.counter.11, y = CO2)) +
  geom_line(color = "darkblue") +
  labs(x = "Ventilation rate (%)", y = "CO2") +
  ggtitle("CO2 trend measured by 178P at Ventilator:5")


######## CH4 DATA VIZ ########
# For Vent_1_comb
CH4plot_Vent_1 <- ggplot(Vent_1_comb, aes(x = Puls.counter.7)) +
  geom_line(aes(y = CH4.x, color = "CH4.x")) +
  geom_line(aes(y = CH4.y, color = "CH4.y")) +
  scale_color_manual(values = c("CH4.x" = "brown", "CH4.y" = "orange3")) +
  labs(x = "Ventilation rate (%)", y = "CH4") +
  ggtitle("CH4 trends measured by 178S and 178Q at Ventilator:1")

# For Vent_2_comb
CH4plot_Vent_2 <- ggplot(Vent_2_comb, aes(x = Puls.counter.8)) +
  geom_line(aes(y = CH4.x, color = "CH4.x")) +
  geom_line(aes(y = CH4.y, color = "CH4.y")) +
  scale_color_manual(values = c("CH4.x" = "brown", "CH4.y" = "orange3")) +
  labs(x = "Ventilation rate (%)", y = "CH4") +
  ggtitle("CH4 trends measured by 178X and 178T at Ventilator:2")

# For Vent_3_comb
CH4plot_Vent_3 <- ggplot(Vent_3_comb, aes(x = Puls.counter.9)) +
  geom_line(aes(y = CH4.x, color = "CH4.x")) +
  geom_line(aes(y = CH4.y, color = "CH4.y")) +
  scale_color_manual(values = c("CH4.x" = "brown", "CH4.y" = "orange3")) +
  labs(x = "Ventilation rate (%)", y = "CH4") +
  ggtitle("CH4 trends measured by 1791 and 1792 at Ventilator:3")

# For Vent_4_comb
CH4plot_Vent_4 <- ggplot(Vent_4_comb, aes(x = Puls.counter.10)) +
  geom_line(aes(y = CH4.x, color = "CH4.x")) +
  geom_line(aes(y = CH4.y, color = "CH4.y")) +
  scale_color_manual(values = c("CH4.x" = "brown", "CH4.y" = "orange3")) +
  labs(x = "Ventilation rate (%)", y = "CH4") +
  ggtitle("CH4 trends measured by 178V and 178F at Ventilator:4")

# For Vent_5_comb
CH4plot_Vent_5 <- ggplot(Vent_5_comb, aes(x = Puls.counter.11, y = CH4)) +
  geom_line(color = "brown") +
  labs(x = "Ventilation rate (%)", y = "CH4") +
  ggtitle("CH4 trend measured by 178P at Ventilator:5")


######## NH3 DATA VIZ ########
# For Vent_1_comb
NH3plot_Vent_1 <- ggplot(Vent_1_comb, aes(x = Puls.counter.7)) +
  geom_line(aes(y = NH3.x, color = "NH3.x")) +
  geom_line(aes(y = NH3.y, color = "NH3.y")) +
  scale_color_manual(values = c("NH3.x" = "darkgreen", "NH3.y" = "green2")) +
  labs(x = "Ventilation rate (%)", y = "NH3") +
  ggtitle("NH3 trends measured by 178S and 178Q at Ventilator:1")

# For Vent_2_comb
NH3plot_Vent_2 <- ggplot(Vent_2_comb, aes(x = Puls.counter.8)) +
  geom_line(aes(y = NH3.x, color = "NH3.x")) +
  geom_line(aes(y = NH3.y, color = "NH3.y")) +
  scale_color_manual(values = c("NH3.x" = "darkgreen", "NH3.y" = "green2")) +
  labs(x = "Ventilation rate (%)", y = "NH3") +
  ggtitle("NH3 trends measured by 178X and 178T at Ventilator:2")

# For Vent_3_comb
NH3plot_Vent_3 <- ggplot(Vent_3_comb, aes(x = Puls.counter.9)) +
  geom_line(aes(y = NH3.x, color = "NH3.x")) +
  geom_line(aes(y = NH3.y, color = "NH3.y")) +
  scale_color_manual(values = c("NH3.x" = "darkgreen", "NH3.y" = "green2")) +
  labs(x = "Ventilation rate (%)", y = "NH3") +
  ggtitle("NH3 trends measured by 1791 and 1792 at Ventilator:3")

# For Vent_4_comb
NH3plot_Vent_4 <- ggplot(Vent_4_comb, aes(x = Puls.counter.10)) +
  geom_line(aes(y = NH3.x, color = "NH3.x")) +
  geom_line(aes(y = NH3.y, color = "NH3.y")) +
  scale_color_manual(values = c("NH3.x" = "darkgreen", "NH3.y" = "green2")) +
  labs(x = "Ventilation rate (%)", y = "NH3") +
  ggtitle("NH3 trends measured by 178V and 178F at Ventilator:4")

# For Vent_5_comb
NH3plot_Vent_5 <- ggplot(Vent_5_comb, aes(x = Puls.counter.11, y = NH3)) +
  geom_line(color = "darkgreen") +
  labs(x = "Ventilation rate (%)", y = "NH3") +
  ggtitle("NH3 trend measured by 178P at Ventilator:5")


# Define the output PDF file
output_file <- "Plots_OTICE-Ventilation_results_2023.pdf"

# Create a PDF device
pdf(output_file, width = 8, height = 5)

# List of plots
plots <- list(
  CO2plot_Vent_1, CO2plot_Vent_2, CO2plot_Vent_3, CO2plot_Vent_4, CO2plot_Vent_5,
  CH4plot_Vent_1, CH4plot_Vent_2, CH4plot_Vent_3, CH4plot_Vent_4, CH4plot_Vent_5,
  NH3plot_Vent_1, NH3plot_Vent_2, NH3plot_Vent_3, NH3plot_Vent_4, NH3plot_Vent_5
)

# Loop through the list and print each plot
for (plot in plots) {
  print(plot)
}

# Close the PDF device
dev.off()




####### Generalized Linear Regression modelling #####

#CO2
#Regression  effect: Ventilation rate  
GLM_CO2.1 <- summary(glm(CO2.x~Puls.counter.7, data=Vent_1_comb))
GLM_CO2.2 <- summary(glm(CO2.x~Puls.counter.8, data=Vent_2_comb))
GLM_CO2.3 <- summary(glm(CO2.x~Puls.counter.9, data=Vent_3_comb))
GLM_CO2.4 <- summary(glm(CO2.x~Puls.counter.10, data=Vent_4_comb))
GLM_CO2.5 <- summary(glm(CO2~Puls.counter.11, data=Vent_5_comb))

#CH4
#Regression  effect: Ventilation rate  
GLM_CH4.1 <- summary(glm(CH4.x~Puls.counter.7, data=Vent_1_comb))
GLM_CH4.2 <- summary(glm(CH4.x~Puls.counter.8, data=Vent_2_comb))
GLM_CH4.3 <- summary(glm(CH4.x~Puls.counter.9, data=Vent_3_comb))
GLM_CH4.4 <- summary(glm(CH4.x~Puls.counter.10, data=Vent_4_comb))
GLM_CH4.5 <- summary(glm(CH4~Puls.counter.11, data=Vent_5_comb))

#NH3
#Regression  effect: Ventilation rate  
GLM_NH3.1 <- summary(glm(NH3.x~Puls.counter.7, data=Vent_1_comb))
GLM_NH3.2 <- summary(glm(NH3.x~Puls.counter.8, data=Vent_2_comb))
GLM_NH3.3 <- summary(glm(NH3.x~Puls.counter.9, data=Vent_3_comb))
GLM_NH3.4 <- summary(glm(NH3.x~Puls.counter.10, data=Vent_4_comb))
GLM_NH3.5 <- summary(glm(NH3~Puls.counter.11, data=Vent_5_comb))

