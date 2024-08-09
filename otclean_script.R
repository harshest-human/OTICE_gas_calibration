library(dplyr)

otclean <- function(input_path, output_path, result_file_name) {
  # Get a list of all .txt files in the input directory
  files <- list.files(path = input_path, pattern = "\\.TXT$", full.names = TRUE)
  
  # Initialize an empty list to store data frames
  data_list <- list()
  
  # Loop through each file
  for (file in files) {
    # Read the data from the file
    data <- read.table(file, sep = "/", header = FALSE, stringsAsFactors = FALSE)
    
    # Select the desired columns and rename them
    selected_data <- data %>%
      select(DATE.TIME = V1, NH3 = V10, Temperature = V11, Humidity = V12)
    
    # Extract the node number from the filename
    node_number <- gsub("^.*_(\\d+)\\.TXT$", "\\1", basename(file))
    
    # Add the Node column
    selected_data$Node <- node_number
    
    # Add the processed data to the list
    data_list[[length(data_list) + 1]] <- selected_data
  }
  
  # Concatenate all data frames in the list
  final_data <- bind_rows(data_list)
  
  # Write the concatenated data to a CSV file
  write.csv(final_data, file = file.path(output_path, result_file_name), row.names = FALSE)
}

# Example
input_path = "D:/Data Analysis/Gas_data/Raw_data/OTICE_v3_raw"
output_path = "D:/Data Analysis/Gas_data/Clean_data/OTICE_clean"
result_file_name = "20240730_OTICEv3_data.csv"

OTICEv3.data <- otclean(input_path, output_path, result_file_name)
