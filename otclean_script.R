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
  
  # Extract DATE.TIME values from NODE 15
  node_15_times <- final_data %>%
    filter(Node == 15) %>%
    pull(DATE.TIME)
  
  # Check if there are DATE.TIME values for NODE 15
  if (length(node_15_times) > 0) {
    # Apply the DATE.TIME values from NODE 15 to all nodes
    final_data <- final_data %>%
      mutate(DATE.TIME = rep(node_15_times, length.out = n()))
  } else {
    warning("No DATE.TIME values found for NODE 15. DATE.TIME alignment not applied.")
  }
  
  # Inspect the first few DATE.TIME values
  print(head(final_data$DATE.TIME))
  
  # Clean DATE.TIME values if necessary
  final_data <- final_data %>%
    mutate(DATE.TIME = gsub("\\[|\\]", "", DATE.TIME))  # Remove brackets if present
  
  # Convert DATE.TIME to POSIXct format
  # Adjust the format string to match the actual format of DATE.TIME values
  final_data <- final_data %>%
    mutate(DATE.TIME = as.POSIXct(DATE.TIME, format = "%Y-%m-%d %H:%M:%S"))
  
  # Check for conversion issues
  if (any(is.na(final_data$DATE.TIME))) {
    warning("Some DATE.TIME values could not be converted to POSIXct. Please check the format.")
  }
  
  # Write the concatenated data to a CSV file
  write.csv(final_data, file = file.path(output_path, result_file_name), row.names = FALSE)
}

# Example usage
input_path = "D:/Data Analysis/Gas_data/Raw_data/OTICE_v3_raw"
output_path = "D:/Data Analysis/Gas_data/Clean_data/OTICE_clean"
result_file_name = "20240730_OTICEv3_data.csv"

OTICEv3.data <- otclean(input_path, output_path, result_file_name)
