##import data, merge datasets, add institution column, clean names
#


library(tidyverse)
library(dplyr)
library(readr)
library(likert)



file_paths <- c(
  "data/Fishers+knowledge+in+research_+DBCA_June+19,+2025_15.10.csv",
  "data/Fishers+knowledge+in+research_+DPIRD_June+19,+2025_15.11.csv",  
  "data/Fishers+knowledge+in+research_+non-gov_July+1,+2025_13.57.csv" 
)

read_and_label <- function(file_path) {
  # Extract institution from filename
  if (grepl("DBCA", file_path, ignore.case = TRUE)) {
    institution <- "dbca"
  } else if (grepl("DPIRD", file_path, ignore.case = TRUE)) {
    institution <- "dpird"
  } else if (grepl("non-gov", file_path, ignore.case = TRUE)) {
    institution <- "nongov"
  } else {
    warning(paste("Could not identify institution for file:", file_path))
    institution <- "Unknown"
  }
  
  # Skip row 1 (short codes) so that row 2 (descriptive headers) becomes column names
  # This will skip the JSON metadata row 3 automatically and start data from row 4
  data <- read.csv(file_path, skip = 1, stringsAsFactors = FALSE)
  
  # Remove the JSON metadata row (which is now row 1 of our data)
  data <- data[-1, ]
  
  # Add institution column
  data$institution <- institution
  
  return(data)
}

# Function to get the descriptive headers from the first file (as the standard)
get_standard_headers <- function(file_path) {
  # Read just the descriptive headers (row 2) from the first file
  # We read with skip=1 so row 2 becomes the header row
  temp_data <- read.csv(file_path, skip = 1, nrows = 1, stringsAsFactors = FALSE)
  descriptive_headers <- colnames(temp_data)
  return(descriptive_headers)
}

# Function to standardize column names based on position
standardize_columns <- function(data_list) {
  # Use the first dataset as the template for column names (by position)
  standard_names <- names(data_list[[1]])
  
  # Apply these names to all datasets
  for (i in 1:length(data_list)) {
    # Check if number of columns match (excluding institution column)
    if ((ncol(data_list[[i]]) - 1) == (length(standard_names) - 1)) {
      names(data_list[[i]])[1:(ncol(data_list[[i]])-1)] <- standard_names[1:(length(standard_names)-1)]
      # Keep institution column name consistent
      names(data_list[[i]])[ncol(data_list[[i]])] <- "institution"
    } else {
      warning(paste("Dataset", i, "has different number of columns"))
      # If different number of columns, standardize up to the minimum
      data_cols <- ncol(data_list[[i]]) - 1  # Exclude institution column
      standard_cols <- length(standard_names) - 1  # Exclude institution column
      min_cols <- min(data_cols, standard_cols)
      names(data_list[[i]])[1:min_cols] <- standard_names[1:min_cols]
      names(data_list[[i]])[ncol(data_list[[i]])] <- "institution"
    }
  }
  
  return(data_list)
}

# Read all files and store in a list
data_list <- lapply(file_paths, read_and_label)

# Standardize column names based on position (using short codes)
data_list <- standardize_columns(data_list)

# Combine all datasets
combined_data <- bind_rows(data_list)

# Now apply the descriptive headers from row 2 of the first file
standard_descriptive_headers <- get_standard_headers(file_paths[1])

# Replace the short code column names with descriptive headers
# (but keep institution column as is)
current_names <- colnames(combined_data)
institution_col <- which(current_names == "institution")

# Apply descriptive headers to all columns except institution
for(i in 1:(ncol(combined_data))) {
  if(i != institution_col && i <= length(standard_descriptive_headers)) {
    colnames(combined_data)[i] <- standard_descriptive_headers[i]
  }
}



combined_data <- combined_data %>%
  select(institution, everything())

#######
##############
#####################tidy column names



# Function to clean survey question headers
clean_question_header <- function(header) {
  # Remove the long preamble text
  cleaned <- gsub("Please\\.score\\.the\\.following\\.statements\\.from\\.0\\.to\\.10.*?Strongly\\.agree\\.+", "", header)
  
  # Remove question numbers (e.g., "1.", "2.", etc.) from the beginning
  cleaned <- gsub("^\\d+\\.+", "", cleaned)
  
  
  # Clean up extra dots and spaces
  cleaned <- gsub("\\.+", " ", cleaned)  # Replace multiple dots with spaces
  cleaned <- gsub("\\s+", " ", cleaned)  # Replace multiple spaces with single space
  cleaned <- trimws(cleaned)  # Remove leading/trailing whitespace
  
  return(cleaned)
}

# Get current column names
current_names <- colnames(combined_data)

# Identify survey question columns (starting from column U, which is column 21)
# Looking for columns that contain the preamble text
survey_cols <- grep("Please\\.score\\.the\\.following\\.statements", current_names)

# Clean the survey question headers
cleaned_names <- current_names
for(i in survey_cols) {
  cleaned_names[i] <- clean_question_header(current_names[i])
}


colnames(combined_data) <- cleaned_names
