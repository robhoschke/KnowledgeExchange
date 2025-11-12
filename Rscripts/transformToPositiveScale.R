# Load required libraries
library(dplyr)

source("Rscripts/likertdata_import_filter.R")


# Define the columns that need to be reversed (negative scale questions)

str(combined_data)

colnames(combined_data)
negative_columns <- c(21, 23, 24, 26, 28, 30, 32, 33, 36, 39, 40, 42)


combined_data_positive <- combined_data


combined_data[,20:43] <- sapply(combined_data[,20:43], as.numeric)


for(col in negative_columns) {
  
  col_name <- names(combined_data)[col]
  
  non_na_before <- sum(!is.na(combined_data[, col]))
  
  combined_data_positive[, col] <- ifelse(
    is.na(combined_data[, col]), 
    NA, 
    12 - combined_data[, col]
  )
  
}


new_names <- names(combined_data_positive)

# Apply the positive rewordings to specific columns
new_names[21] <- "2 Anecdotal knowledge is compatible with scientific modelling"
new_names[23] <- "4 It is easy to discern knowledge from advocacy when engaging fishers"
new_names[24] <- "5 Involving fishers does not compromise the independence of fisheries research and management"
new_names[26] <- "7 Fishers believe scientific models are free from political manipulation"
new_names[28] <- "9 We have sufficient funding to effectively and regularly engage fishers"
new_names[30] <- "11 My team have time to engage fishers"
new_names[32] <- "13 I have the expertise necessary to effectively communicate with fishers"
new_names[33] <- "14 My organisation's protocols support the inclusion of fishers' knowledge in research"
new_names[36] <- "17 Fishers have time to be involved in research projects"
new_names[39] <- "20 I can easily understand the language used by fishers"
new_names[40] <- "21 Fishers can easily articulate their knowledge to scientists"
new_names[42] <- "23 Fishers are willing to share insights with me because they trust their knowledge will not be used against them"

# Apply the new column names to the dataframe
names(combined_data_positive) <- new_names




# Save the new dataset
#write.csv(combined_data_positive, "data/combined_likert_data_positive_scale.csv", row.names = FALSE)

