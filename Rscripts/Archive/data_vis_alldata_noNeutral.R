##plotting likert data without package
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Read the cleaned data (assuming you've run the previous cleaning code)
combined_data <- read.csv("combined_likert_data_cleaned.csv", stringsAsFactors = FALSE)

# Select only the Likert scale columns (from column 21 onwards)
likert_data <- combined_data[, 21:ncol(combined_data)]

# ENHANCED NA REMOVAL OPTIONS:

# Option 1: Remove rows with ANY NA values (most restrictive)
# Uncomment this line if you want complete cases only
# likert_data <- likert_data[complete.cases(likert_data), ]

# Option 2: Remove rows with more than X% NA values (recommended)
# This keeps rows that have at least 70% of questions answered
threshold <- 0.7  # Adjust this value as needed (0.5 = 50%, 0.7 = 70%, etc.)
min_responses <- ceiling(ncol(likert_data) * threshold)
likert_data <- likert_data[rowSums(!is.na(likert_data)) >= min_responses, ]

# Option 3: Remove columns (questions) with too many NAs
# Remove questions where more than 30% of responses are missing
col_threshold <- 0.3  # Adjust as needed
min_valid_responses <- ceiling(nrow(likert_data) * (1 - col_threshold))
likert_data <- likert_data[, colSums(!is.na(likert_data)) >= min_valid_responses]

# Remove any remaining rows with all NA values (safety check)
likert_data <- likert_data[rowSums(!is.na(likert_data)) > 0, ]

# Print information about data after NA removal
cat("Data dimensions after NA removal:\n")
cat("Rows:", nrow(likert_data), "\n")
cat("Columns:", ncol(likert_data), "\n")
cat("Total NA values remaining:", sum(is.na(likert_data)), "\n")
cat("Percentage of complete data:", round((1 - sum(is.na(likert_data))/(nrow(likert_data)*ncol(likert_data))) * 100, 2), "%\n\n")

# Function to create diverging stacked bar chart
create_diverging_chart <- function(data) {
  # Convert to long format and remove NAs
  data_long <- data %>%
    mutate(ResponseID = row_number()) %>%
    pivot_longer(cols = -ResponseID, names_to = "Question", values_to = "Response") %>%
    filter(!is.na(Response)) %>%  # This removes NAs from the analysis
    mutate(Response = as.numeric(Response))
  
  # Validate that responses are in expected range (0-10)
  data_long <- data_long %>%
    filter(Response >= 0 & Response <= 10)
  
  # Calculate percentages for each question and response
  summary_data <- data_long %>%
    group_by(Question, Response) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Question) %>%
    mutate(Total = sum(Count),
           Percentage = Count / Total * 100) %>%
    ungroup()
  
  # Create response categories for better visualization
  summary_data <- summary_data %>%
    mutate(
      ResponseCategory = case_when(
        Response %in% 0:2 ~ "StronglyDisagree",
        Response %in% 3:4 ~ "Disagree",
        Response == 5 ~ "Neutral",
        Response %in% 6:7 ~ "Agree",
        Response %in% 8:10 ~ "StronglyAgree",
        TRUE ~ as.character(Response)
      ),
      ResponseFactor = factor(ResponseCategory, 
                              levels = c("StronglyDisagree", "Disagree", "Neutral", "Agree", "StronglyAgree"))
    )
  
  # Aggregate by new categories
  plot_data <- summary_data %>%
    group_by(Question, ResponseFactor) %>%
    summarise(Percentage = sum(Percentage), .groups = "drop") %>%
    complete(Question, ResponseFactor, fill = list(Percentage = 0))
  
  # Calculate positions for diverging chart (negative/positive split)
  plot_data <- plot_data %>%
    arrange(Question, ResponseFactor) %>%
    group_by(Question) %>%
    mutate(
      # For diverging chart: negative values go left, positive go right
      PercentageAdj = case_when(
        ResponseFactor %in% c("StronglyDisagree", "Disagree") ~ -Percentage,
        ResponseFactor == "Neutral" ~ 0,  # Neutral excluded from display
        ResponseFactor %in% c("Agree", "StronglyAgree") ~ Percentage,
        TRUE ~ 0
      )
    ) %>%
    # Remove neutral for this style of chart
    filter(ResponseFactor != "Neutral") %>%
    ungroup()
  
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = reorder(Question, desc(Question)), 
                             y = PercentageAdj, fill = ResponseFactor)) +
    geom_col(width = 0.8) +
    coord_flip() +
    scale_fill_manual(
      values = c("StronglyDisagree" = "#d73027",
                 "Disagree" = "#fc8d59", 
                 "Agree" = "#91bfdb",
                 "StronglyAgree" = "#4575b4"),
      labels = c("StronglyDisagree" = "Strongly Disagree",
                 "Disagree" = "Disagree",
                 "Agree" = "Agree", 
                 "StronglyAgree" = "Strongly Agree"),
      name = NULL
    ) +
    scale_y_continuous(
      labels = function(x) paste0(abs(x), "%"),
      breaks = seq(-80, 80, 20),
      limits = c(-80, 80)
    ) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
    labs(
      title = "Fishers' Knowledge Survey Results",
      x = NULL,
      y = "percentage"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", size = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

# Create and display the chart
chart <- create_diverging_chart(likert_data)
print(chart)

# Optional: Save the chart
ggsave("likert_diverging_chart_no_nas.png", chart, width = 12, height = 10, dpi = 300)

# Show summary statistics (excluding NAs)
cat("Summary of Likert responses (NAs excluded):\n")
print(summary(likert_data))

cat("\nNumber of valid (non-NA) responses per question:\n")
valid_responses <- likert_data %>% 
  summarise_all(~sum(!is.na(.))) %>%
  t() %>%
  as.data.frame() %>%
  setNames("Valid_Responses") %>%
  mutate(Missing_Responses = nrow(likert_data) - Valid_Responses,
         Percent_Valid = round((Valid_Responses / nrow(likert_data)) * 100, 1))
print(valid_responses)

# Optional: Show which questions have the most missing data
cat("\nQuestions with highest missing data rates:\n")
missing_summary <- valid_responses %>%
  arrange(Percent_Valid) %>%
  head(10)
print(missing_summary)