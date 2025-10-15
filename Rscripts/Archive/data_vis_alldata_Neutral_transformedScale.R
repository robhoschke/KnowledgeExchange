
##to do:1. check how NA's are being addressed
##      2. adjust question wording to reflect transformation



# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)



# Select only the Likert scale columns (from column 21 onwards)
likert_data <- combined_data_positive[, 21:ncol(combined_data_positive)]

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

# Function to create diverging stacked bar chart WITH NEUTRAL responses
create_diverging_chart_with_neutral <- function(data) {
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
  
  # Calculate positions for diverging chart with neutral in center
  plot_data <- plot_data %>%
    arrange(Question, ResponseFactor) %>%
    group_by(Question) %>%
    mutate(
      # Get the neutral percentage to center it
      neutral_pct = ifelse(ResponseFactor == "Neutral", Percentage, 0),
      neutral_half = max(neutral_pct) / 2,
      
      # Calculate cumulative positions
      PercentageAdj = case_when(
        ResponseFactor == "StronglyDisagree" ~ -cumsum(c(Percentage[ResponseFactor == "StronglyDisagree"], 
                                                         Percentage[ResponseFactor == "Disagree"]))[1],
        ResponseFactor == "Disagree" ~ -Percentage[ResponseFactor == "Disagree"] - neutral_half,
        ResponseFactor == "Neutral" ~ 0,
        ResponseFactor == "Agree" ~ Percentage[ResponseFactor == "Agree"] + neutral_half,
        ResponseFactor == "StronglyAgree" ~ cumsum(c(Percentage[ResponseFactor == "Agree"], 
                                                     Percentage[ResponseFactor == "StronglyAgree"]))[2],
        TRUE ~ 0
      ),
      
      # For stacked bars, we need the actual percentage values
      PercentageValue = case_when(
        ResponseFactor %in% c("StronglyDisagree", "Disagree") ~ -Percentage,
        ResponseFactor == "Neutral" ~ Percentage,
        ResponseFactor %in% c("Agree", "StronglyAgree") ~ Percentage,
        TRUE ~ 0
      )
    ) %>%
    ungroup()
  
  
  # Create separate data for negative (left) and positive (right) sides plus neutral
  plot_data_final <- plot_data %>%
    mutate(
      PlotValue = case_when(
        ResponseFactor %in% c("StronglyDisagree", "Disagree") ~ -Percentage,
        ResponseFactor == "Neutral" ~ Percentage,
        ResponseFactor %in% c("Agree", "StronglyAgree") ~ Percentage,
        TRUE ~ 0
      )
    )
  
  # Create the plot
  p <- ggplot(plot_data_final, aes(x = Question)) +
    # Negative responses (left side)
    geom_col(data = filter(plot_data_final, ResponseFactor %in% c("StronglyDisagree", "Disagree")),
             aes(y = PlotValue, fill = ResponseFactor), width = 0.8) +
    # Positive responses (right side)  
    geom_col(data = filter(plot_data_final, ResponseFactor %in% c("Agree", "StronglyAgree")),
             aes(y = PlotValue, fill = ResponseFactor), width = 0.8) +
    # Neutral responses (centered)
    geom_col(data = filter(plot_data_final, ResponseFactor == "Neutral"),
             aes(y = PlotValue/2, fill = ResponseFactor), width = 0.8, position = "identity") +
    geom_col(data = filter(plot_data_final, ResponseFactor == "Neutral"),
             aes(y = -PlotValue/2, fill = ResponseFactor), width = 0.8, position = "identity") +
    coord_flip() +
    scale_fill_manual(
      values = c("StronglyDisagree" = "#d73027",
                 "Disagree" = "#fc8d59", 
                 "Neutral" = "#ffffbf",
                 "Agree" = "#91bfdb",
                 "StronglyAgree" = "#4575b4"),
      labels = c("StronglyDisagree" = "Strongly Disagree",
                 "Disagree" = "Disagree",
                 "Neutral" = "Neutral",
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
      title = "Fishers' Knowledge Survey Results (Including Neutral Responses)",
      x = NULL,
      y = "Percentage"
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

# Create and display the chart with neutral responses
chart_with_neutral <- create_diverging_chart_with_neutral(likert_data)
print(chart_with_neutral)

# Optional: Save the chart
ggsave("likert_diverging_chart_with_neutral.png", chart_with_neutral, width = 12, height = 10, dpi = 300)

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