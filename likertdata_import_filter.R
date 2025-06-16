library(tidyverse)



 <- read.csv('data/collated_survey_responses.csv')
# Convert from wide to long format
responses_long <- responses %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "Question",
    values_to = "Response"
  )


ggplot(responses_long, aes(x = Question, fill = factor(Response))) +
  geom_bar(position = "fill") +  # Use "stack" instead of "fill" for raw counts
  labs(
    x = "Question",
    y = "Proportion of Responses",
    fill = "Response Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Transpose and set new column names
responses_t <- as.data.frame(t(responses))

# First row (original column names) becomes new column names
colnames(responses_t) <- responses_t[1, ]
responses_t <- responses_t[-1, ]

# Add question labels as a new column
responses_t <- responses_t %>%
  rownames_to_column(var = "Question")



responses_long <- responses_t %>%
  pivot_longer(
    cols = -Question,
    names_to = "Respondent",
    values_to = "Response"
  ) %>%
  mutate(Response = as.numeric(Response))


ggplot(responses_long, aes(y = fct_rev(Question), fill = factor(Response))) +
  geom_bar(position = "fill") +  # Use "stack" for raw counts
  labs(
    y = "Question",
    x = "Proportion of Responses",
    fill = "Response Value"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))




