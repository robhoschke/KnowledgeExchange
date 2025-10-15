
###plot using likert package - trasnformed to positive questions 
#####plot using likert package

#install.packages("likert")
library(likert)
library(readr)

#source("Rscripts/transformToPositiveScale.R")   ##not working
#combined_data_positive <- read.csv("data/combined_likert_data_positive_scale.csv") ####edits labels to dots

##run transform code first

str(combined_data_positive)


likert_only_positive <- combined_data_positive[,20:43] %>%
  mutate_if(is.integer, as.factor) %>% 
  as.data.frame() 


all_levels <- c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11")
# Or if numeric: all_levels <- c("1", "2", "3", "4", "5")

# Apply consistent levels to all columns
likert_only_positive <- likert_only_positive %>%
  mutate_all(~factor(., levels = all_levels))

# Now create likert object
likert_obj1 <- likert(likert_only_positive)
plot(likert_obj1)


str(likert_only)

###
######
######### plot using all positive scale






