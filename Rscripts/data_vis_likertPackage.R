
###
#####plot using likert package

#install.packages("likert")


source("Rscripts/likertdata_import_filter.R")

str(combined_data)



likert_only <- combined_data[,20:43] %>%
  mutate_if(is.integer, as.factor) %>% 
  as.data.frame() 


all_levels <- c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11")
# Or if numeric: all_levels <- c("1", "2", "3", "4", "5")

# Apply consistent levels to all columns
likert_only <- likert_only %>%
  mutate_all(~factor(., levels = all_levels))

# Now create likert object
likert_obj <- likert(likert_only)
plot(likert_obj)


str(likert_only)

###
######
######### plot using all positive scale






