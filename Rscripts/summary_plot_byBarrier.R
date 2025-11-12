#### summary plot
##run data filter, convert to possitive, and first section in data vis by theme


trust 
valid_knowledge 
communication 
institutional_resourcing 

## valid_knwoeldge

str(valid_knowledge)
colnames(valid_knowledg)

valid_knowledge <- valid_knowledge %>%
  mutate(across(2:3, as.numeric))

valid_knowledge$rowmeans <- rowMeans(valid_knowledge[,-1])
valid_summary <- valid_knowledge[,c(1,6)]
valid_summary$barrier <- "valid_knowledge"

##trust
str(trust)
colnames(trust)

trust <- trust %>%
  mutate(across(c(2,4,6), as.numeric))

trust$rowmeans <- rowMeans(trust[,-1])
trust_summary <- trust[,c(1,7)]
trust_summary$barrier <- "trust"

##communication
str(communication)
colnames(communication)

communication <- communication %>%
  mutate(across(c(3,4,7), as.numeric))

communication$rowmeans <- rowMeans(communication[,-1])
comm_summary <- communication[,c(1,8)]
comm_summary$barrier <- "comms"


##institutional
str(institutional_resourcing)

institutional_resourcing <- institutional_resourcing %>%
  mutate(across(c(3,5,7), as.numeric))

institutional_resourcing$rowmeans <- rowMeans(institutional_resourcing[,-1], na.rm=TRUE)
inst_summary <- institutional_resourcing[,c(1,9)]
inst_summary$barrier <- "inst"

ggplot(inst_summary, aes(x=institution, y=rowmeans) ) +
  geom_boxplot()


###merge df's

merged_df <- data.frame(
  institution = inst_summary$institution,
  inst_mean = inst_summary$rowmeans,
  comm_mean = comm_summary$rowmeans,
  trust_mean = trust_summary$rowmeans,
  valid_mean = valid_summary$rowmeans
)

ggplot(merged_df, aes(x=institution, y=rowmeans) ) +
  geom_boxplot()



###plot

library(ggplot2)
library(reshape2)

# Melt the data
merged_long <- melt(merged_df, 
                    id.vars = "institution",
                    variable.name = "barrier",
                    value.name = "score")

# rename and Create boxplot


ggplot(merged_long, aes(x = barrier, y = score, fill = institution)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("marine_scientists" = "#D8B365", 
                               "fisheries_scientists" = "#5AB4AC")) +
  labs(title = "Comparison of Barriers by Institution",
       x = "Barrier Type",
       y = "Mean Score",
       fill = "Institution") +
  theme_minimal()






# Relevel institution so fisheries_scientists (High) comes first
merged_long$institution <- factor(merged_long$institution, 
                                  levels = c("fisheries_scientists", "marine_scientists"))

# Relevel barrier to set custom order
merged_long$barrier <- factor(merged_long$barrier,
                              levels = c("inst_mean", "comm_mean", "trust_mean", "valid_mean"),
                              labels = c("Institutional", "Communication", "Trust", "Legitimacy and credibility"))

# Create horizontal boxplot
ggplot(merged_long, aes(x = barrier, y = score, fill = institution)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_manual(values = c("fisheries_scientists" = "#5AB4AC",
                               "marine_scientists" = "#D8B365"),
                    labels = c("High", "Low")) +
  labs(title = "Comparison of Barriers by Institution",
       x = "Barrier Type",
       y = "Mean Score",
       fill = "Management proximity") +
  theme_minimal()





###t-tests

##institution

str(inst_summary)
t_test_result <- t.test(rowmeans ~ institution, data = inst_summary)


welch_result <- t.test(rowmeans ~ institution, 
                       data = inst_summary, 
                       var.equal = FALSE)

print(welch_result)


##legit

str(valid_summary)
t_test_result <- t.test(rowmeans ~ institution, data = valid_summary)


welch_result <- t.test(rowmeans ~ institution, 
                       data = valid_summary, 
                       var.equal = FALSE)

print(welch_result)

##comm

str(comm_summary)
t_test_result <- t.test(rowmeans ~ institution, data = comm_summary)


welch_result <- t.test(rowmeans ~ institution, 
                       data = comm_summary, 
                       var.equal = FALSE)

print(welch_result)

##trust

str(trust_summary)
t_test_result <- t.test(rowmeans ~ institution, data = trust_summary)


welch_result <- t.test(rowmeans ~ institution, 
                       data = trust_summary, 
                       var.equal = FALSE)

print(welch_result)







