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



######plot
###merge df's

merged_df <- data.frame(
  institution = inst_summary$institution,
  inst_mean = inst_summary$rowmeans,
  comm_mean = comm_summary$rowmeans,
  trust_mean = trust_summary$rowmeans,
  valid_mean = valid_summary$rowmeans
)




# Melt the data
merged_long <- melt(merged_df, 
                    id.vars = "institution",
                    variable.name = "barrier",
                    value.name = "score")


merged_long$barrier <- factor(merged_long$barrier, 
                              levels = c("comm_mean","trust_mean", "inst_mean","valid_mean"))


ggplot(merged_long, aes(x = score, y = barrier, fill = institution)) +
  geom_boxplot() +
  scale_y_discrete(labels = c(
    "inst_mean" = "Institutional",
    "comm_mean" = "Communication",
    "trust_mean" = "Trust",
    "valid_mean" = "Legitimacy and credibility"
  )) +
  scale_fill_manual(
    name = "Management proximity",
    values = c("fisheries_scientists" = "#7FA67E", 
               "marine_scientists" = "#8F7FA6"),
    labels = c("fisheries_scientists" = "High", 
               "marine_scientists" = "Low"),
  ) +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(
    x = "Mean Score",
    y = "Barrier Type"
  ) +
  theme_minimal(base_size = 20)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())





###
###### stats tests 


barrier_dfs <- list(
  inst = inst_summary,
  legit = valid_summary,
  comm = comm_summary,
  trust = trust_summary
)


extract_stats <- function(barrier_name, df) {
  test <- t.test(rowmeans ~ institution, data = df, var.equal = FALSE)
  
  means <- tapply(df$rowmeans, df$institution, mean)
  sds <- tapply(df$rowmeans, df$institution, sd)
  ns <- tapply(df$rowmeans, df$institution, length)
  
  # Calculate pooled standard deviation
  pooled_sd <- sqrt(((ns["fisheries_scientists"] - 1) * sds["fisheries_scientists"]^2 + 
                       (ns["marine_scientists"] - 1) * sds["marine_scientists"]^2) / 
                      (ns["fisheries_scientists"] + ns["marine_scientists"] - 2))
  
  # Calculate Cohen's d
  cohens_d <- (means["fisheries_scientists"] - means["marine_scientists"]) / pooled_sd
  
  data.frame(
    barrier = barrier_name,
    fisheries_mean = means["fisheries_scientists"],
    marine_mean = means["marine_scientists"],
    difference = means["fisheries_scientists"] - means["marine_scientists"],
    cohens_d = cohens_d,
    t_statistic = test$statistic,
    df = test$parameter,
    p_value = test$p.value,
    ci_lower = test$conf.int[1],
    ci_upper = test$conf.int[2]
  )
}


# Apply to all dataframes
results_table <- map2_df(names(barrier_dfs), barrier_dfs, extract_stats)

print(results_table)

# Export if needed
write.csv(results_table, "ttests_results.csv", row.names = FALSE)


##
####
##### wilcox test 

barrier_dfs <- list(
  inst = inst_summary,
  legit = valid_summary,
  comm = comm_summary,
  trust = trust_summary
)

extract_stats <- function(barrier_name, df) {
  # Wilcoxon rank-sum test (Mann-Whitney U)
  test <- wilcox.test(rowmeans ~ institution, data = df, exact = FALSE)
  
  # Calculate summary statistics
  means <- tapply(df$rowmeans, df$institution, mean)
  medians <- tapply(df$rowmeans, df$institution, median)
  sds <- tapply(df$rowmeans, df$institution, sd)
  ns <- tapply(df$rowmeans, df$institution, length)
  
  # Calculate rank-biserial correlation (effect size for Mann-Whitney)
  # This is the non-parametric equivalent of Cohen's d
  r_rank_biserial <- 1 - (2 * test$statistic) / (ns["fisheries_scientists"] * ns["marine_scientists"])
  
  data.frame(
    barrier = barrier_name,
    fisheries_mean = means["fisheries_scientists"],
    marine_mean = means["marine_scientists"],
    fisheries_median = medians["fisheries_scientists"],
    marine_median = medians["marine_scientists"],
    fisheries_sd = sds["fisheries_scientists"],
    marine_sd = sds["marine_scientists"],
    W_statistic = test$statistic,
    p_value = test$p.value,
    rank_biserial_r = r_rank_biserial
  )
}

# Apply to all dataframes
results_table <- map2_df(names(barrier_dfs), barrier_dfs, extract_stats)
print(results_table)

# Optional: Format for cleaner display
results_table %>%
  mutate(across(c(fisheries_mean:marine_sd, rank_biserial_r), ~round(., 3)),
         p_value = round(p_value, 4))



