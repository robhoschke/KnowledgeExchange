#####
##comparing DPIRD with non-dpird

library(gridExtra)
library(grid)
library(likert)

###DPIRD###
##combined_data_positive <- read.csv("data/combined_likert_data_positive_scale.csv") ###########need labels to be correct, reading csv adjusts labels to dots
dpird_data_positive <- combined_data_positive[combined_data_positive$institution=="dpird",]



likert_dpird_positive <- dpird_data_positive[,20:43] %>%
  mutate_if(is.integer, as.factor) %>% 
  as.data.frame() 


all_levels <- c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11")
# Or if numeric: all_levels <- c("1", "2", "3", "4", "5")

# Apply consistent levels to all columns
likert_dpird_positive <- likert_dpird_positive %>%
  mutate_all(~factor(., levels = all_levels))

# Now create likert object
likert_obj_dpird <- likert(likert_dpird_positive)
p_dpird <- plot(likert_obj_dpird, text.size = 0)


####
#######
###########non-dpird

nondpird_data_positive <- combined_data_positive[combined_data_positive$institution %in% c("dbca", "nongov"),]


likert_nondpird_positive <- nondpird_data_positive[,20:43] %>%
  mutate_if(is.integer, as.factor) %>% 
  as.data.frame() 


all_levels <- c("1", "2", "3", "4", "5", "6","7", "8", "9", "10", "11")
# Or if numeric: all_levels <- c("1", "2", "3", "4", "5")

# Apply consistent levels to all columns
likert_nondpird_positive <- likert_nondpird_positive %>%
  mutate_all(~factor(., levels = all_levels))

# Now create likert object
likert_obj_nondpird <- likert(likert_nondpird_positive)


###
####
#####before ordering, check that DPIRD order has not changed with each new data entry

p_nondpird <- plot(likert_obj_nondpird, group.order = c("10 Engagement with fishers knowledge is a key part of my role",
                                          "16 Fishers are interested in participating in research",
                                          "13 I have the expertise necessary to effectively communicate with fishers",
                                          "1 Fishers anecdotal knowledge is useful as a basis for management",
                                          "22 Fishers are approachable and easy to engage with",
                                          "15 My organisation encourages me to attend fishers on their vessels to gain firsthand insights into their fishing practices",
                                          "24 Fishers trust that the professional knowledge they share with us scientists will not be shared with the wider public without their approval",
                                          "20 I can easily understand the language used by fishers",
                                          "3 Fishers have a strong sense of how to sustainably manage fisheries",
                                          "12 My organisation encourages me to explore ways to incorporate fishers knowledge into management",
                                          "11 My team have time to engage fishers",
                                          "5 Involving fishers does not compromise the independence of fisheries research and management",
                                          "19 Fishers are easy to get hold of",
                                          "14 My organisation's protocols support the inclusion of fishers' knowledge in research",
                                          "21 Fishers can easily articulate their knowledge to scientists",
                                          "17 Fishers have time to be involved in research projects",
                                          "18 Fishers have a strong understanding of scientific language and approaches",
                                          "9 We have sufficient funding to effectively and regularly engage fishers",
                                          "2 Anecdotal knowledge is compatible with scientific modelling",
                                          "23 Fishers are willing to share insights with me because they trust their knowledge will not be used against them",
                                          "8 Fishers trust us scientists to inform decision making without their input",
                                          "6 Fishers believe the scientific models we use are accurate",
                                          "4 It is easy to discern knowledge from advocacy when engaging fishers",
                                          "7 Fishers believe scientific models are free from political manipulation"), text.size = 0) 





###
######
####### compare plots

p_dpird <- p_dpird +
  theme(plot.margin = margin(5.5, 5.5, 100, 5.5, "pt"),
        legend.position = "none",
        axis.text.y = element_text(size=12),
        title = element_text (size = 15)) +
  labs(title = "Fisheries researchers")


p_nondpird_no_labels <- p_nondpird + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        title = element_text(size = 15),
        plot.margin = margin(5.5, 250, 100, 5.5, "pt"),
        legend.position = "none") +
  labs(title = "Non-fisheries researchers")




combined_plot <- grid.arrange(p_dpird, p_nondpird_no_labels, ncol = 2)




###
#####
########density plot

likert.density.plot(likert_obj_nondpird)
likert.density.plot(likert_obj_nondpird)




