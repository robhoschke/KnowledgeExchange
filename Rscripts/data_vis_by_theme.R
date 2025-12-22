##group questions by theme



###run data filtering and Transform to positive scripts

install.packages('patchwork')
install.packages('kableExtra')

library(tidyr)
library(tidyverse) # for all things data wrangling
library(likert) # for creating a likert plot
library(kableExtra) 
library(gridExtra)
library(grid)
library(patchwork)
library(cowplot)
library(png)

colnames(combined_data_positive)
colnames(combined_data_positive)[20:43] <- gsub("^\\d+\\s", "", colnames(combined_data_positive)[20:43])

combined_data_positive[,1] <- as.factor(combined_data_positive[,1]) 
levels(combined_data_positive$institution)
levels(combined_data_positive$institution) <- c("marine_scientists", "fisheries_scientists", "marine_scientists")

names(combined_data_positive)[20:43] <- gsub("^\\d+\\s+", "", names(combined_data_positive)[20:43])


trust <- combined_data_positive[,c(1,43,42,25,26,27)]
valid_knowledge <- combined_data_positive[,c(1,20,22,23,24)]
communication <- combined_data_positive[,c(1,32,37,38,39,40,41)]
institutional_resourcing <- combined_data_positive[,c(1,28,29,30,31,33,35,36)]

head(valid_knowledge)
colnames(valid_knowledge)
str(valid_knowledge)
names(combined_data_positive)
str(combined_data_positive)


###
#######plots side by side
##
####


##trust##

## fisheries scientists
str(trust)
names(trust)
#trust_fisheries_scientists <- trust[c(5:15),2:6]
trust_fisheries_scientists <- trust[trust$institution == "fisheries_scientists", 2:6]
names(trust_fisheries_scientists)
trust_fisheries_scientists[1:5] <- lapply(trust_fisheries_scientists[1:5], factor, levels=1:11) 
likt_fish <-  likert(trust_fisheries_scientists)


names(trust_fisheries_scientists)
p_fish <- plot(likt_fish, group.order = c(           
                                            "Fishers trust that the professional knowledge they share with us scientists will not be shared with the wider public without their approval",                         
                                            "Fishers are willing to share insights with me because they trust their knowledge will not be used against them",
                                            "Fishers believe the scientific models we use are accurate",
                                            "Fishers trust us scientists to inform decision making without their input",
                                            "Fishers believe scientific models are free from political manipulation"),
                          text.size = 0)



## marine scientists
str(trust)

#trust_marine_scientists <- trust[c(1:4,18:25),2:6]
trust_marine_scientists <- trust[trust$institution == "marine_scientists", 2:6]
trust_marine_scientists[1:5] <- lapply(trust_marine_scientists[1:5], factor, levels=1:11) 
likt_marine <-  likert(trust_marine_scientists)
names(trust_marine_scientists)
p_marine <- plot(likt_marine, group.order = c(                           
                                            "Fishers trust that the professional knowledge they share with us scientists will not be shared with the wider public without their approval",                         
                                            "Fishers are willing to share insights with me because they trust their knowledge will not be used against them",
                                            "Fishers believe the scientific models we use are accurate",
                                            "Fishers trust us scientists to inform decision making without their input",
                                            "Fishers believe scientific models are free from political manipulation"),
                              text.size = 0)


##
###combining plots

p_fishsc <- p_fish +
  theme(plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none",
        axis.text.y = element_text(size=25),
        axis.text.x = element_text(size=18),
        title = element_text (size = 20)) +
  labs(title = "Fisheries agency scientists")


p_marinesc  <- p_marine + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=18),
        title = element_text(size = 20),
        plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none") +
  labs(title = "University and conservation scientists")



##combine plots

combined_plot <- p_fishsc + p_marinesc

final_plot <- ggdraw(combined_plot) +
  draw_image("PlotLegend_1.png", 
             x = 0.49,      # x position (0-1, left to right)
             y = 0.0,      # y position (0-1, bottom to top)  
             width = 0.36,  # width as fraction of plot
             height = 0.48,  # height as fraction of plot
             hjust = 0, vjust = 0)  # anchor point

#export ratio (20,10)

##
####valid knowledge
#
#

## fisheries scientists
names(valid_knowledge)
#valid_fisheries_scientists <- valid_knowledge[c(5:17),2:5]
valid_fisheries_scientists <- valid_knowledge[valid_knowledge$institution == "fisheries_scientists", 2:5]
valid_fisheries_scientists[1:4] <- lapply(valid_fisheries_scientists[1:4], factor, levels=1:11) 
likt_fish <-  likert(valid_fisheries_scientists)
p_fish <- plot(likt_fish, group.order = c( "Fishers anecdotal knowledge is useful as a basis for management",
                                           "Fishers have a strong sense of how to sustainably manage fisheries",
                                           "Involving fishers does not compromise the independence of fisheries research and management",
                                           "It is easy to discern knowledge from advocacy when engaging fishers"),
               text.size = 0)  

## marine scientists

#valid_marine_scientists <- valid_knowledge[c(1:4,18:25),2:5]
valid_marine_scientists <- valid_knowledge[valid_knowledge$institution == "marine_scientists", 2:5]
valid_marine_scientists[1:4] <- lapply(valid_marine_scientists[1:4], factor, levels=1:11) 
likt_marine <-  likert(valid_marine_scientists)
names(valid_marine_scientists)
p_marine <- plot(likt_marine, group.order = c( "Fishers anecdotal knowledge is useful as a basis for management",
                                               "Fishers have a strong sense of how to sustainably manage fisheries",
                                               "Involving fishers does not compromise the independence of fisheries research and management",
                                               "It is easy to discern knowledge from advocacy when engaging fishers"),
                 text.size = 0)  


##
###combining plots

p_fishsc <- p_fish +
  theme(plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none",
        axis.text.y = element_text(size=25),
        axis.text.x = element_text(size=18),
        title = element_text (size = 20)) +
  labs(title = "Fisheries agency scientists")


p_marinesc  <- p_marine + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=18),
        title = element_text(size = 20),
        plot.margin = margin(5.5, 5.5, 250, 5.5, "pt")
        ,
        legend.position = "none") +
  labs(title = "University and conservation scientists")


##combine plots

combined_plot <- p_fishsc + p_marinesc

final_plot <- ggdraw(combined_plot) +
  draw_image("PlotLegend_1.png", 
             x = 0.512,      # x position (0-1, left to right)
             y = 0.0,      # y position (0-1, bottom to top)  
             width = 0.36,  # width as fraction of plot
             height = 0.48,  # height as fraction of plot
             hjust = 0, vjust = 0)  # anchor point

#export ratio (20,10)

##
####
######institutional/resourcing

## fisheries scientists
names(institutional_resourcing)
#institut_fisheries_scientists <- institutional_resourcing[c(5:17),2:8]
institut_fisheries_scientists <- institutional_resourcing[institutional_resourcing$institution == "fisheries_scientists", 2:8]
institut_fisheries_scientists[1:7] <- lapply(institut_fisheries_scientists[1:7], factor, levels=1:11) 
likt_fish <-  likert(institut_fisheries_scientists)
p_fish <- plot(likt_fish, group.order = c( "Engagement with fishers knowledge is a key part of my role",
                                            "Fishers are interested in participating in research",
                                            "My team have time to engage fishers",
                                            "My organisation encourages me to explore ways to incorporate fishers knowledge into management",                         
                                            "My organisation's protocols support the inclusion of fishers' knowledge in research",                          
                                            "Fishers have time to be involved in research projects",
                                            "We have sufficient funding to effectively and regularly engage fishers"),
                                        text.size = 0)


## marine scientists


#institut_marine_scientists <- institutional_resourcing[c(1:4,18:25),2:8]
institut_marine_scientists <- institutional_resourcing[institutional_resourcing$institution == "marine_scientists", 2:8]
institut_marine_scientists[1:7] <- lapply(institut_marine_scientists[1:7], factor, levels=1:11) 
likt_marine <-  likert(institut_marine_scientists)
names(institut_marine_scientists)
p_marine <- plot(likt_marine, group.order = c( "Engagement with fishers knowledge is a key part of my role",
                                               "Fishers are interested in participating in research",
                                               "My team have time to engage fishers",
                                               "My organisation encourages me to explore ways to incorporate fishers knowledge into management",                         
                                               "My organisation's protocols support the inclusion of fishers' knowledge in research",                          
                                               "Fishers have time to be involved in research projects",
                                               "We have sufficient funding to effectively and regularly engage fishers"),
                                              text.size = 0)


##
###combining plots

p_fishsc <- p_fish +
  theme(plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none",
        axis.text.y = element_text(size=25),
        axis.text.x = element_text(size=18),
        title = element_text (size = 20)) +
  labs(title = "Fisheries agency scientists")


p_marinesc  <- p_marine + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=18),
        title = element_text(size = 20),
        plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none") +
  labs(title = "University and conservation scientists")


##combine plots 
combined_plot <- p_fishsc + p_marinesc

final_plot <- ggdraw(combined_plot) +
  draw_image("PlotLegend_1.png", 
             x = 0.50,      # x position (0-1, left to right)
             y = 0.0,      # y position (0-1, bottom to top)  
             width = 0.36,  # width as fraction of plot
             height = 0.48,  # height as fraction of plot
             hjust = 0, vjust = 0)  # anchor point

##export ratio (20, 11)

##
####
######communication

## fisheries scientists
str(communication)
#comm_fisheries_scientists <- communication[c(5:17),2:7]
comm_fisheries_scientists <- communication[communication$institution == "fisheries_scientists", 2:7]
comm_fisheries_scientists[1:6] <- lapply(comm_fisheries_scientists[1:6], factor, levels=1:11) 
likt_fish <-  likert(comm_fisheries_scientists)
p_fish <- plot(likt_fish, text.size = 0)

## marine scientists

#comm_marine_scientists <- communication[c(1:4,18:25),2:7]
comm_marine_scientists <- communication[communication$institution == "marine_scientists", 2:7]
comm_marine_scientists[1:6] <- lapply(comm_marine_scientists[1:6], factor, levels=1:11) 
likt_marine <-  likert(comm_marine_scientists)
names(comm_marine_scientists)
p_marine <- plot(likt_marine, group.order = c("I have the expertise necessary to effectively communicate with fishers",                            
                                              "I can easily understand the language used by fishers",
                                              "Fishers are approachable and easy to engage with",                         
                                              "Fishers are easy to get hold of",                                
                                              "Fishers can easily articulate their knowledge to scientists",                          
                                              "Fishers have a strong understanding of scientific language and approaches"),
                                            text.size = 0)


##
###combining plots

p_fishsc <- p_fish +
  theme(plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none",
        axis.text.y = element_text(size=25),
        axis.text.x = element_text(size=18),
        title = element_text (size = 20)) +
  labs(title = "Fisheries agency scientists")


p_marinesc  <- p_marine + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=18),
        title = element_text(size = 20),
        plot.margin = margin(5.5, 5.5, 250, 5.5, "pt"),
        legend.position = "none") +
  labs(title = "University and conservation scientists")


##combine plots 
combined_plot <- p_fishsc + p_marinesc

final_plot <- ggdraw(combined_plot) +
  draw_image("PlotLegend_1.png", 
             x = 0.505,      # x position (0-1, left to right)
             y = 0.0,      # y position (0-1, bottom to top)  
             width = 0.36,  # width as fraction of plot
             height = 0.48,  # height as fraction of plot
             hjust = 0, vjust = 0)  # anchor point



