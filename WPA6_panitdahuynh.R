# Assignment: WPA 6
# Name: Panitda Huynh
# Date: 19 April 2022

df1 = read.csv("C:/Users/Kyurin Wong/OneDrive/Studium/Master FS 2022/Programming in R/Homework/WPA6/data/tdcs.csv")

head(df1)
glimpse(df1)

#library
library(tidyverse)
###TASK A###
###density plot
ggplot(data = df1, mapping = aes(x = accuracy, fill = acc_spd)) + 
  geom_density(alpha = .8) +
  labs(x = 'Accuracy', y = 'Speed Conditions') 

###histogram
ggplot(data = df1, mapping = aes(x = accuracy, fill = acc_spd)) + 
  geom_histogram(alpha = .8) +
  labs(x = 'Accuracy', y = 'Speed Conditions') + facet_grid(~ df1$tdcs)

  
###violinplot
ggplot(data = df1, mapping = aes(x = accuracy, y = acc_spd)) + 
  geom_violin(draw_quantiles = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  labs(x = 'Accuracy', y = 'Speed Conditions') + facet_grid(~ df1$dataset)

###TASK B###
summary_tdcs_data = summarise(group_by(df1, id, coherence, acc_spd),
                              mean_RT=mean(RT), 
                              mean_accuracy=mean(accuracy))

glimpse(summary_tdcs_data)

#scatterplot
ggplot(data = summary_tdcs_data, mapping = aes(x = mean_RT, y = mean_accuracy, col = coherence)) + 
  geom_point(alpha = .3, size= 1) +
  geom_vline(xintercept = mean(summary_tdcs_data$mean_RT), linetype="dotted") +
  geom_hline(yintercept = mean(summary_tdcs_data$mean_accuracy), linetype="dotted") +
  geom_smooth(method = lm, se=FALSE,fullrange=TRUE,color="grey") +
  labs(x='response times', y='mean accuracy') +
  ggtitle("Relationship between response times and mean accuracy") + facet_grid(~acc_spd) + scale_colour_gradient(low = "blue", high = "red")


#Task C#
#bars
ggplot(data = summary_tdcs_data, mapping = aes(x = factor(coherence), y = mean_accuracy)) + 
  geom_boxplot() +  stat_summary(fun = "position", geom="bar", width=.9, position = 'dodge') +
  labs(x = "Coherence", y='Mean Accuracy') + facet_grid(~acc_spd)

#points
ggplot(data = summary_tdcs_data, mapping = aes(x = factor(coherence), y = mean_accuracy)) + 
  geom_point() +  stat_summary(fun = "position", geom="bar", width=.9, geom = "errorbar") +
  labs(x = "Coherence", y='Mean Accuracy') + facet_grid(~acc_spd)
