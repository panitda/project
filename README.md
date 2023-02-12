# project
# Final Project
# Name: Panitda Huynh
# Date: 24 May 2022

library(tidyverse)
library(dplyr)
library(haven)
library(tidyr)
library(lme4)


# define the data folder
data_folder = '~/OneDrive/Studium/Master FS 2022/Programming in R/Homework/Final Project/Data/'

# define lists of files
list_files = paste(data_folder, list.files(path=data_folder), sep="")

# apply the read_delim function to each element in the list
data_list = lapply(list_files, function(x) read_delim(x, delim='\t')) 

# loop through all the participants to add the participant number as a column as well as the name of the file, since we have no other way to know in which conditions the participants were in
data = NULL
participant = 0

for (n in 1:(length(data_list))){
  participant = participant + 1
  cond_list = strsplit(list_files[n], "_")
  condition = paste(cond_list[[1]][2], "_", cond_list[[1]][3], sep="")
  data=rbind(data,cbind(participant=participant, condition=condition, data_list[[n]]))
}

head(data)
glimpse(data)

data = tibble(data)
### Task 1
#column changes 
data_clean = data %>% 
  rename(
    block_number = blkNum,
    trial_number = trlNum,
    percent_coherence =percentCoherence,
    dots_direction = winningDirection,
    accuracy = correct
  ) %>%
  mutate(
    rt = RT/1000,
    condition = recode(condition,
                       Norm_Trial = "FixedTrial_LowInformation" ,
                       Info_Trial = "FixedTrial_MediumInformation" ,
                       Optim_Trial = "FixedTrial_HighInformation" ,
                       Norm_Time = "FixedTime_LowInformation" ,
                       Info_Time = "FixedTime_MediumInformation" ,
                       Optim_Time= "FixedTime_HighInformation"
    )) %>%
  select (!c(numberofDots, coherentDots, eventCount, averageFrameRate))

view(data_clean)

###Task 2
###How many participants are in the dataset?
n_distinct(data_clean$participant)
n_distinct(data$participant) #to compare

#85 in total

### How many participants were there in each condition?
summarize(group_by(data_clean,condition),
          n_distinct (participant))

### Did the participants in the fixed time conditions perform more or less trials than the participants in the fixed trials conditions?
#add block_type and feedback_type 
data_clean = separate(data_clean, col = condition, into = c("block_type", "feedback_type"), sep = "_")

data_clean$condition = data$condition

view(data_clean)

summarize(n=n(), group_by(data_clean,participant, block_type))

summarize(group_by(data_clean, block_type), n_distinct (trial_number))

#FixedTime                          64
#FixedTrial                         40
# the participants in the fixed time conditions performed more trials than the participants in the fixed trails conditions. 



##Calculate a summary, which includes the average and SD of accuracy and response time per condition (which are 6 in total) and use this to describe the overall performance across participants in these conditions (i.e., which conditions produced hiigher accuracy, which produced faster responses? Do accuracy and speed always trade off?).
data_clean %>% 
  group_by(condition) %>% 
  summarise (meanrt= mean(rt),
             meanaccuracy = mean(accuracy))

###highest accuracy Info_Time
###fastest response Info_Trial
###no perfect trade-off 


##Calculate a summary, which includes the average accuracy and average response time per participant, as well as the percentage of trials below 150 ms (too fast trials) and above 5000 ms (too slow trials). Are there any participants with more then 10% fast or slow trials?
data_clean <- data_clean %>% mutate(slow_trials = case_when(rt >= 5 ~ 1,
                                                            rt < 5 ~ 0))

data_clean <- data_clean %>% mutate(fast_trials = case_when(rt <= 0.15 ~ 1,
                                                            rt > 0.15 ~ 0))

data_clean <- data_clean %>% 
  mutate(meanslowtrials= mean(slow_trials)*100,
         meanfasttrials = mean(fast_trials)*100,
         mean_ACC_p = mean(accuracy))

view(data_clean)


###TASK 3:  Exclude participants

participants_to_exclude = filter(data_clean, data_clean$mean_ACC_p <.6)

for (n in 1:length(participants_to_exclude)) {
  data_clean = data_clean %>% 
    filter(participant != participants_to_exclude[n])
}


#TASK 4: Data visualization
# Plots  

#data_clean %>% 
  #group_by(condition, block_number) %>% 
  #summarize (mean_rt= mean(rt),
   #          mean_accuracy_block_condition = mean_ACC_p)


#plot 1
ggplot(data = data_clean, mapping = aes(x = data_clean$condition, y = data_clean$mean_ACC_p, color=condition)) + 
  geom_line()

#plot 2
ggplot(data = data_clean, mapping = aes(x = data_clean$condition, y = data_clean$rt, color=condition)) + 
  geom_line()

data_clean$mean_accuracy_block_condition = mean(data_clean$accuracy)
data_clean$mean_rt = mean(data_clean$rt)

# point plots accuracy

ggplot(data = data_clean, mapping = aes(x = data_clean$condition, y = mean_accuracy_block_condition)) +
  
  stat_summary(fun = "mean", geom="point") +
  
  stat_summary(fun.data = mean_se, geom = "errorbar", size=.8, width=.4) +
  
  labs(x = '6 Conditions', y = 'Mean Accuracy')


# point plots response time 


ggplot(data = data_clean, mapping = aes(x = condition, y = mean_rt)) +
  
  stat_summary(fun = "mean", geom="point") +
  
  stat_summary(fun.data = mean_se, geom = "errorbar", size=.8, width=.4) +
  
  labs(x = '6 Conditions', y = 'Mean Response Time')




####Part 2

## 2.1 ##

#1: ANOVA


aggregate_data <-  data_clean %>% 
  group_by(participant, block_number, block_type, feedback_type) %>% 
  summarise(mean_rt= mean(rt),
            mean_accuracy = mean(accuracy))


model_fit_anova = aov(mean_rt ~ block_number + block_type + feedback_type, 
                      data = aggregate_data)
summary(model_fit_anova)

#Interpretation: all three (block_number, block_type and feedback_type) had a signficant effect on the mean response time per participant 
# from anova alone it is unclear how each condition influences the mean rt 




#2 ANOVA
model_fit_anova = aov(mean_accuracy ~ block_number + block_type + feedback_type, 
                      data = aggregate_data)
summary(model_fit_anova)



##2.2 FULL MODEL 

controlmodel <- data_clean %>%
    glmerControl(rt ~ block_number + (block_number | participant),
    rt ~ block_type + (block_type | participant),
    rt ~ feedback_type + (feedback_type | participant))
summary(controlmodel)

#multilevel regression
fullmodel  <- lm(rt ~ block_number + block_type + feedback_type, data = data_clean)

summary(fullmodel)

modelnumber  <- lm(rt ~ block_number, data = data_clean)
modelblock <- lm(rt ~ block_type, data = data_clean)
modelfeedback <- lm(rt ~ feedback_type, data = data_clean)

summary(modelnumber) 
summary(modelblock)
summary(modelfeedback)
#all significant
