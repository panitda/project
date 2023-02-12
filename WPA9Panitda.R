# Assignment: WPA 9
# Name: Panitda Huynh
# Date: 14 May 2022

library(tidyverse)
library(ISLR)
library(car)


stumath = read.csv('~/Library/CloudStorage/OneDrive-Personal/Studium/Master FS 2022/Programming in R/Homework/WPA9/data/student-mat.csv', sep = ';')

stupor = read.csv('~/Library/CloudStorage/OneDrive-Personal/Studium/Master FS 2022/Programming in R/Homework/WPA9/data/student-por.csv', sep = ';')

#TASK A#
#1
glimpse(stumath)

model_fit_1 = lm(G3 ~ sex + age + internet, 
               data = stumath)
summary(model_fit_1)
#2 age

#3
glimpse(stupor)

#4
model_fit_2 = lm(G3 ~ sex + age + internet, 
                 data = stupor)
summary(model_fit_2)

#all variables in second model

#5 significant in second model

#TASK B#
#1
model_fit_3 = lm(G2 ~ guardian, 
                 data = stumath)
summary(model_fit_3)
#2 intercept is significant ... so one is better than the other 

#3
meansmath = mean = stumath$guardian
meanspor = mean = stupor$guardian

result = t.test(meansmath, meanspor,
                var.equal = TRUE)
#wait there is no grade ... how?

#TASK C#
model_fit_4= lm(G1 ~ ., 
                data = stumath)
model_4_fitted = filter(stumath, G1 ~.)

arrange(summarize(group_by(stumath, G1 ~ .),
                  meansmath = mean(SalePrice), mean_meanspor))

ggplot(data = stumath, mapping = aes(x = G1, y = age)) + 
  geom_point(alpha = .3, size= 1) +
  geom_vline(xintercept = mean(stumath$G1), linetype="dotted") +
  geom_hline(yintercept = mean(stumath$age), linetype="dotted") +
  geom_smooth(method = lm) +
  labs(x='grade', y='How worried of GW are you?') +
  ggtitle("Relationship between grade 1 and all variables")
        
model_fit_5 = lm(y ~ . - G2 - G3, data=stumath)

model_fit_5 = lm(y ~ . -G3, data=stumath)

model_5_fitted = lm(G3 ~ y ~, data = stumath) #???? error

ggplot(data = stumath, mapping = aes(x = stumath$age, y = stumath$G3)) + 
  geom_point(alpha = 0.8, size= 1) +
  geom_smooth(method = lm, color='red')

### TASK D###
shapiro.test(filter(stumath, Foundation == "G1")$stumath)

shapiro.test(filter(stumath, Foundation == "G2")$stumath)

shapiro.test(filter(stumath, Foundation == "G3")$stumath)

model_fit1 = aov(age ~ G1, 
                data = stumath)
model_fit2 = aov(age ~ G2, 
                data = stumath)

model_fit3 = aov(age ~ G3, 
                 data = stumath)
summary(model_fit1)
summary(model_fit2)
summary(model_fit3)

TukeyHSD(model_fit3)

