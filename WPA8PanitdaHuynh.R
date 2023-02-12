# Assignment: WPA 8
# Name: Panitda Huynh
# Date: 6 May 2022

#install.packages('ISLR')
library(tidyverse)
library(ISLR)

glimpse(Smarket)

#Task A
student_math <- read.csv('~/Library/CloudStorage/OneDrive-Personal/Studium/Master FS 2022/Programming in R/Homework/WPA8/student/student-mat.csv', sep = ';')

ggplot(data = student_math, mapping = aes(x = age, y = G1)) + 
  geom_point(alpha = 0.8, size= 1) +
  geom_smooth(method = lm, color='red')

model_fit_math_1 = lm(age ~ G1, data = student_math)

summary(model_fit_math_1)

#p-value: 0.2038 - negative correlation, result does not reflect a genuine effect.

confint(object = model_fit_math_1, level = .95)


ggplot(data = student_math, mapping = aes(x = absences, y = G1)) + 
  geom_point(alpha = 0.8, size= 1) +
  geom_smooth(method = lm, color='red')

model_fit_math_2 <- lm(absences ~ G1, data = student_math)

summary(model_fit_math_2)

confint(object = model_fit_math_2, level = .95)

#p-value: 0.539, negative correlation, result does not reflect a genuine effect.

#change into numeral ? 
student_math$schoolsup = ifelse(student_math$schoolsup=="Yes",1,0)

ggplot(data = student_math$schoolsup, mapping = aes(x = schoolsup, y = G1)) + 
  geom_point(alpha = 0.8, size= 1) +
  geom_smooth(method = lm, color='red')

model_fit_math_3 <- lm(schoolsup ~ G1, data = student_math)

summary(model_fit_math_3)
#no p-value given despite numeral ... how to deal with NA double? (error says error in double.)

test3 = ggplot(data = student_math, mapping = aes(x = schoolsup, y = G1)) + 
  geom_point(alpha = 0.8, size= 1) +
  geom_smooth(method = lm, color='red')
test2model <- lm(schoolsup ~ G1, data = student_math)

summary(test2model)
#still NA ??

res= residuals(object = student_math)

ggplot(data = student_math, mapping = aes(x = G1, y='schoolsup')) + 
  geom_histogram(aes(y=schoolsup), binwidth=.1, colour="darkgrey", fill="white") + # Note: add aes(y=..density..) to have density instead of frequencies
  labs(x = 'G1', y='school support') + 
  geom_density(alpha=.2, fill="red", colour="darkgrey") +   # Overlay with transparent density plot
geom_vline(aes(xintercept=mean(G1)), color="red", linetype="dashed", size=.5) # Add mean residuals

#5) depending on the statistical position (whether it is on the right or left it shows the direction in the graph - and on its p-value)

ggplot(student_math, mapping = aes(sample = student_math$G1)) +
  stat_qq() + 
  stat_qq_line()

ggplot(student_math, mapping = aes(sample = student_math$schoolsup)) +
  stat_qq() + 
  stat_qq_line()

ggplot(data = student_math, mapping = aes(x = G1, y = schoolsup)) + 
  geom_point(alpha = 0.6, size= 2) + 
  geom_hline(yintercept=0)

#6) i am not sure as the regression is stagnant..
