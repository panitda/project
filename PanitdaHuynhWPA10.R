# Assignment: WPA 10
# Name: Panitda Huynh
# Date: 17 May 2022

library(tidyverse)
library(readxl)
library(lme4)
library(haven)

data = read_excel('~/Library/CloudStorage/OneDrive-Personal/Studium/Master FS 2022/Programming in R/Homework/WPA10/Bressoux Data AnPsycho.xls')

glimpse(data)

###improvement_math = MATH4 and MATH3

improvement_math = lm(MATH4 ~ MATH3, data = data)

glimpse(improvement_math)

ggplot(data = data, mapping = aes(x = MATH3, y = MATH4)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Beginning and end of the year math scores")

lm(MATH4 ~ MATH3, data = data)

randintmodel = lmer(MATH4 ~ MATH3 + (1 | ECOLE2), data = data)

summary(randintmodel)

lm(MATH4 ~ 0 + MATH3, data = data)

randslopemodel = lmer(MATH4 ~ MATH3 + (0 + MATH3 | ECOLE2), data = data)

summary(randslopemodel)

randintslopemodel = lmer(MATH4 ~ MATH3 + (MATH3 | ECOLE2), data = data)

summary(randintslopemodel)
###improvement_french which is the difference between FRAN4 and FRAN3.

improvement_french = lm(FRAN4 ~ FRAN3, data = data)
glimpse(improvement_french)

ggplot(data = data, mapping = aes(x = FRAN4, y = FRAN3)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  ggtitle("Beginning and end of the year french scores")

randintmodel2 = lmer(FRAN4 ~ FRAN3 + (1 | ECOLE2), data = data)

summary(randintmodel2)

lm(FRAN4 ~ 0 + FRAN3, data = data)

randslopemodel2 = lmer(FRAN4 ~ FRAN3 + (0 + FRAN3 | ECOLE2), data = data)

summary(randslopemodel2)

randintslopemodel2 = lmer(FRAN4 ~ FRAN3 + (FRAN3 | ECOLE2), data = data)

summary(randintslopemodel2)

