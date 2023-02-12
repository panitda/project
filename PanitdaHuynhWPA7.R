# Assignment: WPA 7
# Name: Panitda Huynh
# Date: 26 April 2022

install.packages("tidyverse")

###Task A
library(tidyverse)
leisure = read.delim('~/OneDrive/Studium/Master FS 2022/Programming in R/Homework/WPA3/WPA7/raw/data_wpa7.txt', header = TRUE)

?read.delim

#A2 - i am having issues creating the second component for food argument to be flexible...

food = "apples"


feed_me = function(food) {
  output = print(paste("I love to eat", food))
}

feed_me("apples")

#A3 understood the task but the not sure about the formula to calc mean of vector - the command is working and syntax should be right

vec_1 = seq(1, 100, 5)
print(vec_1)

mean(vec_1)

vec_1_mean = function(x, y, z) {
  mean = ((x:y)/z)*2
  
  return(mean)
}

vec_1_mean(1, 100, 5)

#A4 / same issue as in A4
my_mean = function(x) {
    mean = mean(x)
    
    return(mean)
    }

my_mean(50)


#A5
my_mean(leisure$logic) == mean(leisure$logic)

#A6
a = seq(1, 10)

for (i in a) {
  print(i^2)
}

#A7
squares = c(1:10)

for (i in a) {
  squares=(i^2)
}

print(squares)

#Task B 1 & 2

standardize = function(b) {
  ztrans = b-(mean(b))/sd(b)
  print(ztrans)
}

z_leisure = standardize(leisure$logic, leisure$reflex, leisure$age, leisure$comprehension)
z_leisure = standardize(leisure$logic)

columns_to_standardize = data.frame(leisure$logic, leisure$reflex, leisure$age, leisure$comprehension)
                                    
print(columns_to_standardize)

for (b in columns_to_standardize) {
  ztrans = b-(mean(b))/sd(b)
  print(ztrans)
}

#TASK C
#1
library(ggplot2)
ggplot(data = leisure, mapping = aes(x = age, y = reflex)) + 
  geom_point(alpha = .3, size= 1) +
  geom_vline(xintercept = mean(leisure$age), linetype="dotted") +
  geom_hline(yintercept = mean(leisure$reflex), linetype="dotted") +
  geom_smooth(method = lm) +
  labs(x='age', y='refelx') +
  ggtitle("Relationship between age and reflex")

#2
my_plot = function(a, b, data) 
  {
  scatter = ggplot(data, aes(x=a, y=b)) + geom_point() +
    geom_smooth(method=lm, se=FALSE)
  print(scatter)}

#3

my_plot(leisure$age, leisure$reflex, leisure)

#TASK D
#1
e = seq(1:10)
for (i in e) {
  print(i)
}

#2
my_sum = function(e) {
  for (i in e) {
    print(i)
  }}

my_sum(leisure$logic)

#3
my_mean2 = function(e) {
  for (i in e) {
    i = mean(e)
    print(i)
  }}

my_mean2(leisure$logic)

my_mean2(leisure$logic) == my_mean(leisure$logic)


