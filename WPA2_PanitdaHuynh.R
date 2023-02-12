#WPA 2 22.03.2022, Basic R

#df [ row, column]

### TASK A ###
###Create a numeric vector called participants, containing integer numbers from 1 to 20, using c() and seq()

participants <- c(1:20)
participants <- seq(from=1, to=20)

###Create a character vector called conditions, of length 20, containing alternating values of “a” and “b” (“a”, “b”, “a”, “b”, “a”, …), using rep().
conditions = rep(c("a","b"), times=10)
print(conditions)

participants %>% replace_with_na(replace = list(x = 5))

###Create a vector called first_half, containing only the first half of the participants vector’s values.
first_half <- participants[1:10]

###Check that both participants and conditions have length 20, and that first_half has length 10, using length().
print(participants)
print(conditions)
print(first_half)

###Instead of the fifth element in conditions, insert a missing value. Print the conditions after the change.
conditions [5] <- NA

###Create a vector called participants_cond, by pasting together (using paste()) participants and conditions, separated by _. So, the first element should be “1_a”.
participants_cond <- paste(participants, conditions, sep = "_")
                            
###Check that the 5th element of participants_cond is still a missing value.
print(participants_cond) ###does not work

### TASK B ###
#Create a data frame called my_data, with as columns the vectorsparticipants, conditions and participants_cond that you created before. Print the data frame to have a look if it worked.
my_data <- data.frame(participants,conditions, participants_cond)

#add a column called response_times made of 20 samples from the normal distribution, with mean .8 and standard deviation 1. Print the data frame to have a look if it worked.
my_data$response_times <- rnorm(n=20, mean =0.8, sd=1)
print(my_data) 


#Select the values of the response_times column that are negative and set them to 0. Print the data frame to have a look if it worked
#my A: my_data$response_times[my_data$response_times<0] <- 0
my_data$response_times[my_data$response_times<0] <- 0
print(my_data)
#fourth column [] when put in without comma

#Create a new column, called log_response_times, made of the logarithm of response_times.
my_data$log_response_times <- log(my_data$response_times)
print(my_data)

#add a column called correct_response made of 20 samples from the binomial distribution, with size 1 and probability of success .65. Print the data frame to have a look if it worked.
my_data$correct_response <- rbinom(n=20, size=1, prob = .65)
print(my_data)

#note: binom size meaning the amount of times of successes e.g. when flipping a coin here size 1 --> only 1 chance success vs loss

#Calculate the mean proportion of correct responses and the mean response time.
mean(my_data$correct_response)
mean(my_data$response_times)

#Create two data frames, data_correct and data_incorrect made of, respectively, the subset of my_data where correct_response is 1, and the subset of my_data where correct_response is 0. Print the data frame to have a look if it worked. Print the result to check
#my A:data_correct <-  subset(my_data, correct_response ==1)
data_correct = my_data[my_data$correct_response == 1,]
print(data_correct)

#my A: data_incorrect <- subset(my_data, correct_response ==0)
data_incorrect = my_data[my_data$correct_response == 0,]
print(data_incorrect)

#note: [row,column] --> so [] without comma is all column and if [x,] means still all column taken
#see: https://dzone.com/articles/learn-r-how-extract-rows