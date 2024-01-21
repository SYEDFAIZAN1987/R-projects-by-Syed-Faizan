# Syed Faizan 
# 1/12/2024 
# ALY6000 Introduction to Analytics 
# Instructor - Dr. Maria Ayala


rm(list = ls()) # starting with a clear environment 

#Problem1
answer1 <- 123 * 453
answer2 <- 5^2 * 40
answer3 <- TRUE & FALSE
answer4 <- TRUE | FALSE
answer5 <- 75 %% 10
answer6 <- 75 / 10

#Problem2
first_vector <- c(17, 12, -33, 5)

# Problem 3
counting_by_fives <- c(5, 10, 15, 20, 25, 30, 35)

# Problem 4
second_vector <- 20:1

# Problem 5
counting_vector <- 5:15

# Problem 6
grades <- c(96, 100, 85, 92, 81, 72)

# Problem 7
bonus_points_added <- grades + 3

# Problem 8
one_to_one_hundred <- 1:100

# Problem 9
# in result7 20 is added to each number of the numeric class vector named second_vector 
result7 <- second_vector + 20
# in result8 20 is multiplied to each number of the numeric class vector named second_vector
result8 <- second_vector * 20
# in result9 we are checking if the value of a variable in the vector second_vector 
# is greater than or equal to 20, and a logic value which is either TRUE or FALSE 
# is returned based on the comparison. 
result9 <- second_vector >= 20
# in result10 we are checking if the value of a variable in the vector second_vector  
# is equal to 20 or not, and a logic value which is TRUE is returned if the 
# value is not equal to 20 and FALSE 
# is returned if the value is equal to 20. 
result10 <- second_vector != 20

# Problem 10
total <- sum(one_to_one_hundred)

# Problem 11
average_value <- mean(one_to_one_hundred)

# Problem 12
median_value <- median(one_to_one_hundred)

# Problem 13
max_value <- max(one_to_one_hundred)

# Problem 14
min_value <- min(one_to_one_hundred)

# Problem 15
first_value <- second_vector[1]

# Problem 16
first_three_values <- second_vector[1:3]

# Problem 17
vector_from_brackets <- second_vector[c(1, 5, 10, 11)]

# Problem 18
vector_from_boolean_brackets <- first_vector[c(FALSE, TRUE, FALSE, TRUE)]
# In the above problem the  new vector vector_from_boolean_brackets contains only
# the elements of first_vector where the corresponding element is TRUE.
# That is, it selects the elements in first_vector at positions where the logic value
# is TRUE i.e. positions 2 and 4) giving the values 12 and 5.  
# This is a way to selectively extract elements from a vector 
# based on a logical condition.


# Problem 19
second_vector >= 10
# This code checks which elements in second_vector are greater than or equal to 10.

# Problem 20
one_to_one_hundred[one_to_one_hundred >= 20]
# This code extracts elements from one_to_one_hundred that are greater than or equal to 20.

# Problem 21
lowest_grades_removed <- grades[grades > 85]

# Problem 22
middle_grades_removed <- grades[c(-3, -4)]
# A vector of negative indexes has been used to remove the 3rd and 4th elements.

# Problem 23
fifth_vector <- second_vector[-c(5, 10)]

# Problem 24
set.seed(5)
random_vector <- runif(n=10, min=0, max=1000)

# Problem 25
sum_vector <- sum(random_vector)
# In the above problem 25 the sum will vary every time unless the seed is set at
# a single number , in this case 5. 

# Problem 26
cumsum_vector <- cumsum(random_vector)

# Problem 27
mean_vector <- mean(random_vector)

# Problem 28
sd_vector <- sd(random_vector)

# Problem 29
round_vector <- round(random_vector)

# Problem 30
sort_vector <- sort(random_vector)

#Problem 31 
# The data file ds_salaries.csv has been saved in the same directory as the project

#Problem 32
first_dataframe <- read.csv("ds_salaries.csv")

#Problem 33
summary_statistics <- summary(first_dataframe)
print(summary_statistics)
