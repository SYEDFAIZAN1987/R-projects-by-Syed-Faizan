#---------------------------------------------------------#
# Syed Faizan                                             #
# 2024-01-21                                              #
# ALY6000 Introduction to Analytics                       #
# Instructor - Dr. Maria Ayala                            #
#                                                         #
#                                                         #
#                                                         #
#                                                         #
#---------------------------------------------------------#

#Assignment 1 

rm(list = ls()) # starting with a clear environment 

library(tidyverse) #loading packages used
library(janitor)
install.packages("diplyr")
library(dplyr)
library(Hmisc)
library(mice)




# Loading the data sets
data_2015 <- read.csv("2015.csv", header=TRUE) 
baseball <- read.csv("baseball.csv", header=TRUE)

#Assignment_Part-1
#Question-1
data_2015 <- read.csv("2015.csv")
head(data_2015)

#Question-2
names(data_2015)

#Question-3
view(data_2015)

#Question-4
glimpse(data_2015)

#Question-5
library(janitor)
data_2015 <- clean_names(data_2015)
data_2015

#Question-6
happy_df <- data_2015[c('country','region','happiness_score','freedom')]
happy_df

#Question-7
top_ten_df <- head(happy_df,10)
top_ten_df

#Question-8
no_freedom_df<- subset(happy_df, freedom < 0.20)
no_freedom_df

#Question-9
best_freedom_df <- happy_df %>% arrange(desc(freedom))
best_freedom_df

#Question-10
data_2015$gff_stat <- data_2015$family + data_2015$freedom + data_2015$generosity
data_2015

#Question-11
regional_stats_df<- happy_df %>% group_by(region) %>% summarise(country_count=n(),mean_happiness=mean(happiness_score),mean_freedom=mean(freedom))
regional_stats_df

#Assignment_Part-2
#Question-12
baseball<-read.csv("baseball.csv")
baseball

#Question-14
baseball<-subset(baseball, AB>0)
baseball

#Question-15
baseball$BA <- baseball$H/baseball$AB
baseball

#Question-16
baseball$OBP <- (baseball$H+baseball$BB)/(baseball$AB+baseball$BB)
baseball

#Question-17
strikeout_artist <- baseball%>%arrange(desc(SO))
strikeout_artist<-head(strikeout_artist,10)
strikeout_artist

#Question-18
eligible_df <- subset(baseball,AB>=300|G>=100)
eligible_df

#Question-19
hist(eligible_df$BA, xlab="Batting average(BA)",ylab="Frequency",main="Histogram of BA", col="green")




# Problem 20: Data Analysis report and MVP recommendation

baseball <- read.csv("baseball.csv", header=TRUE) # I restored the dataset to its original form
# of 771 obs of 16 variables in order to analyse it afresh.

baseball$BA <- baseball$H / baseball$AB                      #recreated two new variables "batting avg" and
#"on-base percentage" to facilitate analysis.

baseball$OBP <- (baseball$H + baseball$BB) / (baseball$AB + baseball$BB)

dim(baseball)
str(baseball)
summary(baseball)
head(baseball)
tail(baseball)
class(baseball)

# I Analyzed the dataset to get a rough idea of it's contents.

complete.cases(baseball)
na_count <- colSums(is.na(baseball))
print(na_count)

# Determined the complete cases and whether values were missing.


md.pattern(baseball)

#generated a missing data pattern plot showing only Batting avg and ob-base percentage
# are undefined in 45 cases owing to O at bats .

baseball1 <- baseball[baseball$AB > 0, ]
baseball1


# Created a new dataset namely "baseball1" with all the players with 0 at bats removed.

#summarizing of the key statistics as follows

summary(baseball1[, c("BA", "HR", "RBI", "OBP")])

#created 4 barplots to visualize summary

# Sample data
summary_data <- data.frame(
  Metric = c("Batting Average", "Home Runs", "RBIs", "OBP"),
  Minimum = c(0, 0, 0, 0),
  FirstQuartile = c(0.1604, 0, 1, 0.2033),
  Median = c(0.2347, 1, 11, 0.3000),
  Mean = c(0.2088, 5.252, 23.96, 0.2672),
  ThirdQuartile = c(0.2689, 7.750, 41.75, 0.3396),
  Maximum = c(1, 40, 121, 1)
)

print(summary_data)

# Analyzed the ten best players based on key statistics (e.g., OBP, HR, RBI)


# Converting character columns to numeric
numeric_columns <- c("BA", "OBP", "HR", "RBI")
baseball1[, numeric_columns] <- apply(baseball1[, numeric_columns], 2, as.numeric)


# Function to calculate a composite score for each player
calculate_composite_score <- function(data) {
  composite_score <- 0.4 * data$BA + 0.3 * data$OBP +
    0.2 * data$HR + 0.1 * data$RBI
  return(composite_score)
}

# Calculated the composite score and added it to the data frame
baseball1$CompositeScore <- calculate_composite_score(baseball1)

# Ordered the data frame by the composite score in descending order
sorted_data <- baseball1[order(-baseball1$CompositeScore), ]

# Selected the top 10 players
top_10_players <- head(sorted_data, 10)

# Printed the top 10 players
print(top_10_players)

#used similar data to arrive at a candidate for MVP

MVP <- head(sorted_data, 1)

print(MVP)
# Noted the Data for Mike Schmidt

mike_schmidt <- data.frame(
  Category = c("Batting Average", "On-Base Percentage", "Composite Score"),
  Value = c(0.2898551, 0.3884555, 19.53248)
)


#Visualizing the Data
hist(baseball1$Age, main = "Age Distribution of Players", xlab = "Age", col = "lightblue", border = "black")

plot(baseball1$BA, baseball1$HR, main = "Batting Average vs. Home Runs", xlab = "Batting Average", ylab = "Home Runs", col = "blue", pch = 16)

boxplot(baseball1$RBI, main = "Distribution of RBIs", ylab = "RBIs", col = "lightgreen", border = "black")


# Provided a concise executive summary adhering to APA guidelines