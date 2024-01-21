#---------------------------------------------------------#
# Syed Faizan                                             #
# 2024-01-20                                              #
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
library(dplyr)
library(Hmisc)
library(mice)



# Loading the data sets
data_2015 <- read.csv("2015.csv", header=TRUE) 
baseball <- read.csv("baseball.csv", header=TRUE)



# 1. Read the data set 2015.csv 
head(data_2015)                    # We may use
# str(), class(), names() to initially read the data set

# 2. Get column names
names(data_2015)

# 3. View the data set
View(data_2015)

# 4. Glimpse the data set
glimpse(data_2015)

# 5. Install and load janitor package for clean_names
library(janitor)
data_2015 <- clean_names(data_2015)
data_2015

# 6. Select columns from the data set
happy_df <- select(data_2015, country, region, happiness_score, freedom)
happy_df

# 7. Slice the first 10 rows
top_ten_df <- slice(happy_df, 1:10)
top_ten_df

# 8. Filter for freedom values under 0.20
no_freedom_df <- filter(happy_df, freedom < 0.20)
no_freedom_df

# 9. Arrange in descending order by freedom values
best_freedom_df <- arrange(happy_df, desc(freedom))
best_freedom_df

# 10. Create a new column gff_stat
data_2015$gff_stat <- data_2015$family + data_2015$freedom + data_2015$generosity
data_2015

# 11. Group by region and calculate summary stats
regional_stats_df <- happy_df %>%       # NOTE:I have used the %>% (pipe) operator to string operators 
  group_by(region) %>%
  summarise(
    country_count = n(),
    mean_happiness = mean(happiness_score),
    mean_freedom = mean(freedom)
  )
regional_stats_df

# Print the resulting data frames
print(top_ten_df)
print(no_freedom_df)
print(best_freedom_df)
print(data_2015)
print(regional_stats_df)


#Assignment 2 

# Problem 12: Download and read the dataset
baseball <- read.csv("baseball.csv") 

# Problem 13: Explore the data
# I used various exploration functions like str(), summary(), head(), tail(), dim(), 
# to understand and carry out a preliminary exploration of the dataset

str(baseball)
summary(baseball)
head(baseball)
tail(baseball)
dim(baseball)

# Problem 14: Remove players with 0 at bats
baseball <- baseball[baseball$AB > 0, ]
baseball

# I used dim(baseball) to determine that the number of observations reduced from 771 to 726 
# implying 45 players with 0 at bats were removed. 

# Problem 15: Add a new column for batting average (BA)
baseball$BA <- baseball$H / baseball$AB
baseball 

# Problem 16: Create a column for on-base percentage (OBP)
baseball$OBP <- (baseball$H + baseball$BB) / (baseball$AB + baseball$BB)
baseball

# Problem 17: Determine the 10 players who struck out the most
strikeout_artists <- head(baseball[order(baseball$SO, decreasing = TRUE), ], 10)
strikeout_artists


# Problem 18: To be eligible for end-of-season awards, a player must have either at 
# least 300 at bats or appear in at least 100 games. Keep only the players who
# are eligible to be considered and store them in a variable called eligible_df.

eligible_df <- baseball[baseball$AB >= 300 | baseball$G >= 100, ]
eligible_df

# Problem 19: Create a histogram of batting average for eligible players
pdf("eligible_players_histogram.pdf")
hist(eligible_df$BA, main = "Batting Average Histogram", xlim=c(0.1 , 0.4), xlab = "Batting Average", ylab= "Count", col = "green")
dev.off()

# I preferred a PDF file of the Histogram as it would aid inclusion in the report. 

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