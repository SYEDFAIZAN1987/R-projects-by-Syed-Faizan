#---------------------------------------------------------#
# Syed Faizan                                             #
# 2024-02-01                                              #
# ALY6000 Introduction to Analytics - Module 4 Project    #
# Instructor - Dr. Maria Ayala                            #
#                                                         #
#                                                         #
#                                                         #
#                                                         #
#---------------------------------------------------------#

#Starting with a clean environment

rm(list=ls())

#Loading the packages utilized for Data cleaning and Data Analysis and benchmarking


library(tidyverse)
library(rlang)
library(stringr)
library(car)
library(tm)
library(SnowballC)
library(wordcloud)
library(benchmarkme)


#Loading the Dataset - The Dataset being a survey has 4 components
# Namely- 
# 1.The raw Multiple choice Data, 
# 2.The raw Free Form Data
# 3.The questionnaire itself 
# 4.Currency conversion rates 

#Importing 1 #All character responses are converted to factors owing to the nature of 
# multiple choice questions 
rawMC <- read.csv('multiplechoiceResponses.csv', stringsAsFactors = TRUE, header = TRUE)

# Importing 2
rawFF <- read.csv('freeformResponses.csv', stringsAsFactors = FALSE, header = TRUE)

# Importing 3
schema <- read.csv('schema.csv', stringsAsFactors = FALSE, header = TRUE)

#Importing 4
conR <- read.csv('conversionRates.csv', header = TRUE)
#Preliminary examination of Dataset
summary(rawMC)

#Examining Age closely and basic descriptive statistics

Age_sd <- cleanMC %>% 
  filter( !is.na(cleanMC$Age) & !Age == " " ) %>% 
  select(Age)
sd(Age_sd$Age)


# View the cleaned data
head(cleaned_data)
nrow(rawMC)
ncol(rawMC)

#Cleaning of Dataset 
# To ensure safe preservation of raw data duplicate versions of clean and raw data created

cleanMC <- rawMC
cleanFF <- rawFF

#Prior to an analysis of the Data set two functions are created
#to better analyse the multiple choice questions with only
# one option allowed versus multiple answers allowed


library(dplyr)

#the function for the single answer MCQ's shall be named 'onechoice'
# all no responses shall be filtered in the Data set


onechoice <- function(question, filteredMC = cleanMC ) {
  
  filteredMC %>% 
    filter(!UQ(sym(question)) == "") %>% 
    # Group by the responses to the question
    group_by_(question) %>% 
    # Count how many respondents selected each option
    summarise(count = n()) %>% 
    # Calculate what percent of respondents selected each option
    mutate(percent = (count / sum(count)) * 100) %>% 
    # Arrange the counts in descending order
    arrange(desc(count))
  
}


multiplechoice <- function(question, filteredMC = cleanMC){
  
  filteredMC %>% 
    # Remove non-respondent rows
    filter(!UQ(sym(question)) == "") %>%
    # Remove all columns except question
    select(question) %>% 
    # Add a column with the initial number of respondents to question
    mutate(totalCount = n()) %>% 
    # Split multiple answers apart at the comma, but ignore commas inside parentheses
    mutate(selections = strsplit(as.character(UQ(sym(question))), 
                                 '\\([^)]+,(*SKIP)(*FAIL)|,\\s*', perl = TRUE)) %>%
    #  unnest the selections
    unnest(selections) %>% 
    # Group by the selected responses to the question
    group_by(selections) %>% 
    # Count how many respondents selected each option
    summarise(totalCount = max(totalCount),
              count = n()) %>% 
    # Calculate what percent of respondents selected each option
    mutate(percent = (count / totalCount) * 100) %>% 
    # Arrange the counts in descending order
    arrange(desc(count))
  
}

    # Data Analysis

    1. #Gender composition of Dataset 
    
   Gender <- onechoice("GenderSelect")
   
    #Creating a Dataframe to house Gender Data
   Genders <- data.frame(
     category = c("Non-binary, genderqueer, or gender non-conforming", "Male", "Female", "A different identity"),
    
     percent = c(0.4452199, 81.8843632, 16.7137958, 0.9566211)
   )
   
    #Visualization 1
    #Pie Chart of Gender Distribution 
     
   library(ggplot2)
   
   # Create a pie chart
   ggplot(Genders, aes(x = "", y = percent, fill = category)) +
     geom_bar(stat = "identity", width = 1, color = "red") +
     coord_polar("y", start = 0) +
     theme_void() +
     ggtitle("Distribution of Gender")
   
   
   2. # National origin composition of Dataset , for visual clarity only the 15 largest
      # represented nations were chosen
   
      Country <- onechoice("Country")
      
     
      library(dplyr)
      
      # Arranging the data frame by the "count" column in descending order
      Country <- Country %>% arrange(desc(count))
      
      # Selecting the first 15 rows
      National_origin <- Country %>% slice_head(n = 15)
      
      # Visualization of National_origin
      ggplot(National_origin, aes(x = Country, y = count)) + 
        geom_bar(stat = "identity", color = 'yellow', fill = 'red' ) +
        theme(axis.text.x = element_text(angle = 90, 
                                         vjust = 0.5, 
                                         hjust = 1)) + ggtitle("National Origin")
      
      
   3.# Age distribution of participants 
   
     # First we determine the nature and range of the Age entries in the survey to check 
     # for whether cleaning Data is required.
      
        Range_of_Ages <- range(rawMC$Age)
        print(Range_of_Ages)
        
        # This shows the column needs cleaning 
        # It is first coerced to a numeric
        
        cleanMC$Age <- as.numeric(as.character(cleanMC$Age))
        
        age <- onechoice("Age") %>% 
        # Remove values < 10 years and greater than 90 years since it is safe to assume this range
          filter(!Age < 10 & !Age > 90)
        
        ageclean <- cleanMC %>% 
        # Remove any rows where the respondent didn't answer the question
          filter(!Age == "") %>% 
          select(Age)
        
        ggplot(ageclean, aes(x = Age)) + 
          geom_histogram(binwidth = 2, color = 'blue', fill = 'orange') + 
          xlab( "Age (years)" ) + 
          ylab("Number of Respondents" )
        
        #Examining age in a box plot and with respect to Country and Gender
        ageclean %>% 
          summarise(median = median(Age, na.rm = TRUE), sd = sd(Age, na.rm = TRUE))
        
        ggplot(ageclean, aes(x = "", y = Age)) +
          geom_boxplot( color = 'purple' ) +
          ggtitle("Box Plot of Age") + theme_light()
        
        
        #How does the age relate to the top 6 Countries of Origin and Gender
        
          
          top6 <- Country %>% 
          # add a row number to each row
          mutate(row = row_number()) %>% 
          # select only the top 5 countries
          filter(row <= 6) %>% 
          # keep only the country name column
          select(Country) %>% 
          # change these to character elements, instead of factors
          mutate(Country = as.character(Country))
        
        # Create a list of the top 6 countries
        top6Countries <- top6$Country
        
        top6Age <- cleanMC %>% 
          # Keep only entries whose country is included in the top 6 list
          filter(Country %in% top6Countries) %>% 
          # Remove any ages that are under ten years or more than 90 or NA or blank
          filter(Age > 10, Age < 90,
                 !is.na(Age)) %>% 
          filter(!Age == "") %>% 
          filter(!GenderSelect == "") %>%
          # Group the data by country and then age
          group_by(Country, Age, GenderSelect)
        
        ggplot(top6Age, aes(x = Age, fill = Country)) + 
          geom_density(alpha = 0.3) + 
          facet_wrap(~Country) + 
          ylab("Density of Users of a Given Age") + 
          theme(legend.position="none")
        
        ggplot(top6Age, aes(x = Age, fill = GenderSelect)) + 
          geom_density(alpha = 0.3) + 
          facet_wrap(~GenderSelect) + 
          ylab("Density of Users of a Given Age") + 
          theme(legend.position="none")
        
        
    4. #Analyzing Formal education 
       onechoice("FormalEducation")
        
       # Creating a data frame
       Education_df <- data.frame(
         FormalEducation = c(
           "Master's degree",
           "Bachelor's degree",
           "Doctoral degree",
           "Some college/university study without earning a bachelor's degree",
           "Professional degree",
           "I did not complete any formal education past high school",
           "I prefer not to answer"
         ),
         
         percent = c(41.8, 32.0, 15.6, 5.23, 3.00, 1.71, 0.599)
       )
       
       # Print the created data frame
       print(Education_df)
       
       #Visualization
       
       # Create a pie chart
       ggplot(Education_df, aes(x = "", y = percent, fill = FormalEducation)) +
         geom_bar(stat = "identity", width = 1, color = "white") +
         coord_polar("y", start = 0) +
         theme_void() +
         ggtitle("Distribution of Formal Education")
       
       5. #Analyzing coding experience
       
       onechoice("Tenure")
       
       # Creating the data frame
       Tenure_df <- data.frame(
         Tenure = c(
           "1 to 2 years",
           "3 to 5 years",
           "Less than a year",
           "More than 10 years",
           "6 to 10 years",
           "I don't write code to analyze data"
         ),
         
         percent = c(25.3, 24.8, 17.6, 15.0, 12.7, 4.66)
       )
       
       # Print the created data frame
       print(Tenure_df)
       
       #Visualization
       
       
       # Creating a bar graph with rotated x-axis labels to accomodate labels
       ggplot(Tenure_df, aes(x = Tenure, y = percent, fill = Tenure)) +
         geom_bar(stat = "identity") +
         labs(title = "Distribution of Tenure",
              x = "Tenure",
              y = "Percent") +
         scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
       6. #How did the respondents first start learning Data Science/Machine Learning
       
       onechoice("FirstTrainingSelect")
       
       #creating the Data Frame
       
       firstTraining_df <- data.frame(
         FirstTrainingSelect = c(
           "Online courses (coursera, udemy, edx, etc.)",
           "University courses",
           "Self-taught",
           "Work",
           "Other",
           "Kaggle competitions"
         ),
         
         percent = c(36.0, 28.3, 25.1, 6.73, 2.09, 1.75)
       )
       
       # Print the created data frame
       print(firstTraining_df)
       
       # Create a bar graph with rotated x-axis labels to accomodate labels
       ggplot(firstTraining_df, aes(x = FirstTrainingSelect, y = percent, fill = FirstTrainingSelect)) +
         geom_bar(stat = "identity") +
         labs(title = "First training in Data Science/Machine Learning",
              x = "First Training",
              y = "Percent") +
         scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")) +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
       # Creating a new data frame for another visualization
       
       training <- cleanMC %>% 
         # Keep only the columns that start with "LearningCategory" and don't include "FreeForm"
         select(starts_with("LearningCategory"), -contains("FreeForm")) %>% 
         # Set column names
         purrr::set_names(c("Self-taught", "Online Courses", "Work", "University Lecture", "University Practical Course", "Other")) %>% 
         # Re-structure the data
         gather(key = response, value = percent) %>% 
         # Remove any rows where the percentage was NA
         filter(!is.na(percent)) %>% 
         # Change the percentage column to a number
         mutate(percent = as.numeric(percent))
       
       ggplot(training, aes(x = percent, fill = response)) + 
         geom_histogram(bins = 10) + 
         facet_wrap(~response) + 
         ylab("Responses of a given percentage") + 
         theme(legend.position="none")
       # Print the created data frame
       print(firstTraining_df2)
      
       7. #Analytic method used at work
       multiplechoice("WorkAlgorithmsSelect")
       
       install.packages("tibble")
       library(tibble)
       
       install.packages("plotly")
       library(plotly)
       
       #creating a tibble
       
       Algorithms <- tibble(
         selections = c(
           "Regression/Logistic Regression",
           "Decision Trees",
           "Random Forests",
           "Neural Networks",
           "Bayesian Techniques",
           "Ensemble Methods",
           "SVMs",
           "Gradient Boosted Machines",
           "CNNs",
           "RNNs",
           "Other",
           "Evolutionary Approaches",
           "HMMs",
           "Markov Logic Networks",
           "GANs"
         ),
         totalCount = rep(7301, 15),
         count = c(4636, 3640, 3378, 2743, 2236, 2078, 1948, 1742, 1383, 895, 609, 404, 392, 355, 207),
         percent = c(63.5, 49.9, 46.3, 37.6, 30.6, 28.5, 26.7, 23.9, 18.9, 12.3, 8.34, 5.53, 5.37, 4.86, 2.84)
       )
       
       # Creating an interactive bar plot with different colors
       plot_ly(Algorithms, x = ~selections, y = ~percent, type = "bar", text = ~paste("Count: ", count, "<br>Total Count: ", totalCount), marker = list(color = rainbow(nrow(Algorithms)))) %>%
         layout(title = "Interactive Visualization of different techniques/Algorithms used",
                xaxis = list(title = "Selections"),
                yaxis = list(title = "Percent"),
                hovermode = "closest")
       8. #Tools used by the respondents
       
       multiplechoice("WorkToolsSelect")
       
       # Load the tibble package
       library(tibble)
       
       install.packages("tidyverse")
       library(tidyverse)
       
       # Creating a tibble named Tools_tibble
       Tools_tibble <- tibble(
         selections = c("Python", "R", "SQL", "Jupyter notebooks", "TensorFlow", 
                        "Amazon Web services", "Unix shell / awk", "Tableau", 
                        "C/C++", "NoSQL"),
         totalCount = rep(7955, 10),
         count = c(6073, 4708, 4261, 3206, 2256, 1868, 1854, 1619, 1528, 1527),
         percent = c(76.3, 59.2, 53.6, 40.3, 28.4, 23.5, 23.3, 20.4, 19.2, 19.2)
       )
       
       # Print the created tibble
       print(Tools_tibble)
       
       # Loading the required library
       library(ggplot2)
       
       # Creating a vertical faceted bar plot with rotated x-axis labels
       ggplot(Tools_tibble, aes(x = selections, y = percent, fill = selections)) +
         geom_bar(stat = "identity") +
         
         labs(title = "Tools used by Data Analysts",
              x = "Selections",
              y = "Percent") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))  # Rotating and adjusting x-axis labels
       
       9. #Most important skills for getting a job in Data Science according to respondents
       
          #First we manipulate the Data questionnaire to isolate the question needed
       skills <- schema %>% 
         # Keep only the questions which contain the following phrase
         filter(grepl("How important do you think the below skills ", Question, fixed = TRUE)) %>%
         # Remove any questions that contain "FreeForm"
         filter(!grepl("FreeForm", Column, fixed = TRUE)) %>% 
         # Split the question text at the hyphen
         mutate(response = strsplit(as.character(Question), " - ")) %>% 
         # Un-nest the two parts
         unnest(response) %>% 
         # Remove the first part of the question text
         filter(!grepl("How important do you think the below skills ", response, fixed = TRUE)) %>% 
         # Keep only the question text and the question number
         select(-2)
        
       # In the dataset of responses, keep only the responses to this question
       skillResponses <- cleanMC %>% 
         # select only the columns that start with "JobSkillImportance" and don't contain "FreeForm"
         select(starts_with("JobSkillImportance"), -contains("FreeForm")) %>% 
         # Re-structure the data
         gather(key = response, value = frequency) %>% 
         # Remove any entries where the respondent didn't answer this question
         filter(!frequency == "") 
       
       # Combining the possible skills with the responses
       skillResponseNames <- left_join(skillResponses, skills, by = c("response" = "Column")) %>% 
         # Group by the response and then the frequency
         group_by(response.y, frequency) %>% 
         # Calculate the number of respondents that indicated a certain response
         summarise(count = n()) %>% 
         # Re-factor the frequency
         mutate(frequency = factor(frequency, levels = c("Unnecessary", "Nice to have", "Necessary"), ordered = TRUE)) 
       
       # Plotting the responses
       ggplot(skillResponseNames, aes(x = frequency, y = count, fill = response.y)) + 
         geom_bar(stat = "identity") + 
         facet_wrap(~response.y) + 
         ylab("Number of times a response was selected") + 
         theme(legend.position = "none") +
         theme(axis.text.x = element_text(angle = 90, 
                                          vjust = 0.5, 
                                          hjust = 1)) 
       
       
       10. #Finally we visualize Job satisfaction 
       
       jobSatisfaction <- multiplechoice("JobSatisfaction") %>% 
         filter(!selections %in% c("I prefer not to share", "")) %>% 
         mutate(selections = factor(selections, levels = c('1 - Highly Dissatisfied', '2', '3', '4', '5', '6', '7', '8', '9', '10 - Highly Satisfied')))
       
       # Define colors for each level of job satisfaction
       colors <- c("#FF0000", "#FF4500", "#FF8C00", "#FFD700", "#FFFF00", "#ADFF2F", "#7CFC00", "#00FA9A", "#00BFFF", "#0000FF")
       
       # Plot with multiple colors
       ggplot(jobSatisfaction, aes(x = selections, y = count, fill = selections)) +
         geom_bar(stat = "identity") +
         scale_fill_manual(values = colors) +
         labs(title = "Job Satisfaction",
              x = "Satisfaction Level",
              y = "Count") +
        
         theme_minimal()  +
         theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8))  # Rotating and adjusting x-axis labels
       
       
       
       # This concludes our R Script for the Project 4 of Module 4 of ALY 6000
       # Power point presentation based on this script shall now be prepared and presented.
       
       #Bibliography
       # 1. For the functions used in this script a part of the code by Amber Thomas was adapted. 
       # Author - Thomas, Amber 
       # https://www.kaggle.com/code/amberthomas/kaggle-2017-survey-results?kernelSessionId=1679052
       