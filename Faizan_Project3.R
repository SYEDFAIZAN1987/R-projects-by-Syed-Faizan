#---------------------------------------------------------#
# Syed Faizan                                             #
# 2024-01-27                                              #
# ALY6000 Introduction to Analytics - Module 3 Project                      #
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
library(lubridate)
library(pacman)

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session



#  Reading the data set into the dataframe
books <- read_csv("books.csv")  
                                                 #Using tidyverse
books$
  
# 1. To standardize the names in your data frame()
books <- clean_names(books)

summary(books)
  

# 2. Converting the first_publish_date column to a type date using the 'mdy' function
books$first_publish_date = mdy(books$first_publish_date)
books$first_publish_date

# Inspected for NA prior to this code using sum(is.na(books$firstPublishDate))
# checked class of variable to confirm conversion to date object class(books$first_publish_date)                            


# 3. extract the year from the first_publish_date column place it in a new column named year
books$year <- year(books$first_publish_date)
books$year

# 4. Reducing the data set to only include books published between 1990 and 2020
books <- filter(books, year >= 1990 & year <= 2020)
books

# 5. Removing the following columns from the data set: publish_date, edition, characters, price, genres, setting, and isbn
books <- books %>%
  select(-publish_date, -edition, -characters, -price, -genres, -setting, -isbn)

books


# 6. Retaining books that are fewer than 1200 pages
books <- filter(books, pages < 1200)

books

# Step 7: Remove rows with NAs
books <- na.omit(books)

books

# Print the resulting dataset
print(books)

# 8. Glimpse the dataset
glimpse(books)

# 9. Summary statistics
summary(books)

# 10. Create a rating histogram
ggplot(books, aes(x = rating, fill = "red")) +
  geom_histogram(binwidth = 0.25) +
  labs(title = "Histogram of Book Ratings", x = "Rating", y = "Number of Books") 

# 11. Create a horizontal boxplot of pages per book
ggplot(books, aes(x = pages, fill = "red")) +
  geom_boxplot(horizontal = TRUE) +
  labs(title = "Box Plot of Page Counts", x = "Pages") 

# 12. Create a data frame 'by_year' with a count of books by year
by_year <- data.frame(total_books = table(books$year))

by_year

# 13. Create a line plot with points for counts per year from 1990 - 2020
ggplot(by_year, aes(y = total_books.Freq , x = total_books.Var1)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Books Rated Per Year", x = "Year", y = "Total books") 


books <- read.csv("books.csv")
books$

# 14. Create a data frame 'book_publisher' with unique publishers and book counts

  # used na.omit(books$publisher) to omit all NA from publisher before creation of Data Frame 
  

  
unique_publishers <- unique(books$publisher)

# Create a table of counts for each publisher

publisher_counts <- table(books$publisher)

# Create a data frame with columns publisher and book_count
book_publisher <- data.frame(
  publisher = unique_publishers,
  book_count = as.numeric(publisher_counts[match(unique_publishers, names(publisher_counts))])
)

  book_publisher
  
  
# Problems 15 - 22 using 'book_publisher' data frame

# 15. Remove publishers with fewer than 125 books
  
book_publisher_variable <- book_publisher[book_publisher$book_count  >= 125, ]

#This yields 20 publishers with books more than 125

# 16. Order by total number of books in descending order
book_publisher <- book_publisher[order(book_publisher$book_count, decreasing = TRUE), ]

book_publisher


# 17. Add a column 'cum_counts' with cumulative sum of 'book_count'
book_publisher$cum_counts <- cumsum(book_publisher$book_count)

book_publisher

# 18. Add a column 'rel_freq' with relative frequency of 'book_count'


book_publisher$book_count <- as.numeric(book_publisher$book_count)

# Calculate relative frequency
total_books <- sum(na.omit(book_publisher$book_count)) #Be careful to omit NA from book_count column to get 
                                                       # the value of the sum of book count

total_books

book_publisher$rel_freq <- book_publisher$book_count / total_books

book_publisher


# 19. Add a column 'cum_freq' with cumulative sum of 'rel_freq'

book_publisher$cum_freq <- cumsum(book_publisher$rel_freq)

book_publisher

# 20. Make 'publisher' column a factor with levels based on current ordering
book_publisher$publisher <- factor(book_publisher$publisher, levels = book_publisher$publisher)

book_publisher

# 21. Create a Pareto Chart with an ogive of cumulative counts
         # I ordered the data frame by book_count in descending order
book_publisher <- book_publisher[order(book_publisher$book_count, decreasing = TRUE), ]

         # Used head() to choose only first six publishers as depicted in the instruction sheet
top_six_publishers <- head(book_publisher, 6)

        # Created a cumulative sum of book_count for the top six publishers
top_six_publishers$cum_counts <- cumsum(top_six_publishers$book_count)

        # Calculated relative frequency for the top six publishers
top_six_publishers$rel_freq <- top_six_publishers$book_count / sum(top_six_publishers$book_count)

# Created Pareto Chart with ggplot2 for the top six publishers

ggplot(top_six_publishers, aes(x = reorder(publisher, -book_count), y = book_count)) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_line(aes(x = publisher, y = cum_counts, group = 1 ), color = "black") +
  labs(
    x = "Publisher",
    y = "Number of Books",
    title = "Book Counts (1990 - 2020)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 22. Create additional visualizations based on data or additional analysis

# Report Writing (23. Write an executive summary)

# Include an overview, key findings, visualizations, and key statistics in a written report.


