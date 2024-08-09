install.packages("readr")
library(dplyr)
library(ggplot2)
library(caTools)
library(tidyverse)
library(readr)

data <- read_csv("hotel_bookings.csv")

view(data)
summary(data)
str(data)

#- How many rows and columns are in the dataset?
dim(data)

# - What are the first few entries in the `hotel` column?
print(head(data))

#  - What is the average lead time for bookings?
average_booking_time <- mean(data$lead_time)
print(average_booking_time)

# - What is the data type of the `arrival_date_year` column?
class(data$arrival_date_year)

#Check for missing values:
colSums(is.na(data))

data <- data %>%
  mutate(children = ifelse(is.na(children), median(children, na.rm = TRUE), children))

# verify that missing values have been filled:
colSums(is.na(data))

# Convert hotel to factor:
data <- data %>%
  mutate(hotel = as.factor(hotel))

# Convert arrival_date_year to integer:
data <- data %>%
  mutate(arrival_date_year= as.integer(arrival_date_year))

# remove duplicates
data <- data[!duplicated(data), ]


# Calculate the mean and standard deviation of adr:
mean_adr <- mean(data$adr, na.rm = TRUE)
sd_adr <- sd(data$adr, na.rm = TRUE)

# remove outliers beyond 3 standard deviations
data <- data %>%
  filter(adr < mean_adr +3 * sd_adr)

# save the cleaned data 
write.csv(data, "Cleaned_Hotel_Bookings.csv", row.names = FALSE)

# Install and load the readr package
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)

# Set the working directory where you want to save the file
setwd("/Users/hassan/Downloads/R")


# Check if 'data' is a data frame
if (is.data.frame(data)) {
  print("data is a data frame")
} else {
  stop("data is not a data frame")
}

write.csv(data, "Cleaned_Hotel_Bookings.csv", row.names = FALSE)
