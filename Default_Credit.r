# Objective: Use the dataset to predict the default of credit card clients. Create various models and compare their performance.
# Author: Fahim Zahir
# Date: 2022-10-16
# Contact: fahimzahir@protonmail.com

# Load the required packages
library(tidyverse)
library(mlr)
library(rpart)
library(WVPlots)
library(ggplot2)

# Load the dataset
credit <- read.csv("Default_Credit.csv")
view(credit)
#Get column names
colnames(credit)

credit_cp <- credit
view(credit_cp)

#summary function on credit_cp
summary(credit_cp)

# using visulization to see the distribution of the data and find anomalies 
# distribution plot of age
ggplot(credit_cp, aes(x = AGE)) + geom_histogram(bins = 30, color = "black", fill = "white") + labs(title = "Distribution of Age", x = "Age", y = "Count")

#Creating a bar plot for frequency of SEX


ggplot(credit_cp, aes(x = SEX)) + geom_bar(show.legend = TRUE, width = .5)+ labs( title = "Sex Frequency", x = "Sex", y = "Count")

# Create bar plot for percentage proportion of EDUCATION
ggplot(credit_cp, aes(x = EDUCATION)) + geom_bar(show.legend = TRUE) + labs(title = "Education Frequency", x = "Education", y = "Count")

# Show bar chart for MARRIAGE
ggplot(credit_cp, aes(x = MARRIAGE)) + geom_bar(show.legend = TRUE) + labs(title = "Marriage Frequency", x = "Marriage", y = "Count")

q1 <- credit_cp %>% filter(LIMIT_BAL >= 1000 & LIMIT_BAL <= 50000)
View(q1)


ggplot(q1, aes(x = AGE, y = LIMIT_BAL, color = "blue", size = LIMIT_BAL), fill = "white") + geom_point() + labs(title = "Scatterplot of Age and Limit Balance", x = "Age", y = "Limit Balance")

# bubble_chart of LIMIT balance and age

#credit_cp %>% filter((AGE > 70)) %>% select(-AGE) #seems to select all the data that matches the criteria without AGE if you instead do select(AGE) it will actually print out those ages.



ggplot(credit_cp, aes(x = AGE, y = LIMIT_BAL, color = "blue", size = LIMIT_BAL), fill = "white") + geom_point() + labs(title = "Scatterplot of Age and Limit Balance", x = "Age", y = "Limit Balance")


#Establish levels and labels for the categorical variables
#credit_cp$SEX <- factor(credit_cp$SEX, levels = c(1, 2), labels = c("Male", "Female"))
#credit_cp$EDUCATION <- factor(credit_cp$EDUCATION, levels = c(1, 2, 3, 4, 5, 6), labels = c("Graduate School", "University", "High School", "Others", "Unknown", "Unknown"))

#select count of education = 6
#credit_cp %>% filter(EDUCATION == "5") %>% count()
