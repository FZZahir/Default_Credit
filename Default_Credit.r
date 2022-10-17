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
library(corrplot)
library(boot)

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


ggplot(credit_cp, aes(x = BILL_AMT1)) + geom_histogram(bins = 30, color = "black", fill = "white") + labs(title = "Distribution of BILL_AMT1", x = "BILL_AMT1", y = "Count")


ggplot(credit_cp, aes(x = SEX)) + geom_bar(show.legend = TRUE, width = .5)+ labs( title = "Sex Frequency", x = "Sex", y = "Count")

# Create bar plot for percentage proportion of EDUCATION
ggplot(credit_cp, aes(x = EDUCATION)) + geom_bar(show.legend = TRUE) + labs(title = "Education Frequency", x = "Education", y = "Count")

# Show bar chart for MARRIAGE
ggplot(credit_cp, aes(x = MARRIAGE)) + geom_bar(show.legend = TRUE) + labs(title = "Marriage Frequency", x = "Marriage", y = "Count")

#distribution of Bill Amount 1
ggplot(credit_cp, aes(x = BILL_AMT1)) + geom_histogram(bins = 30, color = "black", fill = "white") + labs(title = "Distribution of BILL_AMT1", x = "BILL_AMT1", y = "Count")
# bar chart for Bill Amount 2
ggplot(credit_cp, aes(x = BILL_AMT2)) + geom_histogram(bins = 30, color = "black", fill = "white") + labs(title = "Distribution of BILL_AMT2", x = "BILL_AMT2", y = "Count")

#boxplot of Bill Amount 1
ggplot(credit_cp, aes(x = BILL_AMT1)) + geom_boxplot() + labs(title = "Boxplot of BILL_AMT1", x = "BILL_AMT1", y = "Count")
q1 <- credit_cp %>% filter(LIMIT_BAL >= 1000 & LIMIT_BAL <= 50000)


View(q1)

# facteted boxplot of Bill Amount 1 and 2 

# Hexbin plot of Bill Amount 1 and 2
ggplot(credit_cp, aes(x = BILL_AMT1, y = BILL_AMT2)) + geom_hex() + labs(title = "Hexbin plot of BILL_AMT1 and BILL_AMT2", x = "BILL_AMT1", y = "BILL_AMT2")

# Hexbin plot of LIMIT_BAL and AGE
ggplot(credit_cp, aes(x = LIMIT_BAL, y = AGE)) + geom_hex() + labs(title = "Hexbin plot of LIMIT_BAL and AGE", x = "LIMIT_BAL", y = "AGE")

#Hexbin of default and Age
ggplot(credit_cp, aes(x = AGE, y = default.payment.next.month, )) + geom_hex() + labs(title = "Hexbin plot of DEFAULT and AGE", x = "DEFAULT", y = "AGE")

ggplot(q1, aes(x = AGE, y = LIMIT_BAL, color = "blue", size = LIMIT_BAL), fill = "white") + geom_point() + labs(title = "Scatterplot of Age and Limit Balance", x = "Age", y = "Limit Balance")

# Hexbinplot of LIMIT_BAL and AGE

library(WVPlots) 
HexBinPlot(credit_cp, "AGE", "LIMIT_BAL", "Limit Bal as a function of age") + geom_smooth(color="black", se=FALSE)

#The following plots below wont work until we set lables for the factors/levels

#ggplot(credit_cp, aes(x=MARRIAGE, fill=AGE)) + geom_bar() 

#ggplot(credit_cp, aes(x=AGE, fill=MARRIAGE)) + geom_bar(position = "dodge") 

#ShadowPlot(credit_cp, "MARRIAGE", "AGE", title = "Shadow plot of Marriage and Age")") 

#ggplot(customer_data, aes(x=marital_status, fill=health_ins)) + geom_bar(position = "fill")


# bubble_chart of LIMIT balance and age

#credit_cp %>% filter((AGE > 70)) %>% select(-AGE) #seems to select all the data that matches the criteria without AGE if you instead do select(AGE) it will actually print out those ages.



ggplot(credit_cp, aes(x = AGE, y = LIMIT_BAL, color = "blue", size = LIMIT_BAL), fill = "white") + geom_point() + labs(title = "Scatterplot of Age and Limit Balance", x = "Age", y = "Limit Balance")


#Establish levels and labels for the categorical variables
#credit_cp$SEX <- factor(credit_cp$SEX, levels = c(1, 2), labels = c("Male", "Female"))
#credit_cp$EDUCATION <- factor(credit_cp$EDUCATION, levels = c(1, 2, 3, 4, 5, 6), labels = c("Graduate School", "University", "High School", "Others", "Unknown", "Unknown"))

#select count of education = 6
#credit_cp %>% filter(EDUCATION == "5") %>% count()
