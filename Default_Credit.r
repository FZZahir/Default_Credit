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

