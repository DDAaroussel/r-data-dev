#This a tutorial script to accompany the Kaggle housing market analysis


# Load libraries ----------------------------------------------------------

install.packages("data.table")
install.packages("testthat")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("GGally")
install.packages("e1071")
library(dplyr)
library(ggplot2)
library(data.table)
library(testthat)
library(gridExtra)
library(GGally)
library(e1071)
library(readr)

# Load data ---------------------------------------------------------------

hp_train <- read_csv("train.csv", col_names = TRUE)
hp_test <- read_csv("test.csv", col_name = TRUE)

m <- sapply(hp_train, function(x) sum(is.na(x)))
missing <- m[m>0]
missing
