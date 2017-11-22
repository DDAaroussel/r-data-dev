
# Load packages ----------------------------------------------------------

library(ggplot2)


# Read in data ------------------------------------------------------------

loaddata <- function(file) {
  data <- read.csv(file, header = TRUE, stringsAsFactors=F)
  # compute family size on dataset (including self)
  data$FamilySize <- data$SibSp + data$Parch + 1
  
  data
}

train_titanic <- loaddata("~/R/Kaggle/Titanic/train.csv")

test_titanic <- loaddata("~/R/Kaggle/Titanic/test.csv")

gender_submission <- read.csv("~/R/Kaggle/Titanic/gender_submission.csv")


# Data prep ---------------------------------------------------------------

train_titanic$Survived <- as.logical(train_titanic$Survived)
levels(train_titanic$Survived) <- c("Not survived", "Survived")

for(i in c(3,5,12)) {
  train_titanic[,i] <- as.factor(train_titanic[,i])
}




