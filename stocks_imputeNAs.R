## The Problem:
#
#     Some of the features in the stock market data have NA values, in some cases a huge number of NAs
#     (eg Feature_1 has over 33k NAs out of about 40k values)
#
## Imputation strategy:
#
#     Use ML model to predict missing values based on observed values for each of the features with NAs using
#     the other 24 features
#     The features with NAs are all but 5 and 7

# Load necessary libraries
setwd("C:/Users/USER/Dropbox/Side Project/Machine Learning/Kaggle/Winton Stock Market Challenge")

library(ggplot2)
library(dplyr)
library(reshape2)

# This loads stock data if the current df labeled 'train' is not the stock data or does not exist
# and does not load it if it's already loaded up, saves a bit of computation
if(!exists("current.train")){
      current.train <- "stocks"
      train <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/train.csv")
      test <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/test.csv")
}

if(current.train != "stocks"){
      train <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/train.csv")
      test <- read.csv("D:/R Data/Kaggle/Winton Stock Market Challenge/test.csv")
      current.train <- "stocks"
}

# The features with no NA values are 5 and 7
noNAs <- c("Feature_5", "Feature_7")
feature.names <- names(train)[2:26]
colIndex <- 2:26
feature.names.NAs <- colIndex[-which(feature.names %in% noNAs)]
df.features <- train[,2:26]

# For each feature except 5 and 7, train a model based on the non-NA values and use predicted values
# as imputed values to replace the NAs
for(col in feature.names.NAs[1]){
      df.temp <- df.features[-is.na(df.features[,col]),0]
}