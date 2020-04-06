#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Analyze and classify photos via Support Vector Machine
## Date: 2020-04-05
## Author: Rahul Sharma
#######################################################################

library(keras)
library(e1071)

# Read in metadata and pixel summaries
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
mean.sum <-read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\MeanPixelsSummary.txt",header=FALSE,sep=" ")
median.sum <-read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\MedianPixelsSummary.txt",header=FALSE,sep=" ")
mean.3x3 <-read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\3x3MeanSummary.txt",header=FALSE,sep=" ")
median.3x3 <-read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\3x3MedianSummary.txt",header=FALSE,sep=" ")

# repeat cross-validation 10 times
sum <- 0
rep <- 10
for(i in 1:rep){
  train_index <- sample(1:nrow(median.3x3), 0.8 * nrow(median.3x3 ))
  test_index <- setdiff(1:nrow(median.3x3), train_index)
  
  x.train <- median.3x3[c(train_index),]
  x.test <- median.3x3[c(test_index),]
  
  y.train <- as.numeric(pm$category == "outdoor-day")[c(train_index)]
  y.test <- as.numeric(pm$category == "outdoor-day")[c(test_index)]
  
  out <- svm(y.train, data = x.train, kernel = "polynomial", cost = 0.1, scale = FALSE)
  pred <- predict(out, y.test)
  tab <- table(predicted = pred, actual = y.test)
  
  sum <- sum + sum(diag(tab))/sum(tab)
}

print(sprintf("Our accuracy is: %03f ", sum/rep))

