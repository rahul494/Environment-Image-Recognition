#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Utilize lasso logistic regression to obtain accuracy of pixel summaries
## Date: 2020-04-05
## Author: Rahul Sharma
#######################################################################

library(glmnet)

# Read in metadata and pixel summaries
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
mean.sum <- read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\MeanPixelsSummary.txt",header=FALSE,sep=" ")
median.sum <- read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\MedianPixelsSummary.txt",header=FALSE,sep=" ")

# repeat cross-validation 10 times
sum <- 0
rep <- 10
for(i in 1:rep){
  trainFlag <- (runif(nrow(median.3x3)) > 0.5)
  cvfit = cv.glmnet(data.matrix(median.3x3), y, family = "binomial", type.measure = "class", alpha = 1, subset = trainFlag )
  pred <- predict(cvfit, newx = data.matrix(median.3x3), s = "lambda.min", type = "class")
  sum <- sum + mean((as.numeric(pred > 0.5) == y)[!trainFlag])
}

print(sprintf("Our accuracy is: %03d ", sum/rep))
