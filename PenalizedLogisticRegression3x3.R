#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Utilize lasso logistic regression to obtain accuracy of various pixel summaries
## Date: 2020-04-05
## Author: Rahul Sharma
#######################################################################

library(glmnet)

# Read in metadata and 3x3 pixel summaries
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
mean.3x3 <- read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\3x3MeanSummary.txt", header = FALSE, sep = " ")
median.3x3 <- read.table("C:\\Users\\Rahul\\Documents\\MATH 3333 Data analytics A hands-on approach\\Project\\3x3MedianSummary.txt", header = FALSE, sep = " ")

# Drop observations that have null pixel values
na.rows <- unique(which(is.na(median.3x3), arr.ind = TRUE)[, 1])
median.3x3 <- median.3x3[-c(na.rows), ]
y <- as.numeric(pm$category == "outdoor-day")
y <- y[-c(na.rows)]

# repeat cross-validation 10 times
sum <- 0
rep <- 10
for(i in 1:rep){
  trainFlag <- (runif(nrow(median.3x3)) > 0.5)
  cvfit = cv.glmnet(data.matrix(median.3x3), y, family = "binomial", type.measure = "class", alpha = 1, subset = trainFlag )
  pred <- predict(cvfit,newx = data.matrix(median.3x3),s = "lambda.min",type = "class")
  sum <- sum + mean((as.numeric(pred > 0.5) == y)[!trainFlag])
}

print(sprintf("Our accuracy is: %03d ", sum/rep))
