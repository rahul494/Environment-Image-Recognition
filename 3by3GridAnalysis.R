#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Analyze and classify photos via 3x3 median grid
## Date: 2020-03-18
## Author: Rahul Sharma
#######################################################################

## Read in the library and metadata
library(jpeg)
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
gallery.size <- nrow(pm)

X <- matrix(NA, ncol=9, nrow=gallery.size)

for (k in 1:gallery.size){
  
  img <- readJPEG(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\",pm$name[k]))
  
  # initialize list of columns
  cols <- list(c(), c(), c())
  grid.length <- ceiling(sqrt(length(img)/9))
  
  # partition pixel intensities into 3 columns
  for (c in 1:3) {
    for(i in seq(from=(grid.length * (c - 1) + 1), to=length(img), by=sqrt(length(img)))){
      cols[[c]] <- c(cols[[c]], img[i:(i + (grid.length-1))])
    }
  }
  
  # divide each column into 3 parts, formulating a 3x3 grid
  n <- 1
  grid <- list(c(), c(), c(), c(), c(), c(), c() , c(), c())
  for(i in 1:3){
    grid[[n]] <- split(cols[[i]], ceiling(seq_along(cols[[i]])/(length(cols[[i]])/3)))[1]
    grid[[n+1]] <- split(cols[[i]], ceiling(seq_along(cols[[i]])/(length(cols[[i]])/3)))[2]
    grid[[n+2]] <- split(cols[[i]], ceiling(seq_along(cols[[i]])/(length(cols[[i]])/3)))[3]
    n <- n + 3
  }
  
  grid[[i]]
  
  # find the median value of each partition in the 3x3 grid
  for(i in 1:9){
    X[k,i] <- median(unlist(grid[[i]]), na.rm = TRUE)
  }
  
  print(sprintf("%03d / %03d", k, gallery.size))
}

trainFlag <- (runif(gallery.size) > 0.5) # generate 800 uniform numbers

y <- as.numeric(pm$category == "outdoor-day")

# build a glm model on these median values
out <- glm(y ~ X, family=binomial, subset=trainFlag)
out$iter
summary(out)

# How well did we do?
pred <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
y[order(pred)]
y[!trainFlag][order(pred[!trainFlag])]

mean((as.numeric(pred > 0.5) == y)[trainFlag])
mean((as.numeric(pred > 0.5) == y)[!trainFlag])

## ROC curve (see lecture 12)
roc <- function(y, pred) {
  alpha <- quantile(pred, seq(0,1,by=0.01))
  N <- length(alpha)
  
  sens <- rep(NA,N)
  spec <- rep(NA,N)
  for (i in 1:N) {
    predClass <- as.numeric(pred >= alpha[i])
    sens[i] <- sum(predClass == 1 & y == 1) / sum(y == 1)
    spec[i] <- sum(predClass == 0 & y == 0) / sum(y == 0)
  }
  return(list(fpr=1- spec, tpr=sens))
}

r <- roc(y[!trainFlag], pred[!trainFlag])
plot(r$fpr, r$tpr, xlab="false positive rate", ylab="true positive rate", type="l")
abline(0,1,lty="dashed")

# auc
auc <- function(r) {
  sum((r$fpr) * diff(c(0,r$tpr)))
}
glmAuc <- auc(r)
glmAuc
