#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Analyze and classify photos via 3x3 median grid
## Date: 2020-03-18
## Author: Rahul Sharma
#######################################################################

## Read in the library and metadata
library(jpeg)
img <- readJPEG(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\",pm$name[15]))
arr <- c()
X <- matrix(NA, ncol=9, nrow=10)

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

# find the median value of each partition in the 3x3 grid
for(i in 1:9){
  X[1,i] <- median(unlist(grid[[i]]), na.rm = TRUE)
}


X <- matrix(NA, ncol=9, nrow=10)
img <- readJPEG(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\",pm$name[15]))
X[1,] <- apply(img,3,median)
X
