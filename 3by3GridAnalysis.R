######################################################################################
# Name: Rahul Sharma  
# Last Updated
#
######################################################################################

## Read in the library and metadata
library(jpeg)
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
n <- nrow(pm)

# the 9 predictors correspond to the median value of each portion of the 3x3 grid
X <- matrix(NA, ncol=9, nrow=n)

y <- as.numeric(pm$category == "outdoor-day")

arr = c(1:10000)

cols <- list(c(), c(), c())

# divide pixel intensities into 3 columns
for (c in 1:3) {
  for (i in (3 * (c - 1) + 1):length(arr)) {
    if (i %% 9 == 3 * (c - 1) + 1) {
      cols[[c]] <- c(cols[[c]], arr[i:(i + (2))])
    }
  }
}

grid <- list(c(), c(), c(), c(), c(), c(), c() , c(), c())

n <- 1
for(i in 1:3){
    grid[[n]] <- split(cols[[i]], ceiling(seq_along(cols[[i]])/(length(cols[[i]])/3)))[1]
    n <- n + 1
    grid[[n]] <- split(cols[[i]], ceiling(seq_along(cols[[i]])/(length(cols[[i]])/3)))[2]
    n <- n + 1
    grid[[n]] <- split(cols[[i]], ceiling(seq_along(cols[[i]])/(length(cols[[i]])/3)))[3]
    n <- n + 1
}

for(i in 1:9){
  X[1,i] <- median(unlist(grid[[i]]), na.rm = TRUE)
}
