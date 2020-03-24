#######################################################################
## Project: Image Processing Methods for Environment Classification
## Script purpose: Analyze and Summarize photos via 3x3 mean grid
## Date: 2020-03-23
## Author: Rahul Sharma
#######################################################################

## Read in the library and metadata
library(jpeg)
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
gallery.size <- nrow(pm)

# We use 27 predictors because 9 predictors come from each rgb dimension, 
# taken from the partitioned 3x3 matrix created.
X <- matrix(NA, ncol = 27, nrow = gallery.size)

# M   One diminsional matrix as input
# r   Let r denote the maximum number of columns outputted by the split matrix
# c   Let c denote the number of columns outputted by the split matrix 
#
# Given a matrix M, mat_split() creates a multidimensional matrix where the length 
# and are denoted by r and c. If the r and c values do not divide evenly, NA values 
# will be filled in to accommodate this. 

mat_split <- function(M, r, c) {
  nr <- ceiling(nrow(M) / r)
  nc <- ceiling(ncol(M) / c)
  newM <- matrix(NA, nr * r, nc * c)
  newM[1:nrow(M), 1:ncol(M)] <- M
  
  div_k <- kronecker(matrix(seq_len(nr * nc), nr, byrow = TRUE), matrix(1, r, c))
  matlist <- split(newM, div_k)
  N <- length(matlist)
  mats <- unlist(matlist)
  dim(mats) <- c(r, c, N)
  return(mats)
}

for (k in 1:gallery.size) {
  img <- readJPEG(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\", pm$name[k]))
  
  # Iterate over the 3 rgb values, represented as the 3 dimensions in the readJPEG output
  for (i in 1:3) {
    X[k, (1 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 1][, 1:3])
    X[k, (2 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 1][, 4:6])
    X[k, (3 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 1][, 7:9])
    X[k, (4 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 2][, 1:3])
    X[k, (5 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 2][, 4:6])
    X[k, (6 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 2][, 7:9])
    X[k, (7 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 3][, 1:3])
    X[k, (8 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 3][, 4:6])
    X[k, (9 + 9 * (i - 1))] <-
      mean(mat_split(img[, , i], round(dim(img[, , i])[1] / 3), dim(img[, , i])[2])[, , 3][, 7:9])
  }
  
  print(sprintf("%03d / %03d", k, gallery.size))
}

write.table(X,
            file = "3x3MeanSummary.txt",
            row.names = FALSE,
            col.names = FALSE)
