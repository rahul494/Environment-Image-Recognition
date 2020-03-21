#######################################################################
## Project: Environment Image Recognition
## Script purpose: Summarize pixel intensities according to mean values 
## Date: 2020-03-21
## Author: Rahul Sharma
#######################################################################

## Read in the library and metadata
library(jpeg)
pm <- read.csv("C:\\Users\\Rahul\\Downloads\\photoMetaData.csv")
n <- nrow(pm)

X <- matrix(NA, ncol = 3, nrow = n)
for (j in 1:n) {
  img <-
    readJPEG(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\", pm$name[j]))
  X[j, ] <- apply(img, 3, mean)
  print(sprintf("%03d / %03d", j, n))
}

write.table(X, file="MeanPixelsSummary.txt", row.names=FALSE, col.names=FALSE)
