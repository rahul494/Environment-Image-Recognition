######################################################################################
# Name: Rahul Sharma
# Last Updated
#
######################################################################################

## Read in the library and metadata
library(jpeg)
img <-
  readJPEG(paste0("C:\\Users\\Rahul\\Downloads\\columbiaImages\\", pm$name[1]))

X <- matrix(NA, ncol = 9, nrow = n)
arr <- c(1:1039680)

cols <- list(c(), c(), c())
grid.length <- ceiling(sqrt(length(arr) / 9))

# divide pixel intensities into 3 columns
for (c in 1:3) {
  for (i in seq(
    from = (grid.length * (c - 1) + 1),
    to = length(arr),
    by = sqrt(length(arr))
  )) {
    cols[[c]] <- c(cols[[c]], arr[i:(i + (grid.length - 1))])
  }
}


grid <- list(c(), c(), c(), c(), c(), c(), c() , c(), c())

n <- 1
for (i in 1:3) {
  grid[[n]] <-
    split(cols[[i]], ceiling(seq_along(cols[[i]]) / (length(cols[[i]]) / 3)))[1]
  n <- n + 1
  grid[[n]] <-
    split(cols[[i]], ceiling(seq_along(cols[[i]]) / (length(cols[[i]]) / 3)))[2]
  n <- n + 1
  grid[[n]] <-
    split(cols[[i]], ceiling(seq_along(cols[[i]]) / (length(cols[[i]]) / 3)))[3]
  n <- n + 1
}

for (i in 1:9) {
  X[1, i] <- median(unlist(grid[[i]]), na.rm = TRUE)
}
