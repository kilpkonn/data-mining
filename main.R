library(shotGroups)

rm(list = ls())

source("distance-functions.R")
source("kmeans.R")

# Load the data, note that variable "data_matrix" will appear!!
load(file="./data/gaussians.RData")

sample_size <- floor(0.7 * nrow(data_matrix))

# Set seed to make results reproducible
# set.seed(123)

train_ind <- sample(seq_len(nrow(data_matrix)), size = sample_size)

train_set <- data_matrix[train_ind, ]
test_set <- data_matrix[-train_ind, ]


dist_fn <- function(a, b) {
  res <- minkowsky_dist(a, b, 2)
  return(res)
}

lbls = kmeans(train_set, 3, dist_fn, 10)

for (i in 1:nrow(test_set)) {
  color <- switch(lbls[[i]],"red","green","blue","orange","magenta")
  a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
  plot(a, col=color, type="p", xlim=c(-10,20), ylim=c(-10,20))
  par(new=TRUE)
}

for (i in 1:nrow(test_set)) {
  if (is.na(lbls[[i, 1]])) {
    a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
    plot(x=, col="black", type="p", xlim=c(-10,20), ylim=c(-10,20))
    par(new=TRUE)
  }
}