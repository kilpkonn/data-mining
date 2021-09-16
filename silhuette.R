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

silhuette <- function(data, clusters, point, cluster, s) {
  d_in <- 0
  d_out <- 0
  for (i in 1:nrow(data)) {
    row <- data[i,]
    clst <- clusters[i]
    if (clst == cluster) {
      d_in <- d_in + s(point, row)
    } else {
      d_out <- d_out + s(point, row)
    }
  }
  res <- (d_out - d_in) / max(d_out, d_in)
}

clustes <- kmeans(train_set, 3, dist_fn, 10)
lbls <- silhuette(train_set, clustes, c(5, 5), 1, dist_fn)

lbls <- lbls[,ncol(lbls)-3]
n <- 1
for (t in unique(lbls)) {
  lbls[lbls == t] <- n
  n <- n + 1
}

for (i in 1:nrow(train_set)) {
  color <- switch(lbls[[i]],"red","green","blue","orange","magenta")
  a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
  plot(a, col=color, type="p", xlim=c(-10,20), ylim=c(-10,20))
  par(new=TRUE)
}

for (i in 1:nrow(train_set)) {
  if (is.na(lbls[[i]])) {
    a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
    plot(a, col="black", type="p", xlim=c(-10,20), ylim=c(-10,20))
    par(new=TRUE)
  }
}