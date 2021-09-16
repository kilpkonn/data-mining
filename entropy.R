library(shotGroups)

rm(list = ls())

source("distance-functions.R")
source("kmeans.R")

# Load the data, note that variable "data_matrix" will appear!!
load(file="./data/gaussians.RData")

sample_size <- floor(0.05 * nrow(data_matrix))

# Set seed to make results reproducible
# set.seed(123)

train_ind <- sample(seq_len(nrow(data_matrix)), size = sample_size)

train_set <- data_matrix[train_ind, ]
test_set <- data_matrix[-train_ind, ]


dist_fn <- function(a, b) {
  res <- minkowsky_dist(a, b, 2)
  return(res)
}

entropy_acc <- function(grid, v, buckets, features, n) {
  if (features == length(v)) {
    m <- grid[t(unlist(v))]
    e <- m / n * log2(m / n) + (1 - m / n) * log2(1 - m / n)
    return(-e)
  }
  s <- 0
  for (i in 1:buckets) {
    tmp <- v
    tmp[[length(v) + 1]] <- i
    e <- entropy_acc(grid, tmp, buckets, features, n)
    if (!is.na(e)) {
      s <- s + e
    }
  }
  return(s)
}

# selected - vector of data columns to select
entropy <- function(data, selected, buckets) {
  mins <- apply(data, 2, min)
  maxs <- apply(data, 2, max) + c(0.00000001, length(selected))
  
  grid <- array(0, dim = rep(buckets, length(selected)))
  
  for (i in 1:nrow(data)) {
   row <- data[i,]
   v <- buckets * (row - mins) / (maxs - mins)
   v <- v + c(rep(1, length(selected)))
   v <- floor(v)
   grid[t(v)] <- grid[t(v)] + 1
  }
  total <- nrow(data)
  res <- entropy_acc(grid, c(), buckets, length(selected), total)
  return(res)
}
lbls <- entropy(train_set, c(1, 2), 7)

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