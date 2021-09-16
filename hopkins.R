library(shotGroups)

rm(list = ls())

source("distance-functions.R")
source("kmeans.R")

# Load the data, note that variable "data_matrix" will appear!!
load(file="./data/gaussians2.RData")

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

# s - distance function
# r - size of synthetic data 
hopkins <- function(data, s, r) {
  mins <- apply(data, 2, min)
  maxs <- apply(data, 2, max)
  
  dist_matrix_syn <- matrix(nrow = r, ncol = nrow(data))
  # dist_matrix_real <- matrix(nrow = r, ncol = nrow(data))
  
  real_idxes <- sample(1:nrow(data), size = r)
  dist_matrix_real <- data[real_idxes,]
  
  for (a in 1:r) {
    v <- c()
    for (j in 1:ncol(data)) {
      v[[j]] <- runif(1, mins[j], maxs[j])
    }
    for (i in 1:nrow(data)) {
      dist_matrix_syn[a, i] <- s(v, data[i,])
    }
  }
 distances_syn <- apply(dist_matrix_syn, 1, min) 
 distances_real <- apply(dist_matrix_real, 1, min)
 
 B <- sum(distances_syn)
 A <- sum(distances_real)

 h <- B / (A + B)
 return(h)
}

lbls <- hopkins(train_set, dist_fn, 20)

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