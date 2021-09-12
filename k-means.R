library(shotGroups)

rm(list = ls())

source("distance-functions.R")

# Load the data, note that variable "data_matrix" will appear!!
load(file="./data/gaussians.RData")

sample_size <- floor(0.7 * nrow(data_matrix))

# Set seed to make results reproducible
# set.seed(123)

train_ind <- sample(seq_len(nrow(data_matrix)), size = sample_size)

train_set <- data_matrix[train_ind, ]
test_set <- data_matrix[-train_ind, ]

kmeans <- function (data, k, s, n) {
  dimensions <- ncol(data)
  centroids <- matrix(nrow=k, ncol=dimensions)
  labels <- matrix(0, nrow = nrow(data), ncol = 1)
  
  # Generate random centroids
  for (d in seq(1:dimensions)) {
    min_val <- min(data[,d])
    max_val <- max(data[,d])
    
    for (k_i in seq(along=1:k)){
      centroids[k_i, d] <- runif(1, min = min_val, max = max_val)
    }
  }
  
  # Run n times
  for (i in seq(0:n)) {
    
    # Assign labels to points
    label_counts <- matrix(0, nrow = k, ncol = 1)
    for (r in seq(1:nrow(data))) {
      p <- data[r,]
      
      distances <- matrix(nrow = 1, ncol = k)
      for (j in seq(1:k)) {
        c <- centroids[j,]
        distances[[1, j]] <- s(p, c)
      }
      min_idx <- which.min(distances[1,])
      labels[[r]] <- min_idx
      label_counts[[min_idx]] <- label_counts[[min_idx]] + 1
    }
    
    # Calculate new centroids
    centroids <- matrix(0, nrow=k, ncol=dimensions)
    for (r in 1:nrow(data)) {
        label <- labels[[r]]
        centroids[label,] <- centroids[label,] + data[r,] / label_counts[[label]]
    }
  }
  return(labels)
}

dist_fn <- function(a, b) {
  res <- minkowsky_dist(a, b, 2)
  return(res)
}

lbls = kmeans(train_set, 3, dist_fn, 20)

for (i in seq(1:sample_size )) {
  color <- switch(lbls[[i]],"red","green","blue")
  a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
  plot(a, col=color, type="p", xlim=c(-10,20), ylim=c(-10,20))
  par(new=TRUE)
}
