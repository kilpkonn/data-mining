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

dbscan <- function(data, s, n, r) {
  lbls <- matrix(nrow = nrow(data), ncol = 1)
  lbl <- 1
  
  for (j in 1:nrow(data)) {
    cluster = list()
    merged <- FALSE
    
    if (!is.na(lbls[[j, 1]])) next
    
    # Create core if possible
    for (i in 1:nrow(data)) {
      a <- data[j,]
      b <- data[i,]
      x <- s(a, b)
      if (s(a, b) < r) {
        cluster[[length(cluster) + 1]] <- i
      }
    }
    
    # Check if there are enough points for cluster to be formed
    if (length(cluster) > n) {
      merged <- TRUE
    }
    
    while (merged) {
      merged <- FALSE
      for (i in 1:nrow(data)) {
        # Skip if already in cluster
        if (i %in% cluster || !is.na(lbls[[i, 1]])) next
        
        # Check if can be merged to cluster
        p <- data[i,]
        for (idx in cluster) {
          c <- data[idx,]
          if (s(c, p) < r) {
            cluster[[length(cluster) + 1]] <- i
            merged <- TRUE
            break
          }
        }
      }
    }
    
    # Save cluster if enough points exist
    if (length(cluster) > n) {
      for (idx in cluster) {
        lbls[[idx]] <- lbl
      }
      # Clear for next iteration
      lbl <- lbl + 1
      cluster = list()
    }
  }
  
  return(lbls)
}

dist_fn <- function(a, b) {
  res <- minkowsky_dist(a, b, 2)
  return(res)
}

lbls = dbscan(train_set, dist_fn, 6, 0.48)

for (i in 1:nrow(test_set)) {
  color <- switch(lbls[[i]],"red","green","blue","orange","magenta")
  a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
  plot(a, col=color, type="p", xlim=c(-10,20), ylim=c(-10,20))
  par(new=TRUE)
}

for (i in 1:nrow(test_set)) {
  if (is.na(lbls[[i, 1]])) {
    a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
    plot(a, col="black", type="p", xlim=c(-10,20), ylim=c(-10,20))
    par(new=TRUE)
  }
}