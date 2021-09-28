# k - amount of clusters
# s - distance fn
# epsilon - max centroids change for stopping
kmeans <- function (data, k, s, epsilon) {
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
  
  deltas = c(rep(0, 100))
  deltas[1] <- Inf
  
  # Run max 100 times
  for (i in 2:100) {
    
    # Assign labels to points
    label_counts <- matrix(0, nrow = k, ncol = 1)
    for (r in 1:nrow(data)) {
      p <- data[r,]
      
      distances <- matrix(nrow = 1, ncol = k)
      for (j in 1:k) {
        c <- centroids[j,]
        distances[[1, j]] <- s(p, c)
      }
      min_idx <- which.min(distances[1,])
      labels[[r]] <- min_idx
      label_counts[[min_idx]] <- label_counts[[min_idx]] + 1
    }
    
    # Calculate new centroids
    new_centroids <- matrix(0, nrow=k, ncol=dimensions)
    for (r in 1:nrow(data)) {
      label <- labels[[r]]
      new_centroids[label,] <- new_centroids[label,] + data[r,] / label_counts[[label]]
    }
    
    # Calculate center movements
    for (j in 1:nrow(centroids)) {
      a <- centroids[j,]
      b <- new_centroids[j,]
      deltas[i] <- deltas[i] + s(a, b)
    }
    centroids <- new_centroids
    if (abs(deltas[i] - deltas[i - 1]) < epsilon) {
      break
    }
  }
  return(labels)
}

