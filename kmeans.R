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
