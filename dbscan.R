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