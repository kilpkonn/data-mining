# s - distance function
# l - linkage function
agglomerative_clustering <- function(data, s, l) {
  # Keep whole hierarchy here
  labels <- matrix(1:nrow(data), nrow = nrow(data), ncol = nrow(data))
  
  similarity_matrix <- matrix(nrow = nrow(data), ncol = nrow(data))
  
  for (i in 1:(nrow(data)-1)) {
    for (j in 1:nrow(data)) {
      row1 <- data[i,]
      row2 <- data[j,]
      similarity_matrix[[i, j]] <- s(row1, row2)
    }
  }
  
  for (i in 1:(ncol(labels) - 1)) {
    # TODO: Verify if all are checked
    linkage_matrix <- matrix(nrow = nrow(data), ncol = nrow(data))
    for (cluster1_idx in 1:ncol(similarity_matrix)) {
      for (cluster2_idx in 1:ncol(similarity_matrix)) {
        if (cluster1_idx == cluster2_idx) next
        as <- labels[,i] == cluster1_idx
        bs <- labels[,i] == cluster2_idx
        cluster_distances <- similarity_matrix[as, bs]
        linkage_matrix[[cluster1_idx, cluster2_idx]] <- l(cluster_distances)
      }
    }
    # Find which clusters to merge
    min_row <- which.min(linkage_matrix) %% nrow(linkage_matrix)
    min_col <- which.min(linkage_matrix[min_row,])
    
    for (j in 1:nrow(labels)) {
      if (labels[j,i] == min_col) {
        labels[j, i + 1] <- min_row
      } else {
        labels[j, i + 1] <- labels[j, i]
      }
    }
    
  }
  
  return(labels)
}
