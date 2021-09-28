# clusters - labels vector
# point - point to calculate score for
# cluster - point's cluster
# s - distance fn
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

silhuette_cluster <- function(data, clusters, cluster, s) {
  cluster_data <- data[which(clusters %in% cluster),]
  scores <- rep(NaN, nrow(cluster_data))
  
  for (i in 1:nrow(cluster_data)) {
    row <- cluster_data[i,]
    scores[i] <- silhuette(data, clusters, row, cluster, s)
  }
  res <- sum(scores) / length(scores)
  return(res)
}

silhuette_data <- function(data, clusters, s) {
  lbls <- unique(clusters)
  scores = rep(NaN, length(lbls))
  for (i in 1:length(lbls)) {
    cluster <- lbls[i]
    scores[i] <- silhuette_cluster(data, clusters, cluster, s)
  }
  # Weighted avg could make more sense in some scenarios but not always
  res <- sum(scores) / length(scores)
  return(res)
}

