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

lbls <- agglomerative_clustering(train_set, dist_fn, mean)

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