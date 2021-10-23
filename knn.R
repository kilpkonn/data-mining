
# point - point to classify
# k - Amount of nearest neighbours to consider
# D - Data matrix
# lbls - labels vector
# s - distance function
# return label for point
knn <- function(point, k, D, lbls, s) {
  distances <- c()
  for (i in 1:nrow(D)) {
    other <- D[i,]
    distances[i] <- s(point, other)
  }
  
  k_nearest_idxes <- order(distances)[1:k]
  k_nearest_lbls <- table(lbls[k_nearest_idxes])
  
  lbl <- names(sort(k_nearest_lbls, decreasing = TRUE))[1]
  lbl <- strtoi(lbl)  # Cuz table has strings as names
  return(lbl)
}

knn_main <- function() {
  library(shotGroups)

  source("distance-functions.R")
  load(file="./data/gaussians2.RData")

  sample_size <- floor(0.7 * nrow(data_matrix))

  # Set seed to make results reproducible
  # set.seed(123)

  train_ind <- sample(seq_len(nrow(data_matrix)), size = sample_size)

  train_set <- data_matrix[train_ind, ]
  train_D <- train_set[,1:2]
  train_lbls <- train_set[,3]
  test_set <- data_matrix[-train_ind, ]
  test_D <- test_set[,1:2]


  dist_fn <- function(a, b) {
    res <- minkowsky_dist(a, b, 2)
    return(res)
  }
  
  lbls <- c()
  for (i in 1:nrow(test_D)) {
    lbls[i] <- knn(test_D[i,], 7, train_D, train_lbls, dist_fn)
  }
  
  color <- rainbow(10)[lbls]
  plot(test_D, col=color, type="p", xlim=c(0,20), ylim=c(0,20), xlab = "", ylab = "")
}

knn_main()