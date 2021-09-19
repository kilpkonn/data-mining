library(shotGroups)
library(matlib)

rm(list = ls())

source("distance-functions.R")
source("kmeans.R")

# Load the data, note that variable "data_matrix" will appear!!
load(file="./data/gaussian2.RData")

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

gmm <- function(D, k) {
  mins <- apply(D, 2, min)
  maxs <- apply(D, 2, max)
  d <- ncol(D)
  
  ps <- rep(1/k, k)
  means <- list()
  sigmas <- list()
  for(i in 1:k) {
    # Create mean
    v <- c()
    for (j in 1:d) {
      v[j] <- runif(1, mins[j], maxs[j])
    }
    means[[i]] <- v
    
    # Create cov matrix
    eig_vec <- matrix(runif(d * d, 0, 1), nrow = d, ncol = d)
    eig_val <- matrix(0, nrow = d, ncol = d)
    for(j in 1:d) {
      eig_val[j, j] <- runif(1, 0, maxs[j] - mins[j])
    }
    sigmas[[i]] <- eig_vec %*% eig_val %*% t(eig_vec)
  }
  
  points <- matrix(0, nrow = nrow(D), ncol = k)
  
  loglik <- rep(NaN, 200)
  loglik[1] <- -Inf
  
  for (q in 2:200) {
    points <- matrix(0, nrow = nrow(D), ncol = k)
    for (i in 1:nrow(D)) {
      for (j in 1:k) {
        row <- D[i,]
        S <- sigmas[[j]]
        mean <- means[[j]]
        likelyhood <- 1 / sqrt((2 * pi)**d * det(S)) * exp(-0.5 * t(row - mean) %*% inv(S) %*% (row - mean))
        points[i, j] <- likelyhood
      }
    }
    
    # Deal with inf values
    points[which(!is.finite(points))] <- 0
    # Update Gaussians
    normalizer <- 0
    for (t in 1:k) {
      normalizer <- normalizer + ps[[t]] * points[,t]
    }
    # Update for next step
    tmp_loglik <- 0
    for (j in 1:k) {
      # Find posterior for k
      ppk <- points[,j]
      pk <- ps[[j]]
      p <- pk * ppk / normalizer
      
      # Find new center
      v <- c(rep(0, d))
      for (t in 1:nrow(D)) {
        v <- v + p[[t]] * D[t,]
      }
      v <- v / sum(p)
      wt <- p / sum(p)
      sigmas[[j]] <- cov.wt(D, wt, center = TRUE)[["cov"]]
      means[[j]] <- v
      
      # Calculate loglik for convergion
      tmp_loglik <- tmp_loglik + sum(pk * log(pk * p))  # pk * (log(pk) + log(p)) ?
    }
    loglik[q] <- tmp_loglik
    print(tmp_loglik)
    if (all(is.finite(loglik[q]), is.finite(loglik[q-1])) && abs(loglik[q] - loglik[q - 1]) < 0.00001) {
      break
    }
  }
  
  normalizer <- 0
  for (t in 1:k) {
    normalizer <- normalizer + ps[[t]] * points[,t]
  }
  soft_lbls <- matrix(nrow = nrow(D), ncol = k)
  for (j in 1:k) {
    # Find posterior for k
    ppk <- points[,j]
    pk <- ps[[j]]
    p <- pk * ppk / normalizer
    soft_lbls[,j] <- p
  }
  lbls <- apply(soft_lbls, 1, which.max)
  return(lbls)
}

lbls <- gmm(train_set, 6)

for (i in 1:nrow(train_set)) {
  color <- switch(lbls[[i]],"red","green","blue","orange","magenta","cyan","purple","pink")
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