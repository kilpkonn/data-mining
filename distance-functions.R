library(matlib)

# Distance from vector a to vector b with hyper param p
minkowsky_dist <- function(a, b, p) {
  s <- matrix(0, nrow = 1, ncol = length(a))
  for (i in seq(1:length(a))) {
    s[[1, i]] <- abs(a[[i]] - b[[i]]) ** p
  }
  dist <- sum(s) ** (1 / p)
  return(dist)
}

mahalanobis_dist <- function(a, b, data) {
  C <- cov(data)
  s <- sqrt(t(a - b) %*% inv(C) %*% (a - b))
  return(s)
}

canberra_dist <- function(a, b) {
  s <- matrix(0, nrow = 1, ncol = length(a))
  for (i in seq(1:length(a))) {
    s[[1, i]] <- abs(a[[i]] - b[[i]]) / (abs(a[[i]]) + abs(b[[i]]))
  }
  return(sum(s))
}

cos_dist <- function(a, b) {
  s <- a %*% b / (sqrt(sum(a^2)) + sqrt(sum(b^2)))
  return(s)
}