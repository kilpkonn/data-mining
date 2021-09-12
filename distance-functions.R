
# Distance from vector a to vector b with hyper param p
minkowsky_dist <- function(a, b, p) {
  s <- matrix(0, nrow = 1, ncol = length(a))
  for (i in seq(1:length(a))) {
    s[[1, i]] <- abs(a[[i]] - b[[i]]) ** p
  }
  dist <- sum(s) ** (1 / p)
  return (dist)
}