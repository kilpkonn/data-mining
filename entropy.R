entropy_acc <- function(grid, v, buckets, features, n) {
  if (features == length(v)) {
    m <- grid[t(v)]
    e <- m / n * log2(m / n) + (1 - m / n) * log2(1 - m / n)
    return(-e)
  }
  s <- 0
  for (i in 1:buckets[length(v) + 1]) {
    tmp <- v
    tmp[length(v) + 1] <- i
    e <- entropy_acc(grid, tmp, buckets, features, n)
    if (!is.na(e)) {
      s <- s + e
    }
  }
  return(s)
}

# selected - vector of data columns to select
# buckets - vector of into how many buckets we should split in gived dimension
selected_features_entropy <- function(data, selected, buckets) {
  data <- data[,selected]
  mins <- apply(data, 2, min)
  maxs <- apply(data, 2, max) + c(rep(0.00000001, length(selected)))
  
  grid <- array(0, dim = buckets)
  
  for (i in 1:nrow(data)) {
    row <- data[i,]
    v <- buckets * (row - mins) / (maxs - mins)
    v <- v + c(rep(1, length(selected)))
    v <- floor(v)
    grid[t(v)] <- grid[t(v)] + 1
  }
  total <- nrow(data)
  res <- entropy_acc(grid, c(), buckets, length(selected), total)
  return(res)
}

