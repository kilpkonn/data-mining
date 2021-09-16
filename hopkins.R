# s - distance function
# r - size of synthetic data 
hopkins <- function(data, s, r) {
  mins <- apply(data, 2, min)
  maxs <- apply(data, 2, max)
  
  dist_matrix_syn <- matrix(nrow = r, ncol = nrow(data))
  
  real_idxes <- sample(1:nrow(data), size = r)
  dist_matrix_real <- data[real_idxes,]
  
  for (a in 1:r) {
    v <- c()
    for (j in 1:ncol(data)) {
      v[[j]] <- runif(1, mins[j], maxs[j])
    }
    for (i in 1:nrow(data)) {
      dist_matrix_syn[a, i] <- s(v, data[i,])
    }
  }
 distances_syn <- apply(dist_matrix_syn, 1, min) 
 distances_real <- apply(dist_matrix_real, 1, min)
 
 B <- sum(distances_syn)
 A <- sum(distances_real)

 h <- B / (A + B)
 return(h)
}
