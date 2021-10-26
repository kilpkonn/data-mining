# D - data matrix
pca <- function(D) {
  sigma <- cov(D)
  eigen_vecs <- eigen(sigma)$vectors
  D_new <- D %*% eigen_vecs
  return(D_new)
}

pca_main <- function() {
  A <- matrix(c(1,2,3,4,4,5,6,6,7,8,7,8), nrow = 3, ncol = 4)
  pca_res <- pca(A)
  print(pca_res)
  
}

pca_main()