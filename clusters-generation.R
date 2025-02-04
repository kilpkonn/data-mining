library(matlib)
library(mvtnorm)

rm(list = ls())

# Labels
labels_count <- 4
labels_names <- 1:labels_count
labels_points <- 500

# Distribution

# Eigen vectors are represented in matrix form where each column
# represents a single eigen vector
# Transpose so it's visually better to see
eig_vecs <- list(t(matrix(c(0.4, 0.2,
                            0.2, 0.8), nrow=2, ncol=2)), 
                 t(matrix(c(0.0, 0.8,
                            0.8, 0.3), nrow=2, ncol=2)),
                 t(matrix(c(0.0, 0.4,
                            0.4, 0.0), nrow=2, ncol=2)),
                 t(matrix(c(0.5, 0.3,
                            0.3, 0.6), nrow=2, ncol=2)))

# Eigen values are stored in diagonal matrix for simplicity
eig_vals <- list(matrix(c(8,0,0,26) ,nrow=2, ncol=2),
                 matrix(c(2,0,0,5) ,nrow=2, ncol=2),
                 matrix(c(22,0,0,16) ,nrow=2, ncol=2),
                 matrix(c(5,0,0,8), nrow=2, ncol=2))

# Means for every Gaussian
means <- list(c(3.5, 2),
              c(1, 10),
              c(9, 1),
              c(12, 10))

# Generate Gaussian
data_matrix <- matrix(nrow=labels_count * labels_points, ncol=labels_count)
for (i in seq(labels_count)) {
  eig_vec <- eig_vecs[[i]]  # In matrix form, cols are vectors
  eig_val <- eig_vals[[i]]  # Diagonal matrix of lambdas
  mean <- means[[i]]        # Coordinates in n-dimensional space
  sigma <- eig_vec %*% eig_val %*% t(eig_vec)
  xx <- rmvnorm(n=500, mean=mean, sigma=sigma)  # random multivariate normal distribution function
  color <- switch(i,"red","green","blue","orange");  # Label color
  plot(xx, col=color, type="p", xlim=c(-10,20), ylim=c(-10,20))  # Plot xx
  par(new=TRUE)  # To include the previous plot on the previous = combine plots
  
  # Insert the generated data inside the x matrix and add label
  for (j in seq(along=1:labels_points)){
    data_matrix[(i - 1) * labels_points + j, 1:2] = xx[j,]
    data_matrix[(i - 1) * labels_points + j, 3] = i  # Label
  }
}

# Drop the label (Comment out to keep)
data_matrix <- data_matrix[,1:2]

save(data_matrix, file="./data/gaussians2.RData")
