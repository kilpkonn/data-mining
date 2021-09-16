library(matlib)
library(mvtnorm)

rm(list = ls())

# Labels
labels_count <- 6
labels_names <- 1:labels_count
labels_points <- 500

# Distribution

# Eigen vectors are represented in matrix form where each column
# represents a single eigen vector
# Transpose so it's visually better to see
eig_vecs <- list(t(matrix(c(0.5, 0.0,
                            0.0, 0.5), nrow=2, ncol=2)), 
                 t(matrix(c(0.5, 0.0,
                            0.0, 0.5), nrow=2, ncol=2)),
                 t(matrix(c(0.5, 0.5,
                            0.5, -0.8), nrow=2, ncol=2)),
                 t(matrix(c(0.5, 0.0,
                            0.0, 0.5), nrow=2, ncol=2)),
                 t(matrix(c(0.5, 0.5,
                            0.5, -0.8), nrow=2, ncol=2)),
                 t(matrix(c(0.5, 0.0,
                            0.0, 0.5), nrow=2, ncol=2)))

# Eigen values are stored in diagonal matrix for simplicity
eig_vals <- list(matrix(c(0.6,0,0,5) ,nrow=2, ncol=2),
                 matrix(c(0.5,0,0,5) ,nrow=2, ncol=2),
                 matrix(c(0.5,0,0,2) ,nrow=2, ncol=2),
                 matrix(c(2,0,0,0.6), nrow=2, ncol=2),
                 matrix(c(2,0,0,0.5), nrow=2, ncol=2),
                 matrix(c(0.5,0,0,2), nrow=2, ncol=2))

# Means for every Gaussian
means <- list(c(8, 12),
              c(14, 12),
              c(7, 4),
              c(11, 1),
              c(15, 4),
              c(11, 7))

# Generate Gaussian
data_matrix <- matrix(nrow=labels_count * labels_points, ncol=labels_count)
for (i in seq(labels_count)) {
  eig_vec <- eig_vecs[[i]]  # In matrix form, cols are vectors
  eig_val <- eig_vals[[i]]  # Diagonal matrix of lambdas
  mean <- means[[i]]        # Coordinates in n-dimensional space
  sigma <- eig_vec %*% eig_val %*% t(eig_vec)
  xx <- rmvnorm(n=500, mean=mean, sigma=sigma)  # random multivariate normal distribution function
  color <- switch(i,"red","green","blue","orange","purple","pink","cyan","grey");  # Label color
  plot(xx, col=color, type="p", xlim=c(0,20), ylim=c(0,20))  # Plot xx
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
