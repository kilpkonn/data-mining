library(matlib)

# X - non dependent variables matrix
# y - dependent variable
linear_regression <- function(X, y) {
  y_mean <- mean(y)
  X_mean <- apply(X, 2, mean)
  X <- cbind(rep(1, nrow(X)), X)
  b <- inv(t(X) %*% X) %*% t(X) %*% y
  return(b)
}

linear_regression_main <- function() {
  y <- c(140, 155, 159, 179, 192, 200, 212, 215)
  X <- matrix(c(60, 62, 67, 70, 71, 72, 75, 78,
                22, 25, 24, 20, 15, 14, 14, 11), nrow = 8, ncol = 2)
  b <- linear_regression(X, y)
  print(b)
  
  b_bench <- lm(y ~ V1+V2, data = as.data.frame(X))
  summary(b_bench)
}

linear_regression_main()