library(shotGroups)

rm(list = ls())

source("distance-functions.R")

# Load the data, note that variable "data_matrix" will appear!!
load(file="./data/gaussians.RData")

sample_size <- floor(0.7 * nrow(data_matrix))

# Set seed to make results reproducible
# set.seed(123)

train_ind <- sample(seq_len(nrow(data_matrix)), size = sample_size)

train_set <- data_matrix[train_ind, ]
test_set <- data_matrix[-train_ind, ]

merge_cells <- function(grid, pos, label) {
  for (i in -1:1) {
    # Skip self
    if (i == 0) next
    # Variate in every dimension
    for (c in 1:length(pos)) {
      v <- pos
      v[[c]] <- v[[c]] + i
      for (j in 1:nrow(grid)) {
        nxt <- head(grid[j,], -1)
        if (identical(nxt, v)) {
          if (!any(is.na(nxt)) && is.na(tail(grid[j,], 1))) {
            new <- nxt
            new[[length(new) + 1]] <- label
            grid[j,] <- new
            grid <- merge_cells(grid, nxt, label)
          }
        }
      }
    }
  }
  return(grid)
}

# Make sure there is amount of buckets for each dimesnion!
generic_grid <- function(data, buckets, n) {
  min_vals <- apply(data, 2, min)
  max_vals <- apply(data, 2, max)
  # [x, y, ..., label]
  grid <- matrix(nrow = nrow(data), ncol = (ncol(data) + 1))
  
  for (i in 1:nrow(data)) {
    row <- data[i,]
    v <- buckets * (row - min_vals) / (max_vals - min_vals)
    v <- floor(v)
    v[[length(v) + 1]] <- NaN  # Add label
    grid[i,] <- v
  }
  
  for (i in 1:nrow(grid)) {
    row <- grid[i,]
    cnt <- 0
    for (j in 1:nrow(grid)) {
      other <- grid[j,]
      if (identical(row, other)) {
        cnt <- cnt + 1
      }
    }
    if (cnt < n) {
      grid[i,] <- rep(NaN, ncol(grid))
    }
  }
  
  lbl <- 1
  for (i in 1:nrow(grid)) {
    row <- grid[i,]
    if (is.na(row[1]) || !is.na(tail(row, 1))) next
    grid <- merge_cells(grid, head(row, -1), lbl)
    lbl <- lbl + 1
  }
  lbls <- matrix(grid[,ncol(grid)], nrow = nrow(grid), ncol = 1)
  return(lbls)
}

dist_fn <- function(a, b) {
  res <- minkowsky_dist(a, b, 2)
  return(res)
}

lbls = generic_grid(train_set, c(18, 18), 7)

for (i in 1:nrow(test_set)) {
  color <- switch(lbls[[i]],"red","green","blue","orange","magenta")
  a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
  plot(a, col=color, type="p", xlim=c(-10,20), ylim=c(-10,20))
  par(new=TRUE)
}

for (i in 1:nrow(test_set)) {
  if (is.na(lbls[[i, 1]])) {
    a <- matrix(train_set[i,], nrow = 1, ncol = ncol(train_set))
    plot(a, col="black", type="p", xlim=c(-10,20), ylim=c(-10,20))
    par(new=TRUE)
  }
}