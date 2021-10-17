
# D - dataset (has to have discretized features)
# lbls - labels / classes vector
#
# returns index of feature what to use for splitting
gini <- function(D, lbls) {
  features <- c()
  for (i in 1:ncol(D)) {
    # Split feature into classes (can be discretized numeric value)
    overall_gini <- 0
    for (feature_clazz in unique(D[,i])) {
      # Indexes of group points that's i-th feature is equal to class feature_clazz
      group_idxes <- which(D[,i] == feature_clazz)
      group_lbls <- lbls[group_idxes]
      
      group_tbl <- table(group_lbls)
      lbls_dist <- as.matrix(group_tbl)[,1] / length(group_idxes)
      
      curr_gini <- sum(lbls_dist * lbls_dist)
      w <- length(group_idxes) / nrow(D)
      
      overall_gini <- overall_gini + w * curr_gini
    }
    features[[length(features) + 1]] <- overall_gini
  }
  best <- which.min(features)
  return(best)
}