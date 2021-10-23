# Some good reading for slightly more advanced stuff https://towardsdatascience.com/an-illustrative-introduction-to-fishers-linear-discriminant-9484efee15ac

# D - Data matrix
# lbls - labels vector
# feature - feature index to calculate score for
# returns Fishes score for feature
fisher_score_feature <- function(D, lbls, feature) {
  intra_class_var <- c()
  inter_class_var <- c()
  for (lbl in unique(lbls)) {
    lbl_idxes <- which(lbls == lbl)
    D_lbl <- D[lbl_idxes,]
    lbl_mean <- mean(D_lbl[,feature])  # For feature
    D_mean <- mean(D[,feature])        # For feature
    p_lbl <- nrow(D_lbl) / nrow(D)     # Proportion of points with label
    lbl_var <- var(D_lbl[,feature])              # Variance of points with label    
    
    intra_class_var[length(intra_class_var)+1] <- p_lbl * (lbl_mean - D_mean) ** 2
    inter_class_var[length(inter_class_var)+1] <- p_lbl * lbl_var ** 2
  }
  score <- sum(intra_class_var) / sum(inter_class_var)
  return(score)
}

# D - data matrix
# lbls - labels vector
# Returns fisher scores fector for features
fisher_scores <- function(D, lbls) {
  scores <- rep(0, ncol(D))
  for (i in 1:ncol(D)) {
    scores[i] <- fisher_score_feature(D, lbls, i)
  }
  return(scores)
}

fisher_score_main <- function() {
  source("decision_criteria.R")
  
  tennis_data <- t(matrix(c(2,1,0,0,0,
                          2,1,0,1,0,
                          0,1,0,0,1,
                          1,2,0,0,1,
                          1,0,1,0,1,
                          1,0,1,1,0,
                          0,0,1,1,1,
                          2,2,0,0,0,
                          2,0,1,0,1,
                          1,2,1,0,1,
                          2,2,1,1,1,
                          0,2,0,1,1,
                          0,1,1,0,1,
                          1,2,0,1,0), nrow = 5, ncol = 14))
  lbls <- tennis_data[,5]
  D <- tennis_data[,1:4]
  res <- fisher_scores(D, lbls)
  
  print(res)
}

fisher_score_main()