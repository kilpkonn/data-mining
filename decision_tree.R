library(data.tree)

build_tree_rec <- function(D, lbls, criteria_fn, node) {
  # Check if we can split, R thinks it's better to drop matrix in favour of vec on last step
  if (!is.matrix(D) || length(unique(lbls)) == 1) {
    return(node)
  }
  split_on <- criteria_fn(D, lbls)
  splitted_classes <- unique(D[,split_on])
  for (clazz in splitted_classes) {
    clazz_idxes <- which(D[,split_on] == clazz)
    clazz_D <- D[clazz_idxes, 1:ncol(D) != split_on]
    clazz_lbls <- lbls[clazz_idxes]
    
    decision <- "?"
    if (length(unique(clazz_lbls)) == 1) {
      decision <- clazz_lbls[1]
    }
    
    node_name <- paste("Feat:", split_on, "cls:", clazz, "decision:", decision)
    child <- build_tree_rec(clazz_D, clazz_lbls, criteria_fn, Node$new(node_name))
    node$AddChildNode(child)
  }
  return(node)
}

# D - data
# lbls - Labels for data
# criteria_fn - Decision criteria function, has to return feature index to split on
build_decision_tree <- function(D, lbls, criteria_fn) {
  tree <- build_tree_rec(D, lbls, criteria_fn, Node$new("Root"))
  return(tree)
}

decision_tree_main <- function() {
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
  res <- build_decision_tree(D, lbls, entropy)
  
  print(res)
}

decision_tree_main()