
silhuette <- function(data, clusters, point, cluster, s) {
  d_in <- 0
  d_out <- 0
  for (i in 1:nrow(data)) {
    row <- data[i,]
    clst <- clusters[i]
    if (clst == cluster) {
      d_in <- d_in + s(point, row)
    } else {
      d_out <- d_out + s(point, row)
    }
  }
  res <- (d_out - d_in) / max(d_out, d_in)
}
