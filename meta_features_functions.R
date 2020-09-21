### META-FEATURES FUNCTIONS FOR CLUSTERING PROBLEMS ###

#1 log10 number of examples
log_number_ex <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(log(dim(ds1)[1], 10), 4))
}

#2 log10 number of features
log_number_ftr <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(log(dim(ds1)[2], 10), 4))
}

#3 ratio of the number of examples to features
ratio_ex_ftr <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(dim(ds1)[1]/dim(ds1)[2], 4))
}

#4 multivariate normality skewness (MVN package)
mvn_skewness <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  result <- MVN::mvn(data = ds1, mvnTest = "mardia")
  return(round(varhandle::unfactor(result[[1]][1, 2]), 4))
} 

#5 multivariate normality kurtosis (MVN package)
mvn_kurtosis <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  result <- MVN::mvn(data = ds1, mvnTest = "mardia")
  return(round(varhandle::unfactor(result[[1]][2, 2]), 4))
} 

#6 multivariate normality (MVN package)
multi_norm <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  result <- MVN::mvn(data = ds1, mvnTest = "hz", multivariatePlot = "none") 
  return(round(result[[1]][1, 2], 4))
} 

#7 percentage of outliers (distance based) 
perc_out <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dist <- as.vector(dist(ds1))
  results <- summary(dist)
  q1 <- as.numeric(results[2])
  q3 <- as.numeric(results[5])
  d <- q3 - q1
  outliers <- dist[(dist < (q1 - 1.5*d))|(dist > (q3 + 1.5*d))]
  return(round(length(outliers)/length(dist), 4))
} 

#8 average number of points per PCA dimension
avg_pca <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  ds.pca <- summary(prcomp(ds1, center = TRUE, scale. = TRUE))
  results <- ds.pca[[6]]
  for (i in 1:dim(results)[2]){
    if (results[3, i] >= 0.95){
      break
    }
  }
  return(round(dim(ds1)[1]/i, 4))
}  

#9 ratio of the PCA to the original dimension
ratio_pca <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  ds.pca <- summary(prcomp(ds1, center = TRUE, scale. = TRUE))
  results <- ds.pca[[6]]
  for (i in 1:dim(results)[2]){
    if (results[3, i] >= 0.95){
      break
    }
  }
  return(round(i/dim(ds1)[2], 4))
} 

#10 average absolute correlation
avg_abs_cor <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  return(round(mean(abs(cor(ds1))), 4))
}

#11 percentage of points of low distance  
per_low_dist <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dist <- as.vector(dist(ds1))
  low <- dist[dist < (mean(dist) - sd(dist))]
  return(round(length(low)/length(dist), 4))  
}

#12 percentage of points of high distance  
per_high_dist <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dist <- as.vector(dist(ds1))
  high <- dist[dist > (mean(dist) + sd(dist))]
  return(round(length(high)/length(dist), 4))  
}

#13 eigenvalue centrality of minimum spanning tree 
eigen_cent_mst <- function(ds){
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  graph <- igraph::as.undirected(igraph::graph.adjacency(as.matrix(dist(ds1)), weighted=TRUE))
  mst <- igraph::as.undirected(igraph::mst(graph))
  ec <- igraph::eigen_centrality(mst)
  return(round(ec$value, 4))
}

# epsilon-NN function
enn <- function(ds, e) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- as.matrix(dist(ds1))
  for(i in 1:nrow(ds1)) {
    a <- names(sort(dst[i,])[1:(e+1)])
    b <- rownames(ds1)
    dst[i, setdiff(rownames(ds1), intersect(a, b))] <- 0
  }
  return(dst)
}

#14 network density 
net_dens <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode = "undirected", weighted = TRUE)
  density <- igraph::graph.density(graph)
  return(round(density, 4))
}

#15 clustering coefficient 
clust_coef <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode="undirected", weighted = TRUE)
  clust_coef <- igraph::transitivity(graph, type="global", isolates = "zero")
  return(round(clust_coef, 4))
}

#16 intrinsic dimensionality
intr_dim <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  d <- dist(ds1)
  return(round((mean(d)^2)/(2*sd(d)), 4))
}

#17 contrast
contrast <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  d <- as.matrix(dist(ds1))
  d_max <- vector()
  d_min <- vector()
  rel <- vector()
  for(i in 1:dim(ds1)[1]){
    d_max[i] <- max(d[i, 1:dim(d)[2]])
    x <- d[i, 1:dim(d)[2]]
    d_min[i] <- min(x[x != min(x)]) # selects the smallest after the zero
    rel[i] <- (d_max[i] - d_min[i])/d_min[i]
  }
  return(round(mean(rel), 4))
}

#18 Kleinberg's hub centrality scores
hub_score <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode = "undirected", weighted = TRUE)
  hs <- igraph::hub_score(graph, scale = TRUE)
  return(round(hs$value, 4))
}

#19 average nearest neighbor degree
avg_nnd <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode = "undirected", weighted = TRUE)
  hs <- igraph::knn(graph)
  return(round(mean(hs$knn), 4))
}

#20 Bonacich power centrality scores
power_cent <- function(ds) {
  ds1 <- ds[, 1:(dim(ds)[2]-1)]
  dst <- enn(ds1, 0.15*nrow(ds1))
  graph <- igraph::graph.adjacency(dst, mode = "undirected", weighted = TRUE)
  hs <- igraph::power_centrality(graph)
  norm = (hs - min(hs))/(max(hs) - min(hs))
  return(round(mean(norm), 4))
}
