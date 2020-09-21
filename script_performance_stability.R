## Estimating internal validation indexes - Stability ----------------------------

## gaussian datasets -------------------------------------------------------------

require(data.table)
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(flexclust) # k-medians
require(kmed) # k-medoids
require(ClusterR)
require(e1071) # Fuzzy c-means
require(clValid)
require(fpc)


# Importing gaussian datasets (80 datasets)
gauss_ds <- list()
names1 = list.files(pattern="*.dat")
for(i in 1:length(names1)){
  gauss_ds[[i]] <- fread(names1[i])
}

# importing number of clusters
nc_kmeans <- readRDS('nc_kmeans.Rds')
nc_median <- readRDS('nc_median.Rds')
nc_centroid <- readRDS('nc_centroid.Rds')
nc_single <- readRDS('nc_single.Rds')
nc_complete <- readRDS('nc_complete.Rds')
nc_average <- readRDS('nc_average.Rds')
nc_ward <- readRDS('nc_ward.Rds')
nc_mcquitty <- readRDS('nc_mcquitty.Rds')

# importing original clusters
clusters1.1 <- readRDS("gauss_kmeans.Rds")
clusters2.1 <- readRDS("gauss_kmedians.Rds")
clusters3.1 <- readRDS("gauss_kmedoids.Rds")
clusters4.1 <- readRDS("gauss_single.Rds")
clusters5.1 <- readRDS("gauss_complete.Rds")
clusters6.1 <- readRDS("gauss_average.Rds")
clusters7.1 <- readRDS("gauss_ward.Rds")
clusters8.1 <- readRDS("gauss_closest.Rds")
clusters10.1 <- readRDS("gauss_mbkmeans.Rds")
clusters11.1 <- readRDS("gauss_fcmeans.Rds")

# 1) k-means 

mds1.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusterDel <- kmeans(matDel, nc_kmeans[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters1.1[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds1.1[[i]] <- colMeans(stab)  
}
saveRDS(mds1.1, "kmeans_gaussian_stability.Rds")

# 2) k-medians 

mds2.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- as.matrix(mat[, -del])
    clusterDel <- kcca(matDel, k = nc_median[[i]], family = kccaFamily("kmedians"), simple = TRUE, control = list(initcent="kmeanspp"))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters2.1[[i]], clusterDel@cluster, method = "euclidian")
  }
  mds2.1[[i]] <- colMeans(stab) 
}
saveRDS(mds2.1, "kmedians_gaussian_stability.Rds")

# 4) k-medoids 

mds3.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusterDel <- fastkmed(dist(matDel), nc_centroid[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters3.1[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds3.1[[i]] <- colMeans(stab)  
}
saveRDS(mds3.1, "kmedoids_gaussian_stability.Rds")

# 4) single linkage 

mds4.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'single')
    clusterDel <- as.matrix(cutree(clusters, nc_single[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters4.1[[i]], clusterDel, method = "euclidian")
  }
  mds4.1[[i]] <- colMeans(stab)  
}
saveRDS(mds4.1, "single_gaussian_stability.Rds")

# 5) complete linkage 

mds5.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'complete')
    clusterDel <- as.matrix(cutree(clusters, nc_complete[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters5.1[[i]], clusterDel, method = "euclidian")
  }
  mds5.1[[i]] <- colMeans(stab)  
}
saveRDS(mds5.1, "complete_gaussian_stability.Rds")

# 6) average linkage 

mds6.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'average')
    clusterDel <- as.matrix(cutree(clusters, nc_average[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters6.1[[i]], clusterDel, method = "euclidian")
  }
  mds6.1[[i]] <- colMeans(stab)  
}
saveRDS(mds6.1, "average_gaussian_stability.Rds")

# 7) Ward's method 

mds7.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'ward.D')
    clusterDel <- as.matrix(cutree(clusters, nc_ward[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters7.1[[i]], clusterDel, method = "euclidian")
  }
  mds7.1[[i]] <- colMeans(stab)  
}
saveRDS(mds7.1, "ward_gaussian_stability.Rds")

# 8) closest centroid 

mds8.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'centroid')
    clusterDel <- as.matrix(cutree(clusters, nc_centroid[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters8.1[[i]], clusterDel, method = "euclidian")
  }
  mds8.1[[i]] <- colMeans(stab)  
}
saveRDS(mds8.1, "centroid_gaussian_stability.Rds")

# 10) mini-batch k-means 

mds10.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- MiniBatchKmeans(as.matrix(matDel), clusters = nc_kmeans[[i]])
    clusterDel <- as.matrix(predict_MBatchKMeans(as.matrix(matDel), clusters[[1]], fuzzy = FALSE))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters10.1[[i]], clusterDel, method = "euclidian")
  }
  mds10.1[[i]] <- colMeans(stab)  
}
saveRDS(mds10.1, "mbkmeans_gaussian_stability.Rds")

# 11) fuzzy c-means 

mds11.1 <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  mat <- as.matrix(gauss_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- e1071::cmeans(matDel, nc_kmeans[[i]], 100, verbose = TRUE, dist = "euclidean", method = "cmeans", m = 2)
    clusterDel <- clusters$cluster
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters11.1[[i]], clusterDel, method = "euclidian")
  }
  mds11.1[[i]] <- colMeans(stab)  
}
saveRDS(mds11.1, "fcmeans_gaussian_stability.Rds")



## ellipsoidal datasets -------------------------------------------------------------------------

require(data.table)
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(flexclust) # k-medians
require(kmed) # k-medoids
require(ClusterR)
require(e1071) # Fuzzy c-means
require(clValid)
require(fpc)


# Importing ellipsoidal datasets (80 datasets)
ellips_ds <- list()
names2 = list.files(pattern="*.dat")
for(i in 1:length(names2)){
  ellips_ds[[i]] <- fread(names2[i])
}

# importing number of clusters
nc_kmeans <- readRDS('nc_kmeans2.Rds')
nc_median <- readRDS('nc_median2.Rds')
nc_centroid <- readRDS('nc_centroid2.Rds')
nc_single <- readRDS('nc_single2.Rds')
nc_complete <- readRDS('nc_complete2.Rds')
nc_average <- readRDS('nc_average2.Rds')
nc_ward <- readRDS('nc_ward2.Rds')
nc_mcquitty <- readRDS('nc_mcquitty2.Rds')

clusters1.2 <- readRDS("ellips_kmeans.Rds")
clusters2.2 <- readRDS("ellips_kmedians.Rds")
clusters3.2 <- readRDS("ellips_kmedoids.Rds")
clusters4.2 <- readRDS("ellips_single.Rds")
clusters5.2 <- readRDS("ellips_complete.Rds")
clusters6.2 <- readRDS("ellips_average.Rds")
clusters7.2 <- readRDS("ellips_ward.Rds")
clusters8.2 <- readRDS("ellips_closest.Rds")
clusters10.2 <- readRDS("ellips_mbkmeans.Rds")
clusters11.2 <- readRDS("ellips_fcmeans.Rds")

# 1) k-means 

mds1.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusterDel <- kmeans(matDel, nc_kmeans[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters1.2[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds1.2[[i]] <- colMeans(stab)  
}
saveRDS(mds1.2, "kmeans_ellips_stability.Rds")

# 2) k-medians 

mds2.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- as.matrix(mat[, -del])
    clusterDel <- kcca(matDel, k = nc_median[[i]], family = kccaFamily("kmedians"), simple = TRUE, control = list(initcent="kmeanspp"))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters2.2[[i]], clusterDel@cluster, method = "euclidian")
  }
  mds2.2[[i]] <- colMeans(stab) 
}
saveRDS(mds2.2, "kmedians_ellips_stability.Rds")

# 4) k-medoids 

mds3.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusterDel <- fastkmed(dist(matDel), nc_centroid[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters3.2[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds3.2[[i]] <- colMeans(stab)  
}
saveRDS(mds3.2, "kmedoids_ellips_stability.Rds")

# 4) single linkage 

mds4.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'single')
    clusterDel <- as.matrix(cutree(clusters, nc_single[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters4.2[[i]], clusterDel, method = "euclidian")
  }
  mds4.2[[i]] <- colMeans(stab)  
}
saveRDS(mds4.2, "single_ellips_stability.Rds")

# 5) complete linkage 

mds5.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'complete')
    clusterDel <- as.matrix(cutree(clusters, nc_complete[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters5.2[[i]], clusterDel, method = "euclidian")
  }
  mds5.2[[i]] <- colMeans(stab)  
}
saveRDS(mds5.2, "complete_ellips_stability.Rds")

# 6) average linkage 

mds6.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'average')
    clusterDel <- as.matrix(cutree(clusters, nc_average[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters6.2[[i]], clusterDel, method = "euclidian")
  }
  mds6.2[[i]] <- colMeans(stab)  
}
saveRDS(mds6.2, "average_ellips_stability.Rds")

# 7) Ward's method 

mds7.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'ward.D')
    clusterDel <- as.matrix(cutree(clusters, nc_ward[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters7.2[[i]], clusterDel, method = "euclidian")
  }
  mds7.2[[i]] <- colMeans(stab)  
}
saveRDS(mds7.2, "ward_ellips_stability.Rds")

# 8) closest centroid 

mds8.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'centroid')
    clusterDel <- as.matrix(cutree(clusters, nc_centroid[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters8.2[[i]], clusterDel, method = "euclidian")
  }
  mds8.2[[i]] <- colMeans(stab)  
}
saveRDS(mds8.2, "centroid_ellips_stability.Rds")

# 10) mini-batch k-means 

mds10.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- MiniBatchKmeans(as.matrix(matDel), clusters = nc_kmeans[[i]])
    clusterDel <- as.matrix(predict_MBatchKMeans(as.matrix(matDel), clusters[[1]], fuzzy = FALSE))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters10.2[[i]], clusterDel, method = "euclidian")
  }
  mds10.2[[i]] <- colMeans(stab)  
}
saveRDS(mds10.2, "mbkmeans_ellips_stability.Rds")

# 11) fuzzy c-means 

mds11.2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  mat <- as.matrix(ellips_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- e1071::cmeans(matDel, nc_kmeans[[i]], 100, verbose = FALSE, dist = "euclidean", method = "cmeans", m = 2)
    clusterDel <- clusters$cluster
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters11.2[[i]], clusterDel, method = "euclidian")
  }
  mds11.2[[i]] <- colMeans(stab)  
}
saveRDS(mds11.2, "fcmeans_ellips_stability.Rds")


## small datasets -------------------------------------------------------------------------

require(data.table)
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(flexclust) # k-medians
require(kmed) # k-medoids
require(ClusterR)
require(e1071) # Fuzzy c-means
require(clValid)
require(fpc)

# Importing small datasets (77 datasets)
small_ds <- list()
names3 = list.files(pattern="*.csv")
for(i in 1:length(names3)){
  small_ds[[i]] <- fread(names3[i], colClasses =  'double') 
}

# importing number of clusters
nc_kmeans <- readRDS('nc_kmeans3.Rds')
nc_median <- readRDS('nc_median3.Rds')
nc_centroid <- readRDS('nc_centroid3.Rds')
nc_single <- readRDS('nc_single3.Rds')
nc_complete <- readRDS('nc_complete3.Rds')
nc_average <- readRDS('nc_average3.Rds')
nc_ward <- readRDS('nc_ward3.Rds')
nc_mcquitty <- readRDS('nc_mcquitty3.Rds')

clusters1.3 <- readRDS("small_kmeans.Rds")
clusters2.3 <- readRDS("small_kmedians.Rds")
clusters3.3 <- readRDS("small_kmedoids.Rds")
clusters4.3 <- readRDS("small_single.Rds")
clusters5.3 <- readRDS("small_complete.Rds")
clusters6.3 <- readRDS("small_average.Rds")
clusters7.3 <- readRDS("small_ward.Rds")
clusters8.3 <- readRDS("small_closest.Rds")
clusters10.3 <- readRDS("small_mbkmeans.Rds")
clusters11.3 <- readRDS("small_fcmeans.Rds")

# 1) k-means 

mds1.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusterDel <- kmeans(matDel, nc_kmeans[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters1.3[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds1.3[[i]] <- colMeans(stab)  
}
saveRDS(mds1.3, "kmeans_small_stability.Rds")

# 2) k-medians 

mds2.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- as.matrix(mat[, -del])
    clusterDel <- kcca(matDel, k = nc_median[[i]], family = kccaFamily("kmedians"), simple = TRUE, control = list(initcent="kmeanspp"))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters2.3[[i]], clusterDel@cluster, method = "euclidian")
  }
  mds2.3[[i]] <- colMeans(stab) 
}
saveRDS(mds2.3, "kmedians_small_stability.Rds")

# 3) k-medoids 

mds3.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusterDel <- fastkmed(dist(matDel), nc_centroid[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters3.3[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds3.3[[i]] <- colMeans(stab)  
}
saveRDS(mds3.3, "kmedoids_small_stability.Rds")

# 4) single linkage 

mds4.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'single')
    clusterDel <- as.matrix(cutree(clusters, nc_single[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters4.3[[i]], clusterDel, method = "euclidian")
  }
  mds4.3[[i]] <- colMeans(stab)  
}
saveRDS(mds4.3, "single_small_stability.Rds")

# 5) complete linkage 

mds5.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'complete')
    clusterDel <- as.matrix(cutree(clusters, nc_complete[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters5.3[[i]], clusterDel, method = "euclidian")
  }
  mds5.3[[i]] <- colMeans(stab)  
}
saveRDS(mds5.3, "complete_small_stability.Rds")

# 6) average linkage 

mds6.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'average')
    clusterDel <- as.matrix(cutree(clusters, nc_average[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters6.3[[i]], clusterDel, method = "euclidian")
  }
  mds6.3[[i]] <- colMeans(stab)  
}
saveRDS(mds6.3, "average_small_stability.Rds")

# 7) Ward's method 

mds7.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'ward.D')
    clusterDel <- as.matrix(cutree(clusters, nc_ward[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters7.3[[i]], clusterDel, method = "euclidian")
  }
  mds7.3[[i]] <- colMeans(stab)  
}
saveRDS(mds7.3, "ward_small_stability.Rds")

# 8) closest centroid 

mds8.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'centroid')
    clusterDel <- as.matrix(cutree(clusters, nc_centroid[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters8.3[[i]], clusterDel, method = "euclidian")
  }
  mds8.3[[i]] <- colMeans(stab)  
}
saveRDS(mds8.3, "centroid_small_stability.Rds")

# 10) mini-batch k-means 

mds10.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- MiniBatchKmeans(as.matrix(matDel), clusters = nc_kmeans[[i]])
    clusterDel <- as.matrix(predict_MBatchKMeans(as.matrix(matDel), clusters[[1]], fuzzy = FALSE))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters10.3[[i]], clusterDel, method = "euclidian")
  }
  mds10.3[[i]] <- colMeans(stab)  
}
saveRDS(mds10.3, "mbkmeans_small_stability.Rds")

# 11) fuzzy c-means 

mds11.3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  print(i)
  n[i] <- dim(small_ds[[i]])[2]
  mat <- as.matrix(small_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    matDel <- mat[, -del]
    clusters <- e1071::cmeans(matDel, nc_kmeans[[i]], 100, verbose = TRUE, dist = "euclidean", method = "cmeans", m = 2)
    clusterDel <- clusters$cluster
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters11.3[[i]], clusterDel, method = "euclidian")
  }
  mds11.3[[i]] <- colMeans(stab)  
}
saveRDS(mds11.3, "fcmeans_small_stability.Rds")


## medium datasets -------------------------------------------------------------------------

require(data.table)
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(flexclust) # k-medians
require(kmed) # k-medoids
require(ClusterR)
require(e1071) # Fuzzy c-means
require(clValid)
require(fpc)

# Importing medium datasets (32 datasets)
medium_ds <- list()
names4 = list.files(pattern="*.csv")
for(i in 1:length(names4)){
  medium_ds[[i]] <- fread(names4[i], colClasses =  'double')
}

# importing number of clusters
nc_kmeans <- readRDS('nc_kmeans4.Rds')
nc_median <- readRDS('nc_median4.Rds')
nc_centroid <- readRDS('nc_centroid4.Rds')
nc_single <- readRDS('nc_single4.Rds')
nc_complete <- readRDS('nc_complete4.Rds')
nc_average <- readRDS('nc_average4.Rds')
nc_ward <- readRDS('nc_ward4.Rds')
nc_mcquitty <- readRDS('nc_mcquitty4.Rds')

clusters1.4 <- readRDS("medium_kmeans.Rds")
clusters2.4 <- readRDS("medium_kmedians.Rds")
clusters3.4 <- readRDS("medium_kmedoids.Rds")
clusters4.4 <- readRDS("medium_single.Rds")
clusters5.4 <- readRDS("medium_complete.Rds")
clusters6.4 <- readRDS("medium_average.Rds")
clusters7.4 <- readRDS("medium_ward.Rds")
clusters8.4 <- readRDS("medium_closest.Rds")
clusters10.4 <- readRDS("medium_mbkmeans.Rds")
clusters11.4 <- readRDS("medium_fcmeans.Rds")

# 1) k-means 

mds1.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusterDel <- kmeans(matDel, nc_kmeans[[i]])
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters1.4[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds1.4[[i]] <- colMeans(stab)  
}
saveRDS(mds1.4, "kmeans_medium_stability.Rds")

# 2) k-medians 

mds2.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- as.matrix(mat[, -del])
    clusterDel <- kcca(matDel, k = nc_median[[i]], family = kccaFamily("kmedians"), simple = TRUE, control = list(initcent="kmeanspp"))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters2.4[[i]], clusterDel@cluster, method = "euclidian")
  }
  mds2.4[[i]] <- colMeans(stab) 
}
saveRDS(mds2.4, "kmedians_medium_stability.Rds")

# 3) k-medoids 

mds3.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusterDel <- fastkmed(dist(matDel), nc_centroid[[i]])
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters3.4[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds3.4[[i]] <- colMeans(stab)  
}
saveRDS(mds3.4, "kmedoids_medium_stability.Rds")

# 4) single linkage 

mds4.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'single')
    clusterDel <- as.matrix(cutree(clusters, nc_single[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters4.4[[i]], clusterDel, method = "euclidian")
  }
  mds4.4[[i]] <- colMeans(stab)  
}
saveRDS(mds4.4, "single_medium_stability.Rds")

# 5) complete linkage 

mds5.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'complete')
    clusterDel <- as.matrix(cutree(clusters, nc_complete[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters5.4[[i]], clusterDel, method = "euclidian")
  }
  mds5.4[[i]] <- colMeans(stab)  
}
saveRDS(mds5.4, "complete_medium_stability.Rds")

# 6) average linkage 

mds6.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'average')
    clusterDel <- as.matrix(cutree(clusters, nc_average[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters6.4[[i]], clusterDel, method = "euclidian")
  }
  mds6.4[[i]] <- colMeans(stab)  
}
saveRDS(mds6.4, "average_medium_stability.Rds")

# 7) Ward's method 

mds7.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'ward.D')
    clusterDel <- as.matrix(cutree(clusters, nc_ward[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters7.4[[i]], clusterDel, method = "euclidian")
  }
  mds7.4[[i]] <- colMeans(stab)  
}
saveRDS(mds7.4, "ward_medium_stability.Rds")

# 8) closest centroid 

mds8.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'centroid')
    clusterDel <- as.matrix(cutree(clusters, nc_centroid[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters8.4[[i]], clusterDel, method = "euclidian")
  }
  mds8.4[[i]] <- colMeans(stab)  
}
saveRDS(mds8.4, "centroid_medium_stability.Rds")

# 10) mini-batch k-means 

mds10.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- MiniBatchKmeans(as.matrix(matDel), clusters = nc_kmeans[[i]])
    clusterDel <- as.matrix(predict_MBatchKMeans(as.matrix(matDel), clusters[[1]], fuzzy = FALSE))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters10.4[[i]], clusterDel, method = "euclidian")
  }
  mds10.4[[i]] <- colMeans(stab)  
}
saveRDS(mds10.4, "mbkmeans_medium_stability.Rds")

# 11) fuzzy c-means 

mds11.4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  mat <- as.matrix(medium_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 50, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- e1071::cmeans(matDel, nc_kmeans[[i]], 100, verbose = TRUE, dist = "euclidean", method = "cmeans", m = 2)
    clusterDel <- clusters$cluster
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters11.4[[i]], clusterDel, method = "euclidian")
  }
  mds11.4[[i]] <- colMeans(stab)  
}
saveRDS(mds11.4, "fcmeans_medium_stability.Rds")


## large datasets -------------------------------------------------------------------------

require(data.table)
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(flexclust) # k-medians
require(kmed) # k-medoids
require(ClusterR)
require(e1071) # Fuzzy c-means
require(clValid)
require(fpc)

# Importing large datasets (10 datasets)
large_ds <- list()
names5 = list.files(pattern="*.csv")
for(i in 1:length(names5)){
  large_ds[[i]] <- fread(names5[i], colClasses =  'double')
}

# importing number of clusters
nc_kmeans <- readRDS('nc_kmeans5.Rds')
nc_median <- readRDS('nc_median5.Rds')
nc_centroid <- readRDS('nc_centroid5.Rds')
nc_single <- readRDS('nc_single5.Rds')
nc_complete <- readRDS('nc_complete5.Rds')
nc_average <- readRDS('nc_average5.Rds')
nc_ward <- readRDS('nc_ward5.Rds')
nc_mcquitty <- readRDS('nc_mcquitty5.Rds')

clusters1.5 <- readRDS("large_kmeans.Rds")
clusters2.5 <- readRDS("large_kmedians.Rds")
clusters3.5 <- readRDS("large_kmedoids.Rds")
clusters4.5 <- readRDS("large_single.Rds")
clusters5.5 <- readRDS("large_complete.Rds")
clusters6.5 <- readRDS("large_average.Rds")
clusters7.5 <- readRDS("large_ward.Rds")
clusters8.5 <- readRDS("large_closest.Rds")
clusters10.5 <- readRDS("large_mbkmeans.Rds")
clusters11.5 <- readRDS("large_fcmeans.Rds")

# 1) k-means 

mds1.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusterDel <- kmeans(matDel, nc_kmeans[[i]])
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters1.5[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds1.5[[i]] <- colMeans(stab)  
}
saveRDS(mds1.5, "kmeans_large_stability.Rds")

# 2) k-medians 

mds2.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- as.matrix(mat[, -del])
    clusterDel <- kcca(matDel, k = nc_median[[i]], family = kccaFamily("kmedians"), simple = TRUE, control = list(initcent="kmeanspp"))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters2.5[[i]], clusterDel@cluster, method = "euclidian")
  }
  mds2.5[[i]] <- colMeans(stab) 
}
saveRDS(mds2.5, "kmedians_large_stability.Rds")

# 3) k-medoids 

mds3.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusterDel <- fastkmed(dist(matDel), nc_centroid[[i]])
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters3.5[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds3.5[[i]] <- colMeans(stab)  
}
saveRDS(mds3.5, "kmedoids_large_stability.Rds")

# 4) single linkage 

mds4.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'single')
    clusterDel <- as.matrix(cutree(clusters, nc_single[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters4.5[[i]], clusterDel, method = "euclidian")
  }
  mds4.5[[i]] <- colMeans(stab)  
}
saveRDS(mds4.5, "single_large_stability.Rds")

# 5) complete linkage 

mds5.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'complete')
    clusterDel <- as.matrix(cutree(clusters, nc_complete[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters5.5[[i]], clusterDel, method = "euclidian")
  }
  mds5.5[[i]] <- colMeans(stab)  
}
saveRDS(mds5.5, "complete_large_stability.Rds")

# 6) average linkage 

mds6.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'average')
    clusterDel <- as.matrix(cutree(clusters, nc_average[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters6.5[[i]], clusterDel, method = "euclidian")
  }
  mds6.5[[i]] <- colMeans(stab)  
}
saveRDS(mds6.5, "average_large_stability.Rds")

# 7) Ward's method 

mds7.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'ward.D')
    clusterDel <- as.matrix(cutree(clusters, nc_ward[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters7.5[[i]], clusterDel, method = "euclidian")
  }
  mds7.5[[i]] <- colMeans(stab)  
}
saveRDS(mds7.5, "ward_large_stability.Rds")

# 8) closest centroid 

mds8.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'centroid')
    clusterDel <- as.matrix(cutree(clusters, nc_centroid[[i]]))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters8.5[[i]], clusterDel, method = "euclidian")
  }
  mds8.5[[i]] <- colMeans(stab)  
}
saveRDS(mds8.5, "centroid_large_stability.Rds")

# 10) mini-batch k-means 

mds10.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- MiniBatchKmeans(as.matrix(matDel), clusters = nc_kmeans[[i]])
    clusterDel <- as.matrix(predict_MBatchKMeans(as.matrix(matDel), clusters[[1]], fuzzy = FALSE))
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters10.5[[i]], clusterDel, method = "euclidian")
  }
  mds10.5[[i]] <- colMeans(stab)  
}
saveRDS(mds10.5, "mbkmeans_large_stability.Rds")

# 11) fuzzy c-means 

mds11.5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  mat <- as.matrix(large_ds[[i]][, -(n[i]:n[i])])
  colMat <- c(1:ncol(mat))
  s <- sort(sample(colMat, 15, replace = FALSE))
  stab <- matrix(0, nrow = length(s), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in s){
    print(c(i, match(del, s)))
    matDel <- mat[, -del]
    clusters <- e1071::cmeans(matDel, nc_kmeans[[i]], 100, verbose = TRUE, dist = "euclidean", method = "cmeans", m = 2)
    clusterDel <- clusters$cluster
    stab[match(del, s), ] <- stability(mat, Dist = NULL, del, clusters11.5[[i]], clusterDel, method = "euclidian")
  }
  mds11.5[[i]] <- colMeans(stab)  
}
saveRDS(mds11.5, "fcmeans_large_stability.Rds")


## github datasets -------------------------------------------------------------------------

require(data.table)
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(flexclust) # k-medians
require(kmed) # k-medoids
require(ClusterR)
require(e1071) # Fuzzy c-means
require(clValid)
require(fpc)

# Importing other_datasets - github (101 datasets)
git_ds <- list()
names6 = list.files(pattern="*.txt")
for(i in 1:length(names6)){
  git_ds[[i]] <- fread(names6[i])
}

# importing number of clusters
nc_kmeans <- readRDS('nc_kmeans6.Rds')
nc_median <- readRDS('nc_median6.Rds')
nc_centroid <- readRDS('nc_centroid6.Rds')
nc_single <- readRDS('nc_single6.Rds')
nc_complete <- readRDS('nc_complete6.Rds')
nc_average <- readRDS('nc_average6.Rds')
nc_ward <- readRDS('nc_ward6.Rds')
nc_mcquitty <- readRDS('nc_mcquitty6.Rds')

clusters1.6 <- readRDS("git_kmeans.Rds")
clusters2.6 <- readRDS("git_kmedians.Rds")
clusters3.6 <- readRDS("git_kmedoids.Rds")
clusters4.6 <- readRDS("git_single.Rds")
clusters5.6 <- readRDS("git_complete.Rds")
clusters6.6 <- readRDS("git_average.Rds")
clusters7.6 <- readRDS("git_ward.Rds")
clusters8.6 <- readRDS("git_closest.Rds")
clusters10.6 <- readRDS("git_mbkmeans.Rds")
clusters11.6 <- readRDS("git_fcmeans.Rds")


# 1) k-means 

mds1.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusterDel <- kmeans(matDel, nc_kmeans[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters1.6[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds1.6[[i]] <- colMeans(stab)  
}
saveRDS(mds1.6, "kmeans_git_stability.Rds")

# 2) k-medians 

mds2.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- as.matrix(mat[, -del])
    clusterDel <- kcca(matDel, k = nc_median[[i]], family = kccaFamily("kmedians"), simple = TRUE, control = list(initcent="kmeanspp"))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters2.6[[i]], clusterDel@cluster, method = "euclidian")
  }
  mds2.6[[i]] <- colMeans(stab) 
}
saveRDS(mds2.6, "kmedians_git_stability.Rds")

# 4) k-medoids 

mds3.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusterDel <- fastkmed(dist(matDel), nc_centroid[[i]])
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters3.6[[i]], clusterDel[[1]], method = "euclidian")
  }
  mds3.6[[i]] <- colMeans(stab)  
}
saveRDS(mds3.6, "kmedoids_git_stability.Rds")

# 4) single linkage 

mds4.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'single')
    clusterDel <- as.matrix(cutree(clusters, nc_single[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters4.6[[i]], clusterDel, method = "euclidian")
  }
  mds4.6[[i]] <- colMeans(stab)  
}
saveRDS(mds4.6, "single_git_stability.Rds")

# 5) complete linkage 

mds5.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'complete')
    clusterDel <- as.matrix(cutree(clusters, nc_complete[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters5.6[[i]], clusterDel, method = "euclidian")
  }
  mds5.6[[i]] <- colMeans(stab)  
}
saveRDS(mds5.6, "complete_git_stability.Rds")

# 6) average linkage 

mds6.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'average')
    clusterDel <- as.matrix(cutree(clusters, nc_average[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters6.6[[i]], clusterDel, method = "euclidian")
  }
  mds6.6[[i]] <- colMeans(stab)  
}
saveRDS(mds6.6, "average_git_stability.Rds")

# 7) Ward's method 

mds7.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'ward.D')
    clusterDel <- as.matrix(cutree(clusters, nc_ward[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters7.6[[i]], clusterDel, method = "euclidian")
  }
  mds7.6[[i]] <- colMeans(stab)  
}
saveRDS(mds7.6, "ward_git_stability.Rds")

# 8) closest centroid 

mds8.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- hclust(dist(matDel), 'centroid')
    clusterDel <- as.matrix(cutree(clusters, nc_centroid[[i]]))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters8.6[[i]], clusterDel, method = "euclidian")
  }
  mds8.6[[i]] <- colMeans(stab)  
}
saveRDS(mds8.6, "centroid_git_stability.Rds")

# 10) mini-batch k-means 

mds10.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- MiniBatchKmeans(as.matrix(matDel), clusters = nc_kmeans[[i]])
    clusterDel <- as.matrix(predict_MBatchKMeans(as.matrix(matDel), clusters[[1]], fuzzy = FALSE))
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters10.6[[i]], clusterDel, method = "euclidian")
  }
  mds10.6[[i]] <- colMeans(stab)  
}
saveRDS(mds10.6, "mbkmeans_git_stability.Rds")

# 11) fuzzy c-means 

mds11.6 <- list()
n <- vector()
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  mat <- as.matrix(git_ds[[i]][, -(n[i]:n[i])])
  stab <- matrix(0, nrow = ncol(mat), ncol = 4)
  colnames(stab) <- c("APN","AD","ADM","FOM")
  for(del in 1:ncol(mat)){
    print(c(i, del))
    matDel <- mat[, -del]
    clusters <- e1071::cmeans(matDel, nc_kmeans[[i]], 100, verbose = TRUE, dist = "euclidean", method = "cmeans", m = 2)
    clusterDel <- clusters$cluster
    stab[del, ] <- stability(mat, Dist = NULL, del, clusters11.6[[i]], clusterDel, method = "euclidian")
  }
  mds11.6[[i]] <- colMeans(stab)  
}
saveRDS(mds11.6, "fcmeans_git_stability.Rds")

