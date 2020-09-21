### CLUSTERING ALGORITHMS PERFORMANCE MEASUREMENT ###

# Loading packages --------------------------------------------------------

require(data.table)
require(MixGHD) # ARI
require(EMCluster) # probabilistic model-based clustering
require(mclust)
require(dbscan)
require(Gmedian) # k-medians
require(kmed) # k-medoids
require(ClusterR) # mini-batch kmeans
require(ppclust) # Fuzzy c-means
require(inaparc) # Fuzzy c-means
require(clusterCrit) # validation
require(NbClust) # number of clusters 
require(clValid)
library(e1071)

# Importing datasets ------------------------------------------------------

# Importing ellips datasets (80 datasets)
gauss_ds <- list()
names1 = list.files(pattern="*.dat")
for(i in 1:length(names1)){
  gauss_ds[[i]] <- fread(names1[i])
}

# Importing ellipsoidal datasets (80 datasets)
ellips_ds <- list()
names2 = list.files(pattern="*.dat")
for(i in 1:length(names2)){
  ellips_ds[[i]] <- fread(names2[i])
}

# Importing other datasets - small (77 datasets)
small_ds <- list()
names3 = list.files(pattern="*.csv")
for(i in 1:length(names3)){
  small_ds[[i]] <- fread(names3[i], colClasses = 'double')
}

# Importing other datasets - medium (32 datasets)
medium_ds <- list()
names4 = list.files(pattern="*.csv")
for(i in 1:length(names4)){
  medium_ds[[i]] <- fread(names4[i])
}

# Importing other datasets - large (10 datasets)
large_ds <- list()
names5 = list.files(pattern="*.csv")
for(i in 1:length(names5)){
  large_ds[[i]] <- fread(names5[i])
}

# Importing other_datasets - github (101 datasets)
git_ds <- list()
names6 = list.files(pattern="*.txt")
for(i in 1:length(names6)){
  git_ds[[i]] <- fread(names6[i])
}


# Generating number of clusters ------------------------------------------------

# gaussian datasets

nc_kmeans <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'kmeans', index = 'silhouette')
  nc_kmeans[[i]] <- as.integer(result$Best.nc[1])
}

nc_median <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'median', index = 'silhouette')
  nc_median[[i]] <- as.integer(result$Best.nc[1])
}

nc_centroid <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'centroid', index = 'silhouette')
  nc_centroid[[i]] <- as.integer(result$Best.nc[1])
}

nc_single <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'single', index = 'silhouette')
  nc_single[[i]] <- as.integer(result$Best.nc[1])
}

nc_complete <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'complete', index = 'silhouette')
  nc_complete[[i]] <- as.integer(result$Best.nc[1])
}

nc_average <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'average', index = 'silhouette')
  nc_average[[i]] <- as.integer(result$Best.nc[1])
}

nc_ward <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'ward.D', index = 'silhouette')
  nc_ward[[i]] <- as.integer(result$Best.nc[1])
}

nc_mcquitty <- list()
n <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  result <- NbClust(gauss_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'mcquitty', index = 'silhouette')
  nc_mcquitty[[i]] <- as.integer(result$Best.nc[1])
}

saveRDS(nc_kmeans, "nc_kmeans.Rds")
saveRDS(nc_median, "nc_median.Rds")
saveRDS(nc_centroid, "nc_centroid.Rds")
saveRDS(nc_single, "nc_single.Rds")
saveRDS(nc_complete, "nc_complete.Rds")
saveRDS(nc_average, "nc_average.Rds")
saveRDS(nc_ward, "nc_ward.Rds")
saveRDS(nc_mcquitty, "nc_mcquitty.Rds")

# ellipsoidal datasets

nc_kmeans2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'kmeans', index = 'silhouette')
  nc_kmeans2[[i]] <- as.integer(result$Best.nc[1])
}

nc_median2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'median', index = 'silhouette')
  nc_median2[[i]] <- as.integer(result$Best.nc[1])
}

nc_centroid2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'centroid', index = 'silhouette')
  nc_centroid2[[i]] <- as.integer(result$Best.nc[1])
}

nc_single2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'single', index = 'silhouette')
  nc_single2[[i]] <- as.integer(result$Best.nc[1])
}

nc_complete2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'complete', index = 'silhouette')
  nc_complete2[[i]] <- as.integer(result$Best.nc[1])
}

nc_average2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'average', index = 'silhouette')
  nc_average2[[i]] <- as.integer(result$Best.nc[1])
}

nc_ward2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'ward.D', index = 'silhouette')
  nc_ward2[[i]] <- as.integer(result$Best.nc[1])
}

nc_mcquitty2 <- list()
n <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  result <- NbClust(ellips_ds[[i]][, -(n[i]:n[i])], min.nc = 4, max.nc = 40, method = 'mcquitty', index = 'silhouette')
  nc_mcquitty2[[i]] <- as.integer(result$Best.nc[1])
}

saveRDS(nc_kmeans2, "nc_kmeans2.Rds")
saveRDS(nc_median2, "nc_median2.Rds")
saveRDS(nc_centroid2, "nc_centroid2.Rds")
saveRDS(nc_single2, "nc_single2.Rds")
saveRDS(nc_complete2, "nc_complete2.Rds")
saveRDS(nc_average2, "nc_average2.Rds")
saveRDS(nc_ward2, "nc_ward2.Rds")
saveRDS(nc_mcquitty2, "nc_mcquitty2.Rds")

# small datasets

nc_kmeans3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'kmeans', index = 'silhouette')
  nc_kmeans3[[i]] <- as.integer(result$Best.nc[1])
}

nc_median3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'median', index = 'silhouette')
  nc_median3[[i]] <- as.integer(result$Best.nc[1])
}

nc_centroid3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'centroid', index = 'silhouette')
  nc_centroid3[[i]] <- as.integer(result$Best.nc[1])
}

nc_single3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'single', index = 'silhouette')
  nc_single3[[i]] <- as.integer(result$Best.nc[1])
}

nc_complete3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'complete', index = 'silhouette')
  nc_complete3[[i]] <- as.integer(result$Best.nc[1])
}

nc_average3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'average', index = 'silhouette')
  nc_average3[[i]] <- as.integer(result$Best.nc[1])
}

nc_ward3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'ward.D', index = 'silhouette')
  nc_ward3[[i]] <- as.integer(result$Best.nc[1])
}

nc_mcquitty3 <- list()
n <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  result <- NbClust(small_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'mcquitty', index = 'silhouette')
  nc_mcquitty3[[i]] <- as.integer(result$Best.nc[1])
}

saveRDS(nc_kmeans3, "nc_kmeans3.Rds")
saveRDS(nc_median3, "nc_median3.Rds")
saveRDS(nc_centroid3, "nc_centroid3.Rds")
saveRDS(nc_single3, "nc_single3.Rds")
saveRDS(nc_complete3, "nc_complete3.Rds")
saveRDS(nc_average3, "nc_average3.Rds")
saveRDS(nc_ward3, "nc_ward3.Rds")
saveRDS(nc_mcquitty3, "nc_mcquitty3.Rds")


# medium datasets

nc_kmeans4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'kmeans', index = 'silhouette')
  nc_kmeans4[[i]] <- as.integer(result$Best.nc[1])
}

nc_median4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'median', index = 'silhouette')
  nc_median4[[i]] <- as.integer(result$Best.nc[1])
}

nc_centroid4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'centroid', index = 'silhouette')
  nc_centroid4[[i]] <- as.integer(result$Best.nc[1])
}

nc_single4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'single', index = 'silhouette')
  nc_single4[[i]] <- as.integer(result$Best.nc[1])
}

nc_complete4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'complete', index = 'silhouette')
  nc_complete4[[i]] <- as.integer(result$Best.nc[1])
}

nc_average4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'average', index = 'silhouette')
  nc_average4[[i]] <- as.integer(result$Best.nc[1])
}

nc_ward4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'ward.D', index = 'silhouette')
  nc_ward4[[i]] <- as.integer(result$Best.nc[1])
}

nc_mcquitty4 <- list()
n <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  result <- NbClust(medium_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'mcquitty', index = 'silhouette')
  nc_mcquitty4[[i]] <- as.integer(result$Best.nc[1])
}

saveRDS(nc_kmeans4, "nc_kmeans4.Rds")
saveRDS(nc_median4, "nc_median4.Rds")
saveRDS(nc_centroid4, "nc_centroid4.Rds")
saveRDS(nc_single4, "nc_single4.Rds")
saveRDS(nc_complete4, "nc_complete4.Rds")
saveRDS(nc_average4, "nc_average4.Rds")
saveRDS(nc_ward4, "nc_ward4.Rds")
saveRDS(nc_mcquitty4, "nc_mcquitty4.Rds")

# large datasets

nc_kmeans5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'kmeans', index = 'silhouette')
  nc_kmeans5[[i]] <- as.integer(result$Best.nc[1])
}

nc_median5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'median', index = 'silhouette')
  nc_median5[[i]] <- as.integer(result$Best.nc[1])
}

nc_centroid5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'centroid', index = 'silhouette')
  nc_centroid5[[i]] <- as.integer(result$Best.nc[1])
}

nc_single5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'single', index = 'silhouette')
  nc_single5[[i]] <- as.integer(result$Best.nc[1])
}

nc_complete5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'complete', index = 'silhouette')
  nc_complete5[[i]] <- as.integer(result$Best.nc[1])
}

nc_average5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'average', index = 'silhouette')
  nc_average5[[i]] <- as.integer(result$Best.nc[1])
}

nc_ward5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'ward.D', index = 'silhouette')
  nc_ward5[[i]] <- as.integer(result$Best.nc[1])
}

nc_mcquitty5 <- list()
n <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  result <- NbClust(large_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 50, method = 'mcquitty', index = 'silhouette')
  nc_mcquitty5[[i]] <- as.integer(result$Best.nc[1])
}

saveRDS(nc_kmeans5, "nc_kmeans5.Rds")
saveRDS(nc_median5, "nc_median5.Rds")
saveRDS(nc_centroid5, "nc_centroid5.Rds")
saveRDS(nc_single5, "nc_single5.Rds")
saveRDS(nc_complete5, "nc_complete5.Rds")
saveRDS(nc_average5, "nc_average5.Rds")
saveRDS(nc_ward5, "nc_ward5.Rds")
saveRDS(nc_mcquitty5, "nc_mcquitty5.Rds")

# github datasets

nc_kmeans6 <- list()
n <- vector()
print('kmeans')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'kmeans', index = 'silhouette')
  nc_kmeans6[[i]] <- as.integer(result$Best.nc[1])
}

nc_median6 <- list()
n <- vector()
print('kmedians')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'median', index = 'silhouette')
  nc_median6[[i]] <- as.integer(result$Best.nc[1])
}

nc_centroid6 <- list()
n <- vector()
print('centroid')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'centroid', index = 'silhouette')
  nc_centroid6[[i]] <- as.integer(result$Best.nc[1])
}

nc_single6 <- list()
n <- vector()
print('single')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'single', index = 'silhouette')
  nc_single6[[i]] <- as.integer(result$Best.nc[1])
}

nc_complete6 <- list()
n <- vector()
print('complete')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'complete', index = 'silhouette')
  nc_complete6[[i]] <- as.integer(result$Best.nc[1])
}

nc_average6 <- list()
n <- vector()
print('average')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'average', index = 'silhouette')
  nc_average6[[i]] <- as.integer(result$Best.nc[1])
}

nc_ward6 <- list()
n <- vector()
print('ward')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'ward.D', index = 'silhouette')
  nc_ward6[[i]] <- as.integer(result$Best.nc[1])
}

nc_mcquitty6 <- list()
n <- vector()
print('mcquitty')
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  result <- NbClust(git_ds[[i]][, -(n[i]:n[i])], min.nc = 2, max.nc = 40, method = 'mcquitty', index = 'silhouette')
  nc_mcquitty6[[i]] <- as.integer(result$Best.nc[1])
}

saveRDS(nc_kmeans6, "nc_kmeans6.Rds")
saveRDS(nc_median6, "nc_median6.Rds")
saveRDS(nc_centroid6, "nc_centroid6.Rds")
saveRDS(nc_single6, "nc_single6.Rds")
saveRDS(nc_complete6, "nc_complete6.Rds")
saveRDS(nc_average6, "nc_average6.Rds")
saveRDS(nc_ward6, "nc_ward6.Rds")
saveRDS(nc_mcquitty6, "nc_mcquitty6.Rds")


## Generating clusters -----------------------------------------------------------

# Gaussian datasets
nc_kmeans <- readRDS('nc_kmeans.Rds')
nc_median <- readRDS('nc_median.Rds')
nc_centroid <- readRDS('nc_centroid.Rds')
nc_single <- readRDS('nc_single.Rds')
nc_complete <- readRDS('nc_complete.Rds')
nc_average <- readRDS('nc_average.Rds')
nc_ward <- readRDS('nc_ward.Rds')
nc_mcquitty <- readRDS('nc_mcquitty.Rds')

# 1) k-means
mds1.1 <- list()
n <- vector()
time_kmeans_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kmeans(gauss_ds[[i]][, -(n[i]:n[i])], nc_kmeans[[i]])
  t2 <- Sys.time()
  mds1.1[[i]] <- clusters[[1]]
  time_kmeans_gauss[i] <- round(t2-t1, 5)
}

# 2) k-medians
mds2.1 <- list()
n <- vector()
time_kmedians_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kGmedian(gauss_ds[[i]][, -(n[i]:n[i])], nc_median[[i]])
  t2 <- Sys.time()
  mds2.1[[i]] <- clusters[[1]]
  time_kmedians_gauss[i] <- round(t2-t1, 5)
}

# 3) k-medoids 
mds3.1 <- list()
n <- vector()
time_kmedoids_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- fastkmed(dist(gauss_ds[[i]][, -(n[i]:n[i])]), nc_centroid[[i]])
  t2 <- Sys.time()
  mds3.1[[i]] <- clusters[[1]]
  time_kmedoids_gauss[i] <- round(t2-t1, 5)
}

# 4) single linkage
mds4.1 <- list()
n <- vector()
time_single_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.1[[i]] <- as.matrix(cutree(clusters, nc_single[[i]]))
  t2 <- Sys.time()
  time_single_gauss[i] <- round(t2-t1, 5)
}

# 5) complete linkage
mds5.1 <- list()
n <- vector()
time_complete_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.1[[i]] <- as.matrix(cutree(clusters, nc_complete[[i]]))
  t2 <- Sys.time()
  time_complete_gauss[i] <- round(t2-t1, 5)
}

# 6) average linkage
mds6.1 <- list()
n <- vector()
time_average_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.1[[i]] <- as.matrix(cutree(clusters, nc_average[[i]]))
  t2 <- Sys.time()
  time_average_gauss[i] <- round(t2-t1, 5)
}

# 7) Ward's method
mds7.1 <- list()
n <- vector()
time_wards_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.1[[i]] <- as.matrix(cutree(clusters, nc_ward[[i]]))
  t2 <- Sys.time()
  time_wards_gauss[i] <- round(t2-t1, 5)
}

# 8) closest centroid
mds8.1 <- list()
n <- vector()
time_closest_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(gauss_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.1[[i]] <- as.matrix(cutree(clusters, nc_centroid[[i]]))
  t2 <- Sys.time()
  time_closest_gauss[i] <- round(t2-t1, 5)
}

# 9) EM algorithm
mds9.1 <- list()
n <- vector()
time_em_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- Mclust(gauss_ds[[i]][, -(n[i]:n[i])], G = nc_kmeans[[i]])
  t2 <- Sys.time()
  mds9.1[[i]] <- clusters$classification
  time_em_gauss[i] <- round(t2-t1, 5)
}

# 10) mini-batch k-means
mds10.1 <- list()
n <- vector()
time_mbkmeans_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- MiniBatchKmeans(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters = nc_kmeans[[i]])
  mds10.1[[i]] <- predict_MBatchKMeans(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
  t2 <- Sys.time()
  time_mbkmeans_gauss[i] <- round(t2-t1, 5)
}

# 11) fuzzy c-means 
mds11.1 <- list()
n <- vector()
time_fcmeans_gauss <- vector()
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- e1071::cmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans[[i]], iter.max = 100, dist = "euclidean", m = 2)
  mds11.1[[i]] <- clusters$cluster
  t2 <- Sys.time()
  time_fcmeans_gauss[i] <- round(t2-t1, 5)
}

saveRDS(mds1.1, "gauss_kmeans.Rds")
saveRDS(mds2.1, "gauss_kmedians.Rds")
saveRDS(mds3.1, "gauss_kmedoids.Rds")
saveRDS(mds4.1, "gauss_single.Rds")
saveRDS(mds5.1, "gauss_complete.Rds")
saveRDS(mds6.1, "gauss_average.Rds")
saveRDS(mds7.1, "gauss_ward.Rds")
saveRDS(mds8.1, "gauss_closest.Rds")
saveRDS(mds9.1, "gauss_em.Rds")
saveRDS(mds10.1, "gauss_mbkmeans.Rds")
saveRDS(mds11.1, "gauss_fcmeans.Rds")


# ellipsoidal datasets
nc_kmeans2 <- readRDS('nc_kmeans2.Rds')
nc_median2 <- readRDS('nc_median2.Rds')
nc_centroid2 <- readRDS('nc_centroid2.Rds')
nc_single2 <- readRDS('nc_single2.Rds')
nc_complete2 <- readRDS('nc_complete2.Rds')
nc_average2 <- readRDS('nc_average2.Rds')
nc_ward2 <- readRDS('nc_ward2.Rds')
nc_mcquitty2 <- readRDS('nc_mcquitty2.Rds')

# 1) k-means
mds1.2 <- list()
n <- vector()
time_kmeans_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kmeans(ellips_ds[[i]][, -(n[i]:n[i])], nc_kmeans2[[i]])
  t2 <- Sys.time()
  mds1.2[[i]] <- clusters[[1]]
  time_kmeans_ellips[i] <- round(t2-t1, 5)
}

# 2) k-medians
mds2.2 <- list()
n <- vector()
time_kmedians_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kGmedian(ellips_ds[[i]][, -(n[i]:n[i])], nc_median2[[i]])
  t2 <- Sys.time()
  mds2.2[[i]] <- clusters[[1]]
  time_kmedians_ellips[i] <- round(t2-t1, 5)
}

# 3) k-medoids 
mds3.2 <- list()
n <- vector()
time_kmedoids_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- fastkmed(dist(ellips_ds[[i]][, -(n[i]:n[i])]), nc_centroid2[[i]])
  t2 <- Sys.time()
  mds3.2[[i]] <- clusters[[1]]
  time_kmedoids_ellips[i] <- round(t2-t1, 5)
}

# 4) single linkage
mds4.2 <- list()
n <- vector()
time_single_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.2[[i]] <- as.matrix(cutree(clusters, nc_single2[[i]]))
  t2 <- Sys.time()
  time_single_ellips[i] <- round(t2-t1, 5)
}

# 5) complete linkage
mds5.2 <- list()
n <- vector()
time_complete_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.2[[i]] <- as.matrix(cutree(clusters, nc_complete2[[i]]))
  t2 <- Sys.time()
  time_complete_ellips[i] <- round(t2-t1, 5)
}

# 6) average linkage
mds6.2 <- list()
n <- vector()
time_average_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.2[[i]] <- as.matrix(cutree(clusters, nc_average2[[i]]))
  t2 <- Sys.time()
  time_average_ellips[i] <- round(t2-t1, 5)
}

# 7) Ward's method
mds7.2 <- list()
n <- vector()
time_wards_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.2[[i]] <- as.matrix(cutree(clusters, nc_ward2[[i]]))
  t2 <- Sys.time()
  time_wards_ellips[i] <- round(t2-t1, 5)
}

# 8) closest centroid
mds8.2 <- list()
n <- vector()
time_closest_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(ellips_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.2[[i]] <- as.matrix(cutree(clusters, nc_centroid2[[i]]))
  t2 <- Sys.time()
  time_closest_ellips[i] <- round(t2-t1, 5)
}

# 9) EM algorithm
mds9.2 <- list()
n <- vector()
time_em_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- Mclust(ellips_ds[[i]][, -(n[i]:n[i])], G = nc_kmeans2[[i]])
  t2 <- Sys.time()
  mds9.2[[i]] <- clusters$classification
  time_em_ellips[i] <- round(t2-t1, 5)
}

# 10) mini-batch k-means
mds10.2 <- list()
n <- vector()
time_mbkmeans_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- MiniBatchKmeans(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters = nc_kmeans2[[i]])
  mds10.2[[i]] <- predict_MBatchKMeans(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
  t2 <- Sys.time()
  time_mbkmeans_ellips[i] <- round(t2-t1, 5)
}

# 11) fuzzy c-means 
mds11.2 <- list()
n <- vector()
time_fcmeans_ellips <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- e1071::cmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans2[[i]], iter.max = 100, dist = "euclidean", m = 2)
  mds11.2[[i]] <- clusters$cluster
  t2 <- Sys.time()
  time_fcmeans_ellips[i] <- round(t2-t1, 5)
}

saveRDS(mds1.2, "ellips_kmeans.Rds")
saveRDS(mds2.2, "ellips_kmedians.Rds")
saveRDS(mds3.2, "ellips_kmedoids.Rds")
saveRDS(mds4.2, "ellips_single.Rds")
saveRDS(mds5.2, "ellips_complete.Rds")
saveRDS(mds6.2, "ellips_average.Rds")
saveRDS(mds7.2, "ellips_ward.Rds")
saveRDS(mds8.2, "ellips_closest.Rds")
saveRDS(mds9.2, "ellips_em.Rds")
saveRDS(mds10.2, "ellips_mbkmeans.Rds")
saveRDS(mds11.2, "ellips_fcmeans.Rds")


# small datasets
nc_kmeans3 <- readRDS('nc_kmeans3.Rds')
nc_median3 <- readRDS('nc_median3.Rds')
nc_centroid3 <- readRDS('nc_centroid3.Rds')
nc_single3 <- readRDS('nc_single3.Rds')
nc_complete3 <- readRDS('nc_complete3.Rds')
nc_average3 <- readRDS('nc_average3.Rds')
nc_ward3 <- readRDS('nc_ward3.Rds')
nc_mcquitty3 <- readRDS('nc_mcquitty3.Rds')

# 1) k-means
mds1.3 <- list()
n <- vector()
time_kmeans_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kmeans(small_ds[[i]][, -(n[i]:n[i])], nc_kmeans3[[i]])
  t2 <- Sys.time()
  mds1.3[[i]] <- clusters[[1]]
  time_kmeans_small[i] <- round(t2-t1, 5)
}

# 2) k-medians
mds2.3 <- list()
n <- vector()
time_kmedians_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kGmedian(small_ds[[i]][, -(n[i]:n[i])], nc_median3[[i]])
  t2 <- Sys.time()
  mds2.3[[i]] <- clusters[[1]]
  time_kmedians_small[i] <- round(t2-t1, 5)
}

# 3) k-medoids 
mds3.3 <- list()
n <- vector()
time_kmedoids_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- fastkmed(dist(small_ds[[i]][, -(n[i]:n[i])]), nc_centroid3[[i]])
  t2 <- Sys.time()
  mds3.3[[i]] <- clusters[[1]]
  time_kmedoids_small[i] <- round(t2-t1, 5)
}

# 4) single linkage
mds4.3 <- list()
n <- vector()
time_single_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.3[[i]] <- as.matrix(cutree(clusters, nc_single3[[i]]))
  t2 <- Sys.time()
  time_single_small[i] <- round(t2-t1, 5)
}

# 5) complete linkage
mds5.3 <- list()
n <- vector()
time_complete_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.3[[i]] <- as.matrix(cutree(clusters, nc_complete3[[i]]))
  t2 <- Sys.time()
  time_complete_small[i] <- round(t2-t1, 5)
}

# 6) average linkage
mds6.3 <- list()
n <- vector()
time_average_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.3[[i]] <- as.matrix(cutree(clusters, nc_average3[[i]]))
  t2 <- Sys.time()
  time_average_small[i] <- round(t2-t1, 5)
}

# 7) Ward's method
mds7.3 <- list()
n <- vector()
time_wards_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.3[[i]] <- as.matrix(cutree(clusters, nc_ward3[[i]]))
  t2 <- Sys.time()
  time_wards_small[i] <- round(t2-t1, 5)
}

# 8) closest centroid
mds8.3 <- list()
n <- vector()
time_closest_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(small_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.3[[i]] <- as.matrix(cutree(clusters, nc_centroid3[[i]]))
  t2 <- Sys.time()
  time_closest_small[i] <- round(t2-t1, 5)
}

# 9) EM algorithm
mds9.3 <- list()
n <- vector()
time_em_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- Mclust(small_ds[[i]][, -(n[i]:n[i])], G = nc_kmeans3[[i]])
  t2 <- Sys.time()
  mds9.3[[i]] <- clusters$classification
  time_em_small[i] <- round(t2-t1, 5)
}

# 10) mini-batch k-means
mds10.3 <- list()
n <- vector()
time_mbkmeans_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- MiniBatchKmeans(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters = nc_kmeans3[[i]])
  mds10.3[[i]] <- predict_MBatchKMeans(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
  t2 <- Sys.time()
  time_mbkmeans_small[i] <- round(t2-t1, 5)
}

# 11) fuzzy c-means 
mds11.3 <- list()
n <- vector()
time_fcmeans_small <- vector()
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- e1071::cmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans3[[i]], iter.max = 100, dist = "euclidean", m = 2)
  mds11.3[[i]] <- clusters$cluster
  t2 <- Sys.time()
  time_fcmeans_small[i] <- round(t2-t1, 5)
}

saveRDS(mds1.3, "small_kmeans.Rds")
saveRDS(mds2.3, "small_kmedians.Rds")
saveRDS(mds3.3, "small_kmedoids.Rds")
saveRDS(mds4.3, "small_single.Rds")
saveRDS(mds5.3, "small_complete.Rds")
saveRDS(mds6.3, "small_average.Rds")
saveRDS(mds7.3, "small_ward.Rds")
saveRDS(mds8.3, "small_closest.Rds")
saveRDS(mds9.3, "small_em.Rds")
saveRDS(mds10.3, "small_mbkmeans.Rds")
saveRDS(mds11.3, "small_fcmeans.Rds")

saveRDS(time_kmeans_small, "time_kmeans_small.Rds")
saveRDS(time_kmedians_small, "time_kmedians_small.Rds")
saveRDS(time_kmedoids_small, "time_kmedoids_small.Rds")
saveRDS(time_single_small, "time_single_small.Rds")
saveRDS(time_complete_small, "time_complete_small.Rds")
saveRDS(time_average_small, "time_average_small.Rds")
saveRDS(time_wards_small, "time_wards_small.Rds")
saveRDS(time_closest_small, "time_closest_small.Rds")
saveRDS(time_mbkmeans_small, "time_mbkmeans_small.Rds")
saveRDS(time_fcmeans_small, "time_fcmeans_small.Rds")


# medium datasets
nc_kmeans4 <- readRDS('nc_kmeans4.Rds')
nc_median4 <- readRDS('nc_median4.Rds')
nc_centroid4 <- readRDS('nc_centroid4.Rds')
nc_single4 <- readRDS('nc_single4.Rds')
nc_complete4 <- readRDS('nc_complete4.Rds')
nc_average4 <- readRDS('nc_average4.Rds')
nc_ward4 <- readRDS('nc_ward4.Rds')
nc_mcquitty4 <- readRDS('nc_mcquitty4.Rds')

# 1) k-means
mds1.4 <- list()
n <- vector()
time_kmeans_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kmeans(medium_ds[[i]][, -(n[i]:n[i])], nc_kmeans4[[i]])
  t2 <- Sys.time()
  mds1.4[[i]] <- clusters[[1]]
  time_kmeans_medium[i] <- round(t2-t1, 5)
}

# 2) k-medians
mds2.4 <- list()
n <- vector()
time_kmedians_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kGmedian(medium_ds[[i]][, -(n[i]:n[i])], nc_median4[[i]])
  t2 <- Sys.time()
  mds2.4[[i]] <- clusters[[1]]
  time_kmedians_medium[i] <- round(t2-t1, 5)
}

# 3) k-medoids 
mds3.4 <- list()
n <- vector()
time_kmedoids_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- fastkmed(dist(medium_ds[[i]][, -(n[i]:n[i])]), nc_centroid4[[i]])
  t2 <- Sys.time()
  mds3.4[[i]] <- clusters[[1]]
  time_kmedoids_medium[i] <- round(t2-t1, 5)
}

# 4) single linkage
mds4.4 <- list()
n <- vector()
time_single_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.4[[i]] <- as.matrix(cutree(clusters, nc_single4[[i]]))
  t2 <- Sys.time()
  time_single_medium[i] <- round(t2-t1, 5)
}

# 5) complete linkage
mds5.4 <- list()
n <- vector()
time_complete_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.4[[i]] <- as.matrix(cutree(clusters, nc_complete4[[i]]))
  t2 <- Sys.time()
  time_complete_medium[i] <- round(t2-t1, 5)
}

# 6) average linkage
mds6.4 <- list()
n <- vector()
time_average_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.4[[i]] <- as.matrix(cutree(clusters, nc_average4[[i]]))
  t2 <- Sys.time()
  time_average_medium[i] <- round(t2-t1, 5)
}

# 7) Ward's method
mds7.4 <- list()
n <- vector()
time_wards_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.4[[i]] <- as.matrix(cutree(clusters, nc_ward4[[i]]))
  t2 <- Sys.time()
  time_wards_medium[i] <- round(t2-t1, 5)
}

# 8) closest centroid
mds8.4 <- list()
n <- vector()
time_closest_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(medium_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.4[[i]] <- as.matrix(cutree(clusters, nc_centroid4[[i]]))
  t2 <- Sys.time()
  time_closest_medium[i] <- round(t2-t1, 5)
}

# 9) EM algorithm
mds9.4 <- list()
n <- vector()
time_em_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- Mclust(medium_ds[[i]][, -(n[i]:n[i])], G = nc_kmeans4[[i]])
  t2 <- Sys.time()
  mds9.4[[i]] <- clusters$classification
  time_em_medium[i] <- round(t2-t1, 5)
}

# 10) mini-batch k-means
mds10.4 <- list()
n <- vector()
time_mbkmeans_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- MiniBatchKmeans(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters = nc_kmeans4[[i]])
  mds10.4[[i]] <- predict_MBatchKMeans(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
  t2 <- Sys.time()
  time_mbkmeans_medium[i] <- round(t2-t1, 5)
}

# 11) fuzzy c-means 
mds11.4 <- list()
n <- vector()
time_fcmeans_medium <- vector()
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- e1071::cmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans4[[i]], iter.max = 100, dist = "euclidean", m = 2)
  mds11.4[[i]] <- clusters$cluster
  t2 <- Sys.time()
  time_fcmeans_medium[i] <- round(t2-t1, 5)
}

saveRDS(mds1.4, "medium_kmeans.Rds")
saveRDS(mds2.4, "medium_kmedians.Rds")
saveRDS(mds3.4, "medium_kmedoids.Rds")
saveRDS(mds4.4, "medium_single.Rds")
saveRDS(mds5.4, "medium_complete.Rds")
saveRDS(mds6.4, "medium_average.Rds")
saveRDS(mds7.4, "medium_ward.Rds")
saveRDS(mds8.4, "medium_closest.Rds")
saveRDS(mds9.4, "medium_em.Rds")
saveRDS(mds10.4, "medium_mbkmeans.Rds")
saveRDS(mds11.4, "medium_fcmeans.Rds")

saveRDS(time_kmeans_medium, "time_kmeans_medium.Rds")
saveRDS(time_kmedians_medium, "time_kmedians_medium.Rds")
saveRDS(time_kmedoids_medium, "time_kmedoids_medium.Rds")
saveRDS(time_single_medium, "time_single_medium.Rds")
saveRDS(time_complete_medium, "time_complete_medium.Rds")
saveRDS(time_average_medium, "time_average_medium.Rds")
saveRDS(time_wards_medium, "time_wards_medium.Rds")
saveRDS(time_closest_medium, "time_closest_medium.Rds")
saveRDS(time_mbkmeans_medium, "time_mbkmeans_medium.Rds")
saveRDS(time_fcmeans_medium, "time_fcmeans_medium.Rds")


# large datasets
nc_kmeans5 <- readRDS('nc_kmeans5.Rds')
nc_median5 <- readRDS('nc_median5.Rds')
nc_centroid5 <- readRDS('nc_centroid5.Rds')
nc_single5 <- readRDS('nc_single5.Rds')
nc_complete5 <- readRDS('nc_complete5.Rds')
nc_average5 <- readRDS('nc_average5.Rds')
nc_ward5 <- readRDS('nc_ward5.Rds')
nc_mcquitty5 <- readRDS('nc_mcquitty5.Rds')

# 1) k-means
mds1.5 <- list()
n <- vector()
time_kmeans_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kmeans(large_ds[[i]][, -(n[i]:n[i])], nc_kmeans5[[i]])
  t2 <- Sys.time()
  mds1.5[[i]] <- clusters[[1]]
  time_kmeans_large[i] <- round(t2-t1, 5)
}

# 2) k-medians
mds2.5 <- list()
n <- vector()
time_kmedians_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kGmedian(large_ds[[i]][, -(n[i]:n[i])], nc_median5[[i]])
  t2 <- Sys.time()
  mds2.5[[i]] <- clusters[[1]]
  time_kmedians_large[i] <- round(t2-t1, 5)
}

# 3) k-medoids 
mds3.5 <- list()
n <- vector()
time_kmedoids_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- fastkmed(dist(large_ds[[i]][, -(n[i]:n[i])]), nc_centroid5[[i]])
  t2 <- Sys.time()
  mds3.5[[i]] <- clusters[[1]]
  time_kmedoids_large[i] <- round(t2-t1, 5)
}

# 4) single linkage
mds4.5 <- list()
n <- vector()
time_single_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.5[[i]] <- as.matrix(cutree(clusters, nc_single5[[i]]))
  t2 <- Sys.time()
  time_single_large[i] <- round(t2-t1, 5)
}

# 5) complete linkage
mds5.5 <- list()
n <- vector()
time_complete_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.5[[i]] <- as.matrix(cutree(clusters, nc_complete5[[i]]))
  t2 <- Sys.time()
  time_complete_large[i] <- round(t2-t1, 5)
}

# 6) average linkage
mds6.5 <- list()
n <- vector()
time_average_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.5[[i]] <- as.matrix(cutree(clusters, nc_average5[[i]]))
  t2 <- Sys.time()
  time_average_large[i] <- round(t2-t1, 5)
}

# 7) Ward's method
mds7.5 <- list()
n <- vector()
time_wards_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.5[[i]] <- as.matrix(cutree(clusters, nc_ward5[[i]]))
  t2 <- Sys.time()
  time_wards_large[i] <- round(t2-t1, 5)
}

# 8) closest centroid
mds8.5 <- list()
n <- vector()
time_closest_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(large_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.5[[i]] <- as.matrix(cutree(clusters, nc_centroid5[[i]]))
  t2 <- Sys.time()
  time_closest_large[i] <- round(t2-t1, 5)
}

# 9) EM algorithm
mds9.5 <- list()
n <- vector()
time_em_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- Mclust(large_ds[[i]][, -(n[i]:n[i])], G = nc_kmeans5[[i]])
  t2 <- Sys.time()
  mds9.5[[i]] <- clusters$classification
  time_em_large[i] <- round(t2-t1, 5)
}

# 10) mini-batch k-means
mds10.5 <- list()
n <- vector()
time_mbkmeans_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- MiniBatchKmeans(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters = nc_kmeans5[[i]])
  mds10.5[[i]] <- predict_MBatchKMeans(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
  t2 <- Sys.time()
  time_mbkmeans_large[i] <- round(t2-t1, 5)
}

# 11) fuzzy c-means 
mds11.5 <- list()
n <- vector()
time_fcmeans_large <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- e1071::cmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans5[[i]], iter.max = 100, dist = "euclidean", m = 2)
  mds11.5[[i]] <- clusters$cluster
  t2 <- Sys.time()
  time_fcmeans_large[i] <- round(t2-t1, 5)
}

saveRDS(mds1.5, "large_kmeans.Rds")
saveRDS(mds2.5, "large_kmedians.Rds")
saveRDS(mds3.5, "large_kmedoids.Rds")
saveRDS(mds4.5, "large_single.Rds")
saveRDS(mds5.5, "large_complete.Rds")
saveRDS(mds6.5, "large_average.Rds")
saveRDS(mds7.5, "large_ward.Rds")
saveRDS(mds8.5, "large_closest.Rds")
saveRDS(mds9.5, "large_em.Rds")
saveRDS(mds10.5, "large_mbkmeans.Rds")
saveRDS(mds11.5, "large_fcmeans.Rds")

saveRDS(time_kmeans_large, "time_kmeans_large.Rds")
saveRDS(time_kmedians_large, "time_kmedians_large.Rds")
saveRDS(time_kmedoids_large, "time_kmedoids_large.Rds")
saveRDS(time_single_large, "time_single_large.Rds")
saveRDS(time_complete_large, "time_complete_large.Rds")
saveRDS(time_average_large, "time_average_large.Rds")
saveRDS(time_wards_large, "time_wards_large.Rds")
saveRDS(time_closest_large, "time_closest_large.Rds")
saveRDS(time_mbkmeans_large, "time_mbkmeans_large.Rds")
saveRDS(time_fcmeans_large, "time_fcmeans_large.Rds")


# github datasets
nc_kmeans6 <- readRDS('nc_kmeans6.Rds')
nc_median6 <- readRDS('nc_median6.Rds')
nc_centroid6 <- readRDS('nc_centroid6.Rds')
nc_single6 <- readRDS('nc_single6.Rds')
nc_complete6 <- readRDS('nc_complete6.Rds')
nc_average6 <- readRDS('nc_average6.Rds')
nc_ward6 <- readRDS('nc_ward6.Rds')
nc_mcquitty6 <- readRDS('nc_mcquitty6.Rds')

# 1) k-means
mds1.6 <- list()
n <- vector()
time_kmeans_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans6[[i]])
  t2 <- Sys.time()
  mds1.6[[i]] <- clusters[[1]]
  time_kmeans_git[i] <- round(t2-t1, 5)
}

# 2) k-medians
mds2.6 <- list()
n <- vector()
time_kmedians_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- kGmedian(git_ds[[i]][, -(n[i]:n[i])], nc_median6[[i]])
  t2 <- Sys.time()
  mds2.6[[i]] <- clusters[[1]]
  time_kmedians_git[i] <- round(t2-t1, 5)
}

# 3) k-medoids 
mds3.6 <- list()
n <- vector()
time_kmedoids_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- fastkmed(dist(git_ds[[i]][, -(n[i]:n[i])]), nc_centroid6[[i]])
  t2 <- Sys.time()
  mds3.6[[i]] <- clusters[[1]]
  time_kmedoids_git[i] <- round(t2-t1, 5)
}

# 4) single linkage
mds4.6 <- list()
n <- vector()
time_single_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(git_ds[[i]][, -(n[i]:n[i])]), 'single')
  mds4.6[[i]] <- as.matrix(cutree(clusters, nc_single6[[i]]))
  t2 <- Sys.time()
  time_single_git[i] <- round(t2-t1, 5)
}

# 5) complete linkage
mds5.6 <- list()
n <- vector()
time_complete_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(git_ds[[i]][, -(n[i]:n[i])]), 'complete')
  mds5.6[[i]] <- as.matrix(cutree(clusters, nc_complete6[[i]]))
  t2 <- Sys.time()
  time_complete_git[i] <- round(t2-t1, 5)
}

# 6) average linkage
mds6.6 <- list()
n <- vector()
time_average_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(git_ds[[i]][, -(n[i]:n[i])]), 'average')
  mds6.6[[i]] <- as.matrix(cutree(clusters, nc_average6[[i]]))
  t2 <- Sys.time()
  time_average_git[i] <- round(t2-t1, 5)
}

# 7) Ward's method
mds7.6 <- list()
n <- vector()
time_wards_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(git_ds[[i]][, -(n[i]:n[i])]), 'ward.D')
  mds7.6[[i]] <- as.matrix(cutree(clusters, nc_ward6[[i]]))
  t2 <- Sys.time()
  time_wards_git[i] <- round(t2-t1, 5)
}

# 8) closest centroid
mds8.6 <- list()
n <- vector()
time_closest_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- hclust(dist(git_ds[[i]][, -(n[i]:n[i])]), 'centroid')
  mds8.6[[i]] <- as.matrix(cutree(clusters, nc_centroid6[[i]]))
  t2 <- Sys.time()
  time_closest_git[i] <- round(t2-t1, 5)
}

# 9) EM algorithm
mds9.6 <- list()
n <- vector()
time_em_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- Mclust(git_ds[[i]][, -(n[i]:n[i])], G = nc_kmeans6[[i]])
  t2 <- Sys.time()
  mds9.6[[i]] <- clusters$classification
  time_em_git[i] <- round(t2-t1, 5)
}

# 10) mini-batch k-means
mds10.6 <- list()
n <- vector()
time_mbkmeans_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- MiniBatchKmeans(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters = nc_kmeans6[[i]])
  mds10.6[[i]] <- predict_MBatchKMeans(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters[[1]], fuzzy = FALSE)
  t2 <- Sys.time()
  time_mbkmeans_git[i] <- round(t2-t1, 5)
}

# 11) fuzzy c-means 
mds11.6 <- list()
n <- vector()
time_fcmeans_git <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  t1 <- Sys.time()
  clusters <- e1071::cmeans(git_ds[[i]][, -(n[i]:n[i])], nc_kmeans6[[i]], iter.max = 100, dist = "euclidean", m = 2)
  mds11.6[[i]] <- clusters$cluster
  t2 <- Sys.time()
  time_fcmeans_git[i] <- round(t2-t1, 5)
}

saveRDS(mds1.6, "git_kmeans.Rds")
saveRDS(mds2.6, "git_kmedians.Rds")
saveRDS(mds3.6, "git_kmedoids.Rds")
saveRDS(mds4.6, "git_single.Rds")
saveRDS(mds5.6, "git_complete.Rds")
saveRDS(mds6.6, "git_average.Rds")
saveRDS(mds7.6, "git_ward.Rds")
saveRDS(mds8.6, "git_closest.Rds")
saveRDS(mds9.6, "git_em.Rds")
saveRDS(mds10.6, "git_mbkmeans.Rds")
saveRDS(mds11.6, "git_fcmeans.Rds")

saveRDS(time_kmeans_git, "time_kmeans_git.Rds")
saveRDS(time_kmedians_git, "time_kmedians_git.Rds")
saveRDS(time_kmedoids_git, "time_kmedoids_git.Rds")
saveRDS(time_single_git, "time_single_git.Rds")
saveRDS(time_complete_git, "time_complete_git.Rds")
saveRDS(time_average_git, "time_average_git.Rds")
saveRDS(time_wards_git, "time_wards_git.Rds")
saveRDS(time_closest_git, "time_closest_git.Rds")
saveRDS(time_mbkmeans_git, "time_mbkmeans_git.Rds")
saveRDS(time_fcmeans_git, "time_fcmeans_git.Rds")


## Estimating internal validation indexes ----------------------------------------

## gaussian datasets -------------------------------------------------------------

require(data.table)
require(clusterCrit) 
require(clValid)
require(fpc)

# Importing gaussian datasets (80 datasets)
gauss_ds <- list()
names1 = list.files(pattern="*.dat")
for(i in 1:length(names1)){
  gauss_ds[[i]] <- fread(names1[i])
}

# importing number of clusters
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
n <- vector()
kmeans_gaussian_bh <- vector() # Ball-Hall
kmeans_gaussian_ch <- vector() # Calinski-Harabasz
kmeans_gaussian_db <- vector() # Davies-Bouldin
kmeans_gaussian_dunn <- vector() # Dunn
kmeans_gaussian_sd <- vector() # SD Distance
kmeans_gaussian_rl <- vector() # Ratkowsky-Lance
kmeans_gaussian_silh <- vector() # Silhouette
kmeans_gaussian_connectivity <- vector() # Connectivity
kmeans_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), clusters1.1[[i]], c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters1.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmeans_gaussian_bh[i] <- bh$ball_hall
  kmeans_gaussian_ch[i] <- ch$calinski_harabasz
  kmeans_gaussian_db[i] <- db$davies_bouldin
  kmeans_gaussian_dunn[i] <- dunn$dunn
  kmeans_gaussian_sd[i] <- sd$sd_dis
  kmeans_gaussian_rl[i] <- rl$ratkowsky_lance
  kmeans_gaussian_silh[i] <- silh$silhouette
  kmeans_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, clusters1.1[[i]], Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmeans_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(kmeans_gaussian_bh, "kmeans_gaussian_bh.Rds")
saveRDS(kmeans_gaussian_ch, "kmeans_gaussian_ch.Rds")
saveRDS(kmeans_gaussian_db, "kmeans_gaussian_db.Rds")
saveRDS(kmeans_gaussian_dunn, "kmeans_gaussian_dunn.Rds")
saveRDS(kmeans_gaussian_sd, "kmeans_gaussian_sd.Rds")
saveRDS(kmeans_gaussian_rl, "kmeans_gaussian_rl.Rds")
saveRDS(kmeans_gaussian_silh, "kmeans_gaussian_silh.Rds")
saveRDS(kmeans_gaussian_connectivity, "kmeans_gaussian_connectivity.Rds")
saveRDS(kmeans_gaussian_hub, "kmeans_gaussian_hub.Rds")

# 2) k-medians 
n <- vector()
kmedians_gaussian_bh <- vector() # Ball-Hall
kmedians_gaussian_ch <- vector() # Calinski-Harabasz
kmedians_gaussian_db <- vector() # Davies-Bouldin
kmedians_gaussian_dunn <- vector() # Dunn
kmedians_gaussian_sd <- vector() # SD Distance
kmedians_gaussian_rl <- vector() # Ratkowsky-Lance
kmedians_gaussian_silh <- vector() # Silhouette
kmedians_gaussian_connectivity <- vector() # Connectivity
kmedians_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters2.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedians_gaussian_bh[i] <- bh$ball_hall
  kmedians_gaussian_ch[i] <- ch$calinski_harabasz
  kmedians_gaussian_db[i] <- db$davies_bouldin
  kmedians_gaussian_dunn[i] <- dunn$dunn
  kmedians_gaussian_sd[i] <- sd$sd_dis
  kmedians_gaussian_rl[i] <- rl$ratkowsky_lance
  kmedians_gaussian_silh[i] <- silh$silhouette
  kmedians_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters2.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedians_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(kmedians_gaussian_bh, "kmedians_gaussian_bh.Rds")
saveRDS(kmedians_gaussian_ch, "kmedians_gaussian_ch.Rds")
saveRDS(kmedians_gaussian_db, "kmedians_gaussian_db.Rds")
saveRDS(kmedians_gaussian_dunn, "kmedians_gaussian_dunn.Rds")
saveRDS(kmedians_gaussian_sd, "kmedians_gaussian_sd.Rds")
saveRDS(kmedians_gaussian_rl, "kmedians_gaussian_rl.Rds")
saveRDS(kmedians_gaussian_silh, "kmedians_gaussian_silh.Rds")
saveRDS(kmedians_gaussian_connectivity, "kmedians_gaussian_connectivity.Rds")
saveRDS(kmedians_gaussian_hub, "kmedians_gaussian_hub.Rds")

# 3) k-medoids 
n <- vector()
kmedoids_gaussian_bh <- vector() # Ball-Hall
kmedoids_gaussian_ch <- vector() # Calinski-Harabasz
kmedoids_gaussian_db <- vector() # Davies-Bouldin
kmedoids_gaussian_dunn <- vector() # Dunn
kmedoids_gaussian_sd <- vector() # SD Distance
kmedoids_gaussian_rl <- vector() # Ratkowsky-Lance
kmedoids_gaussian_silh <- vector() # Silhouette
kmedoids_gaussian_connectivity <- vector() # Connectivity
kmedoids_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters3.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedoids_gaussian_bh[i] <- bh$ball_hall
  kmedoids_gaussian_ch[i] <- ch$calinski_harabasz
  kmedoids_gaussian_db[i] <- db$davies_bouldin
  kmedoids_gaussian_dunn[i] <- dunn$dunn
  kmedoids_gaussian_sd[i] <- sd$sd_dis
  kmedoids_gaussian_rl[i] <- rl$ratkowsky_lance
  kmedoids_gaussian_silh[i] <- silh$silhouette
  kmedoids_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters3.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedoids_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(kmedoids_gaussian_bh, "kmedoids_gaussian_bh.Rds")
saveRDS(kmedoids_gaussian_ch, "kmedoids_gaussian_ch.Rds")
saveRDS(kmedoids_gaussian_db, "kmedoids_gaussian_db.Rds")
saveRDS(kmedoids_gaussian_dunn, "kmedoids_gaussian_dunn.Rds")
saveRDS(kmedoids_gaussian_sd, "kmedoids_gaussian_sd.Rds")
saveRDS(kmedoids_gaussian_rl, "kmedoids_gaussian_rl.Rds")
saveRDS(kmedoids_gaussian_silh, "kmedoids_gaussian_silh.Rds")
saveRDS(kmedoids_gaussian_connectivity, "kmedoids_gaussian_connectivity.Rds")
saveRDS(kmedoids_gaussian_hub, "kmedoids_gaussian_hub.Rds")

# 4) single linkage 
n <- vector()
single_gaussian_bh <- vector() # Ball-Hall
single_gaussian_ch <- vector() # Calinski-Harabasz
single_gaussian_db <- vector() # Davies-Bouldin
single_gaussian_dunn <- vector() # Dunn
single_gaussian_sd <- vector() # SD Distance
single_gaussian_rl <- vector() # Ratkowsky-Lance
single_gaussian_silh <- vector() # Silhouette
single_gaussian_connectivity <- vector()
single_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters4.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  single_gaussian_bh[i] <- bh$ball_hall
  single_gaussian_ch[i] <- ch$calinski_harabasz
  single_gaussian_db[i] <- db$davies_bouldin
  single_gaussian_dunn[i] <- dunn$dunn
  single_gaussian_sd[i] <- sd$sd_dis
  single_gaussian_rl[i] <- rl$ratkowsky_lance
  single_gaussian_silh[i] <- silh$silhouette
  single_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters4.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  single_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(single_gaussian_bh, "single_gaussian_bh.Rds")
saveRDS(single_gaussian_ch, "single_gaussian_ch.Rds")
saveRDS(single_gaussian_db, "single_gaussian_db.Rds")
saveRDS(single_gaussian_dunn, "single_gaussian_dunn.Rds")
saveRDS(single_gaussian_sd, "single_gaussian_sd.Rds")
saveRDS(single_gaussian_rl, "single_gaussian_rl.Rds")
saveRDS(single_gaussian_silh, "single_gaussian_silh.Rds")
saveRDS(single_gaussian_connectivity, "single_gaussian_connectivity.Rds")
saveRDS(single_gaussian_hub, "single_gaussian_hub.Rds")

# 5) complete linkage 
n <- vector()
complete_gaussian_bh <- vector() # Ball-Hall
complete_gaussian_ch <- vector() # Calinski-Harabasz
complete_gaussian_db <- vector() # Davies-Bouldin
complete_gaussian_dunn <- vector() # Dunn
complete_gaussian_sd <- vector() # SD Distance
complete_gaussian_rl <- vector() # Ratkowsky-Lance
complete_gaussian_silh <- vector() # Silhouette
complete_gaussian_connectivity <- vector() # Connectivity
complete_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters5.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  complete_gaussian_bh[i] <- bh$ball_hall
  complete_gaussian_ch[i] <- ch$calinski_harabasz
  complete_gaussian_db[i] <- db$davies_bouldin
  complete_gaussian_dunn[i] <- dunn$dunn
  complete_gaussian_sd[i] <- sd$sd_dis
  complete_gaussian_rl[i] <- rl$ratkowsky_lance
  complete_gaussian_silh[i] <- silh$silhouette
  complete_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters5.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  complete_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(complete_gaussian_bh, "complete_gaussian_bh.Rds")
saveRDS(complete_gaussian_ch, "complete_gaussian_ch.Rds")
saveRDS(complete_gaussian_db, "complete_gaussian_db.Rds")
saveRDS(complete_gaussian_dunn, "complete_gaussian_dunn.Rds")
saveRDS(complete_gaussian_sd, "complete_gaussian_sd.Rds")
saveRDS(complete_gaussian_rl, "complete_gaussian_rl.Rds")
saveRDS(complete_gaussian_silh, "complete_gaussian_silh.Rds")
saveRDS(complete_gaussian_connectivity, "complete_gaussian_connectivity.Rds")
saveRDS(complete_gaussian_hub, "complete_gaussian_hub.Rds")

# 6) average linkage 
n <- vector()
average_gaussian_bh <- vector() # Ball-Hall
average_gaussian_ch <- vector() # Calinski-Harabasz
average_gaussian_db <- vector() # Davies-Bouldin
average_gaussian_dunn <- vector() # Dunn
average_gaussian_sd <- vector() # SD Distance
average_gaussian_rl <- vector() # Ratkowsky-Lance
average_gaussian_silh <- vector() # Silhouette
average_gaussian_connectivity <- vector() # Connectivity
average_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters6.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  average_gaussian_bh[i] <- bh$ball_hall
  average_gaussian_ch[i] <- ch$calinski_harabasz
  average_gaussian_db[i] <- db$davies_bouldin
  average_gaussian_dunn[i] <- dunn$dunn
  average_gaussian_sd[i] <- sd$sd_dis
  average_gaussian_rl[i] <- rl$ratkowsky_lance
  average_gaussian_silh[i] <- silh$silhouette
  average_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters6.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  average_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(average_gaussian_bh, "average_gaussian_bh.Rds")
saveRDS(average_gaussian_ch, "average_gaussian_ch.Rds")
saveRDS(average_gaussian_db, "average_gaussian_db.Rds")
saveRDS(average_gaussian_dunn, "average_gaussian_dunn.Rds")
saveRDS(average_gaussian_sd, "average_gaussian_sd.Rds")
saveRDS(average_gaussian_rl, "average_gaussian_rl.Rds")
saveRDS(average_gaussian_silh, "average_gaussian_silh.Rds")
saveRDS(average_gaussian_connectivity, "average_gaussian_connectivity.Rds")
saveRDS(average_gaussian_hub, "average_gaussian_hub.Rds")

# 7) Ward's method 
n <- vector()
ward_gaussian_bh <- vector() # Ball-Hall
ward_gaussian_ch <- vector() # Calinski-Harabasz
ward_gaussian_db <- vector() # Davies-Bouldin
ward_gaussian_dunn <- vector() # Dunn
ward_gaussian_sd <- vector() # SD Distance
ward_gaussian_rl <- vector() # Ratkowsky-Lance
ward_gaussian_silh <- vector() # Silhouette
ward_gaussian_connectivity <- vector() # Connectivity
ward_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters7.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  ward_gaussian_bh[i] <- bh$ball_hall
  ward_gaussian_ch[i] <- ch$calinski_harabasz
  ward_gaussian_db[i] <- db$davies_bouldin
  ward_gaussian_dunn[i] <- dunn$dunn
  ward_gaussian_sd[i] <- sd$sd_dis
  ward_gaussian_rl[i] <- rl$ratkowsky_lance
  ward_gaussian_silh[i] <- silh$silhouette
  ward_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters7.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  ward_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(ward_gaussian_bh, "ward_gaussian_bh.Rds")
saveRDS(ward_gaussian_ch, "ward_gaussian_ch.Rds")
saveRDS(ward_gaussian_db, "ward_gaussian_db.Rds")
saveRDS(ward_gaussian_dunn, "ward_gaussian_dunn.Rds")
saveRDS(ward_gaussian_sd, "ward_gaussian_sd.Rds")
saveRDS(ward_gaussian_rl, "ward_gaussian_rl.Rds")
saveRDS(ward_gaussian_silh, "ward_gaussian_silh.Rds")
saveRDS(ward_gaussian_connectivity, "ward_gaussian_connectivity.Rds")
saveRDS(ward_gaussian_hub, "ward_gaussian_hub.Rds")

# 8) closest centroid 
n <- vector()
closest_gaussian_bh <- vector() # Ball-Hall
closest_gaussian_ch <- vector() # Calinski-Harabasz
closest_gaussian_db <- vector() # Davies-Bouldin
closest_gaussian_dunn <- vector() # Dunn
closest_gaussian_sd <- vector() # SD Distance
closest_gaussian_rl <- vector() # Ratkowsky-Lance
closest_gaussian_silh <- vector() # Silhouette
closest_gaussian_connectivity <- vector() # Connectivity
closest_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters8.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  closest_gaussian_bh[i] <- bh$ball_hall
  closest_gaussian_ch[i] <- ch$calinski_harabasz
  closest_gaussian_db[i] <- db$davies_bouldin
  closest_gaussian_dunn[i] <- dunn$dunn
  closest_gaussian_sd[i] <- sd$sd_dis
  closest_gaussian_rl[i] <- rl$ratkowsky_lance
  closest_gaussian_silh[i] <- silh$silhouette
  closest_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters8.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  closest_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(closest_gaussian_bh, "closest_gaussian_bh.Rds")
saveRDS(closest_gaussian_ch, "closest_gaussian_ch.Rds")
saveRDS(closest_gaussian_db, "closest_gaussian_db.Rds")
saveRDS(closest_gaussian_dunn, "closest_gaussian_dunn.Rds")
saveRDS(closest_gaussian_sd, "closest_gaussian_sd.Rds")
saveRDS(closest_gaussian_rl, "closest_gaussian_rl.Rds")
saveRDS(closest_gaussian_silh, "closest_gaussian_silh.Rds")
saveRDS(closest_gaussian_connectivity, "closest_gaussian_connectivity.Rds")
saveRDS(closest_gaussian_hub, "closest_gaussian_hub.Rds")

# 10) mini-batch k-means 
n <- vector()
mbkmeans_gaussian_bh <- vector() # Ball-Hall
mbkmeans_gaussian_ch <- vector() # Calinski-Harabasz
mbkmeans_gaussian_db <- vector() # Davies-Bouldin
mbkmeans_gaussian_dunn <- vector() # Dunn
mbkmeans_gaussian_sd <- vector() # SD Distance
mbkmeans_gaussian_rl <- vector() # Ratkowsky-Lance
mbkmeans_gaussian_silh <- vector() # Silhouette
mbkmeans_gaussian_connectivity <- vector() # Connectivity
mbkmeans_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters10.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  mbkmeans_gaussian_bh[i] <- bh$ball_hall
  mbkmeans_gaussian_ch[i] <- ch$calinski_harabasz
  mbkmeans_gaussian_db[i] <- db$davies_bouldin
  mbkmeans_gaussian_dunn[i] <- dunn$dunn
  mbkmeans_gaussian_sd[i] <- sd$sd_dis
  mbkmeans_gaussian_rl[i] <- rl$ratkowsky_lance
  mbkmeans_gaussian_silh[i] <- silh$silhouette
  mbkmeans_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters10.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  mbkmeans_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(mbkmeans_gaussian_bh, "mbkmeans_gaussian_bh.Rds")
saveRDS(mbkmeans_gaussian_ch, "mbkmeans_gaussian_ch.Rds")
saveRDS(mbkmeans_gaussian_db, "mbkmeans_gaussian_db.Rds")
saveRDS(mbkmeans_gaussian_dunn, "mbkmeans_gaussian_dunn.Rds")
saveRDS(mbkmeans_gaussian_sd, "mbkmeans_gaussian_sd.Rds")
saveRDS(mbkmeans_gaussian_rl, "mbkmeans_gaussian_rl.Rds")
saveRDS(mbkmeans_gaussian_silh, "mbkmeans_gaussian_silh.Rds")
saveRDS(mbkmeans_gaussian_connectivity, "mbkmeans_gaussian_connectivity.Rds")
saveRDS(mbkmeans_gaussian_hub, "mbkmeans_gaussian_hub.Rds")

# 11) fuzzy c-means 
n <- vector()
fcmeans_gaussian_bh <- vector() # Ball-Hall
fcmeans_gaussian_ch <- vector() # Calinski-Harabasz
fcmeans_gaussian_db <- vector() # Davies-Bouldin
fcmeans_gaussian_dunn <- vector() # Dunn
fcmeans_gaussian_sd <- vector() # SD Distance
fcmeans_gaussian_rl <- vector() # Ratkowsky-Lance
fcmeans_gaussian_silh <- vector() # Silhouette
fcmeans_gaussian_connectivity <- vector() # Connectivity
fcmeans_gaussian_hub <- vector() # Hubert
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  bh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.1[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(gauss_ds[[i]]), clusters11.1[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  fcmeans_gaussian_bh[i] <- bh$ball_hall
  fcmeans_gaussian_ch[i] <- ch$calinski_harabasz
  fcmeans_gaussian_db[i] <- db$davies_bouldin
  fcmeans_gaussian_dunn[i] <- dunn$dunn
  fcmeans_gaussian_sd[i] <- sd$sd_dis
  fcmeans_gaussian_rl[i] <- rl$ratkowsky_lance
  fcmeans_gaussian_silh[i] <- silh$silhouette
  fcmeans_gaussian_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters11.1[[i]]), Data = as.matrix(gauss_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  fcmeans_gaussian_hub[i] <- hub$pearsongamma
}

saveRDS(fcmeans_gaussian_bh, "fcmeans_gaussian_bh.Rds")
saveRDS(fcmeans_gaussian_ch, "fcmeans_gaussian_ch.Rds")
saveRDS(fcmeans_gaussian_db, "fcmeans_gaussian_db.Rds")
saveRDS(fcmeans_gaussian_dunn, "fcmeans_gaussian_dunn.Rds")
saveRDS(fcmeans_gaussian_sd, "fcmeans_gaussian_sd.Rds")
saveRDS(fcmeans_gaussian_rl, "fcmeans_gaussian_rl.Rds")
saveRDS(fcmeans_gaussian_silh, "fcmeans_gaussian_silh.Rds")
saveRDS(fcmeans_gaussian_connectivity, "fcmeans_gaussian_connectivity.Rds")
saveRDS(fcmeans_gaussian_hub, "fcmeans_gaussian_hub.Rds")


## ellipsoidal datasets -------------------------------------------------------------------------

require(data.table)
require(clusterCrit) 
require(clValid)
require(fpc)


# Importing ellipsoidal datasets (80 datasets)
ellips_ds <- list()
names2 = list.files(pattern="*.dat")
for(i in 1:length(names2)){
  ellips_ds[[i]] <- fread(names2[i])
}

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
n <- vector()
kmeans_ellips_bh <- vector() # Ball-Hall
kmeans_ellips_ch <- vector() # Calinski-Harabasz
kmeans_ellips_db <- vector() # Davies-Bouldin
kmeans_ellips_dunn <- vector() # Dunn
kmeans_ellips_sd <- vector() # SD Distance
kmeans_ellips_rl <- vector() # Ratkowsky-Lance
kmeans_ellips_silh <- vector() # Silhouette
kmeans_ellips_connectivity <- vector() # Connectivity
kmeans_ellips_hub <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), clusters1.2[[i]], c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters1.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmeans_ellips_bh[i] <- bh$ball_hall
  kmeans_ellips_ch[i] <- ch$calinski_harabasz
  kmeans_ellips_db[i] <- db$davies_bouldin
  kmeans_ellips_dunn[i] <- dunn$dunn
  kmeans_ellips_sd[i] <- sd$sd_dis
  kmeans_ellips_rl[i] <- rl$ratkowsky_lance
  kmeans_ellips_silh[i] <- silh$silhouette
  kmeans_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, clusters1.2[[i]], Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmeans_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(kmeans_ellips_bh, "kmeans_ellips_bh.Rds")
saveRDS(kmeans_ellips_ch, "kmeans_ellips_ch.Rds")
saveRDS(kmeans_ellips_db, "kmeans_ellips_db.Rds")
saveRDS(kmeans_ellips_dunn, "kmeans_ellips_dunn.Rds")
saveRDS(kmeans_ellips_sd, "kmeans_ellips_sd.Rds")
saveRDS(kmeans_ellips_rl, "kmeans_ellips_rl.Rds")
saveRDS(kmeans_ellips_silh, "kmeans_ellips_silh.Rds")
saveRDS(kmeans_ellips_connectivity, "kmeans_ellips_connectivity.Rds")
saveRDS(kmeans_ellips_hub, "kmeans_ellips_hub.Rds")

# 2) k-medians 
n <- vector()
kmedians_ellips_bh <- vector() # Ball-Hall
kmedians_ellips_ch <- vector() # Calinski-Harabasz
kmedians_ellips_db <- vector() # Davies-Bouldin
kmedians_ellips_dunn <- vector() # Dunn
kmedians_ellips_sd <- vector() # SD Distance
kmedians_ellips_rl <- vector() # Ratkowsky-Lance
kmedians_ellips_silh <- vector() # Silhouette
kmedians_ellips_connectivity <- vector()
kmedians_ellips_hub <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters2.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedians_ellips_bh[i] <- bh$ball_hall
  kmedians_ellips_ch[i] <- ch$calinski_harabasz
  kmedians_ellips_db[i] <- db$davies_bouldin
  kmedians_ellips_dunn[i] <- dunn$dunn
  kmedians_ellips_sd[i] <- sd$sd_dis
  kmedians_ellips_rl[i] <- rl$ratkowsky_lance
  kmedians_ellips_silh[i] <- silh$silhouette
  kmedians_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters2.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedians_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(kmedians_ellips_bh, "kmedians_ellips_bh.Rds")
saveRDS(kmedians_ellips_ch, "kmedians_ellips_ch.Rds")
saveRDS(kmedians_ellips_db, "kmedians_ellips_db.Rds")
saveRDS(kmedians_ellips_dunn, "kmedians_ellips_dunn.Rds")
saveRDS(kmedians_ellips_sd, "kmedians_ellips_sd.Rds")
saveRDS(kmedians_ellips_rl, "kmedians_ellips_rl.Rds")
saveRDS(kmedians_ellips_silh, "kmedians_ellips_silh.Rds")
saveRDS(kmedians_ellips_connectivity, "kmedians_ellips_connectivity.Rds")
saveRDS(kmedians_ellips_hub, "kmedians_ellips_hub.Rds")

# 3) k-medoids 
n <- vector()
kmedoids_ellips_bh <- vector() # Ball-Hall
kmedoids_ellips_ch <- vector() # Calinski-Harabasz
kmedoids_ellips_db <- vector() # Davies-Bouldin
kmedoids_ellips_dunn <- vector() # Dunn
kmedoids_ellips_sd <- vector() # SD Distance
kmedoids_ellips_rl <- vector() # Ratkowsky-Lance
kmedoids_ellips_silh <- vector() # Silhouette
kmedoids_ellips_connectivity <- vector()
kmedoids_ellips_hub <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters3.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedoids_ellips_bh[i] <- bh$ball_hall
  kmedoids_ellips_ch[i] <- ch$calinski_harabasz
  kmedoids_ellips_db[i] <- db$davies_bouldin
  kmedoids_ellips_dunn[i] <- dunn$dunn
  kmedoids_ellips_sd[i] <- sd$sd_dis
  kmedoids_ellips_rl[i] <- rl$ratkowsky_lance
  kmedoids_ellips_silh[i] <- silh$silhouette
  kmedoids_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters3.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedoids_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(kmedoids_ellips_bh, "kmedoids_ellips_bh.Rds")
saveRDS(kmedoids_ellips_ch, "kmedoids_ellips_ch.Rds")
saveRDS(kmedoids_ellips_db, "kmedoids_ellips_db.Rds")
saveRDS(kmedoids_ellips_dunn, "kmedoids_ellips_dunn.Rds")
saveRDS(kmedoids_ellips_sd, "kmedoids_ellips_sd.Rds")
saveRDS(kmedoids_ellips_rl, "kmedoids_ellips_rl.Rds")
saveRDS(kmedoids_ellips_silh, "kmedoids_ellips_silh.Rds")
saveRDS(kmedoids_ellips_connectivity, "kmedoids_ellips_connectivity.Rds")
saveRDS(kmedoids_ellips_hub, "kmedoids_ellips_hub.Rds")

# 4) single linkage 
n <- vector()
single_ellips_bh <- vector() # Ball-Hall
single_ellips_ch <- vector() # Calinski-Harabasz
single_ellips_db <- vector() # Davies-Bouldin
single_ellips_dunn <- vector() # Dunn
single_ellips_sd <- vector() # SD Distance
single_ellips_rl <- vector() # Ratkowsky-Lance
single_ellips_silh <- vector() # Silhouette
single_ellips_connectivity <- vector()
single_ellips_hub <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters4.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  single_ellips_bh[i] <- bh$ball_hall
  single_ellips_ch[i] <- ch$calinski_harabasz
  single_ellips_db[i] <- db$davies_bouldin
  single_ellips_dunn[i] <- dunn$dunn
  single_ellips_sd[i] <- sd$sd_dis
  single_ellips_rl[i] <- rl$ratkowsky_lance
  single_ellips_silh[i] <- silh$silhouette
  single_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters4.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  single_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(single_ellips_bh, "single_ellips_bh.Rds")
saveRDS(single_ellips_ch, "single_ellips_ch.Rds")
saveRDS(single_ellips_db, "single_ellips_db.Rds")
saveRDS(single_ellips_dunn, "single_ellips_dunn.Rds")
saveRDS(single_ellips_sd, "single_ellips_sd.Rds")
saveRDS(single_ellips_rl, "single_ellips_rl.Rds")
saveRDS(single_ellips_silh, "single_ellips_silh.Rds")
saveRDS(single_ellips_connectivity, "single_ellips_connectivity.Rds")
saveRDS(single_ellips_hub, "single_ellips_hub.Rds")

# 5) complete linkage 
n <- vector()
complete_ellips_bh <- vector() # Ball-Hall
complete_ellips_ch <- vector() # Calinski-Harabasz
complete_ellips_db <- vector() # Davies-Bouldin
complete_ellips_dunn <- vector() # Dunn
complete_ellips_sd <- vector() # SD Distance
complete_ellips_rl <- vector() # Ratkowsky-Lance
complete_ellips_silh <- vector() # Silhouette
complete_ellips_connectivity <- vector()
complete_ellips_hub <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters5.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  complete_ellips_bh[i] <- bh$ball_hall
  complete_ellips_ch[i] <- ch$calinski_harabasz
  complete_ellips_db[i] <- db$davies_bouldin
  complete_ellips_dunn[i] <- dunn$dunn
  complete_ellips_sd[i] <- sd$sd_dis
  complete_ellips_rl[i] <- rl$ratkowsky_lance
  complete_ellips_silh[i] <- silh$silhouette
  complete_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters5.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  complete_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(complete_ellips_bh, "complete_ellips_bh.Rds")
saveRDS(complete_ellips_ch, "complete_ellips_ch.Rds")
saveRDS(complete_ellips_db, "complete_ellips_db.Rds")
saveRDS(complete_ellips_dunn, "complete_ellips_dunn.Rds")
saveRDS(complete_ellips_sd, "complete_ellips_sd.Rds")
saveRDS(complete_ellips_rl, "complete_ellips_rl.Rds")
saveRDS(complete_ellips_silh, "complete_ellips_silh.Rds")
saveRDS(complete_ellips_connectivity, "complete_ellips_connectivity.Rds")
saveRDS(complete_ellips_hub, "complete_ellips_hub.Rds")

# 6) average linkage 
n <- vector()
average_ellips_bh <- vector() # Ball-Hall
average_ellips_ch <- vector() # Calinski-Harabasz
average_ellips_db <- vector() # Davies-Bouldin
average_ellips_dunn <- vector() # Dunn
average_ellips_sd <- vector() # SD Distance
average_ellips_rl <- vector() # Ratkowsky-Lance
average_ellips_silh <- vector() # Silhouette
average_ellips_connectivity <- vector()
average_ellips_hub <- vector()
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters6.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  average_ellips_bh[i] <- bh$ball_hall
  average_ellips_ch[i] <- ch$calinski_harabasz
  average_ellips_db[i] <- db$davies_bouldin
  average_ellips_dunn[i] <- dunn$dunn
  average_ellips_sd[i] <- sd$sd_dis
  average_ellips_rl[i] <- rl$ratkowsky_lance
  average_ellips_silh[i] <- silh$silhouette
  average_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters6.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  average_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(average_ellips_bh, "average_ellips_bh.Rds")
saveRDS(average_ellips_ch, "average_ellips_ch.Rds")
saveRDS(average_ellips_db, "average_ellips_db.Rds")
saveRDS(average_ellips_dunn, "average_ellips_dunn.Rds")
saveRDS(average_ellips_sd, "average_ellips_sd.Rds")
saveRDS(average_ellips_rl, "average_ellips_rl.Rds")
saveRDS(average_ellips_silh, "average_ellips_silh.Rds")
saveRDS(average_ellips_connectivity, "average_ellips_connectivity.Rds")
saveRDS(average_ellips_hub, "average_ellips_hub.Rds")

# 7) Ward's method 
n <- vector()
ward_ellips_bh <- vector() # Ball-Hall
ward_ellips_ch <- vector() # Calinski-Harabasz
ward_ellips_db <- vector() # Davies-Bouldin
ward_ellips_dunn <- vector() # Dunn
ward_ellips_sd <- vector() # SD Distance
ward_ellips_rl <- vector() # Ratkowsky-Lance
ward_ellips_silh <- vector() # Silhouette
ward_ellips_connectivity <- vector()
ward_ellips_hub <- vector() 
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters7.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  ward_ellips_bh[i] <- bh$ball_hall
  ward_ellips_ch[i] <- ch$calinski_harabasz
  ward_ellips_db[i] <- db$davies_bouldin
  ward_ellips_dunn[i] <- dunn$dunn
  ward_ellips_sd[i] <- sd$sd_dis
  ward_ellips_rl[i] <- rl$ratkowsky_lance
  ward_ellips_silh[i] <- silh$silhouette
  ward_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters7.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  ward_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(ward_ellips_bh, "ward_ellips_bh.Rds")
saveRDS(ward_ellips_ch, "ward_ellips_ch.Rds")
saveRDS(ward_ellips_db, "ward_ellips_db.Rds")
saveRDS(ward_ellips_dunn, "ward_ellips_dunn.Rds")
saveRDS(ward_ellips_sd, "ward_ellips_sd.Rds")
saveRDS(ward_ellips_rl, "ward_ellips_rl.Rds")
saveRDS(ward_ellips_silh, "ward_ellips_silh.Rds")
saveRDS(ward_ellips_connectivity, "ward_ellips_connectivity.Rds")
saveRDS(ward_ellips_hub, "ward_ellips_hub.Rds")

# 8) closest centroid 
n <- vector()
closest_ellips_bh <- vector() # Ball-Hall
closest_ellips_ch <- vector() # Calinski-Harabasz
closest_ellips_db <- vector() # Davies-Bouldin
closest_ellips_dunn <- vector() # Dunn
closest_ellips_sd <- vector() # SD Distance
closest_ellips_rl <- vector() # Ratkowsky-Lance
closest_ellips_silh <- vector() # Silhouette
closest_ellips_connectivity <- vector()
closest_ellips_hub <- vector() 
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters8.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  closest_ellips_bh[i] <- bh$ball_hall
  closest_ellips_ch[i] <- ch$calinski_harabasz
  closest_ellips_db[i] <- db$davies_bouldin
  closest_ellips_dunn[i] <- dunn$dunn
  closest_ellips_sd[i] <- sd$sd_dis
  closest_ellips_rl[i] <- rl$ratkowsky_lance
  closest_ellips_silh[i] <- silh$silhouette
  closest_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters8.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  closest_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(closest_ellips_bh, "closest_ellips_bh.Rds")
saveRDS(closest_ellips_ch, "closest_ellips_ch.Rds")
saveRDS(closest_ellips_db, "closest_ellips_db.Rds")
saveRDS(closest_ellips_dunn, "closest_ellips_dunn.Rds")
saveRDS(closest_ellips_sd, "closest_ellips_sd.Rds")
saveRDS(closest_ellips_rl, "closest_ellips_rl.Rds")
saveRDS(closest_ellips_silh, "closest_ellips_silh.Rds")
saveRDS(closest_ellips_connectivity, "closest_ellips_connectivity.Rds")
saveRDS(closest_ellips_hub, "closest_ellips_hub.Rds")

# 10) mini-batch k-means 
n <- vector()
mbkmeans_ellips_bh <- vector() # Ball-Hall
mbkmeans_ellips_ch <- vector() # Calinski-Harabasz
mbkmeans_ellips_db <- vector() # Davies-Bouldin
mbkmeans_ellips_dunn <- vector() # Dunn
mbkmeans_ellips_sd <- vector() # SD Distance
mbkmeans_ellips_rl <- vector() # Ratkowsky-Lance
mbkmeans_ellips_silh <- vector() # Silhouette
mbkmeans_ellips_connectivity <- vector()
mbkmeans_ellips_hub <- vector() 
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters10.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  mbkmeans_ellips_bh[i] <- bh$ball_hall
  mbkmeans_ellips_ch[i] <- ch$calinski_harabasz
  mbkmeans_ellips_db[i] <- db$davies_bouldin
  mbkmeans_ellips_dunn[i] <- dunn$dunn
  mbkmeans_ellips_sd[i] <- sd$sd_dis
  mbkmeans_ellips_rl[i] <- rl$ratkowsky_lance
  mbkmeans_ellips_silh[i] <- silh$silhouette
  mbkmeans_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters10.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  mbkmeans_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(mbkmeans_ellips_bh, "mbkmeans_ellips_bh.Rds")
saveRDS(mbkmeans_ellips_ch, "mbkmeans_ellips_ch.Rds")
saveRDS(mbkmeans_ellips_db, "mbkmeans_ellips_db.Rds")
saveRDS(mbkmeans_ellips_dunn, "mbkmeans_ellips_dunn.Rds")
saveRDS(mbkmeans_ellips_sd, "mbkmeans_ellips_sd.Rds")
saveRDS(mbkmeans_ellips_rl, "mbkmeans_ellips_rl.Rds")
saveRDS(mbkmeans_ellips_silh, "mbkmeans_ellips_silh.Rds")
saveRDS(mbkmeans_ellips_connectivity, "mbkmeans_ellips_connectivity.Rds")
saveRDS(mbkmeans_ellips_hub, "mbkmeans_ellips_hub.Rds")

# 11) fuzzy c-means 
n <- vector()
fcmeans_ellips_bh <- vector() # Ball-Hall
fcmeans_ellips_ch <- vector() # Calinski-Harabasz
fcmeans_ellips_db <- vector() # Davies-Bouldin
fcmeans_ellips_dunn <- vector() # Dunn
fcmeans_ellips_sd <- vector() # SD Distance
fcmeans_ellips_rl <- vector() # Ratkowsky-Lance
fcmeans_ellips_silh <- vector() # Silhouette
fcmeans_ellips_connectivity <- vector()
fcmeans_ellips_hub <- vector() 
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  bh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.2[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(ellips_ds[[i]]), clusters11.2[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  fcmeans_ellips_bh[i] <- bh$ball_hall
  fcmeans_ellips_ch[i] <- ch$calinski_harabasz
  fcmeans_ellips_db[i] <- db$davies_bouldin
  fcmeans_ellips_dunn[i] <- dunn$dunn
  fcmeans_ellips_sd[i] <- sd$sd_dis
  fcmeans_ellips_rl[i] <- rl$ratkowsky_lance
  fcmeans_ellips_silh[i] <- silh$silhouette
  fcmeans_ellips_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters11.2[[i]]), Data = as.matrix(ellips_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  fcmeans_ellips_hub[i] <- hub$pearsongamma
}

saveRDS(fcmeans_ellips_bh, "fcmeans_ellips_bh.Rds")
saveRDS(fcmeans_ellips_ch, "fcmeans_ellips_ch.Rds")
saveRDS(fcmeans_ellips_db, "fcmeans_ellips_db.Rds")
saveRDS(fcmeans_ellips_dunn, "fcmeans_ellips_dunn.Rds")
saveRDS(fcmeans_ellips_sd, "fcmeans_ellips_sd.Rds")
saveRDS(fcmeans_ellips_rl, "fcmeans_ellips_rl.Rds")
saveRDS(fcmeans_ellips_silh, "fcmeans_ellips_silh.Rds")
saveRDS(fcmeans_ellips_connectivity, "fcmeans_ellips_connectivity.Rds")
saveRDS(fcmeans_ellips_hub, "fcmeans_ellips_hub.Rds")


## small datasets -------------------------------------------------------------------------

require(data.table)
require(clusterCrit) 
require(fpc)
require(clValid)

# Importing small datasets (77 datasets)
small_ds <- list()
names3 = list.files(pattern="*.csv")
for(i in 1:length(names3)){
  small_ds[[i]] <- fread(names3[i], colClasses =  'double') 
}

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
n <- vector()
kmeans_small_bh <- vector() # Ball-Hall
kmeans_small_ch <- vector() # Calinski-Harabasz
kmeans_small_db <- vector() # Davies-Bouldin
kmeans_small_dunn <- vector() # Dunn
kmeans_small_sd <- vector() # SD Distance
kmeans_small_rl <- vector() # Ratkowsky-Lance
kmeans_small_silh <- vector() # Silhouette
kmeans_small_connectivity <- vector()
kmeans_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), clusters1.3[[i]], c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters1.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmeans_small_bh[i] <- bh$ball_hall
  kmeans_small_ch[i] <- ch$calinski_harabasz
  kmeans_small_db[i] <- db$davies_bouldin
  kmeans_small_dunn[i] <- dunn$dunn
  kmeans_small_sd[i] <- sd$sd_dis
  kmeans_small_rl[i] <- rl$ratkowsky_lance
  kmeans_small_silh[i] <- silh$silhouette
  kmeans_small_connectivity[i] <- clValid::connectivity(distance = NULL, clusters1.3[[i]], Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmeans_small_hub[i] <- hub$pearsongamma
}

saveRDS(kmeans_small_bh, "kmeans_small_bh.Rds")
saveRDS(kmeans_small_ch, "kmeans_small_ch.Rds")
saveRDS(kmeans_small_db, "kmeans_small_db.Rds")
saveRDS(kmeans_small_dunn, "kmeans_small_dunn.Rds")
saveRDS(kmeans_small_sd, "kmeans_small_sd.Rds")
saveRDS(kmeans_small_rl, "kmeans_small_rl.Rds")
saveRDS(kmeans_small_silh, "kmeans_small_silh.Rds")
saveRDS(kmeans_small_connectivity, "kmeans_small_connectivity.Rds")
saveRDS(kmeans_small_hub, "kmeans_small_hub.Rds")

# 2) k-medians 
n <- vector()
kmedians_small_bh <- vector() # Ball-Hall
kmedians_small_ch <- vector() # Calinski-Harabasz
kmedians_small_db <- vector() # Davies-Bouldin
kmedians_small_dunn <- vector() # Dunn
kmedians_small_sd <- vector() # SD Distance
kmedians_small_rl <- vector() # Ratkowsky-Lance
kmedians_small_silh <- vector() # Silhouette
kmedians_small_connectivity <- vector()
kmedians_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters2.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedians_small_bh[i] <- bh$ball_hall
  kmedians_small_ch[i] <- ch$calinski_harabasz
  kmedians_small_db[i] <- db$davies_bouldin
  kmedians_small_dunn[i] <- dunn$dunn
  kmedians_small_sd[i] <- sd$sd_dis
  kmedians_small_rl[i] <- rl$ratkowsky_lance
  kmedians_small_silh[i] <- silh$silhouette
  kmedians_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters2.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedians_small_hub[i] <- hub$pearsongamma
}

saveRDS(kmedians_small_bh, "kmedians_small_bh.Rds")
saveRDS(kmedians_small_ch, "kmedians_small_ch.Rds")
saveRDS(kmedians_small_db, "kmedians_small_db.Rds")
saveRDS(kmedians_small_dunn, "kmedians_small_dunn.Rds")
saveRDS(kmedians_small_sd, "kmedians_small_sd.Rds")
saveRDS(kmedians_small_rl, "kmedians_small_rl.Rds")
saveRDS(kmedians_small_silh, "kmedians_small_silh.Rds")
saveRDS(kmedians_small_connectivity, "kmedians_small_connectivity.Rds")
saveRDS(kmedians_small_hub, "kmedians_small_hub.Rds")

# 3) k-medoids 
n <- vector()
kmedoids_small_bh <- vector() # Ball-Hall
kmedoids_small_ch <- vector() # Calinski-Harabasz
kmedoids_small_db <- vector() # Davies-Bouldin
kmedoids_small_dunn <- vector() # Dunn
kmedoids_small_sd <- vector() # SD Distance
kmedoids_small_rl <- vector() # Ratkowsky-Lance
kmedoids_small_silh <- vector() # Silhouette
kmedoids_small_connectivity <- vector()
kmedoids_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters3.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedoids_small_bh[i] <- bh$ball_hall
  kmedoids_small_ch[i] <- ch$calinski_harabasz
  kmedoids_small_db[i] <- db$davies_bouldin
  kmedoids_small_dunn[i] <- dunn$dunn
  kmedoids_small_sd[i] <- sd$sd_dis
  kmedoids_small_rl[i] <- rl$ratkowsky_lance
  kmedoids_small_silh[i] <- silh$silhouette
  kmedoids_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters3.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedoids_small_hub[i] <- hub$pearsongamma
}

saveRDS(kmedoids_small_bh, "kmedoids_small_bh.Rds")
saveRDS(kmedoids_small_ch, "kmedoids_small_ch.Rds")
saveRDS(kmedoids_small_db, "kmedoids_small_db.Rds")
saveRDS(kmedoids_small_dunn, "kmedoids_small_dunn.Rds")
saveRDS(kmedoids_small_sd, "kmedoids_small_sd.Rds")
saveRDS(kmedoids_small_rl, "kmedoids_small_rl.Rds")
saveRDS(kmedoids_small_silh, "kmedoids_small_silh.Rds")
saveRDS(kmedoids_small_connectivity, "kmedoids_small_connectivity.Rds")
saveRDS(kmedoids_small_hub, "kmedoids_small_hub.Rds")

# 4) single linkage 
n <- vector()
single_small_bh <- vector() # Ball-Hall
single_small_ch <- vector() # Calinski-Harabasz
single_small_db <- vector() # Davies-Bouldin
single_small_dunn <- vector() # Dunn
single_small_sd <- vector() # SD Distance
single_small_rl <- vector() # Ratkowsky-Lance
single_small_silh <- vector() # Silhouette
single_small_connectivity <- vector()
single_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters4.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  single_small_bh[i] <- bh$ball_hall
  single_small_ch[i] <- ch$calinski_harabasz
  single_small_db[i] <- db$davies_bouldin
  single_small_dunn[i] <- dunn$dunn
  single_small_sd[i] <- sd$sd_dis
  single_small_rl[i] <- rl$ratkowsky_lance
  single_small_silh[i] <- silh$silhouette
  single_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters4.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  single_small_hub[i] <- hub$pearsongamma
}

saveRDS(single_small_bh, "single_small_bh.Rds")
saveRDS(single_small_ch, "single_small_ch.Rds")
saveRDS(single_small_db, "single_small_db.Rds")
saveRDS(single_small_dunn, "single_small_dunn.Rds")
saveRDS(single_small_sd, "single_small_sd.Rds")
saveRDS(single_small_rl, "single_small_rl.Rds")
saveRDS(single_small_silh, "single_small_silh.Rds")
saveRDS(single_small_connectivity, "single_small_connectivity.Rds")
saveRDS(single_small_hub, "single_small_hub.Rds")

# 5) complete linkage 
n <- vector()
complete_small_bh <- vector() # Ball-Hall
complete_small_ch <- vector() # Calinski-Harabasz
complete_small_db <- vector() # Davies-Bouldin
complete_small_dunn <- vector() # Dunn
complete_small_sd <- vector() # SD Distance
complete_small_rl <- vector() # Ratkowsky-Lance
complete_small_silh <- vector() # Silhouette
complete_small_connectivity <- vector()
complete_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters5.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  complete_small_bh[i] <- bh$ball_hall
  complete_small_ch[i] <- ch$calinski_harabasz
  complete_small_db[i] <- db$davies_bouldin
  complete_small_dunn[i] <- dunn$dunn
  complete_small_sd[i] <- sd$sd_dis
  complete_small_rl[i] <- rl$ratkowsky_lance
  complete_small_silh[i] <- silh$silhouette
  complete_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters5.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  complete_small_hub[i] <- hub$pearsongamma
}

saveRDS(complete_small_bh, "complete_small_bh.Rds")
saveRDS(complete_small_ch, "complete_small_ch.Rds")
saveRDS(complete_small_db, "complete_small_db.Rds")
saveRDS(complete_small_dunn, "complete_small_dunn.Rds")
saveRDS(complete_small_sd, "complete_small_sd.Rds")
saveRDS(complete_small_rl, "complete_small_rl.Rds")
saveRDS(complete_small_silh, "complete_small_silh.Rds")
saveRDS(complete_small_connectivity, "complete_small_connectivity.Rds")
saveRDS(complete_small_hub, "complete_small_hub.Rds")

# 6) average linkage 
n <- vector()
average_small_bh <- vector() # Ball-Hall
average_small_ch <- vector() # Calinski-Harabasz
average_small_db <- vector() # Davies-Bouldin
average_small_dunn <- vector() # Dunn
average_small_sd <- vector() # SD Distance
average_small_rl <- vector() # Ratkowsky-Lance
average_small_silh <- vector() # Silhouette
average_small_connectivity <- vector()
average_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters6.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  average_small_bh[i] <- bh$ball_hall
  average_small_ch[i] <- ch$calinski_harabasz
  average_small_db[i] <- db$davies_bouldin
  average_small_dunn[i] <- dunn$dunn
  average_small_sd[i] <- sd$sd_dis
  average_small_rl[i] <- rl$ratkowsky_lance
  average_small_silh[i] <- silh$silhouette
  average_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters6.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  average_small_hub[i] <- hub$pearsongamma
}

saveRDS(average_small_bh, "average_small_bh.Rds")
saveRDS(average_small_ch, "average_small_ch.Rds")
saveRDS(average_small_db, "average_small_db.Rds")
saveRDS(average_small_dunn, "average_small_dunn.Rds")
saveRDS(average_small_sd, "average_small_sd.Rds")
saveRDS(average_small_rl, "average_small_rl.Rds")
saveRDS(average_small_silh, "average_small_silh.Rds")
saveRDS(average_small_connectivity, "average_small_connectivity.Rds")
saveRDS(average_small_hub, "average_small_hub.Rds")

# 7) Ward's method 
n <- vector()
ward_small_bh <- vector() # Ball-Hall
ward_small_ch <- vector() # Calinski-Harabasz
ward_small_db <- vector() # Davies-Bouldin
ward_small_dunn <- vector() # Dunn
ward_small_sd <- vector() # SD Distance
ward_small_rl <- vector() # Ratkowsky-Lance
ward_small_silh <- vector() # Silhouette
ward_small_connectivity <- vector()
ward_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters7.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  ward_small_bh[i] <- bh$ball_hall
  ward_small_ch[i] <- ch$calinski_harabasz
  ward_small_db[i] <- db$davies_bouldin
  ward_small_dunn[i] <- dunn$dunn
  ward_small_sd[i] <- sd$sd_dis
  ward_small_rl[i] <- rl$ratkowsky_lance
  ward_small_silh[i] <- silh$silhouette
  ward_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters7.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  ward_small_hub[i] <- hub$pearsongamma
}

saveRDS(ward_small_bh, "ward_small_bh.Rds")
saveRDS(ward_small_ch, "ward_small_ch.Rds")
saveRDS(ward_small_db, "ward_small_db.Rds")
saveRDS(ward_small_dunn, "ward_small_dunn.Rds")
saveRDS(ward_small_sd, "ward_small_sd.Rds")
saveRDS(ward_small_rl, "ward_small_rl.Rds")
saveRDS(ward_small_silh, "ward_small_silh.Rds")
saveRDS(ward_small_connectivity, "ward_small_connectivity.Rds")
saveRDS(ward_small_hub, "ward_small_hub.Rds")

# 8) closest centroid 
n <- vector()
closest_small_bh <- vector() # Ball-Hall
closest_small_ch <- vector() # Calinski-Harabasz
closest_small_db <- vector() # Davies-Bouldin
closest_small_dunn <- vector() # Dunn
closest_small_sd <- vector() # SD Distance
closest_small_rl <- vector() # Ratkowsky-Lance
closest_small_silh <- vector() # Silhouette
closest_small_connectivity <- vector()
closest_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters8.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  closest_small_bh[i] <- bh$ball_hall
  closest_small_ch[i] <- ch$calinski_harabasz
  closest_small_db[i] <- db$davies_bouldin
  closest_small_dunn[i] <- dunn$dunn
  closest_small_sd[i] <- sd$sd_dis
  closest_small_rl[i] <- rl$ratkowsky_lance
  closest_small_silh[i] <- silh$silhouette
  closest_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters8.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  closest_small_hub[i] <- hub$pearsongamma
}

saveRDS(closest_small_bh, "closest_small_bh.Rds")
saveRDS(closest_small_ch, "closest_small_ch.Rds")
saveRDS(closest_small_db, "closest_small_db.Rds")
saveRDS(closest_small_dunn, "closest_small_dunn.Rds")
saveRDS(closest_small_sd, "closest_small_sd.Rds")
saveRDS(closest_small_rl, "closest_small_rl.Rds")
saveRDS(closest_small_silh, "closest_small_silh.Rds")
saveRDS(closest_small_connectivity, "closest_small_connectivity.Rds")
saveRDS(closest_small_hub, "closest_small_hub.Rds")

# 10) mini-batch k-means 
n <- vector()
mbkmeans_small_bh <- vector() # Ball-Hall
mbkmeans_small_ch <- vector() # Calinski-Harabasz
mbkmeans_small_db <- vector() # Davies-Bouldin
mbkmeans_small_dunn <- vector() # Dunn
mbkmeans_small_sd <- vector() # SD Distance
mbkmeans_small_rl <- vector() # Ratkowsky-Lance
mbkmeans_small_silh <- vector() # Silhouette
mbkmeans_small_connectivity <- vector()
mbkmeans_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters10.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  mbkmeans_small_bh[i] <- bh$ball_hall
  mbkmeans_small_ch[i] <- ch$calinski_harabasz
  mbkmeans_small_db[i] <- db$davies_bouldin
  mbkmeans_small_dunn[i] <- dunn$dunn
  mbkmeans_small_sd[i] <- sd$sd_dis
  mbkmeans_small_rl[i] <- rl$ratkowsky_lance
  mbkmeans_small_silh[i] <- silh$silhouette
  mbkmeans_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters10.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  mbkmeans_small_hub[i] <- hub$pearsongamma
}

saveRDS(mbkmeans_small_bh, "mbkmeans_small_bh.Rds")
saveRDS(mbkmeans_small_ch, "mbkmeans_small_ch.Rds")
saveRDS(mbkmeans_small_db, "mbkmeans_small_db.Rds")
saveRDS(mbkmeans_small_dunn, "mbkmeans_small_dunn.Rds")
saveRDS(mbkmeans_small_sd, "mbkmeans_small_sd.Rds")
saveRDS(mbkmeans_small_rl, "mbkmeans_small_rl.Rds")
saveRDS(mbkmeans_small_silh, "mbkmeans_small_silh.Rds")
saveRDS(mbkmeans_small_connectivity, "mbkmeans_small_connectivity.Rds")
saveRDS(mbkmeans_small_hub, "mbkmeans_small_hub.Rds")

# 11) fuzzy c-means 
n <- vector()
fcmeans_small_bh <- vector() # Ball-Hall
fcmeans_small_ch <- vector() # Calinski-Harabasz
fcmeans_small_db <- vector() # Davies-Bouldin
fcmeans_small_dunn <- vector() # Dunn
fcmeans_small_sd <- vector() # SD Distance
fcmeans_small_rl <- vector() # Ratkowsky-Lance
fcmeans_small_silh <- vector() # Silhouette
fcmeans_small_connectivity <- vector()
fcmeans_small_hub <- vector() # Hubert
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  bh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(small_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.3[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(small_ds[[i]]), clusters11.3[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  fcmeans_small_bh[i] <- bh$ball_hall
  fcmeans_small_ch[i] <- ch$calinski_harabasz
  fcmeans_small_db[i] <- db$davies_bouldin
  fcmeans_small_dunn[i] <- dunn$dunn
  fcmeans_small_sd[i] <- sd$sd_dis
  fcmeans_small_rl[i] <- rl$ratkowsky_lance
  fcmeans_small_silh[i] <- silh$silhouette
  fcmeans_small_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters11.3[[i]]), Data = as.matrix(small_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  fcmeans_small_hub[i] <- hub$pearsongamma
}

saveRDS(fcmeans_small_bh, "fcmeans_small_bh.Rds")
saveRDS(fcmeans_small_ch, "fcmeans_small_ch.Rds")
saveRDS(fcmeans_small_db, "fcmeans_small_db.Rds")
saveRDS(fcmeans_small_dunn, "fcmeans_small_dunn.Rds")
saveRDS(fcmeans_small_sd, "fcmeans_small_sd.Rds")
saveRDS(fcmeans_small_rl, "fcmeans_small_rl.Rds")
saveRDS(fcmeans_small_silh, "fcmeans_small_silh.Rds")
saveRDS(fcmeans_small_connectivity, "fcmeans_small_connectivity.Rds")
saveRDS(fcmeans_small_hub, "fcmeans_small_hub.Rds")


## medium datasets -------------------------------------------------------------------------

require(data.table)
require(clusterCrit) 
require(fpc)
require(clValid)

# Importing medium datasets (32 datasets)
medium_ds <- list()
names4 = list.files(pattern="*.csv")
for(i in 1:length(names4)){
  medium_ds[[i]] <- fread(names4[i], colClasses =  'double')
}

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
n <- vector()
kmeans_medium_bh <- vector() # Ball-Hall
kmeans_medium_ch <- vector() # Calinski-Harabasz
kmeans_medium_db <- vector() # Davies-Bouldin
kmeans_medium_dunn <- vector() # Dunn
kmeans_medium_sd <- vector() # SD Distance
kmeans_medium_rl <- vector() # Ratkowsky-Lance
kmeans_medium_silh <- vector() # Silhouette
kmeans_medium_connectivity <- vector()
kmeans_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), clusters1.4[[i]], c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters1.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmeans_medium_bh[i] <- bh$ball_hall
  kmeans_medium_ch[i] <- ch$calinski_harabasz
  kmeans_medium_db[i] <- db$davies_bouldin
  kmeans_medium_dunn[i] <- dunn$dunn
  kmeans_medium_sd[i] <- sd$sd_dis
  kmeans_medium_rl[i] <- rl$ratkowsky_lance
  kmeans_medium_silh[i] <- silh$silhouette
  kmeans_medium_connectivity[i] <- clValid::connectivity(distance = NULL, clusters1.4[[i]], Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmeans_medium_hub[i] <- hub$pearsongamma
}

saveRDS(kmeans_medium_bh, "kmeans_medium_bh.Rds")
saveRDS(kmeans_medium_ch, "kmeans_medium_ch.Rds")
saveRDS(kmeans_medium_db, "kmeans_medium_db.Rds")
saveRDS(kmeans_medium_dunn, "kmeans_medium_dunn.Rds")
saveRDS(kmeans_medium_sd, "kmeans_medium_sd.Rds")
saveRDS(kmeans_medium_rl, "kmeans_medium_rl.Rds")
saveRDS(kmeans_medium_silh, "kmeans_medium_silh.Rds")
saveRDS(kmeans_medium_connectivity, "kmeans_medium_connectivity.Rds")
saveRDS(kmeans_medium_hub, "kmeans_medium_hub.Rds")

# 2) k-medians 
n <- vector()
kmedians_medium_bh <- vector() # Ball-Hall
kmedians_medium_ch <- vector() # Calinski-Harabasz
kmedians_medium_db <- vector() # Davies-Bouldin
kmedians_medium_dunn <- vector() # Dunn
kmedians_medium_sd <- vector() # SD Distance
kmedians_medium_rl <- vector() # Ratkowsky-Lance
kmedians_medium_silh <- vector() # Silhouette
kmedians_medium_connectivity <- vector()
kmedians_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters2.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedians_medium_bh[i] <- bh$ball_hall
  kmedians_medium_ch[i] <- ch$calinski_harabasz
  kmedians_medium_db[i] <- db$davies_bouldin
  kmedians_medium_dunn[i] <- dunn$dunn
  kmedians_medium_sd[i] <- sd$sd_dis
  kmedians_medium_rl[i] <- rl$ratkowsky_lance
  kmedians_medium_silh[i] <- silh$silhouette
  kmedians_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters2.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedians_medium_hub[i] <- hub$pearsongamma
}

saveRDS(kmedians_medium_bh, "kmedians_medium_bh.Rds")
saveRDS(kmedians_medium_ch, "kmedians_medium_ch.Rds")
saveRDS(kmedians_medium_db, "kmedians_medium_db.Rds")
saveRDS(kmedians_medium_dunn, "kmedians_medium_dunn.Rds")
saveRDS(kmedians_medium_sd, "kmedians_medium_sd.Rds")
saveRDS(kmedians_medium_rl, "kmedians_medium_rl.Rds")
saveRDS(kmedians_medium_silh, "kmedians_medium_silh.Rds")
saveRDS(kmedians_medium_connectivity, "kmedians_medium_connectivity.Rds")
saveRDS(kmedians_medium_hub, "kmedians_medium_hub.Rds")

# 3) k-medoids 
n <- vector()
kmedoids_medium_bh <- vector() # Ball-Hall
kmedoids_medium_ch <- vector() # Calinski-Harabasz
kmedoids_medium_db <- vector() # Davies-Bouldin
kmedoids_medium_dunn <- vector() # Dunn
kmedoids_medium_sd <- vector() # SD Distance
kmedoids_medium_rl <- vector() # Ratkowsky-Lance
kmedoids_medium_silh <- vector() # Silhouette
kmedoids_medium_connectivity <- vector()
kmedoids_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters3.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedoids_medium_bh[i] <- bh$ball_hall
  kmedoids_medium_ch[i] <- ch$calinski_harabasz
  kmedoids_medium_db[i] <- db$davies_bouldin
  kmedoids_medium_dunn[i] <- dunn$dunn
  kmedoids_medium_sd[i] <- sd$sd_dis
  medoids_medium_rl[i] <- rl$ratkowsky_lance
  kmedoids_medium_silh[i] <- silh$silhouette
  kmedoids_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters3.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedoids_medium_hub[i] <- hub$pearsongamma
}

saveRDS(kmedoids_medium_bh, "kmedoids_medium_bh.Rds")
saveRDS(kmedoids_medium_ch, "kmedoids_medium_ch.Rds")
saveRDS(kmedoids_medium_db, "kmedoids_medium_db.Rds")
saveRDS(kmedoids_medium_dunn, "kmedoids_medium_dunn.Rds")
saveRDS(kmedoids_medium_sd, "kmedoids_medium_sd.Rds")
saveRDS(kmedoids_medium_rl, "kmedoids_medium_rl.Rds")
saveRDS(kmedoids_medium_silh, "kmedoids_medium_silh.Rds")
saveRDS(kmedoids_medium_connectivity, "kmedoids_medium_connectivity.Rds")
saveRDS(kmedoids_medium_hub, "kmedoids_medium_hub.Rds")

# 4) single linkage 
n <- vector()
single_medium_bh <- vector() # Ball-Hall
single_medium_ch <- vector() # Calinski-Harabasz
single_medium_db <- vector() # Davies-Bouldin
single_medium_dunn <- vector() # Dunn
single_medium_sd <- vector() # SD Distance
single_medium_rl <- vector() # Ratkowsky-Lance
single_medium_silh <- vector() # Silhouette
single_medium_connectivity <- vector()
single_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters4.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  single_medium_bh[i] <- bh$ball_hall
  single_medium_ch[i] <- ch$calinski_harabasz
  single_medium_db[i] <- db$davies_bouldin
  single_medium_dunn[i] <- dunn$dunn
  single_medium_sd[i] <- sd$sd_dis
  single_medium_rl[i] <- rl$ratkowsky_lance
  single_medium_silh[i] <- silh$silhouette
  single_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters4.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  single_medium_hub[i] <- hub$pearsongamma
}

saveRDS(single_medium_bh, "single_medium_bh.Rds")
saveRDS(single_medium_ch, "single_medium_ch.Rds")
saveRDS(single_medium_db, "single_medium_db.Rds")
saveRDS(single_medium_dunn, "single_medium_dunn.Rds")
saveRDS(single_medium_sd, "single_medium_sd.Rds")
saveRDS(single_medium_rl, "single_medium_rl.Rds")
saveRDS(single_medium_silh, "single_medium_silh.Rds")
saveRDS(single_medium_connectivity, "single_medium_connectivity.Rds")
saveRDS(single_medium_hub, "single_medium_hub.Rds")

# 5) complete linkage 
n <- vector()
complete_medium_bh <- vector() # Ball-Hall
complete_medium_ch <- vector() # Calinski-Harabasz
complete_medium_db <- vector() # Davies-Bouldin
complete_medium_dunn <- vector() # Dunn
complete_medium_sd <- vector() # SD Distance
complete_medium_rl <- vector() # Ratkowsky-Lance
complete_medium_silh <- vector() # Silhouette
complete_medium_connectivity <- vector()
complete_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters5.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  complete_medium_bh[i] <- bh$ball_hall
  complete_medium_ch[i] <- ch$calinski_harabasz
  complete_medium_db[i] <- db$davies_bouldin
  complete_medium_dunn[i] <- dunn$dunn
  complete_medium_sd[i] <- sd$sd_dis
  complete_medium_rl[i] <- rl$ratkowsky_lance
  complete_medium_silh[i] <- silh$silhouette
  complete_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters5.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  complete_medium_hub[i] <- hub$pearsongamma
}

saveRDS(complete_medium_bh, "complete_medium_bh.Rds")
saveRDS(complete_medium_ch, "complete_medium_ch.Rds")
saveRDS(complete_medium_db, "complete_medium_db.Rds")
saveRDS(complete_medium_dunn, "complete_medium_dunn.Rds")
saveRDS(complete_medium_sd, "complete_medium_sd.Rds")
saveRDS(complete_medium_rl, "complete_medium_rl.Rds")
saveRDS(complete_medium_silh, "complete_medium_silh.Rds")
saveRDS(complete_medium_connectivity, "complete_medium_connectivity.Rds")
saveRDS(complete_medium_hub, "complete_medium_hub.Rds")

# 6) average linkage 
n <- vector()
average_medium_bh <- vector() # Ball-Hall
average_medium_ch <- vector() # Calinski-Harabasz
average_medium_db <- vector() # Davies-Bouldin
average_medium_dunn <- vector() # Dunn
average_medium_sd <- vector() # SD Distance
average_medium_rl <- vector() # Ratkowsky-Lance
average_medium_silh <- vector() # Silhouette
average_medium_connectivity <- vector()
average_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters6.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  average_medium_bh[i] <- bh$ball_hall
  average_medium_ch[i] <- ch$calinski_harabasz
  average_medium_db[i] <- db$davies_bouldin
  average_medium_dunn[i] <- dunn$dunn
  average_medium_sd[i] <- sd$sd_dis
  average_medium_rl[i] <- rl$ratkowsky_lance
  average_medium_silh[i] <- silh$silhouette
  average_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters6.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  average_medium_hub[i] <- hub$pearsongamma
}

saveRDS(average_medium_bh, "average_medium_bh.Rds")
saveRDS(average_medium_ch, "average_medium_ch.Rds")
saveRDS(average_medium_db, "average_medium_db.Rds")
saveRDS(average_medium_dunn, "average_medium_dunn.Rds")
saveRDS(average_medium_sd, "average_medium_sd.Rds")
saveRDS(average_medium_rl, "average_medium_rl.Rds")
saveRDS(average_medium_silh, "average_medium_silh.Rds")
saveRDS(average_medium_connectivity, "average_medium_connectivity.Rds")
saveRDS(average_medium_hub, "average_medium_hub.Rds")

# 7) Ward's method 
n <- vector()
ward_medium_bh <- vector() # Ball-Hall
ward_medium_ch <- vector() # Calinski-Harabasz
ward_medium_db <- vector() # Davies-Bouldin
ward_medium_dunn <- vector() # Dunn
ward_medium_sd <- vector() # SD Distance
ward_medium_rl <- vector() # Ratkowsky-Lance
ward_medium_silh <- vector() # Silhouette
ward_medium_connectivity <- vector()
ward_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters7.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  ward_medium_bh[i] <- bh$ball_hall
  ward_medium_ch[i] <- ch$calinski_harabasz
  ward_medium_db[i] <- db$davies_bouldin
  ward_medium_dunn[i] <- dunn$dunn
  ward_medium_sd[i] <- sd$sd_dis
  ward_medium_rl[i] <- rl$ratkowsky_lance
  ward_medium_silh[i] <- silh$silhouette
  ward_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters7.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  ward_medium_hub[i] <- hub$pearsongamma
}

saveRDS(ward_medium_bh, "ward_medium_bh.Rds")
saveRDS(ward_medium_ch, "ward_medium_ch.Rds")
saveRDS(ward_medium_db, "ward_medium_db.Rds")
saveRDS(ward_medium_dunn, "ward_medium_dunn.Rds")
saveRDS(ward_medium_sd, "ward_medium_sd.Rds")
saveRDS(ward_medium_rl, "ward_medium_rl.Rds")
saveRDS(ward_medium_silh, "ward_medium_silh.Rds")
saveRDS(ward_medium_connectivity, "ward_medium_connectivity.Rds")
saveRDS(ward_medium_hub, "ward_medium_hub.Rds")

# 8) closest centroid 
n <- vector()
closest_medium_bh <- vector() # Ball-Hall
closest_medium_ch <- vector() # Calinski-Harabasz
closest_medium_db <- vector() # Davies-Bouldin
closest_medium_dunn <- vector() # Dunn
closest_medium_sd <- vector() # SD Distance
closest_medium_rl <- vector() # Ratkowsky-Lance
closest_medium_silh <- vector() # Silhouette
closest_medium_connectivity <- vector()
closest_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters8.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  closest_medium_bh[i] <- bh$ball_hall
  closest_medium_ch[i] <- ch$calinski_harabasz
  closest_medium_db[i] <- db$davies_bouldin
  closest_medium_dunn[i] <- dunn$dunn
  closest_medium_sd[i] <- sd$sd_dis
  closest_medium_rl[i] <- rl$ratkowsky_lance
  closest_medium_silh[i] <- silh$silhouette
  closest_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters8.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  closest_medium_hub[i] <- hub$pearsongamma
}

saveRDS(closest_medium_bh, "closest_medium_bh.Rds")
saveRDS(closest_medium_ch, "closest_medium_ch.Rds")
saveRDS(closest_medium_db, "closest_medium_db.Rds")
saveRDS(closest_medium_dunn, "closest_medium_dunn.Rds")
saveRDS(closest_medium_sd, "closest_medium_sd.Rds")
saveRDS(closest_medium_rl, "closest_medium_rl.Rds")
saveRDS(closest_medium_silh, "closest_medium_silh.Rds")
saveRDS(closest_medium_connectivity, "closest_medium_connectivity.Rds")
saveRDS(closest_medium_hub, "closest_medium_hub.Rds")

# 10) mini-batch k-means 
n <- vector()
mbkmeans_medium_bh <- vector() # Ball-Hall
mbkmeans_medium_ch <- vector() # Calinski-Harabasz
mbkmeans_medium_db <- vector() # Davies-Bouldin
mbkmeans_medium_dunn <- vector() # Dunn
mbkmeans_medium_sd <- vector() # SD Distance
mbkmeans_medium_rl <- vector() # Ratkowsky-Lance
mbkmeans_medium_silh <- vector() # Silhouette
mbkmeans_medium_connectivity <- vector()
mbkmeans_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters10.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  mbkmeans_medium_bh[i] <- bh$ball_hall
  mbkmeans_medium_ch[i] <- ch$calinski_harabasz
  mbkmeans_medium_db[i] <- db$davies_bouldin
  mbkmeans_medium_dunn[i] <- dunn$dunn
  mbkmeans_medium_sd[i] <- sd$sd_dis
  mbkmeans_medium_rl[i] <- rl$ratkowsky_lance
  mbkmeans_medium_silh[i] <- silh$silhouette
  mbkmeans_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters10.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  mbkmeans_medium_hub[i] <- hub$pearsongamma
}

saveRDS(mbkmeans_medium_bh, "mbkmeans_medium_bh.Rds")
saveRDS(mbkmeans_medium_ch, "mbkmeans_medium_ch.Rds")
saveRDS(mbkmeans_medium_db, "mbkmeans_medium_db.Rds")
saveRDS(mbkmeans_medium_dunn, "mbkmeans_medium_dunn.Rds")
saveRDS(mbkmeans_medium_sd, "mbkmeans_medium_sd.Rds")
saveRDS(mbkmeans_medium_rl, "mbkmeans_medium_rl.Rds")
saveRDS(mbkmeans_medium_silh, "mbkmeans_medium_silh.Rds")
saveRDS(mbkmeans_medium_connectivity, "mbkmeans_medium_connectivity.Rds")
saveRDS(mbkmeans_medium_hub, "mbkmeans_medium_hub.Rds")

# 11) fuzzy c-means 
n <- vector()
fcmeans_medium_bh <- vector() # Ball-Hall
fcmeans_medium_ch <- vector() # Calinski-Harabasz
fcmeans_medium_db <- vector() # Davies-Bouldin
fcmeans_medium_dunn <- vector() # Dunn
fcmeans_medium_sd <- vector() # SD Distance
fcmeans_medium_rl <- vector() # Ratkowsky-Lance
fcmeans_medium_silh <- vector() # Silhouette
fcmeans_medium_connectivity <- vector()
fcmeans_medium_hub <- vector() # Hubert
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  bh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.4[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(medium_ds[[i]]), clusters11.4[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  fcmeans_medium_bh[i] <- bh$ball_hall
  fcmeans_medium_ch[i] <- ch$calinski_harabasz
  fcmeans_medium_db[i] <- db$davies_bouldin
  fcmeans_medium_dunn[i] <- dunn$dunn
  fcmeans_medium_sd[i] <- sd$sd_dis
  fcmeans_medium_rl[i] <- rl$ratkowsky_lance
  fcmeans_medium_silh[i] <- silh$silhouette
  fcmeans_medium_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters11.4[[i]]), Data = as.matrix(medium_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  fcmeans_medium_hub[i] <- hub$pearsongamma
}

saveRDS(fcmeans_medium_bh, "fcmeans_medium_bh.Rds")
saveRDS(fcmeans_medium_ch, "fcmeans_medium_ch.Rds")
saveRDS(fcmeans_medium_db, "fcmeans_medium_db.Rds")
saveRDS(fcmeans_medium_dunn, "fcmeans_medium_dunn.Rds")
saveRDS(fcmeans_medium_sd, "fcmeans_medium_sd.Rds")
saveRDS(fcmeans_medium_rl, "fcmeans_medium_rl.Rds")
saveRDS(fcmeans_medium_silh, "fcmeans_medium_silh.Rds")
saveRDS(fcmeans_medium_connectivity, "fcmeans_medium_connectivity.Rds")
saveRDS(fcmeans_medium_hub, "fcmeans_medium_hub.Rds")


## large datasets -------------------------------------------------------------------------

require(data.table)
require(clusterCrit) 
require(fpc)
require(clValid)

# Importing large datasets (10 datasets)
large_ds <- list()
names5 = list.files(pattern="*.csv")
for(i in 1:length(names5)){
  large_ds[[i]] <- fread(names5[i], colClasses =  'double')
}

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
n <- vector()
kmeans_large_bh <- vector() # Ball-Hall
kmeans_large_ch <- vector() # Calinski-Harabasz
kmeans_large_db <- vector() # Davies-Bouldin
kmeans_large_dunn <- vector() # Dunn
kmeans_large_sd <- vector() # SD Distance
kmeans_large_rl <- vector() # Ratkowsky-Lance
kmeans_large_silh <- vector() # Silhouette
kmeans_large_connectivity <- vector()
kmeans_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), clusters1.5[[i]], c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters1.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmeans_large_bh[i] <- bh$ball_hall
  kmeans_large_ch[i] <- ch$calinski_harabasz
  kmeans_large_db[i] <- db$davies_bouldin
  kmeans_large_dunn[i] <- dunn$dunn
  kmeans_large_sd[i] <- sd$sd_dis
  kmeans_large_rl[i] <- rl$ratkowsky_lance
  kmeans_large_silh[i] <- silh$silhouette
  kmeans_large_connectivity[i] <- clValid::connectivity(distance = NULL, clusters1.5[[i]], Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmeans_large_hub[i] <- hub$pearsongamma
}

saveRDS(kmeans_large_bh, "kmeans_large_bh.Rds")
saveRDS(kmeans_large_ch, "kmeans_large_ch.Rds")
saveRDS(kmeans_large_db, "kmeans_large_db.Rds")
saveRDS(kmeans_large_dunn, "kmeans_large_dunn.Rds")
saveRDS(kmeans_large_sd, "kmeans_large_sd.Rds")
saveRDS(kmeans_large_rl, "kmeans_large_rl.Rds")
saveRDS(kmeans_large_silh, "kmeans_large_silh.Rds")
saveRDS(kmeans_large_connectivity, "kmeans_large_connectivity.Rds")
saveRDS(kmeans_large_hub, "kmeans_large_hub.Rds")

# 2) k-medians 
n <- vector()
kmedians_large_bh <- vector() # Ball-Hall
kmedians_large_ch <- vector() # Calinski-Harabasz
kmedians_large_db <- vector() # Davies-Bouldin
kmedians_large_dunn <- vector() # Dunn
kmedians_large_sd <- vector() # SD Distance
kmedians_large_rl <- vector() # Ratkowsky-Lance
kmedians_large_silh <- vector() # Silhouette
kmedians_large_connectivity <- vector()
kmedians_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters2.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedians_large_bh[i] <- bh$ball_hall
  kmedians_large_ch[i] <- ch$calinski_harabasz
  kmedians_large_db[i] <- db$davies_bouldin
  kmedians_large_dunn[i] <- dunn$dunn
  kmedians_large_sd[i] <- sd$sd_dis
  kmedians_large_rl[i] <- rl$ratkowsky_lance
  kmedians_large_silh[i] <- silh$silhouette
  kmedians_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters2.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedians_large_hub[i] <- hub$pearsongamma
}

saveRDS(kmedians_large_bh, "kmedians_large_bh.Rds")
saveRDS(kmedians_large_ch, "kmedians_large_ch.Rds")
saveRDS(kmedians_large_db, "kmedians_large_db.Rds")
saveRDS(kmedians_large_dunn, "kmedians_large_dunn.Rds")
saveRDS(kmedians_large_sd, "kmedians_large_sd.Rds")
saveRDS(kmedians_large_rl, "kmedians_large_rl.Rds")
saveRDS(kmedians_large_silh, "kmedians_large_silh.Rds")
saveRDS(kmedians_large_connectivity, "kmedians_large_connectivity.Rds")
saveRDS(kmedians_large_hub, "kmedians_large_hub.Rds")

# 3) k-medoids 
n <- vector()
kmedoids_large_bh <- vector() # Ball-Hall
kmedoids_large_ch <- vector() # Calinski-Harabasz
kmedoids_large_db <- vector() # Davies-Bouldin
kmedoids_large_dunn <- vector() # Dunn
kmedoids_large_sd <- vector() # SD Distance
kmedoids_large_rl <- vector() # Ratkowsky-Lance
kmedoids_large_silh <- vector() # Silhouette
kmedoids_large_connectivity <- vector()
kmedoids_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters3.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedoids_large_bh[i] <- bh$ball_hall
  kmedoids_large_ch[i] <- ch$calinski_harabasz
  kmedoids_large_db[i] <- db$davies_bouldin
  kmedoids_large_dunn[i] <- dunn$dunn
  kmedoids_large_sd[i] <- sd$sd_dis
  kmedoids_large_rl[i] <- rl$ratkowsky_lance
  kmedoids_large_silh[i] <- silh$silhouette
  kmedoids_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters3.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedoids_large_hub[i] <- hub$pearsongamma
}

saveRDS(kmedoids_large_bh, "kmedoids_large_bh.Rds")
saveRDS(kmedoids_large_ch, "kmedoids_large_ch.Rds")
saveRDS(kmedoids_large_db, "kmedoids_large_db.Rds")
saveRDS(kmedoids_large_dunn, "kmedoids_large_dunn.Rds")
saveRDS(kmedoids_large_sd, "kmedoids_large_sd.Rds")
saveRDS(kmedoids_large_rl, "kmedoids_large_rl.Rds")
saveRDS(kmedoids_large_silh, "kmedoids_large_silh.Rds")
saveRDS(kmedoids_large_connectivity, "kmedoids_large_connectivity.Rds")
saveRDS(kmedoids_large_hub, "kmedoids_large_hub.Rds")

# 4) single linkage 
n <- vector()
single_large_bh <- vector() # Ball-Hall
single_large_ch <- vector() # Calinski-Harabasz
single_large_db <- vector() # Davies-Bouldin
single_large_dunn <- vector() # Dunn
single_large_sd <- vector() # SD Distance
single_large_rl <- vector() # Ratkowsky-Lance
single_large_silh <- vector() # Silhouette
single_large_connectivity <- vector()
single_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters4.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  single_large_bh[i] <- bh$ball_hall
  single_large_ch[i] <- ch$calinski_harabasz
  single_large_db[i] <- db$davies_bouldin
  single_large_dunn[i] <- dunn$dunn
  single_large_sd[i] <- sd$sd_dis
  single_large_rl[i] <- rl$ratkowsky_lance
  single_large_silh[i] <- silh$silhouette
  single_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters4.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  single_large_hub[i] <- hub$pearsongamma
}

saveRDS(single_large_bh, "single_large_bh.Rds")
saveRDS(single_large_ch, "single_large_ch.Rds")
saveRDS(single_large_db, "single_large_db.Rds")
saveRDS(single_large_dunn, "single_large_dunn.Rds")
saveRDS(single_large_sd, "single_large_sd.Rds")
saveRDS(single_large_rl, "single_large_rl.Rds")
saveRDS(single_large_silh, "single_large_silh.Rds")
saveRDS(single_large_connectivity, "single_large_connectivity.Rds")
saveRDS(single_large_hub, "single_large_hub.Rds")

# 5) complete linkage 
n <- vector()
complete_large_bh <- vector() # Ball-Hall
complete_large_ch <- vector() # Calinski-Harabasz
complete_large_db <- vector() # Davies-Bouldin
complete_large_dunn <- vector() # Dunn
complete_large_sd <- vector() # SD Distance
complete_large_rl <- vector() # Ratkowsky-Lance
complete_large_silh <- vector() # Silhouette
complete_large_connectivity <- vector()
complete_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters5.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  complete_large_bh[i] <- bh$ball_hall
  complete_large_ch[i] <- ch$calinski_harabasz
  complete_large_db[i] <- db$davies_bouldin
  complete_large_dunn[i] <- dunn$dunn
  complete_large_sd[i] <- sd$sd_dis
  complete_large_rl[i] <- rl$ratkowsky_lance
  complete_large_silh[i] <- silh$silhouette
  complete_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters5.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  complete_large_hub[i] <- hub$pearsongamma
}

saveRDS(complete_large_bh, "complete_large_bh.Rds")
saveRDS(complete_large_ch, "complete_large_ch.Rds")
saveRDS(complete_large_db, "complete_large_db.Rds")
saveRDS(complete_large_dunn, "complete_large_dunn.Rds")
saveRDS(complete_large_sd, "complete_large_sd.Rds")
saveRDS(complete_large_rl, "complete_large_rl.Rds")
saveRDS(complete_large_silh, "complete_large_silh.Rds")
saveRDS(complete_large_connectivity, "complete_large_connectivity.Rds")
saveRDS(complete_large_hub, "complete_large_hub.Rds")

# 6) average linkage 
n <- vector()
average_large_bh <- vector() # Ball-Hall
average_large_ch <- vector() # Calinski-Harabasz
average_large_db <- vector() # Davies-Bouldin
average_large_dunn <- vector() # Dunn
average_large_sd <- vector() # SD Distance
average_large_rl <- vector() # Ratkowsky-Lance
average_large_silh <- vector() # Silhouette
average_large_connectivity <- vector()
average_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters6.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  average_large_bh[i] <- bh$ball_hall
  average_large_ch[i] <- ch$calinski_harabasz
  average_large_db[i] <- db$davies_bouldin
  average_large_dunn[i] <- dunn$dunn
  average_large_sd[i] <- sd$sd_dis
  average_large_rl[i] <- rl$ratkowsky_lance
  average_large_silh[i] <- silh$silhouette
  average_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters6.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  average_large_hub[i] <- hub$pearsongamma
}

saveRDS(average_large_bh, "average_large_bh.Rds")
saveRDS(average_large_ch, "average_large_ch.Rds")
saveRDS(average_large_db, "average_large_db.Rds")
saveRDS(average_large_dunn, "average_large_dunn.Rds")
saveRDS(average_large_sd, "average_large_sd.Rds")
saveRDS(average_large_rl, "average_large_rl.Rds")
saveRDS(average_large_silh, "average_large_silh.Rds")
saveRDS(average_large_connectivity, "average_large_connectivity.Rds")
saveRDS(average_large_hub, "average_large_hub.Rds")

# 7) Ward's method 
n <- vector()
ward_large_bh <- vector() # Ball-Hall
ward_large_ch <- vector() # Calinski-Harabasz
ward_large_db <- vector() # Davies-Bouldin
ward_large_dunn <- vector() # Dunn
ward_large_sd <- vector() # SD Distance
ward_large_rl <- vector() # Ratkowsky-Lance
ward_large_silh <- vector() # Silhouette
ward_large_connectivity <- vector()
ward_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters7.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  ward_large_bh[i] <- bh$ball_hall
  ward_large_ch[i] <- ch$calinski_harabasz
  ward_large_db[i] <- db$davies_bouldin
  ward_large_dunn[i] <- dunn$dunn
  ward_large_sd[i] <- sd$sd_dis
  ward_large_rl[i] <- rl$ratkowsky_lance
  ward_large_silh[i] <- silh$silhouette
  ward_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters7.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  ward_large_hub[i] <- hub$pearsongamma
}

saveRDS(ward_large_bh, "ward_large_bh.Rds")
saveRDS(ward_large_ch, "ward_large_ch.Rds")
saveRDS(ward_large_db, "ward_large_db.Rds")
saveRDS(ward_large_dunn, "ward_large_dunn.Rds")
saveRDS(ward_large_sd, "ward_large_sd.Rds")
saveRDS(ward_large_rl, "ward_large_rl.Rds")
saveRDS(ward_large_silh, "ward_large_silh.Rds")
saveRDS(ward_large_connectivity, "ward_large_connectivity.Rds")
saveRDS(ward_large_hub, "ward_large_hub.Rds")

# 8) closest centroid 
n <- vector()
closest_large_bh <- vector() # Ball-Hall
closest_large_ch <- vector() # Calinski-Harabasz
closest_large_db <- vector() # Davies-Bouldin
closest_large_dunn <- vector() # Dunn
closest_large_sd <- vector() # SD Distance
closest_large_rl <- vector() # Ratkowsky-Lance
closest_large_silh <- vector() # Silhouette
closest_large_connectivity <- vector()
closest_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters8.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  closest_large_bh[i] <- bh$ball_hall
  closest_large_ch[i] <- ch$calinski_harabasz
  closest_large_db[i] <- db$davies_bouldin
  closest_large_dunn[i] <- dunn$dunn
  closest_large_sd[i] <- sd$sd_dis
  closest_large_rl[i] <- rl$ratkowsky_lance
  closest_large_silh[i] <- silh$silhouette
  closest_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters8.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  closest_large_hub[i] <- hub$pearsongamma
}

saveRDS(closest_large_bh, "closest_large_bh.Rds")
saveRDS(closest_large_ch, "closest_large_ch.Rds")
saveRDS(closest_large_db, "closest_large_db.Rds")
saveRDS(closest_large_dunn, "closest_large_dunn.Rds")
saveRDS(closest_large_sd, "closest_large_sd.Rds")
saveRDS(closest_large_rl, "closest_large_rl.Rds")
saveRDS(closest_large_silh, "closest_large_silh.Rds")
saveRDS(closest_large_connectivity, "closest_large_connectivity.Rds")
saveRDS(closest_large_hub, "closest_large_hub.Rds")

# 10) mini-batch k-means 
n <- vector()
mbkmeans_large_bh <- vector() # Ball-Hall
mbkmeans_large_ch <- vector() # Calinski-Harabasz
mbkmeans_large_db <- vector() # Davies-Bouldin
mbkmeans_large_dunn <- vector() # Dunn
mbkmeans_large_sd <- vector() # SD Distance
mbkmeans_large_rl <- vector() # Ratkowsky-Lance
mbkmeans_large_silh <- vector() # Silhouette
mbkmeans_large_connectivity <- vector()
mbkmeans_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters10.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  mbkmeans_large_bh[i] <- bh$ball_hall
  mbkmeans_large_ch[i] <- ch$calinski_harabasz
  mbkmeans_large_db[i] <- db$davies_bouldin
  mbkmeans_large_dunn[i] <- dunn$dunn
  mbkmeans_large_sd[i] <- sd$sd_dis
  mbkmeans_large_rl[i] <- rl$ratkowsky_lance
  mbkmeans_large_silh[i] <- silh$silhouette
  mbkmeans_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters10.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  mbkmeans_large_hub[i] <- hub$pearsongamma
}

saveRDS(mbkmeans_large_bh, "mbkmeans_large_bh.Rds")
saveRDS(mbkmeans_large_ch, "mbkmeans_large_ch.Rds")
saveRDS(mbkmeans_large_db, "mbkmeans_large_db.Rds")
saveRDS(mbkmeans_large_dunn, "mbkmeans_large_dunn.Rds")
saveRDS(mbkmeans_large_sd, "mbkmeans_large_sd.Rds")
saveRDS(mbkmeans_large_rl, "mbkmeans_large_rl.Rds")
saveRDS(mbkmeans_large_silh, "mbkmeans_large_silh.Rds")
saveRDS(mbkmeans_large_connectivity, "mbkmeans_large_connectivity.Rds")
saveRDS(mbkmeans_large_hub, "mbkmeans_large_hub.Rds")

# 11) fuzzy c-means 
n <- vector()
fcmeans_large_bh <- vector() # Ball-Hall
fcmeans_large_ch <- vector() # Calinski-Harabasz
fcmeans_large_db <- vector() # Davies-Bouldin
fcmeans_large_dunn <- vector() # Dunn
fcmeans_large_sd <- vector() # SD Distance
fcmeans_large_rl <- vector() # Ratkowsky-Lance
fcmeans_large_silh <- vector() # Silhouette
fcmeans_large_connectivity <- vector()
fcmeans_large_hub <- vector()
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  bh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(large_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.5[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(large_ds[[i]]), clusters11.5[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  fcmeans_large_bh[i] <- bh$ball_hall
  fcmeans_large_ch[i] <- ch$calinski_harabasz
  fcmeans_large_db[i] <- db$davies_bouldin
  fcmeans_large_dunn[i] <- dunn$dunn
  fcmeans_large_sd[i] <- sd$sd_dis
  fcmeans_large_rl[i] <- rl$ratkowsky_lance
  fcmeans_large_silh[i] <- silh$silhouette
  fcmeans_large_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters11.5[[i]]), Data = as.matrix(large_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  fcmeans_large_hub[i] <- hub$pearsongamma
}

saveRDS(fcmeans_large_bh, "fcmeans_large_bh.Rds")
saveRDS(fcmeans_large_ch, "fcmeans_large_ch.Rds")
saveRDS(fcmeans_large_db, "fcmeans_large_db.Rds")
saveRDS(fcmeans_large_dunn, "fcmeans_large_dunn.Rds")
saveRDS(fcmeans_large_sd, "fcmeans_large_sd.Rds")
saveRDS(fcmeans_large_rl, "fcmeans_large_rl.Rds")
saveRDS(fcmeans_large_silh, "fcmeans_large_silh.Rds")
saveRDS(fcmeans_large_connectivity, "fcmeans_large_connectivity.Rds")
saveRDS(fcmeans_large_hub, "fcmeans_large_hub.Rds")


## github datasets -------------------------------------------------------------------------

require(data.table)
require(clusterCrit) 
require(fpc)
require(clValid)

# Importing other_datasets - github (101 datasets)
git_ds <- list()
names6 = list.files(pattern="*.txt")
for(i in 1:length(names6)){
  git_ds[[i]] <- fread(names6[i])
}

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
n <- vector()
kmeans_git_bh <- vector() # Ball-Hall
kmeans_git_ch <- vector() # Calinski-Harabasz
kmeans_git_db <- vector() # Davies-Bouldin
kmeans_git_dunn <- vector() # Dunn
kmeans_git_sd <- vector() # SD Distance
kmeans_git_rl <- vector() # Ratkowsky-Lance
kmeans_git_silh <- vector() # Silhouette
kmeans_git_connectivity <- vector() # Connectivity
kmeans_git_hub <- vector() # Hubert
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), clusters1.6[[i]], c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters1.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmeans_git_bh[i] <- bh$ball_hall
  kmeans_git_ch[i] <- ch$calinski_harabasz
  kmeans_git_db[i] <- db$davies_bouldin
  kmeans_git_dunn[i] <- dunn$dunn
  kmeans_git_sd[i] <- sd$sd_dis
  kmeans_git_rl[i] <- rl$ratkowsky_lance
  kmeans_git_silh[i] <- silh$silhouette
  kmeans_git_connectivity[i] <- clValid::connectivity(distance = NULL, clusters1.6[[i]], Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmeans_git_hub[i] <- hub$pearsongamma
}

saveRDS(kmeans_git_bh, "kmeans_git_bh.Rds")
saveRDS(kmeans_git_ch, "kmeans_git_ch.Rds")
saveRDS(kmeans_git_db, "kmeans_git_db.Rds")
saveRDS(kmeans_git_dunn, "kmeans_git_dunn.Rds")
saveRDS(kmeans_git_sd, "kmeans_git_sd.Rds")
saveRDS(kmeans_git_rl, "kmeans_git_rl.Rds")
saveRDS(kmeans_git_silh, "kmeans_git_silh.Rds")
saveRDS(kmeans_git_connectivity, "kmeans_git_connectivity.Rds")
saveRDS(kmeans_git_hub, "kmeans_git_hub.Rds")

# 2) k-medians 
n <- vector()
kmedians_git_bh <- vector() # Ball-Hall
kmedians_git_ch <- vector() # Calinski-Harabasz
kmedians_git_db <- vector() # Davies-Bouldin
kmedians_git_dunn <- vector() # Dunn
kmedians_git_sd <- vector() # SD Distance
kmedians_git_rl <- vector() # Ratkowsky-Lance
kmedians_git_silh <- vector() # Silhouette
kmedians_git_connectivity <- vector()
kmedians_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters2.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters2.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedians_git_bh[i] <- bh$ball_hall
  kmedians_git_ch[i] <- ch$calinski_harabasz
  kmedians_git_db[i] <- db$davies_bouldin
  kmedians_git_dunn[i] <- dunn$dunn
  kmedians_git_sd[i] <- sd$sd_dis
  kmedians_git_rl[i] <- rl$ratkowsky_lance
  kmedians_git_silh[i] <- silh$silhouette
  kmedians_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters2.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedians_git_hub[i] <- hub$pearsongamma
}

saveRDS(kmedians_git_bh, "kmedians_git_bh.Rds")
saveRDS(kmedians_git_ch, "kmedians_git_ch.Rds")
saveRDS(kmedians_git_db, "kmedians_git_db.Rds")
saveRDS(kmedians_git_dunn, "kmedians_git_dunn.Rds")
saveRDS(kmedians_git_sd, "kmedians_git_sd.Rds")
saveRDS(kmedians_git_rl, "kmedians_git_rl.Rds")
saveRDS(kmedians_git_silh, "kmedians_git_silh.Rds")
saveRDS(kmedians_git_connectivity, "kmedians_git_connectivity.Rds")
saveRDS(kmedians_git_hub, "kmedians_git_hub.Rds")

# 3) k-medoids 
n <- vector()
kmedoids_git_bh <- vector() # Ball-Hall
kmedoids_git_ch <- vector() # Calinski-Harabasz
kmedoids_git_db <- vector() # Davies-Bouldin
kmedoids_git_dunn <- vector() # Dunn
kmedoids_git_sd <- vector() # SD Distance
kmedoids_git_rl <- vector() # Ratkowsky-Lance
kmedoids_git_silh <- vector() # Silhouette
kmedoids_git_connectivity <- vector()
kmedoids_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters3.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters3.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  kmedoids_git_bh[i] <- bh$ball_hall
  kmedoids_git_ch[i] <- ch$calinski_harabasz
  kmedoids_git_db[i] <- db$davies_bouldin
  kmedoids_git_dunn[i] <- dunn$dunn
  kmedoids_git_sd[i] <- sd$sd_dis
  kmedoids_git_rl[i] <- rl$ratkowsky_lance
  kmedoids_git_silh[i] <- silh$silhouette
  kmedoids_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters3.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  kmedoids_git_hub[i] <- hub$pearsongamma
}

saveRDS(kmedoids_git_bh, "kmedoids_git_bh.Rds")
saveRDS(kmedoids_git_ch, "kmedoids_git_ch.Rds")
saveRDS(kmedoids_git_db, "kmedoids_git_db.Rds")
saveRDS(kmedoids_git_dunn, "kmedoids_git_dunn.Rds")
saveRDS(kmedoids_git_sd, "kmedoids_git_sd.Rds")
saveRDS(kmedoids_git_rl, "kmedoids_git_rl.Rds")
saveRDS(kmedoids_git_silh, "kmedoids_git_silh.Rds")
saveRDS(kmedoids_git_connectivity, "kmedoids_git_connectivity.Rds")
saveRDS(kmedoids_git_hub, "kmedoids_git_hub.Rds")

# 4) single linkage 
n <- vector()
single_git_bh <- vector() # Ball-Hall
single_git_ch <- vector() # Calinski-Harabasz
single_git_db <- vector() # Davies-Bouldin
single_git_dunn <- vector() # Dunn
single_git_sd <- vector() # SD Distance
single_git_rl <- vector() # Ratkowsky-Lance
single_git_silh <- vector() # Silhouette
single_git_connectivity <- vector()
single_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters4.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters4.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  single_git_bh[i] <- bh$ball_hall
  single_git_ch[i] <- ch$calinski_harabasz
  single_git_db[i] <- db$davies_bouldin
  single_git_dunn[i] <- dunn$dunn
  single_git_sd[i] <- sd$sd_dis
  single_git_rl[i] <- rl$ratkowsky_lance
  single_git_silh[i] <- silh$silhouette
  single_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters4.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  single_git_hub[i] <- hub$pearsongamma
}

saveRDS(single_git_bh, "single_git_bh.Rds")
saveRDS(single_git_ch, "single_git_ch.Rds")
saveRDS(single_git_db, "single_git_db.Rds")
saveRDS(single_git_dunn, "single_git_dunn.Rds")
saveRDS(single_git_sd, "single_git_sd.Rds")
saveRDS(single_git_rl, "single_git_rl.Rds")
saveRDS(single_git_silh, "single_git_silh.Rds")
saveRDS(single_git_connectivity, "single_git_connectivity.Rds")
saveRDS(single_git_hub, "single_git_hub.Rds")

# 5) complete linkage 
n <- vector()
complete_git_bh <- vector() # Ball-Hall
complete_git_ch <- vector() # Calinski-Harabasz
complete_git_db <- vector() # Davies-Bouldin
complete_git_dunn <- vector() # Dunn
complete_git_sd <- vector() # SD Distance
complete_git_rl <- vector() # Ratkowsky-Lance
complete_git_silh <- vector() # Silhouette
complete_git_connectivity <- vector()
complete_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters5.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters5.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  complete_git_bh[i] <- bh$ball_hall
  complete_git_ch[i] <- ch$calinski_harabasz
  complete_git_db[i] <- db$davies_bouldin
  complete_git_dunn[i] <- dunn$dunn
  complete_git_sd[i] <- sd$sd_dis
  complete_git_rl[i] <- rl$ratkowsky_lance
  complete_git_silh[i] <- silh$silhouette
  complete_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters5.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  complete_git_hub[i] <- hub$pearsongamma
}

saveRDS(complete_git_bh, "complete_git_bh.Rds")
saveRDS(complete_git_ch, "complete_git_ch.Rds")
saveRDS(complete_git_db, "complete_git_db.Rds")
saveRDS(complete_git_dunn, "complete_git_dunn.Rds")
saveRDS(complete_git_sd, "complete_git_sd.Rds")
saveRDS(complete_git_rl, "complete_git_rl.Rds")
saveRDS(complete_git_silh, "complete_git_silh.Rds")
saveRDS(complete_git_connectivity, "complete_git_connectivity.Rds")
saveRDS(complete_git_hub, "complete_git_hub.Rds")

# 6) average linkage 
n <- vector()
average_git_bh <- vector() # Ball-Hall
average_git_ch <- vector() # Calinski-Harabasz
average_git_db <- vector() # Davies-Bouldin
average_git_dunn <- vector() # Dunn
average_git_sd <- vector() # SD Distance
average_git_rl <- vector() # Ratkowsky-Lance
average_git_silh <- vector() # Silhouette
average_git_connectivity <- vector()
average_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters6.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters6.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  average_git_bh[i] <- bh$ball_hall
  average_git_ch[i] <- ch$calinski_harabasz
  average_git_db[i] <- db$davies_bouldin
  average_git_dunn[i] <- dunn$dunn
  average_git_sd[i] <- sd$sd_dis
  average_git_rl[i] <- rl$ratkowsky_lance
  average_git_silh[i] <- silh$silhouette
  average_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters6.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  average_git_hub[i] <- hub$pearsongamma
}

saveRDS(average_git_bh, "average_git_bh.Rds")
saveRDS(average_git_ch, "average_git_ch.Rds")
saveRDS(average_git_db, "average_git_db.Rds")
saveRDS(average_git_dunn, "average_git_dunn.Rds")
saveRDS(average_git_sd, "average_git_sd.Rds")
saveRDS(average_git_rl, "average_git_rl.Rds")
saveRDS(average_git_silh, "average_git_silh.Rds")
saveRDS(average_git_connectivity, "average_git_connectivity.Rds")
saveRDS(average_git_hub, "average_git_hub.Rds")

# 7) Ward's method 
n <- vector()
ward_git_bh <- vector() # Ball-Hall
ward_git_ch <- vector() # Calinski-Harabasz
ward_git_db <- vector() # Davies-Bouldin
ward_git_dunn <- vector() # Dunn
ward_git_sd <- vector() # SD Distance
ward_git_rl <- vector() # Ratkowsky-Lance
ward_git_silh <- vector() # Silhouette
ward_git_connectivity <- vector()
ward_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters7.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters7.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  ward_git_bh[i] <- bh$ball_hall
  ward_git_ch[i] <- ch$calinski_harabasz
  ward_git_db[i] <- db$davies_bouldin
  ward_git_dunn[i] <- dunn$dunn
  ward_git_sd[i] <- sd$sd_dis
  ward_git_rl[i] <- rl$ratkowsky_lance
  ward_git_silh[i] <- silh$silhouette
  ward_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters7.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  ward_git_hub[i] <- hub$pearsongamma
}

saveRDS(ward_git_bh, "ward_git_bh.Rds")
saveRDS(ward_git_ch, "ward_git_ch.Rds")
saveRDS(ward_git_db, "ward_git_db.Rds")
saveRDS(ward_git_dunn, "ward_git_dunn.Rds")
saveRDS(ward_git_sd, "ward_git_sd.Rds")
saveRDS(ward_git_rl, "ward_git_rl.Rds")
saveRDS(ward_git_silh, "ward_git_silh.Rds")
saveRDS(ward_git_connectivity, "ward_git_connectivity.Rds")
saveRDS(ward_git_hub, "ward_git_hub.Rds")

# 8) closest centroid 
n <- vector()
closest_git_bh <- vector() # Ball-Hall
closest_git_ch <- vector() # Calinski-Harabasz
closest_git_db <- vector() # Davies-Bouldin
closest_git_dunn <- vector() # Dunn
closest_git_sd <- vector() # SD Distance
closest_git_rl <- vector() # Ratkowsky-Lance
closest_git_silh <- vector() # Silhouette
closest_git_connectivity <- vector()
closest_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters8.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters8.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  closest_git_bh[i] <- bh$ball_hall
  closest_git_ch[i] <- ch$calinski_harabasz
  closest_git_db[i] <- db$davies_bouldin
  closest_git_dunn[i] <- dunn$dunn
  closest_git_sd[i] <- sd$sd_dis
  closest_git_rl[i] <- rl$ratkowsky_lance
  closest_git_silh[i] <- silh$silhouette
  closest_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters8.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  closest_git_hub[i] <- hub$pearsongamma
}

saveRDS(closest_git_bh, "closest_git_bh.Rds")
saveRDS(closest_git_ch, "closest_git_ch.Rds")
saveRDS(closest_git_db, "closest_git_db.Rds")
saveRDS(closest_git_dunn, "closest_git_dunn.Rds")
saveRDS(closest_git_sd, "closest_git_sd.Rds")
saveRDS(closest_git_rl, "closest_git_rl.Rds")
saveRDS(closest_git_silh, "closest_git_silh.Rds")
saveRDS(closest_git_connectivity, "closest_git_connectivity.Rds")
saveRDS(closest_git_hub, "closest_git_hub.Rds")

# 10) mini-batch k-means 
n <- vector()
mbkmeans_git_bh <- vector() # Ball-Hall
mbkmeans_git_ch <- vector() # Calinski-Harabasz
mbkmeans_git_db <- vector() # Davies-Bouldin
mbkmeans_git_dunn <- vector() # Dunn
mbkmeans_git_sd <- vector() # SD Distance
mbkmeans_git_rl <- vector() # Ratkowsky-Lance
mbkmeans_git_silh <- vector() # Silhouette
mbkmeans_git_connectivity <- vector()
mbkmeans_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters10.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters10.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  mbkmeans_git_bh[i] <- bh$ball_hall
  mbkmeans_git_ch[i] <- ch$calinski_harabasz
  mbkmeans_git_db[i] <- db$davies_bouldin
  mbkmeans_git_dunn[i] <- dunn$dunn
  mbkmeans_git_sd[i] <- sd$sd_dis
  mbkmeans_git_rl[i] <- rl$ratkowsky_lance
  mbkmeans_git_silh[i] <- silh$silhouette
  mbkmeans_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters10.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  mbkmeans_git_hub[i] <- hub$pearsongamma
}

saveRDS(mbkmeans_git_bh, "mbkmeans_git_bh.Rds")
saveRDS(mbkmeans_git_ch, "mbkmeans_git_ch.Rds")
saveRDS(mbkmeans_git_db, "mbkmeans_git_db.Rds")
saveRDS(mbkmeans_git_dunn, "mbkmeans_git_dunn.Rds")
saveRDS(mbkmeans_git_sd, "mbkmeans_git_sd.Rds")
saveRDS(mbkmeans_git_rl, "mbkmeans_git_rl.Rds")
saveRDS(mbkmeans_git_silh, "mbkmeans_git_silh.Rds")
saveRDS(mbkmeans_git_connectivity, "mbkmeans_git_connectivity.Rds")
saveRDS(mbkmeans_git_hub, "mbkmeans_git_hub.Rds")

# 11) fuzzy c-means 
n <- vector()
fcmeans_git_bh <- vector() # Ball-Hall
fcmeans_git_ch <- vector() # Calinski-Harabasz
fcmeans_git_db <- vector() # Davies-Bouldin
fcmeans_git_dunn <- vector() # Dunn
fcmeans_git_sd <- vector() # SD Distance
fcmeans_git_rl <- vector() # Ratkowsky-Lance
fcmeans_git_silh <- vector() # Silhouette
fcmeans_git_connectivity <- vector()
fcmeans_git_hub <- vector()
for(i in 1:length(git_ds)){
  print(i)
  n[i] <- dim(git_ds[[i]])[2]
  bh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('Ball_Hall'))
  ch <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('Calinski_Harabasz'))
  db <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('Davies_Bouldin'))
  dunn <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('Dunn'))
  sd <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('SD_Dis'))
  rl <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('Ratkowsky_Lance'))
  silh <- intCriteria(as.matrix(git_ds[[i]][, -(n[i]:n[i])]), as.integer(clusters11.6[[i]]), c('Silhouette'))
  hub <- cluster.stats(d = dist(git_ds[[i]]), clusters11.6[[i]], silhouette = FALSE, wgap = FALSE, sepindex = FALSE, sepwithnoise = FALSE)
  fcmeans_git_bh[i] <- bh$ball_hall
  fcmeans_git_ch[i] <- ch$calinski_harabasz
  fcmeans_git_db[i] <- db$davies_bouldin
  fcmeans_git_dunn[i] <- dunn$dunn
  fcmeans_git_sd[i] <- sd$sd_dis
  fcmeans_git_rl[i] <- rl$ratkowsky_lance
  fcmeans_git_silh[i] <- silh$silhouette
  fcmeans_git_connectivity[i] <- clValid::connectivity(distance = NULL, as.integer(clusters11.6[[i]]), Data = as.matrix(git_ds[[i]][, -(n[i]:n[i])]), neighbSize = 10, method = "euclidean")
  fcmeans_git_hub[i] <- hub$pearsongamma
}

saveRDS(fcmeans_git_bh, "fcmeans_git_bh.Rds")
saveRDS(fcmeans_git_ch, "fcmeans_git_ch.Rds")
saveRDS(fcmeans_git_db, "fcmeans_git_db.Rds")
saveRDS(fcmeans_git_dunn, "fcmeans_git_dunn.Rds")
saveRDS(fcmeans_git_sd, "fcmeans_git_sd.Rds")
saveRDS(fcmeans_git_rl, "fcmeans_git_rl.Rds")
saveRDS(fcmeans_git_silh, "fcmeans_git_silh.Rds")
saveRDS(fcmeans_git_connectivity, "fcmeans_git_connectivity.Rds")
saveRDS(fcmeans_git_hub, "fcmeans_git_hub.Rds")


## Generating performance measures - metadata - gaussian and ellipsoidal datasets --------------------------

# Ball-Hall ------------------------------------------------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_bh <- metadata
kmeans_gaussian_bh <- readRDS('kmeans_gaussian_bh.Rds')
kmeans_ellips_bh <- readRDS('kmeans_ellips_bh.Rds')
bh <- as.matrix(c(kmeans_gaussian_bh, kmeans_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_bh <- readRDS('kmedians_gaussian_bh.Rds')
kmedians_ellips_bh <- readRDS('kmedians_ellips_bh.Rds')
bh <- as.matrix(c(kmedians_gaussian_bh, kmedians_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_bh <- readRDS('kmedoids_gaussian_bh.Rds')
kmedoids_ellips_bh <- readRDS('kmedoids_ellips_bh.Rds')
bh <- as.matrix(c(kmedoids_gaussian_bh, kmedoids_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_bh <- readRDS('single_gaussian_bh.Rds')
single_ellips_bh <- readRDS('single_ellips_bh.Rds')
bh <- as.matrix(c(single_gaussian_bh, single_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_bh <- readRDS('complete_gaussian_bh.Rds')
complete_ellips_bh <- readRDS('complete_ellips_bh.Rds')
bh <- as.matrix(c(complete_gaussian_bh, complete_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_bh <- readRDS('average_gaussian_bh.Rds')
average_ellips_bh <- readRDS('average_ellips_bh.Rds')
bh <- as.matrix(c(average_gaussian_bh, average_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_bh <- readRDS('ward_gaussian_bh.Rds')
ward_ellips_bh <- readRDS('ward_ellips_bh.Rds')
bh <- as.matrix(c(ward_gaussian_bh, ward_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_bh <- readRDS('closest_gaussian_bh.Rds')
closest_ellips_bh <- readRDS('closest_ellips_bh.Rds')
bh <- as.matrix(c(closest_gaussian_bh, closest_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_bh <- readRDS('mbkmeans_gaussian_bh.Rds')
mbkmeans_ellips_bh <- readRDS('mbkmeans_ellips_bh.Rds')
bh <- as.matrix(c(mbkmeans_gaussian_bh, mbkmeans_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_bh <- readRDS('fcmeans_gaussian_bh.Rds')
fcmeans_ellips_bh <- readRDS('fcmeans_ellips_bh.Rds')
bh <- as.matrix(c(fcmeans_gaussian_bh, fcmeans_ellips_bh))
metadata_bh <- cbind(metadata_bh, bh)
colnames(metadata_bh)[colnames(metadata_bh) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_bh, 'metadata_bh.csv', row.names = FALSE)


# Calinski-Harabasz ------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_ch <- metadata
kmeans_gaussian_ch <- readRDS('kmeans_gaussian_ch.Rds')
kmeans_ellips_ch <- readRDS('kmeans_ellips_ch.Rds')
ch <- as.matrix(c(kmeans_gaussian_ch, kmeans_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_ch <- readRDS('kmedians_gaussian_ch.Rds')
kmedians_ellips_ch <- readRDS('kmedians_ellips_ch.Rds')
ch <- as.matrix(c(kmedians_gaussian_ch, kmedians_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_ch <- readRDS('kmedoids_gaussian_ch.Rds')
kmedoids_ellips_ch <- readRDS('kmedoids_ellips_ch.Rds')
ch <- as.matrix(c(kmedoids_gaussian_ch, kmedoids_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_ch <- readRDS('single_gaussian_ch.Rds')
single_ellips_ch <- readRDS('single_ellips_ch.Rds')
ch <- as.matrix(c(single_gaussian_ch, single_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_ch <- readRDS('complete_gaussian_ch.Rds')
complete_ellips_ch <- readRDS('complete_ellips_ch.Rds')
ch <- as.matrix(c(complete_gaussian_ch, complete_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_ch <- readRDS('average_gaussian_ch.Rds')
average_ellips_ch <- readRDS('average_ellips_ch.Rds')
ch <- as.matrix(c(average_gaussian_ch, average_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_ch <- readRDS('ward_gaussian_ch.Rds')
ward_ellips_ch <- readRDS('ward_ellips_ch.Rds')
ch <- as.matrix(c(ward_gaussian_ch, ward_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_ch <- readRDS('closest_gaussian_ch.Rds')
closest_ellips_ch <- readRDS('closest_ellips_ch.Rds')
ch <- as.matrix(c(closest_gaussian_ch, closest_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_ch <- readRDS('mbkmeans_gaussian_ch.Rds')
mbkmeans_ellips_ch <- readRDS('mbkmeans_ellips_ch.Rds')
ch <- as.matrix(c(mbkmeans_gaussian_ch, mbkmeans_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_ch <- readRDS('fcmeans_gaussian_ch.Rds')
fcmeans_ellips_ch <- readRDS('fcmeans_ellips_ch.Rds')
ch <- as.matrix(c(fcmeans_gaussian_ch, fcmeans_ellips_ch))
metadata_ch <- cbind(metadata_ch, ch)
colnames(metadata_ch)[colnames(metadata_ch) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_ch, 'metadata_ch.csv', row.names = FALSE)


# Connectivity ----------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_connectivity <- metadata
kmeans_gaussian_connectivity <- readRDS('kmeans_gaussian_connectivity.Rds')
kmeans_ellips_connectivity <- readRDS('kmeans_ellips_connectivity.Rds')
connectivity <- as.matrix(c(kmeans_gaussian_connectivity, kmeans_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_connectivity <- readRDS('kmedians_gaussian_connectivity.Rds')
kmedians_ellips_connectivity <- readRDS('kmedians_ellips_connectivity.Rds')
connectivity <- as.matrix(c(kmedians_gaussian_connectivity, kmedians_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_connectivity <- readRDS('kmedoids_gaussian_connectivity.Rds')
kmedoids_ellips_connectivity <- readRDS('kmedoids_ellips_connectivity.Rds')
connectivity <- as.matrix(c(kmedoids_gaussian_connectivity, kmedoids_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_connectivity <- readRDS('single_gaussian_connectivity.Rds')
single_ellips_connectivity <- readRDS('single_ellips_connectivity.Rds')
connectivity <- as.matrix(c(single_gaussian_connectivity, single_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_connectivity <- readRDS('complete_gaussian_connectivity.Rds')
complete_ellips_connectivity <- readRDS('complete_ellips_connectivity.Rds')
connectivity <- as.matrix(c(complete_gaussian_connectivity, complete_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_connectivity <- readRDS('average_gaussian_connectivity.Rds')
average_ellips_connectivity <- readRDS('average_ellips_connectivity.Rds')
connectivity <- as.matrix(c(average_gaussian_connectivity, average_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_connectivity <- readRDS('ward_gaussian_connectivity.Rds')
ward_ellips_connectivity <- readRDS('ward_ellips_connectivity.Rds')
connectivity <- as.matrix(c(ward_gaussian_connectivity, ward_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_connectivity <- readRDS('closest_gaussian_connectivity.Rds')
closest_ellips_connectivity <- readRDS('closest_ellips_connectivity.Rds')
connectivity <- as.matrix(c(closest_gaussian_connectivity, closest_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_connectivity <- readRDS('mbkmeans_gaussian_connectivity.Rds')
mbkmeans_ellips_connectivity <- readRDS('mbkmeans_ellips_connectivity.Rds')
connectivity <- as.matrix(c(mbkmeans_gaussian_connectivity, mbkmeans_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_connectivity <- readRDS('fcmeans_gaussian_connectivity.Rds')
fcmeans_ellips_connectivity <- readRDS('fcmeans_ellips_connectivity.Rds')
connectivity <- as.matrix(c(fcmeans_gaussian_connectivity, fcmeans_ellips_connectivity))
metadata_connectivity <- cbind(metadata_connectivity, connectivity)
colnames(metadata_connectivity)[colnames(metadata_connectivity) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_connectivity, 'metadata_connectivity.csv', row.names = FALSE)


# Davies-Bouldin --------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_db <- metadata
kmeans_gaussian_db <- readRDS('kmeans_gaussian_db.Rds')
kmeans_ellips_db <- readRDS('kmeans_ellips_db.Rds')
db <- as.matrix(c(kmeans_gaussian_db, kmeans_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_db <- readRDS('kmedians_gaussian_db.Rds')
kmedians_ellips_db <- readRDS('kmedians_ellips_db.Rds')
db <- as.matrix(c(kmedians_gaussian_db, kmedians_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_db <- readRDS('kmedoids_gaussian_db.Rds')
kmedoids_ellips_db <- readRDS('kmedoids_ellips_db.Rds')
db <- as.matrix(c(kmedoids_gaussian_db, kmedoids_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_db <- readRDS('single_gaussian_db.Rds')
single_ellips_db <- readRDS('single_ellips_db.Rds')
db <- as.matrix(c(single_gaussian_db, single_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_db <- readRDS('complete_gaussian_db.Rds')
complete_ellips_db <- readRDS('complete_ellips_db.Rds')
db <- as.matrix(c(complete_gaussian_db, complete_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_db <- readRDS('average_gaussian_db.Rds')
average_ellips_db <- readRDS('average_ellips_db.Rds')
db <- as.matrix(c(average_gaussian_db, average_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_db <- readRDS('ward_gaussian_db.Rds')
ward_ellips_db <- readRDS('ward_ellips_db.Rds')
db <- as.matrix(c(ward_gaussian_db, ward_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_db <- readRDS('closest_gaussian_db.Rds')
closest_ellips_db <- readRDS('closest_ellips_db.Rds')
db <- as.matrix(c(closest_gaussian_db, closest_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_db <- readRDS('mbkmeans_gaussian_db.Rds')
mbkmeans_ellips_db <- readRDS('mbkmeans_ellips_db.Rds')
db <- as.matrix(c(mbkmeans_gaussian_db, mbkmeans_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_db <- readRDS('fcmeans_gaussian_db.Rds')
fcmeans_ellips_db <- readRDS('fcmeans_ellips_db.Rds')
db <- as.matrix(c(fcmeans_gaussian_db, fcmeans_ellips_db))
metadata_db <- cbind(metadata_db, db)
colnames(metadata_db)[colnames(metadata_db) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_db, 'metadata_db.csv', row.names = FALSE)


# Dunn ------------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_dunn <- metadata
kmeans_gaussian_dunn <- readRDS('kmeans_gaussian_dunn.Rds')
kmeans_ellips_dunn <- readRDS('kmeans_ellips_dunn.Rds')
dunn <- as.matrix(c(kmeans_gaussian_dunn, kmeans_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_dunn <- readRDS('kmedians_gaussian_dunn.Rds')
kmedians_ellips_dunn <- readRDS('kmedians_ellips_dunn.Rds')
dunn <- as.matrix(c(kmedians_gaussian_dunn, kmedians_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_dunn <- readRDS('kmedoids_gaussian_dunn.Rds')
kmedoids_ellips_dunn <- readRDS('kmedoids_ellips_dunn.Rds')
dunn <- as.matrix(c(kmedoids_gaussian_dunn, kmedoids_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_dunn <- readRDS('single_gaussian_dunn.Rds')
single_ellips_dunn <- readRDS('single_ellips_dunn.Rds')
dunn <- as.matrix(c(single_gaussian_dunn, single_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_dunn <- readRDS('complete_gaussian_dunn.Rds')
complete_ellips_dunn <- readRDS('complete_ellips_dunn.Rds')
dunn <- as.matrix(c(complete_gaussian_dunn, complete_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_dunn <- readRDS('average_gaussian_dunn.Rds')
average_ellips_dunn <- readRDS('average_ellips_dunn.Rds')
dunn <- as.matrix(c(average_gaussian_dunn, average_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_dunn <- readRDS('ward_gaussian_dunn.Rds')
ward_ellips_dunn <- readRDS('ward_ellips_dunn.Rds')
dunn <- as.matrix(c(ward_gaussian_dunn, ward_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_dunn <- readRDS('closest_gaussian_dunn.Rds')
closest_ellips_dunn <- readRDS('closest_ellips_dunn.Rds')
dunn <- as.matrix(c(closest_gaussian_dunn, closest_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_dunn <- readRDS('mbkmeans_gaussian_dunn.Rds')
mbkmeans_ellips_dunn <- readRDS('mbkmeans_ellips_dunn.Rds')
dunn <- as.matrix(c(mbkmeans_gaussian_dunn, mbkmeans_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_dunn <- readRDS('fcmeans_gaussian_dunn.Rds')
fcmeans_ellips_dunn <- readRDS('fcmeans_ellips_dunn.Rds')
dunn <- as.matrix(c(fcmeans_gaussian_dunn, fcmeans_ellips_dunn))
metadata_dunn <- cbind(metadata_dunn, dunn)
colnames(metadata_dunn)[colnames(metadata_dunn) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_dunn, 'metadata_dunn.csv', row.names = FALSE)


# Ratkowsky-Lance -------------------------------------------------

require(data.table)
etadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_rl <- metadata
kmeans_gaussian_rl <- readRDS('kmeans_gaussian_rl.Rds')
kmeans_ellips_rl <- readRDS('kmeans_ellips_rl.Rds')
rl <- as.matrix(c(kmeans_gaussian_rl, kmeans_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_rl <- readRDS('kmedians_gaussian_rl.Rds')
kmedians_ellips_rl <- readRDS('kmedians_ellips_rl.Rds')
rl <- as.matrix(c(kmedians_gaussian_rl, kmedians_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_rl <- readRDS('kmedoids_gaussian_rl.Rds')
kmedoids_ellips_rl <- readRDS('kmedoids_ellips_rl.Rds')
rl <- as.matrix(c(kmedoids_gaussian_rl, kmedoids_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_rl <- readRDS('single_gaussian_rl.Rds')
single_ellips_rl <- readRDS('single_ellips_rl.Rds')
rl <- as.matrix(c(single_gaussian_rl, single_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_rl <- readRDS('complete_gaussian_rl.Rds')
complete_ellips_rl <- readRDS('complete_ellips_rl.Rds')
rl <- as.matrix(c(complete_gaussian_rl, complete_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_rl <- readRDS('average_gaussian_rl.Rds')
average_ellips_rl <- readRDS('average_ellips_rl.Rds')
rl <- as.matrix(c(average_gaussian_rl, average_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_rl <- readRDS('ward_gaussian_rl.Rds')
ward_ellips_rl <- readRDS('ward_ellips_rl.Rds')
rl <- as.matrix(c(ward_gaussian_rl, ward_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_rl <- readRDS('closest_gaussian_rl.Rds')
closest_ellips_rl <- readRDS('closest_ellips_rl.Rds')
rl <- as.matrix(c(closest_gaussian_rl, closest_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_rl <- readRDS('mbkmeans_gaussian_rl.Rds')
mbkmeans_ellips_rl <- readRDS('mbkmeans_ellips_rl.Rds')
rl <- as.matrix(c(mbkmeans_gaussian_rl, mbkmeans_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_rl <- readRDS('fcmeans_gaussian_rl.Rds')
fcmeans_ellips_rl <- readRDS('fcmeans_ellips_rl.Rds')
rl <- as.matrix(c(fcmeans_gaussian_rl, fcmeans_ellips_rl))
metadata_rl <- cbind(metadata_rl, rl)
colnames(metadata_rl)[colnames(metadata_rl) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_rl, 'metadata_rl.csv', row.names = FALSE)


# SD Distance ------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_sd <- metadata
kmeans_gaussian_sd <- readRDS('kmeans_gaussian_sd.Rds')
kmeans_ellips_sd <- readRDS('kmeans_ellips_sd.Rds')
sd <- as.matrix(c(kmeans_gaussian_sd, kmeans_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_sd <- readRDS('kmedians_gaussian_sd.Rds')
kmedians_ellips_sd <- readRDS('kmedians_ellips_sd.Rds')
sd <- as.matrix(c(kmedians_gaussian_sd, kmedians_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_sd <- readRDS('kmedoids_gaussian_sd.Rds')
kmedoids_ellips_sd <- readRDS('kmedoids_ellips_sd.Rds')
sd <- as.matrix(c(kmedoids_gaussian_sd, kmedoids_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_sd <- readRDS('single_gaussian_sd.Rds')
single_ellips_sd <- readRDS('single_ellips_sd.Rds')
sd <- as.matrix(c(single_gaussian_sd, single_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_sd <- readRDS('complete_gaussian_sd.Rds')
complete_ellips_sd <- readRDS('complete_ellips_sd.Rds')
sd <- as.matrix(c(complete_gaussian_sd, complete_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_sd <- readRDS('average_gaussian_sd.Rds')
average_ellips_sd <- readRDS('average_ellips_sd.Rds')
sd <- as.matrix(c(average_gaussian_sd, average_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_sd <- readRDS('ward_gaussian_sd.Rds')
ward_ellips_sd <- readRDS('ward_ellips_sd.Rds')
sd <- as.matrix(c(ward_gaussian_sd, ward_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_sd <- readRDS('closest_gaussian_sd.Rds')
closest_ellips_sd <- readRDS('closest_ellips_sd.Rds')
sd <- as.matrix(c(closest_gaussian_sd, closest_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_sd <- readRDS('mbkmeans_gaussian_sd.Rds')
mbkmeans_ellips_sd <- readRDS('mbkmeans_ellips_sd.Rds')
sd <- as.matrix(c(mbkmeans_gaussian_sd, mbkmeans_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_sd <- readRDS('fcmeans_gaussian_sd.Rds')
fcmeans_ellips_sd <- readRDS('fcmeans_ellips_sd.Rds')
sd <- as.matrix(c(fcmeans_gaussian_sd, fcmeans_ellips_sd))
metadata_sd <- cbind(metadata_sd, sd)
colnames(metadata_sd)[colnames(metadata_sd) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_sd, 'metadata_sd.csv', row.names = FALSE)


# Silhouette ------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[1:160, 1:16]

# k-means
metadata_silh <- metadata
kmeans_gaussian_silh <- readRDS('kmeans_gaussian_silh.Rds')
kmeans_ellips_silh <- readRDS('kmeans_ellips_silh.Rds')
silh <- as.matrix(c(kmeans_gaussian_silh, kmeans_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_gaussian_silh <- readRDS('kmedians_gaussian_silh.Rds')
kmedians_ellips_silh <- readRDS('kmedians_ellips_silh.Rds')
silh <- as.matrix(c(kmedians_gaussian_silh, kmedians_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_gaussian_silh <- readRDS('kmedoids_gaussian_silh.Rds')
kmedoids_ellips_silh <- readRDS('kmedoids_ellips_silh.Rds')
silh <- as.matrix(c(kmedoids_gaussian_silh, kmedoids_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_gaussian_silh <- readRDS('single_gaussian_silh.Rds')
single_ellips_silh <- readRDS('single_ellips_silh.Rds')
silh <- as.matrix(c(single_gaussian_silh, single_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_single'

# complete linkage
complete_gaussian_silh <- readRDS('complete_gaussian_silh.Rds')
complete_ellips_silh <- readRDS('complete_ellips_silh.Rds')
silh <- as.matrix(c(complete_gaussian_silh, complete_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_complete'

# average linkage
average_gaussian_silh <- readRDS('average_gaussian_silh.Rds')
average_ellips_silh <- readRDS('average_ellips_silh.Rds')
silh <- as.matrix(c(average_gaussian_silh, average_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_average'

# Ward's method
ward_gaussian_silh <- readRDS('ward_gaussian_silh.Rds')
ward_ellips_silh <- readRDS('ward_ellips_silh.Rds')
silh <- as.matrix(c(ward_gaussian_silh, ward_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_ward'

# closest centroid
closest_gaussian_silh <- readRDS('closest_gaussian_silh.Rds')
closest_ellips_silh <- readRDS('closest_ellips_silh.Rds')
silh <- as.matrix(c(closest_gaussian_silh, closest_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_gaussian_silh <- readRDS('mbkmeans_gaussian_silh.Rds')
mbkmeans_ellips_silh <- readRDS('mbkmeans_ellips_silh.Rds')
silh <- as.matrix(c(mbkmeans_gaussian_silh, mbkmeans_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_gaussian_silh <- readRDS('fcmeans_gaussian_silh.Rds')
fcmeans_ellips_silh <- readRDS('fcmeans_ellips_silh.Rds')
silh <- as.matrix(c(fcmeans_gaussian_silh, fcmeans_ellips_silh))
metadata_silh <- cbind(metadata_silh, silh)
colnames(metadata_silh)[colnames(metadata_silh) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_silh, 'metadata_silh.csv', row.names = FALSE)



## Generating performance measures - metadata - small, medium and large datasets --------------------------

# Ball-Hall ------------------------------------------------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_bh <- readRDS('kmeans_small_bh.Rds')
kmeans_medium_bh <- readRDS('kmeans_medium_bh.Rds')
kmeans_large_bh <- readRDS('kmeans_large_bh.Rds')
bh2 <- as.matrix(c(kmeans_small_bh, kmeans_medium_bh, kmeans_large_bh))
metadata_bh2 <- cbind(metadata, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_bh <- readRDS('kmedians_small_bh.Rds')
kmedians_medium_bh <- readRDS('kmedians_medium_bh.Rds')
kmedians_large_bh <- readRDS('kmedians_large_bh.Rds')
bh2 <- as.matrix(c(kmedians_small_bh, kmedians_medium_bh, kmedians_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_bh <- readRDS('kmedoids_small_bh.Rds')
kmedoids_medium_bh <- readRDS('kmedoids_medium_bh.Rds')
kmedoids_large_bh <- readRDS('kmedoids_large_bh.Rds')
bh2 <- as.matrix(c(kmedoids_small_bh, kmedoids_medium_bh, kmedoids_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_bh <- readRDS('single_small_bh.Rds')
single_medium_bh <- readRDS('single_medium_bh.Rds')
single_large_bh <- readRDS('single_large_bh.Rds')
bh2 <- as.matrix(c(single_small_bh, single_medium_bh, single_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_bh <- readRDS('complete_small_bh.Rds')
complete_medium_bh <- readRDS('complete_medium_bh.Rds')
complete_large_bh <- readRDS('complete_large_bh.Rds')
bh2 <- as.matrix(c(complete_small_bh, complete_medium_bh, complete_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_bh <- readRDS('average_small_bh.Rds')
average_medium_bh <- readRDS('average_medium_bh.Rds')
average_large_bh <- readRDS('average_large_bh.Rds')
bh2 <- as.matrix(c(average_small_bh, average_medium_bh, average_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_bh <- readRDS('ward_small_bh.Rds')
ward_medium_bh <- readRDS('ward_medium_bh.Rds')
ward_large_bh <- readRDS('ward_large_bh.Rds')
bh2 <- as.matrix(c(ward_small_bh, ward_medium_bh, ward_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_bh <- readRDS('closest_small_bh.Rds')
closest_medium_bh <- readRDS('closest_medium_bh.Rds')
closest_large_bh <- readRDS('closest_large_bh.Rds')
bh2 <- as.matrix(c(closest_small_bh, closest_medium_bh, closest_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_bh <- readRDS('mbkmeans_small_bh.Rds')
mbkmeans_medium_bh <- readRDS('mbkmeans_medium_bh.Rds')
mbkmeans_large_bh <- readRDS('mbkmeans_large_bh.Rds')
bh2 <- as.matrix(c(mbkmeans_small_bh, mbkmeans_medium_bh, mbkmeans_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_bh <- readRDS('fcmeans_small_bh.Rds')
fcmeans_medium_bh <- readRDS('fcmeans_medium_bh.Rds')
fcmeans_large_bh <- readRDS('fcmeans_large_bh.Rds')
bh2 <- as.matrix(c(fcmeans_small_bh, fcmeans_medium_bh, fcmeans_large_bh))
metadata_bh2 <- cbind(metadata_bh2, bh2)
colnames(metadata_bh2)[colnames(metadata_bh2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_bh2, 'metadata_bh2.csv', row.names = FALSE)


# Calinski-Harabasz ------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_ch <- readRDS('kmeans_small_ch.Rds')
kmeans_medium_ch <- readRDS('kmeans_medium_ch.Rds')
kmeans_large_ch <- readRDS('kmeans_large_ch.Rds')
ch2 <- as.matrix(c(kmeans_small_ch, kmeans_medium_ch, kmeans_large_ch))
metadata_ch2 <- cbind(metadata, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_ch <- readRDS('kmedians_small_ch.Rds')
kmedians_medium_ch <- readRDS('kmedians_medium_ch.Rds')
kmedians_large_ch <- readRDS('kmedians_large_ch.Rds')
ch2 <- as.matrix(c(kmedians_small_ch, kmedians_medium_ch, kmedians_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_ch <- readRDS('kmedoids_small_ch.Rds')
kmedoids_medium_ch <- readRDS('kmedoids_medium_ch.Rds')
kmedoids_large_ch <- readRDS('kmedoids_large_ch.Rds')
ch2 <- as.matrix(c(kmedoids_small_ch, kmedoids_medium_ch, kmedoids_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_ch <- readRDS('single_small_ch.Rds')
single_medium_ch <- readRDS('single_medium_ch.Rds')
single_large_ch <- readRDS('single_large_ch.Rds')
ch2 <- as.matrix(c(single_small_ch, single_medium_ch, single_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_ch <- readRDS('complete_small_ch.Rds')
complete_medium_ch <- readRDS('complete_medium_ch.Rds')
complete_large_ch <- readRDS('complete_large_ch.Rds')
ch2 <- as.matrix(c(complete_small_ch, complete_medium_ch, complete_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_ch <- readRDS('average_small_ch.Rds')
average_medium_ch <- readRDS('average_medium_ch.Rds')
average_large_ch <- readRDS('average_large_ch.Rds')
ch2 <- as.matrix(c(average_small_ch, average_medium_ch, average_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_ch <- readRDS('ward_small_ch.Rds')
ward_medium_ch <- readRDS('ward_medium_ch.Rds')
ward_large_ch <- readRDS('ward_large_ch.Rds')
ch2 <- as.matrix(c(ward_small_ch, ward_medium_ch, ward_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_ch <- readRDS('closest_small_ch.Rds')
closest_medium_ch <- readRDS('closest_medium_ch.Rds')
closest_large_ch <- readRDS('closest_large_ch.Rds')
ch2 <- as.matrix(c(closest_small_ch, closest_medium_ch, closest_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_ch <- readRDS('mbkmeans_small_ch.Rds')
mbkmeans_medium_ch <- readRDS('mbkmeans_medium_ch.Rds')
mbkmeans_large_ch <- readRDS('mbkmeans_large_ch.Rds')
ch2 <- as.matrix(c(mbkmeans_small_ch, mbkmeans_medium_ch, mbkmeans_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_ch <- readRDS('fcmeans_small_ch.Rds')
fcmeans_medium_ch <- readRDS('fcmeans_medium_ch.Rds')
fcmeans_large_ch <- readRDS('fcmeans_large_ch.Rds')
ch2 <- as.matrix(c(fcmeans_small_ch, fcmeans_medium_ch, fcmeans_large_ch))
metadata_ch2 <- cbind(metadata_ch2, ch2)
colnames(metadata_ch2)[colnames(metadata_ch2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_ch2, 'metadata_ch2.csv', row.names = FALSE)


# Connectivity ----------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_connectivity <- readRDS('kmeans_small_connectivity.Rds')
kmeans_medium_connectivity <- readRDS('kmeans_medium_connectivity.Rds')
kmeans_large_connectivity <- readRDS('kmeans_large_connectivity.Rds')
connectivity2 <- as.matrix(c(kmeans_small_connectivity, kmeans_medium_connectivity, kmeans_large_connectivity))
metadata_connectivity2 <- cbind(metadata, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_connectivity <- readRDS('kmedians_small_connectivity.Rds')
kmedians_medium_connectivity <- readRDS('kmedians_medium_connectivity.Rds')
kmedians_large_connectivity <- readRDS('kmedians_large_connectivity.Rds')
connectivity2 <- as.matrix(c(kmedians_small_connectivity, kmedians_medium_connectivity, kmedians_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_connectivity <- readRDS('kmedoids_small_connectivity.Rds')
kmedoids_medium_connectivity <- readRDS('kmedoids_medium_connectivity.Rds')
kmedoids_large_connectivity <- readRDS('kmedoids_large_connectivity.Rds')
connectivity2 <- as.matrix(c(kmedoids_small_connectivity, kmedoids_medium_connectivity, kmedoids_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_connectivity <- readRDS('single_small_connectivity.Rds')
single_medium_connectivity <- readRDS('single_medium_connectivity.Rds')
single_large_connectivity <- readRDS('single_large_connectivity.Rds')
connectivity2 <- as.matrix(c(single_small_connectivity, single_medium_connectivity, single_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_connectivity <- readRDS('complete_small_connectivity.Rds')
complete_medium_connectivity <- readRDS('complete_medium_connectivity.Rds')
complete_large_connectivity <- readRDS('complete_large_connectivity.Rds')
connectivity2 <- as.matrix(c(complete_small_connectivity, complete_medium_connectivity, complete_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_connectivity <- readRDS('average_small_connectivity.Rds')
average_medium_connectivity <- readRDS('average_medium_connectivity.Rds')
average_large_connectivity <- readRDS('average_large_connectivity.Rds')
connectivity2 <- as.matrix(c(average_small_connectivity, average_medium_connectivity, average_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_connectivity <- readRDS('ward_small_connectivity.Rds')
ward_medium_connectivity <- readRDS('ward_medium_connectivity.Rds')
ward_large_connectivity <- readRDS('ward_large_connectivity.Rds')
connectivity2 <- as.matrix(c(ward_small_connectivity, ward_medium_connectivity, ward_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_connectivity <- readRDS('closest_small_connectivity.Rds')
closest_medium_connectivity <- readRDS('closest_medium_connectivity.Rds')
closest_large_connectivity <- readRDS('closest_large_connectivity.Rds')
connectivity2 <- as.matrix(c(closest_small_connectivity, closest_medium_connectivity, closest_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_connectivity <- readRDS('mbkmeans_small_connectivity.Rds')
mbkmeans_medium_connectivity <- readRDS('mbkmeans_medium_connectivity.Rds')
mbkmeans_large_connectivity <- readRDS('mbkmeans_large_connectivity.Rds')
connectivity2 <- as.matrix(c(mbkmeans_small_connectivity, mbkmeans_medium_connectivity, mbkmeans_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_connectivity <- readRDS('fcmeans_small_connectivity.Rds')
fcmeans_medium_connectivity <- readRDS('fcmeans_medium_connectivity.Rds')
fcmeans_large_connectivity <- readRDS('fcmeans_large_connectivity.Rds')
connectivity2 <- as.matrix(c(fcmeans_small_connectivity, fcmeans_medium_connectivity, fcmeans_large_connectivity))
metadata_connectivity2 <- cbind(metadata_connectivity2, connectivity2)
colnames(metadata_connectivity2)[colnames(metadata_connectivity2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_connectivity2, 'metadata_connectivity2.csv', row.names = FALSE)

# Davies-Bouldin --------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_db <- readRDS('kmeans_small_db.Rds')
kmeans_medium_db <- readRDS('kmeans_medium_db.Rds')
kmeans_large_db <- readRDS('kmeans_large_db.Rds')
db2 <- as.matrix(c(kmeans_small_db, kmeans_medium_db, kmeans_large_db))
metadata_db2 <- cbind(metadata, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_db <- readRDS('kmedians_small_db.Rds')
kmedians_medium_db <- readRDS('kmedians_medium_db.Rds')
kmedians_large_db <- readRDS('kmedians_large_db.Rds')
db2 <- as.matrix(c(kmedians_small_db, kmedians_medium_db, kmedians_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_db <- readRDS('kmedoids_small_db.Rds')
kmedoids_medium_db <- readRDS('kmedoids_medium_db.Rds')
kmedoids_large_db <- readRDS('kmedoids_large_db.Rds')
db2 <- as.matrix(c(kmedoids_small_db, kmedoids_medium_db, kmedoids_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_db <- readRDS('single_small_db.Rds')
single_medium_db <- readRDS('single_medium_db.Rds')
single_large_db <- readRDS('single_large_db.Rds')
db2 <- as.matrix(c(single_small_db, single_medium_db, single_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_db <- readRDS('complete_small_db.Rds')
complete_medium_db <- readRDS('complete_medium_db.Rds')
complete_large_db <- readRDS('complete_large_db.Rds')
db2 <- as.matrix(c(complete_small_db, complete_medium_db, complete_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_db <- readRDS('average_small_db.Rds')
average_medium_db <- readRDS('average_medium_db.Rds')
average_large_db <- readRDS('average_large_db.Rds')
db2 <- as.matrix(c(average_small_db, average_medium_db, average_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_db <- readRDS('ward_small_db.Rds')
ward_medium_db <- readRDS('ward_medium_db.Rds')
ward_large_db <- readRDS('ward_large_db.Rds')
db2 <- as.matrix(c(ward_small_db, ward_medium_db, ward_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_db <- readRDS('closest_small_db.Rds')
closest_medium_db <- readRDS('closest_medium_db.Rds')
closest_large_db <- readRDS('closest_large_db.Rds')
db2 <- as.matrix(c(closest_small_db, closest_medium_db, closest_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_db <- readRDS('mbkmeans_small_db.Rds')
mbkmeans_medium_db <- readRDS('mbkmeans_medium_db.Rds')
mbkmeans_large_db <- readRDS('mbkmeans_large_db.Rds')
db2 <- as.matrix(c(mbkmeans_small_db, mbkmeans_medium_db, mbkmeans_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_db <- readRDS('fcmeans_small_db.Rds')
fcmeans_medium_db <- readRDS('fcmeans_medium_db.Rds')
fcmeans_large_db <- readRDS('fcmeans_large_db.Rds')
db2 <- as.matrix(c(fcmeans_small_db, fcmeans_medium_db, fcmeans_large_db))
metadata_db2 <- cbind(metadata_db2, db2)
colnames(metadata_db2)[colnames(metadata_db2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_db2, 'metadata_db2.csv', row.names = FALSE)


# Dunn ------------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_dunn <- readRDS('kmeans_small_dunn.Rds')
kmeans_medium_dunn <- readRDS('kmeans_medium_dunn.Rds')
kmeans_large_dunn <- readRDS('kmeans_large_dunn.Rds')
dunn2 <- as.matrix(c(kmeans_small_dunn, kmeans_medium_dunn, kmeans_large_dunn))
metadata_dunn2 <- cbind(metadata, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_dunn <- readRDS('kmedians_small_dunn.Rds')
kmedians_medium_dunn <- readRDS('kmedians_medium_dunn.Rds')
kmedians_large_dunn <- readRDS('kmedians_large_dunn.Rds')
dunn2 <- as.matrix(c(kmedians_small_dunn, kmedians_medium_dunn, kmedians_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_dunn <- readRDS('kmedoids_small_dunn.Rds')
kmedoids_medium_dunn <- readRDS('kmedoids_medium_dunn.Rds')
kmedoids_large_dunn <- readRDS('kmedoids_large_dunn.Rds')
dunn2 <- as.matrix(c(kmedoids_small_dunn, kmedoids_medium_dunn, kmedoids_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_dunn <- readRDS('single_small_dunn.Rds')
single_medium_dunn <- readRDS('single_medium_dunn.Rds')
single_large_dunn <- readRDS('single_large_dunn.Rds')
dunn2 <- as.matrix(c(single_small_dunn, single_medium_dunn, single_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_dunn <- readRDS('complete_small_dunn.Rds')
complete_medium_dunn <- readRDS('complete_medium_dunn.Rds')
complete_large_dunn <- readRDS('complete_large_dunn.Rds')
dunn2 <- as.matrix(c(complete_small_dunn, complete_medium_dunn, complete_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_dunn <- readRDS('average_small_dunn.Rds')
average_medium_dunn <- readRDS('average_medium_dunn.Rds')
average_large_dunn <- readRDS('average_large_dunn.Rds')
dunn2 <- as.matrix(c(average_small_dunn, average_medium_dunn, average_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_dunn <- readRDS('ward_small_dunn.Rds')
ward_medium_dunn <- readRDS('ward_medium_dunn.Rds')
ward_large_dunn <- readRDS('ward_large_dunn.Rds')
dunn2 <- as.matrix(c(ward_small_dunn, ward_medium_dunn, ward_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_dunn <- readRDS('closest_small_dunn.Rds')
closest_medium_dunn <- readRDS('closest_medium_dunn.Rds')
closest_large_dunn <- readRDS('closest_large_dunn.Rds')
dunn2 <- as.matrix(c(closest_small_dunn, closest_medium_dunn, closest_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_dunn <- readRDS('mbkmeans_small_dunn.Rds')
mbkmeans_medium_dunn <- readRDS('mbkmeans_medium_dunn.Rds')
mbkmeans_large_dunn <- readRDS('mbkmeans_large_dunn.Rds')
dunn2 <- as.matrix(c(mbkmeans_small_dunn, mbkmeans_medium_dunn, mbkmeans_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_dunn <- readRDS('fcmeans_small_dunn.Rds')
fcmeans_medium_dunn <- readRDS('fcmeans_medium_dunn.Rds')
fcmeans_large_dunn <- readRDS('fcmeans_large_dunn.Rds')
dunn2 <- as.matrix(c(fcmeans_small_dunn, fcmeans_medium_dunn, fcmeans_large_dunn))
metadata_dunn2 <- cbind(metadata_dunn2, dunn2)
colnames(metadata_dunn2)[colnames(metadata_dunn2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_dunn2, 'metadata_dunn2.csv', row.names = FALSE)


# Ratkowsky-Lance -------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_rl <- readRDS('kmeans_small_rl.Rds')
kmeans_medium_rl <- readRDS('kmeans_medium_rl.Rds')
kmeans_large_rl <- readRDS('kmeans_large_rl.Rds')
rl2 <- as.matrix(c(kmeans_small_rl, kmeans_medium_rl, kmeans_large_rl))
metadata_rl2 <- cbind(metadata, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_rl <- readRDS('kmedians_small_rl.Rds')
kmedians_medium_rl <- readRDS('kmedians_medium_rl.Rds')
kmedians_large_rl <- readRDS('kmedians_large_rl.Rds')
rl2 <- as.matrix(c(kmedians_small_rl, kmedians_medium_rl, kmedians_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_rl <- readRDS('kmedoids_small_rl.Rds')
kmedoids_medium_rl <- readRDS('kmedoids_medium_rl.Rds')
kmedoids_large_rl <- readRDS('kmedoids_large_rl.Rds')
rl2 <- as.matrix(c(kmedoids_small_rl, kmedoids_medium_rl, kmedoids_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_rl <- readRDS('single_small_rl.Rds')
single_medium_rl <- readRDS('single_medium_rl.Rds')
single_large_rl <- readRDS('single_large_rl.Rds')
rl2 <- as.matrix(c(single_small_rl, single_medium_rl, single_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_rl <- readRDS('complete_small_rl.Rds')
complete_medium_rl <- readRDS('complete_medium_rl.Rds')
complete_large_rl <- readRDS('complete_large_rl.Rds')
rl2 <- as.matrix(c(complete_small_rl, complete_medium_rl, complete_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_rl <- readRDS('average_small_rl.Rds')
average_medium_rl <- readRDS('average_medium_rl.Rds')
average_large_rl <- readRDS('average_large_rl.Rds')
rl2 <- as.matrix(c(average_small_rl, average_medium_rl, average_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_rl <- readRDS('ward_small_rl.Rds')
ward_medium_rl <- readRDS('ward_medium_rl.Rds')
ward_large_rl <- readRDS('ward_large_rl.Rds')
rl2 <- as.matrix(c(ward_small_rl, ward_medium_rl, ward_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_rl <- readRDS('closest_small_rl.Rds')
closest_medium_rl <- readRDS('closest_medium_rl.Rds')
closest_large_rl <- readRDS('closest_large_rl.Rds')
rl2 <- as.matrix(c(closest_small_rl, closest_medium_rl, closest_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_rl <- readRDS('mbkmeans_small_rl.Rds')
mbkmeans_medium_rl <- readRDS('mbkmeans_medium_rl.Rds')
mbkmeans_large_rl <- readRDS('mbkmeans_large_rl.Rds')
rl2 <- as.matrix(c(mbkmeans_small_rl, mbkmeans_medium_rl, mbkmeans_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_rl <- readRDS('fcmeans_small_rl.Rds')
fcmeans_medium_rl <- readRDS('fcmeans_medium_rl.Rds')
fcmeans_large_rl <- readRDS('fcmeans_large_rl.Rds')
rl2 <- as.matrix(c(fcmeans_small_rl, fcmeans_medium_rl, fcmeans_large_rl))
metadata_rl2 <- cbind(metadata_rl2, rl2)
colnames(metadata_rl2)[colnames(metadata_rl2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_rl2, 'metadata_rl2.csv', row.names = FALSE)


# SD Distance ------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_sd <- readRDS('kmeans_small_sd.Rds')
kmeans_medium_sd <- readRDS('kmeans_medium_sd.Rds')
kmeans_large_sd <- readRDS('kmeans_large_sd.Rds')
sd2 <- as.matrix(c(kmeans_small_sd, kmeans_medium_sd, kmeans_large_sd))
metadata_sd2 <- cbind(metadata, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_sd <- readRDS('kmedians_small_sd.Rds')
kmedians_medium_sd <- readRDS('kmedians_medium_sd.Rds')
kmedians_large_sd <- readRDS('kmedians_large_sd.Rds')
sd2 <- as.matrix(c(kmedians_small_sd, kmedians_medium_sd, kmedians_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_sd <- readRDS('kmedoids_small_sd.Rds')
kmedoids_medium_sd <- readRDS('kmedoids_medium_sd.Rds')
kmedoids_large_sd <- readRDS('kmedoids_large_sd.Rds')
sd2 <- as.matrix(c(kmedoids_small_sd, kmedoids_medium_sd, kmedoids_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_sd <- readRDS('single_small_sd.Rds')
single_medium_sd <- readRDS('single_medium_sd.Rds')
single_large_sd <- readRDS('single_large_sd.Rds')
sd2 <- as.matrix(c(single_small_sd, single_medium_sd, single_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_sd <- readRDS('complete_small_sd.Rds')
complete_medium_sd <- readRDS('complete_medium_sd.Rds')
complete_large_sd <- readRDS('complete_large_sd.Rds')
sd2 <- as.matrix(c(complete_small_sd, complete_medium_sd, complete_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_sd <- readRDS('average_small_sd.Rds')
average_medium_sd <- readRDS('average_medium_sd.Rds')
average_large_sd <- readRDS('average_large_sd.Rds')
sd2 <- as.matrix(c(average_small_sd, average_medium_sd, average_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_sd <- readRDS('ward_small_sd.Rds')
ward_medium_sd <- readRDS('ward_medium_sd.Rds')
ward_large_sd <- readRDS('ward_large_sd.Rds')
sd2 <- as.matrix(c(ward_small_sd, ward_medium_sd, ward_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_sd <- readRDS('closest_small_sd.Rds')
closest_medium_sd <- readRDS('closest_medium_sd.Rds')
closest_large_sd <- readRDS('closest_large_sd.Rds')
sd2 <- as.matrix(c(closest_small_sd, closest_medium_sd, closest_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_sd <- readRDS('mbkmeans_small_sd.Rds')
mbkmeans_medium_sd <- readRDS('mbkmeans_medium_sd.Rds')
mbkmeans_large_sd <- readRDS('mbkmeans_large_sd.Rds')
sd2 <- as.matrix(c(mbkmeans_small_sd, mbkmeans_medium_sd, mbkmeans_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_sd <- readRDS('fcmeans_small_sd.Rds')
fcmeans_medium_sd <- readRDS('fcmeans_medium_sd.Rds')
fcmeans_large_sd <- readRDS('fcmeans_large_sd.Rds')
sd2 <- as.matrix(c(fcmeans_small_sd, fcmeans_medium_sd, fcmeans_large_sd))
metadata_sd2 <- cbind(metadata_sd2, sd2)
colnames(metadata_sd2)[colnames(metadata_sd2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_sd2, 'metadata_sd2.csv', row.names = FALSE)


# Silhouette ------------------------------------------------------

require(data.table)
metadata2 <- fread('metadata2.csv')
metadata <- metadata2[161:279, 1:16] 

# k-means
kmeans_small_silh <- readRDS('kmeans_small_silh.Rds')
kmeans_medium_silh <- readRDS('kmeans_medium_silh.Rds')
kmeans_large_silh <- readRDS('kmeans_large_silh.Rds')
silh2 <- as.matrix(c(kmeans_small_silh, kmeans_medium_silh, kmeans_large_silh))
metadata_silh2 <- cbind(metadata, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_kmeans'

# k-medians
kmedians_small_silh <- readRDS('kmedians_small_silh.Rds')
kmedians_medium_silh <- readRDS('kmedians_medium_silh.Rds')
kmedians_large_silh <- readRDS('kmedians_large_silh.Rds')
silh2 <- as.matrix(c(kmedians_small_silh, kmedians_medium_silh, kmedians_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_kmedians'

# k-medoids
kmedoids_small_silh <- readRDS('kmedoids_small_silh.Rds')
kmedoids_medium_silh <- readRDS('kmedoids_medium_silh.Rds')
kmedoids_large_silh <- readRDS('kmedoids_large_silh.Rds')
silh2 <- as.matrix(c(kmedoids_small_silh, kmedoids_medium_silh, kmedoids_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_kmedoids'

# single linkage
single_small_silh <- readRDS('single_small_silh.Rds')
single_medium_silh <- readRDS('single_medium_silh.Rds')
single_large_silh <- readRDS('single_large_silh.Rds')
silh2 <- as.matrix(c(single_small_silh, single_medium_silh, single_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_single'

# complete linkage
complete_small_silh <- readRDS('complete_small_silh.Rds')
complete_medium_silh <- readRDS('complete_medium_silh.Rds')
complete_large_silh <- readRDS('complete_large_silh.Rds')
silh2 <- as.matrix(c(complete_small_silh, complete_medium_silh, complete_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_complete'

# average linkage
average_small_silh <- readRDS('average_small_silh.Rds')
average_medium_silh <- readRDS('average_medium_silh.Rds')
average_large_silh <- readRDS('average_large_silh.Rds')
silh2 <- as.matrix(c(average_small_silh, average_medium_silh, average_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_average'

# Ward's method
ward_small_silh <- readRDS('ward_small_silh.Rds')
ward_medium_silh <- readRDS('ward_medium_silh.Rds')
ward_large_silh <- readRDS('ward_large_silh.Rds')
silh2 <- as.matrix(c(ward_small_silh, ward_medium_silh, ward_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_ward'

# closest centroid
closest_small_silh <- readRDS('closest_small_silh.Rds')
closest_medium_silh <- readRDS('closest_medium_silh.Rds')
closest_large_silh <- readRDS('closest_large_silh.Rds')
silh2 <- as.matrix(c(closest_small_silh, closest_medium_silh, closest_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_closest'

# mini-batch k-means
mbkmeans_small_silh <- readRDS('mbkmeans_small_silh.Rds')
mbkmeans_medium_silh <- readRDS('mbkmeans_medium_silh.Rds')
mbkmeans_large_silh <- readRDS('mbkmeans_large_silh.Rds')
silh2 <- as.matrix(c(mbkmeans_small_silh, mbkmeans_medium_silh, mbkmeans_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_mbkmeans'

# fuzzy c-means
fcmeans_small_silh <- readRDS('fcmeans_small_silh.Rds')
fcmeans_medium_silh <- readRDS('fcmeans_medium_silh.Rds')
fcmeans_large_silh <- readRDS('fcmeans_large_silh.Rds')
silh2 <- as.matrix(c(fcmeans_small_silh, fcmeans_medium_silh, fcmeans_large_silh))
metadata_silh2 <- cbind(metadata_silh2, silh2)
colnames(metadata_silh2)[colnames(metadata_silh2) == 'V1'] <- 'algo_fcmeans'

write.csv(metadata_silh2, 'metadata_silh2.csv', row.names = FALSE)


## consolidation of metadata
metadata_bh <- fread('metadata_bh.csv')
metadata_bh2 <- fread('metadata_bh2.csv')
metadata_bh_full <- rbind(metadata_bh, metadata_bh2)
write.csv(metadata_bh_full, 'metadata_bh_full.csv', row.names = FALSE)

metadata_ch <- fread('metadata_ch.csv')
metadata_ch2 <- fread('metadata_ch2.csv')
metadata_ch_full <- rbind(metadata_ch, metadata_ch2)
write.csv(metadata_ch_full, 'metadata_ch_full.csv', row.names = FALSE)

metadata_connectivity <- fread('metadata_connectivity.csv')
metadata_connectivity2 <- fread('metadata_connectivity2.csv')
metadata_connectivity_full <- rbind(metadata_connectivity, metadata_connectivity2)
write.csv(metadata_connectivity_full, 'metadata_connectivity_full.csv', row.names = FALSE)

metadata_db <- fread('metadata_db.csv')
metadata_db2 <- fread('metadata_db2.csv')
metadata_db_full <- rbind(metadata_db, metadata_db2)
write.csv(metadata_db_full, 'metadata_db_full.csv', row.names = FALSE)

metadata_dunn <- fread('metadata_dunn.csv')
metadata_dunn2 <- fread('metadata_dunn2.csv')
metadata_dunn_full <- rbind(metadata_dunn, metadata_dunn2)
write.csv(metadata_dunn_full, 'metadata_dunn_full.csv', row.names = FALSE)

metadata_rl <- fread('metadata_rl.csv')
metadata_rl2 <- fread('metadata_rl2.csv')
metadata_rl_full <- rbind(metadata_rl, metadata_rl2)
write.csv(metadata_rl_full, 'metadata_rl_full.csv', row.names = FALSE)

metadata_sd <- fread('metadata_sd.csv')
metadata_sd2 <- fread('metadata_sd2.csv')
metadata_sd_full <- rbind(metadata_sd, metadata_sd2)
write.csv(metadata_sd_full, 'metadata_sd_full.csv', row.names = FALSE)

metadata_silh <- fread('metadata_silh.csv')
metadata_silh2 <- fread('metadata_silh2.csv')
metadata_silh_full <- rbind(metadata_silh, metadata_silh2)
write.csv(metadata_silh_full, 'metadata_silh_full.csv', row.names = FALSE)


## Generating performance measures - metadata - github datasets ---------------------------------------------------------


require(data.table)

min_max <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Importing other_datasets - github (101 datasets)
git_ds <- list()
names6 = list.files(pattern="*.txt")
for(i in 1:length(names6)){
  git_ds[[i]] <- fread(names6[i])
}


# CH ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_ch_ant <- fread('metadata_ch_full.csv')
alg_ch_ant <- metadata_ch_ant[, 17:26]

kmeans_git_ch <- as.matrix(readRDS('kmeans_git_ch.Rds'))
kmedians_git_ch <- as.matrix(readRDS('kmedians_git_ch.Rds'))
kmedoids_git_ch <- as.matrix(readRDS('kmedoids_git_ch.Rds'))
single_git_ch <- as.matrix(readRDS('single_git_ch.Rds'))
complete_git_ch <- as.matrix(readRDS('complete_git_ch.Rds'))
average_git_ch <- as.matrix(readRDS('average_git_ch.Rds'))
ward_git_ch <- as.matrix(readRDS('ward_git_ch.Rds'))
closest_git_ch <- as.matrix(readRDS('closest_git_ch.Rds'))
mbkmeans_git_ch <- as.matrix(readRDS('mbkmeans_git_ch.Rds'))
fcmeans_git_ch <- as.matrix(readRDS('fcmeans_git_ch.Rds'))
alg_ch <- cbind(kmeans_git_ch, kmedians_git_ch, kmedoids_git_ch, single_git_ch, complete_git_ch,
                average_git_ch, ward_git_ch, closest_git_ch, mbkmeans_git_ch, fcmeans_git_ch)
colnames(alg_ch) <- colnames(alg_ch_ant)
alg_ch_final <- rbind(alg_ch_ant, alg_ch)
metadata_ch_final <- cbind(metadata, alg_ch_final)
metadata_ch_final$feature_kurtosis <- min_max(metadata_ch_final$feature_kurtosis)

write.csv(metadata_ch_final, 'metadata_ch_final.csv', row.names = FALSE)

# BH ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_bh_ant <- fread('metadata_bh_full.csv')
alg_bh_ant <- metadata_bh_ant[, 17:26]

kmeans_git_bh <- as.matrix(readRDS('kmeans_git_bh.Rds'))
kmedians_git_bh <- as.matrix(readRDS('kmedians_git_bh.Rds'))
kmedoids_git_bh <- as.matrix(readRDS('kmedoids_git_bh.Rds'))
single_git_bh <- as.matrix(readRDS('single_git_bh.Rds'))
complete_git_bh <- as.matrix(readRDS('complete_git_bh.Rds'))
average_git_bh <- as.matrix(readRDS('average_git_bh.Rds'))
ward_git_bh <- as.matrix(readRDS('ward_git_bh.Rds'))
closest_git_bh <- as.matrix(readRDS('closest_git_bh.Rds'))
mbkmeans_git_bh <- as.matrix(readRDS('mbkmeans_git_bh.Rds'))
fcmeans_git_bh <- as.matrix(readRDS('fcmeans_git_bh.Rds'))
alg_bh <- cbind(kmeans_git_bh, kmedians_git_bh, kmedoids_git_bh, single_git_bh, complete_git_bh,
                average_git_bh, ward_git_bh, closest_git_bh, mbkmeans_git_bh, fcmeans_git_bh)
colnames(alg_bh) <- colnames(alg_bh_ant)
alg_bh_final <- rbind(alg_bh_ant, alg_bh)
metadata_bh_final <- cbind(metadata, alg_bh_final)
metadata_bh_final$feature_kurtosis <- min_max(metadata_bh_final$feature_kurtosis)

write.csv(metadata_bh_final, 'metadata_bh_final.csv', row.names = FALSE)

# Connectivity ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_connectivity_ant <- fread('metadata_connectivity_full.csv')
alg_connectivity_ant <- metadata_connectivity_ant[, 17:26]

kmeans_git_connectivity <- as.matrix(readRDS('kmeans_git_connectivity.Rds'))
kmedians_git_connectivity <- as.matrix(readRDS('kmedians_git_connectivity.Rds'))
kmedoids_git_connectivity <- as.matrix(readRDS('kmedoids_git_connectivity.Rds'))
single_git_connectivity <- as.matrix(readRDS('single_git_connectivity.Rds'))
complete_git_connectivity <- as.matrix(readRDS('complete_git_connectivity.Rds'))
average_git_connectivity <- as.matrix(readRDS('average_git_connectivity.Rds'))
ward_git_connectivity <- as.matrix(readRDS('ward_git_connectivity.Rds'))
closest_git_connectivity <- as.matrix(readRDS('closest_git_connectivity.Rds'))
mbkmeans_git_connectivity <- as.matrix(readRDS('mbkmeans_git_connectivity.Rds'))
fcmeans_git_connectivity <- as.matrix(readRDS('fcmeans_git_connectivity.Rds'))
alg_connectivity <- cbind(kmeans_git_connectivity, kmedians_git_connectivity, kmedoids_git_connectivity, single_git_connectivity, complete_git_connectivity,
                average_git_connectivity, ward_git_connectivity, closest_git_connectivity, mbkmeans_git_connectivity, fcmeans_git_connectivity)
colnames(alg_connectivity) <- colnames(alg_connectivity_ant)
alg_connectivity_final <- rbind(alg_connectivity_ant, alg_connectivity)
metadata_connectivity_final <- cbind(metadata, alg_connectivity_final)
metadata_connectivity_final$feature_kurtosis <- min_max(metadata_connectivity_final$feature_kurtosis)

write.csv(metadata_connectivity_final, 'metadata_connectivity_final.csv', row.names = FALSE)


# DB ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_db_ant <- fread('metadata_db_full.csv')
alg_db_ant <- metadata_db_ant[, 17:26]

kmeans_git_db <- as.matrix(readRDS('kmeans_git_db.Rds'))
kmedians_git_db <- as.matrix(readRDS('kmedians_git_db.Rds'))
kmedoids_git_db <- as.matrix(readRDS('kmedoids_git_db.Rds'))
single_git_db <- as.matrix(readRDS('single_git_db.Rds'))
complete_git_db <- as.matrix(readRDS('complete_git_db.Rds'))
average_git_db <- as.matrix(readRDS('average_git_db.Rds'))
ward_git_db <- as.matrix(readRDS('ward_git_db.Rds'))
closest_git_db <- as.matrix(readRDS('closest_git_db.Rds'))
mbkmeans_git_db <- as.matrix(readRDS('mbkmeans_git_db.Rds'))
fcmeans_git_db <- as.matrix(readRDS('fcmeans_git_db.Rds'))
alg_db <- cbind(kmeans_git_db, kmedians_git_db, kmedoids_git_db, single_git_db, complete_git_db,
                          average_git_db, ward_git_db, closest_git_db, mbkmeans_git_db, fcmeans_git_db)
colnames(alg_db) <- colnames(alg_db_ant)
alg_db_final <- rbind(alg_db_ant, alg_db)
metadata_db_final <- cbind(metadata, alg_db_final)
metadata_db_final$feature_kurtosis <- min_max(metadata_db_final$feature_kurtosis)

write.csv(metadata_db_final, 'metadata_db_final.csv', row.names = FALSE)


# Dunn ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_dunn_ant <- fread('metadata_dunn_full.csv')
alg_dunn_ant <- metadata_dunn_ant[, 17:26]

kmeans_git_dunn <- as.matrix(readRDS('kmeans_git_dunn.Rds'))
kmedians_git_dunn <- as.matrix(readRDS('kmedians_git_dunn.Rds'))
kmedoids_git_dunn <- as.matrix(readRDS('kmedoids_git_dunn.Rds'))
single_git_dunn <- as.matrix(readRDS('single_git_dunn.Rds'))
complete_git_dunn <- as.matrix(readRDS('complete_git_dunn.Rds'))
average_git_dunn <- as.matrix(readRDS('average_git_dunn.Rds'))
ward_git_dunn <- as.matrix(readRDS('ward_git_dunn.Rds'))
closest_git_dunn <- as.matrix(readRDS('closest_git_dunn.Rds'))
mbkmeans_git_dunn <- as.matrix(readRDS('mbkmeans_git_dunn.Rds'))
fcmeans_git_dunn <- as.matrix(readRDS('fcmeans_git_dunn.Rds'))
alg_dunn <- cbind(kmeans_git_dunn, kmedians_git_dunn, kmedoids_git_dunn, single_git_dunn, complete_git_dunn,
                          average_git_dunn, ward_git_dunn, closest_git_dunn, mbkmeans_git_dunn, fcmeans_git_dunn)
colnames(alg_dunn) <- colnames(alg_dunn_ant)
alg_dunn_final <- rbind(alg_dunn_ant, alg_dunn)
metadata_dunn_final <- cbind(metadata, alg_dunn_final)
metadata_dunn_final$feature_kurtosis <- min_max(metadata_dunn_final$feature_kurtosis)

write.csv(metadata_dunn_final, 'metadata_dunn_final.csv', row.names = FALSE)


# Hub ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_hub_ant <- fread('metadata_hub.csv')
alg_hub_ant <- metadata_hub_ant[, 22:31]

kmeans_git_hub <- as.matrix(readRDS('kmeans_git_hub.Rds'))
kmedians_git_hub <- as.matrix(readRDS('kmedians_git_hub.Rds'))
kmedoids_git_hub <- as.matrix(readRDS('kmedoids_git_hub.Rds'))
single_git_hub <- as.matrix(readRDS('single_git_hub.Rds'))
complete_git_hub <- as.matrix(readRDS('complete_git_hub.Rds'))
average_git_hub <- as.matrix(readRDS('average_git_hub.Rds'))
ward_git_hub <- as.matrix(readRDS('ward_git_hub.Rds'))
closest_git_hub <- as.matrix(readRDS('closest_git_hub.Rds'))
mbkmeans_git_hub <- as.matrix(readRDS('mbkmeans_git_hub.Rds'))
fcmeans_git_hub <- as.matrix(readRDS('fcmeans_git_hub.Rds'))
alg_hub <- cbind(kmeans_git_hub, kmedians_git_hub, kmedoids_git_hub, single_git_hub, complete_git_hub,
                          average_git_hub, ward_git_hub, closest_git_hub, mbkmeans_git_hub, fcmeans_git_hub)
colnames(alg_hub) <- colnames(alg_hub_ant)
alg_hub_final <- rbind(alg_hub_ant, alg_hub)
metadata_hub_final <- cbind(metadata, alg_hub_final)
metadata_hub_final$feature_kurtosis <- min_max(metadata_hub_final$feature_kurtosis)
metadata_hub_final$algo_single <- min_max(metadata_hub_final$algo_single)

write.csv(metadata_hub_final, 'metadata_hub_final.csv', row.names = FALSE)


# RL ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_rl_ant <- fread('metadata_rl_full.csv')
alg_rl_ant <- metadata_rl_ant[, 17:26]

kmeans_git_rl <- as.matrix(readRDS('kmeans_git_rl.Rds'))
kmedians_git_rl <- as.matrix(readRDS('kmedians_git_rl.Rds'))
kmedoids_git_rl <- as.matrix(readRDS('kmedoids_git_rl.Rds'))
single_git_rl <- as.matrix(readRDS('single_git_rl.Rds'))
complete_git_rl <- as.matrix(readRDS('complete_git_rl.Rds'))
average_git_rl <- as.matrix(readRDS('average_git_rl.Rds'))
ward_git_rl <- as.matrix(readRDS('ward_git_rl.Rds'))
closest_git_rl <- as.matrix(readRDS('closest_git_rl.Rds'))
mbkmeans_git_rl <- as.matrix(readRDS('mbkmeans_git_rl.Rds'))
fcmeans_git_rl <- as.matrix(readRDS('fcmeans_git_rl.Rds'))
alg_rl <- cbind(kmeans_git_rl, kmedians_git_rl, kmedoids_git_rl, single_git_rl, complete_git_rl,
                          average_git_rl, ward_git_rl, closest_git_rl, mbkmeans_git_rl, fcmeans_git_rl)
colnames(alg_rl) <- colnames(alg_rl_ant)
alg_rl_final <- rbind(alg_rl_ant, alg_rl)
metadata_rl_final <- cbind(metadata, alg_rl_final)
metadata_rl_final$feature_kurtosis <- min_max(metadata_rl_final$feature_kurtosis)

write.csv(metadata_rl_final, 'metadata_rl_final.csv', row.names = FALSE)


# SD ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_sd_ant <- fread('metadata_sd_full.csv')
alg_sd_ant <- metadata_sd_ant[, 17:26]

kmeans_git_sd <- as.matrix(readRDS('kmeans_git_sd.Rds'))
kmedians_git_sd <- as.matrix(readRDS('kmedians_git_sd.Rds'))
kmedoids_git_sd <- as.matrix(readRDS('kmedoids_git_sd.Rds'))
single_git_sd <- as.matrix(readRDS('single_git_sd.Rds'))
complete_git_sd <- as.matrix(readRDS('complete_git_sd.Rds'))
average_git_sd <- as.matrix(readRDS('average_git_sd.Rds'))
ward_git_sd <- as.matrix(readRDS('ward_git_sd.Rds'))
closest_git_sd <- as.matrix(readRDS('closest_git_sd.Rds'))
mbkmeans_git_sd <- as.matrix(readRDS('mbkmeans_git_sd.Rds'))
fcmeans_git_sd <- as.matrix(readRDS('fcmeans_git_sd.Rds'))
alg_sd <- cbind(kmeans_git_sd, kmedians_git_sd, kmedoids_git_sd, single_git_sd, complete_git_sd,
                          average_git_sd, ward_git_sd, closest_git_sd, mbkmeans_git_sd, fcmeans_git_sd)
colnames(alg_sd) <- colnames(alg_sd_ant)
alg_sd_final <- rbind(alg_sd_ant, alg_sd)
metadata_sd_final <- cbind(metadata, alg_sd_final)
metadata_sd_final$feature_kurtosis <- min_max(metadata_sd_final$feature_kurtosis)

write.csv(metadata_sd_final, 'metadata_sd_final.csv', row.names = FALSE)


# Silh ----------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_silh_ant <- fread('metadata_silh_full.csv')
alg_silh_ant <- metadata_silh_ant[, 17:26]

kmeans_git_silh <- as.matrix(readRDS('kmeans_git_silh.Rds'))
kmedians_git_silh <- as.matrix(readRDS('kmedians_git_silh.Rds'))
kmedoids_git_silh <- as.matrix(readRDS('kmedoids_git_silh.Rds'))
single_git_silh <- as.matrix(readRDS('single_git_silh.Rds'))
complete_git_silh <- as.matrix(readRDS('complete_git_silh.Rds'))
average_git_silh <- as.matrix(readRDS('average_git_silh.Rds'))
ward_git_silh <- as.matrix(readRDS('ward_git_silh.Rds'))
closest_git_silh <- as.matrix(readRDS('closest_git_silh.Rds'))
mbkmeans_git_silh <- as.matrix(readRDS('mbkmeans_git_silh.Rds'))
fcmeans_git_silh <- as.matrix(readRDS('fcmeans_git_silh.Rds'))
alg_silh <- cbind(kmeans_git_silh, kmedians_git_silh, kmedoids_git_silh, single_git_silh, complete_git_silh,
                          average_git_silh, ward_git_silh, closest_git_silh, mbkmeans_git_silh, fcmeans_git_silh)
colnames(alg_silh) <- colnames(alg_silh_ant)
alg_silh_final <- rbind(alg_silh_ant, alg_silh)
metadata_silh_final <- cbind(metadata, alg_silh_final)
metadata_silh_final$feature_kurtosis <- min_max(metadata_silh_final$feature_kurtosis)

write.csv(metadata_silh_final, 'metadata_silh_final.csv', row.names = FALSE)


# Stability  ------------------------------------------------------------------

metadata <- fread('metafeatures_all.csv')

metadata_stability_apn_ant <- fread('metadata_stability_apn.csv')
alg_stability_apn_ant <- metadata_stability_apn_ant[, 22:31]

metadata_stability_ad_ant <- fread('metadata_stability_ad.csv')
alg_stability_ad_ant <- metadata_stability_ad_ant[, 22:31]

metadata_stability_adm_ant <- fread('metadata_stability_adm.csv')
alg_stability_adm_ant <- metadata_stability_adm_ant[, 22:31]

metadata_stability_fom_ant <- fread('metadata_stability_fom.csv')
alg_stability_fom_ant <- metadata_stability_fom_ant[, 22:31]

kmeans_git_stability <- readRDS('kmeans_git_stability.Rds')
kmeans_git_stability <- as.data.frame(do.call(rbind, kmeans_git_stability))

kmedians_git_stability <- readRDS('kmedians_git_stability.Rds')
kmedians_git_stability <- as.data.frame(do.call(rbind, kmedians_git_stability))

kmedoids_git_stability <- readRDS('kmedoids_git_stability.Rds')
kmedoids_git_stability <- as.data.frame(do.call(rbind, kmedoids_git_stability))

single_git_stability <- readRDS('single_git_stability.Rds')
single_git_stability <- as.data.frame(do.call(rbind, single_git_stability))

complete_git_stability <- readRDS('complete_git_stability.Rds')
complete_git_stability <- as.data.frame(do.call(rbind, complete_git_stability))

average_git_stability <- readRDS('average_git_stability.Rds')
average_git_stability <- as.data.frame(do.call(rbind, average_git_stability))

ward_git_stability <- readRDS('ward_git_stability.Rds')
ward_git_stability <- as.data.frame(do.call(rbind, ward_git_stability))

centroid_git_stability <- readRDS('centroid_git_stability.Rds')
centroid_git_stability <- as.data.frame(do.call(rbind, centroid_git_stability))

mbkmeans_git_stability <- readRDS('mbkmeans_git_stability.Rds')
mbkmeans_git_stability <- as.data.frame(do.call(rbind, mbkmeans_git_stability))

fcmeans_git_stability <- readRDS('fcmeans_git_stability.Rds')
fcmeans_git_stability <- as.data.frame(do.call(rbind, fcmeans_git_stability))


# APN
alg_stability_apn <- cbind(kmeans_git_stability[, 1], kmedians_git_stability[, 1], kmedoids_git_stability[, 1], single_git_stability[, 1], complete_git_stability[, 1],
                          average_git_stability[, 1], ward_git_stability[, 1], centroid_git_stability[, 1], mbkmeans_git_stability[, 1], fcmeans_git_stability[, 1])
colnames(alg_stability_apn) <- colnames(alg_stability_apn_ant)
alg_stability_apn_final <- rbind(alg_stability_apn_ant, alg_stability_apn)
metadata_stability_apn_final <- cbind(metadata, alg_stability_apn_final)
metadata_stability_apn_final$feature_kurtosis <- min_max(metadata_stability_apn_final$feature_kurtosis)

write.csv(metadata_stability_apn_final, 'metadata_stability_apn_final.csv', row.names = FALSE)

# AD
alg_stability_ad <- cbind(kmeans_git_stability[, 2], kmedians_git_stability[, 2], kmedoids_git_stability[, 2], single_git_stability[, 2], complete_git_stability[, 2],
                           average_git_stability[, 2], ward_git_stability[, 2], centroid_git_stability[, 2], mbkmeans_git_stability[, 2], fcmeans_git_stability[, 2])
colnames(alg_stability_ad) <- colnames(alg_stability_ad_ant)
alg_stability_ad_final <- rbind(alg_stability_ad_ant, alg_stability_ad)
metadata_stability_ad_final <- cbind(metadata, alg_stability_ad_final)
metadata_stability_ad_final$feature_kurtosis <- min_max(metadata_stability_ad_final$feature_kurtosis)

write.csv(metadata_stability_ad_final, 'metadata_stability_ad_final.csv', row.names = FALSE)

# ADM
alg_stability_adm <- cbind(kmeans_git_stability[, 3], kmedians_git_stability[, 3], kmedoids_git_stability[, 3], single_git_stability[, 3], complete_git_stability[, 3],
                          average_git_stability[, 3], ward_git_stability[, 3], centroid_git_stability[, 3], mbkmeans_git_stability[, 3], fcmeans_git_stability[, 3])
colnames(alg_stability_adm) <- colnames(alg_stability_adm_ant)
alg_stability_adm_final <- rbind(alg_stability_adm_ant, alg_stability_adm)
metadata_stability_adm_final <- cbind(metadata, alg_stability_adm_final)
metadata_stability_adm_final$feature_kurtosis <- min_max(metadata_stability_adm_final$feature_kurtosis)

write.csv(metadata_stability_adm_final, 'metadata_stability_adm_final.csv', row.names = FALSE)

# FOM
alg_stability_fom <- cbind(kmeans_git_stability[, 4], kmedians_git_stability[, 4], kmedoids_git_stability[, 4], single_git_stability[, 4], complete_git_stability[, 4],
                           average_git_stability[, 4], ward_git_stability[, 4], centroid_git_stability[, 4], mbkmeans_git_stability[, 4], fcmeans_git_stability[, 4])
colnames(alg_stability_fom) <- colnames(alg_stability_fom_ant)
alg_stability_fom_final <- rbind(alg_stability_fom_ant, alg_stability_fom)
metadata_stability_fom_final <- cbind(metadata, alg_stability_fom_final)
metadata_stability_fom_final$feature_kurtosis <- min_max(metadata_stability_fom_final$feature_kurtosis)

write.csv(metadata_stability_fom_final, 'metadata_stability_fom_final.csv', row.names = FALSE)

