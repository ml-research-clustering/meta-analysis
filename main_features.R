### MEASURING COMPLEXITY OF CLUSTERING PROBLEMS ###

library(data.table)
source('meta_features_functions.R')

## Importing datasets

# Importing gaussian datasets (80 datasets)
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
  small_ds[[i]] <- fread(names3[i])
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

# Importing other_datasets - github (107 datasets)
git_ds <- list()
names6 = list.files(pattern="*.txt")
for(i in 1:length(names6)){
  git_ds[[i]] <- fread(names6[i])
}


## Calculating meta-features

# gaussian 
mds1 <- data.frame()
for(i in 1:length(gauss_ds)){ 
  mds1[i, 1] <- log_number_ex(gauss_ds[[i]])
  mds1[i, 2] <- log_number_ftr(gauss_ds[[i]])
  mds1[i, 3] <- ratio_ex_ftr(gauss_ds[[i]])
  mds1[i, 4] <- mvn_skewness(gauss_ds[[i]])
  mds1[i, 5] <- mvn_kurtosis(gauss_ds[[i]])
  mds1[i, 6] <- multi_norm(gauss_ds[[i]])
  mds1[i, 7] <- perc_out(gauss_ds[[i]])
  mds1[i, 8] <- avg_pca(gauss_ds[[i]])
  mds1[i, 9] <- ratio_pca(gauss_ds[[i]])
  mds1[i, 10] <- avg_abs_cor(gauss_ds[[i]])
  mds1[i, 11] <- per_low_dist(gauss_ds[[i]])
  mds1[i, 12] <- per_high_dist(gauss_ds[[i]])
  mds1[i, 13] <- eigen_cent_mst(gauss_ds[[i]])
  mds1[i, 14] <- net_dens(gauss_ds[[i]])
  mds1[i, 15] <- clust_coef(gauss_ds[[i]])
  mds1[i, 16] <- intr_dim(gauss_ds[[i]])
  mds1[i, 17] <- contrast(gauss_ds[[i]])
  mds1[i, 18] <- hub_score(gauss_ds[[i]])
  mds1[i, 19] <- avg_nnd(gauss_ds[[i]])
  mds1[i, 20] <- power_cent(gauss_ds[[i]])
}

# ellipsoidal 
mds2 <- data.frame()
for(i in 1:length(ellips_ds)){
  mds2[i, 1] <- log_number_ex(ellips_ds[[i]])
  mds2[i, 2] <- log_number_ftr(ellips_ds[[i]])
  mds2[i, 3] <- ratio_ex_ftr(ellips_ds[[i]])
  mds2[i, 4] <- mvn_skewness(ellips_ds[[i]])
  mds2[i, 5] <- mvn_kurtosis(ellips_ds[[i]])
  mds2[i, 6] <- multi_norm(ellips_ds[[i]])
  mds2[i, 7] <- perc_out(ellips_ds[[i]])
  mds2[i, 8] <- avg_pca(ellips_ds[[i]])
  mds2[i, 9] <- ratio_pca(ellips_ds[[i]])
  mds2[i, 10] <- avg_abs_cor(ellips_ds[[i]])
  mds2[i, 11] <- per_low_dist(ellips_ds[[i]])
  mds2[i, 12] <- per_high_dist(ellips_ds[[i]])
  mds2[i, 13] <- eigen_cent_mst(ellips_ds[[i]])
  mds2[i, 14] <- net_dens(ellips_ds[[i]])
  mds2[i, 15] <- clust_coef(ellips_ds[[i]])
  mds2[i, 16] <- intr_dim(ellips_ds[[i]])
  mds2[i, 17] <- contrast(ellips_ds[[i]])
  mds2[i, 18] <- hub_score(ellips_ds[[i]])
  mds2[i, 19] <- avg_nnd(ellips_ds[[i]])
  mds2[i, 20] <- power_cent(ellips_ds[[i]])
}

# CSV small 
mds3 <- data.frame()
for(i in 1:length(small_ds)){
  mds3[i, 1] <- log_number_ex(small_ds[[i]])
  mds3[i, 2] <- log_number_ftr(small_ds[[i]])
  mds3[i, 3] <- ratio_ex_ftr(small_ds[[i]])
  mds3[i, 4] <- mvn_skewness(small_ds[[i]])
  mds3[i, 5] <- mvn_kurtosis(small_ds[[i]])
  mds3[i, 6] <- multi_norm(small_ds[[i]])
  mds3[i, 7] <- perc_out(small_ds[[i]])
  mds3[i, 8] <- avg_pca(small_ds[[i]])
  mds3[i, 9] <- ratio_pca(small_ds[[i]])
  mds3[i, 10] <- avg_abs_cor(small_ds[[i]])
  mds3[i, 11] <- per_low_dist(small_ds[[i]])
  mds3[i, 12] <- per_high_dist(small_ds[[i]])
  mds3[i, 13] <- eigen_cent_mst(small_ds[[i]])
  mds3[i, 14] <- net_dens(small_ds[[i]])
  mds3[i, 15] <- clust_coef(small_ds[[i]])
  mds3[i, 16] <- intr_dim(ellips_ds[[i]])
  mds3[i, 17] <- contrast(ellips_ds[[i]])
  mds3[i, 18] <- hub_score(ellips_ds[[i]])
  mds3[i, 19] <- avg_nnd(ellips_ds[[i]])
  mds3[i, 20] <- power_cent(ellips_ds[[i]])
}

# CSV medium 
mds4 <- data.frame()
for(i in 1:length(medium_ds)){
  mds4[i, 1] <- log_number_ex(medium_ds[[i]])
  mds4[i, 2] <- log_number_ftr(medium_ds[[i]])
  mds4[i, 3] <- ratio_ex_ftr(medium_ds[[i]])
  mds4[i, 4] <- mvn_skewness(medium_ds[[i]])
  mds4[i, 5] <- mvn_kurtosis(medium_ds[[i]])
  mds4[i, 6] <- multi_norm(medium_ds[[i]])
  mds4[i, 7] <- perc_out(medium_ds[[i]])
  mds4[i, 8] <- avg_pca(medium_ds[[i]])
  mds4[i, 9] <- ratio_pca(medium_ds[[i]])
  mds4[i, 10] <- avg_abs_cor(medium_ds[[i]])
  mds4[i, 11] <- per_low_dist(medium_ds[[i]])
  mds4[i, 12] <- per_high_dist(medium_ds[[i]])
  mds4[i, 13] <- eigen_cent_mst(medium_ds[[i]])
  mds4[i, 14] <- net_dens(medium_ds[[i]])
  mds4[i, 15] <- clust_coef(medium_ds[[i]])
  mds4[i, 16] <- intr_dim(ellips_ds[[i]])
  mds4[i, 17] <- contrast(ellips_ds[[i]])
  mds4[i, 18] <- hub_score(ellips_ds[[i]])
  mds4[i, 19] <- avg_nnd(ellips_ds[[i]])
  mds4[i, 20] <- power_cent(ellips_ds[[i]])
}

# CSV large 
mds5 <- data.frame()
for(i in 1:length(large_ds)){
  mds5[i, 1] <- log_number_ex(large_ds[[i]])
  mds5[i, 2] <- log_number_ftr(large_ds[[i]])
  mds5[i, 3] <- ratio_ex_ftr(large_ds[[i]])
  mds5[i, 4] <- mvn_skewness(large_ds[[i]])
  mds5[i, 5] <- mvn_kurtosis(large_ds[[i]])
  mds5[i, 6] <- multi_norm(large_ds[[i]])
  mds5[i, 7] <- perc_out(large_ds[[i]])
  mds5[i, 8] <- avg_pca(large_ds[[i]])
  mds5[i, 9] <- ratio_pca(large_ds[[i]])
  mds5[i, 10] <- avg_abs_cor(large_ds[[i]])
  mds5[i, 11] <- per_low_dist(large_ds[[i]])
  mds5[i, 12] <- per_high_dist(large_ds[[i]])
  mds5[i, 13] <- eigen_cent_mst(large_ds[[i]])
  mds5[i, 14] <- net_dens(large_ds[[i]])
  mds5[i, 15] <- clust_coef(large_ds[[i]])
  mds5[i, 16] <- intr_dim(ellips_ds[[i]])
  mds5[i, 17] <- contrast(ellips_ds[[i]])
  mds5[i, 18] <- hub_score(ellips_ds[[i]])
  mds5[i, 19] <- avg_nnd(ellips_ds[[i]])
  mds5[i, 20] <- power_cent(ellips_ds[[i]])
}

# Github 
mds6 <- data.frame()
for(i in 1:length(git_ds)){
  print(i)
  mds6[i, 1] <- log_number_ex(git_ds[[i]])
  mds6[i, 2] <- log_number_ftr(git_ds[[i]])
  mds6[i, 3] <- ratio_ex_ftr(git_ds[[i]])
  mds6[i, 4] <- mvn_skewness(git_ds[[i]])
  mds6[i, 5] <- mvn_kurtosis(git_ds[[i]])
  mds6[i, 6] <- multi_norm(git_ds[[i]])
  mds6[i, 7] <- perc_out(git_ds[[i]])
  mds6[i, 8] <- avg_pca(git_ds[[i]])
  mds6[i, 9] <- ratio_pca(git_ds[[i]])
  mds6[i, 10] <- avg_abs_cor(git_ds[[i]])
  mds6[i, 11] <- per_low_dist(git_ds[[i]])
  mds6[i, 12] <- per_high_dist(git_ds[[i]])
  mds6[i, 13] <- eigen_cent_mst(git_ds[[i]])
  mds6[i, 14] <- net_dens(git_ds[[i]])
  mds6[i, 15] <- clust_coef(git_ds[[i]])
  mds6[i, 16] <- intr_dim(git_ds[[i]])
  mds6[i, 17] <- contrast(git_ds[[i]])
  mds6[i, 18] <- hub_score(git_ds[[i]])
  mds6[i, 19] <- avg_nnd(git_ds[[i]])
  mds6[i, 20] <- power_cent(git_ds[[i]])
}

# generating metadata

mds1 <- fread("mds1.csv")
mds2 <- fread("mds2.csv")
mds3 <- fread("mds3.csv")
mds4 <- fread("mds4.csv")
mds5 <- fread("mds5.csv")
mds6 <- fread("mds6.csv")

mds1[, 1] <- names1
mds2[, 1] <- names2
mds3[, 1] <- names3
mds4[, 1] <- names4
mds5[, 1] <- names5
mds6[, 1] <- names6

mds <- rbind(mds1, mds2, mds3, mds4, mds5, mds6)
write.csv(mds, "mds.csv", row.names = FALSE)







