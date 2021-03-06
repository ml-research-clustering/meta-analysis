## INSTANCE SPACE BY DATASETS CHARACTERISTICS ##

require(ggplot2)
require(data.table)
require(hrbrthemes)
require(pals)
require(viridis)
require(dplyr)

# importa Gaussian datasets (80 datasets)
gauss_ds <- list()
names1 = list.files(pattern="*.dat")
for(i in 1:length(names1)){
  gauss_ds[[i]] <- fread(names1[i])
}

# importing ellipsoidal datasets (80 datasets)
ellips_ds <- list()
names2 = list.files(pattern="*.dat")
for(i in 1:length(names2)){
  ellips_ds[[i]] <- fread(names2[i])
}

# importing small datasets (77 datasets)
small_ds <- list()
names3 = list.files(pattern="*.csv")
for(i in 1:length(names3)){
  small_ds[[i]] <- fread(names3[i])
}

# importing medium datasets (32 datasets)
medium_ds <- list()
names4 = list.files(pattern="*.csv")
for(i in 1:length(names4)){
  medium_ds[[i]] <- fread(names4[i])
}

# importing large datasets (10 datasets)
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

# importing coordinates from instance space # file generated by MATILDA
instance_space <- fread('coordinates.csv')
instance_space <- cbind(instance_space, c(rep('Gaussian', times = 80), rep('Ellipsoidal', times = 80), rep('Gaussian', times = 77), rep('Gaussian', times = 32), rep('Gaussian', times = 10), rep('Multiple shapes', times = 101)))
colnames(instance_space)[colnames(instance_space) == 'V2'] <- 'dataset'
instance_space[226:233, 4] <- 'Multiple shapes'

# generating information about datasets
k_gauss <- vector()
n <- vector()
att_gauss <- vector()
obs_gauss <- vector()
i <- 1
for(i in 1:length(gauss_ds)){
  n[i] <- dim(gauss_ds[[i]])[2]
  obs_gauss[i] <- dim(gauss_ds[[i]])[1]
  att_gauss[i] <- (dim(gauss_ds[[i]])[2])-1
  k_gauss[i] <- max(gauss_ds[[i]][, n[i]:n[i]])
}

k_ellips <- vector()
n <- vector()
att_ellips <- vector()
obs_ellips <- vector()
i <- 1
for(i in 1:length(ellips_ds)){
  n[i] <- dim(ellips_ds[[i]])[2]
  obs_ellips[i] <- dim(ellips_ds[[i]])[1]
  att_ellips[i] <- dim(ellips_ds[[i]])[2]-1
  k_ellips[i] <- max(ellips_ds[[i]][, n[i]:n[i]])
}

k_small <- vector()
n <- vector()
att_small <- vector()
obs_small <- vector()
i <- 1
for(i in 1:length(small_ds)){
  n[i] <- dim(small_ds[[i]])[2]
  obs_small[i] <- dim(small_ds[[i]])[1]
  att_small[i] <- dim(small_ds[[i]])[2]-1
  k_small[i] <- max(small_ds[[i]][, n[i]:n[i]])
}

k_medium <- vector()
n <- vector()
att_medium <- vector()
obs_medium <- vector()
i <- 1
for(i in 1:length(medium_ds)){
  n[i] <- dim(medium_ds[[i]])[2]
  obs_medium[i] <- dim(medium_ds[[i]])[1]
  att_medium[i] <- dim(medium_ds[[i]])[2]-1
  k_medium[i] <- max(medium_ds[[i]][, n[i]:n[i]])
}

k_large <- vector()
n <- vector()
att_large <- vector()
obs_large <- vector()
i <- 1
for(i in 1:length(large_ds)){
  n[i] <- dim(large_ds[[i]])[2]
  obs_large[i] <- dim(large_ds[[i]])[1]
  att_large[i] <- dim(large_ds[[i]])[2]-1
  k_large[i] <- max(large_ds[[i]][, n[i]:n[i]])
}

k_git <- vector()
n <- vector()
att_git <- vector()
obs_git <- vector()
i <- 1
for(i in 1:length(git_ds)){
  n[i] <- dim(git_ds[[i]])[2]
  obs_git[i] <- dim(git_ds[[i]])[1]
  att_git[i] <- dim(git_ds[[i]])[2]-1
  k_git[i] <- max(git_ds[[i]][, n[i]:n[i]])
}


instance_space <- cbind(instance_space, c(obs_gauss, obs_ellips, obs_small, obs_medium, obs_large, obs_git))
colnames(instance_space)[colnames(instance_space) == 'V2'] <- 'points'

instance_space <- cbind(instance_space, c(att_gauss, att_ellips, att_small, att_medium, att_large, att_git))
colnames(instance_space)[colnames(instance_space) == 'V2'] <- 'attributes'

instance_space <- cbind(instance_space, c(k_gauss, k_ellips, k_small, k_medium, k_large, k_git))
colnames(instance_space)[colnames(instance_space) == 'V2'] <- 'clusters'

# plotting the graphics

# by groups
ggplot(instance_space, aes(x = z_1, y = z_2, color = dataset)) + 
  geom_point(alpha = 1) + 
  labs(title = "datasets by groups") +
  scale_size(range = c(1, 10)) + 
  xlab("Z1") +
  ylab("Z2") +
  scale_color_manual(values = as.vector(parula(3))) +
  theme(
    axis.text.x  = element_text(hjust = 0, size = 14),
    axis.text.y  = element_text(hjust = 0, size = 14),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.background = element_rect(colour = "black"),
    legend.box.background = element_rect(colour = "black"),
    legend.key = element_rect(colour = "white", fill = NA),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    title = element_text(size = 14)
  )

# by number of examples
ggplot(instance_space, aes(x = z_1, y = z_2, color = points)) + 
  geom_point(alpha = 1) + 
  labs(title = '# examples') +
  scale_size(range = c(1, 10)) +
  xlab("Z1") +
  ylab("Z2") +
  scale_color_gradientn(trans = "log10", colors = as.vector(parula(20))) +
  theme(
    axis.text.x  = element_text(hjust = 0, size = 14),
    axis.text.y  = element_text(hjust = 0, size = 14),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(colour = "white", fill = NA),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    title = element_text(size = 14)
  )

# by number of attributes
ggplot(instance_space, aes(x = z_1, y = z_2, color = attributes)) + 
  geom_point(alpha = 1) + 
  labs(title = "# attributes") +
  scale_size(range = c(1, 10)) + 
  xlab("Z1") +
  ylab("Z2") +
  scale_color_gradientn(trans = "log10", colors = as.vector(parula(20))) +
  theme(
    axis.text.x  = element_text(hjust = 0, size = 14),
    axis.text.y  = element_text(hjust = 0, size = 14),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(colour = "white", fill = NA),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    title = element_text(size = 14)
  )

# by number of clusters
ggplot(instance_space, aes(x = z_1, y = z_2, color = clusters)) + 
  geom_point(alpha = 1) + 
  labs(title = "# clusters") +
  scale_size(range = c(1, 10)) + 
  xlab("Z1") +
  ylab("Z2") +
  scale_color_gradientn(trans = "log10", colors = as.vector(parula(24))) +
  theme(
    axis.text.x  = element_text(hjust = 0, size = 14),
    axis.text.y  = element_text(hjust = 0, size = 14),
    plot.title = element_text(hjust = 0.5),
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(colour = "white", fill = NA),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    title = element_text(size = 14)
  )








