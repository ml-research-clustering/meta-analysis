## GENERATING AVERAGE RANKING PERFORMANCE MEASURE ##

require(data.table)
require(ggplot2)
require(pals)
require(robustHD)

## importing data
metadata_bh <- fread('metadata_bh.csv')
bh <- metadata_bh[, 22:31]

metadata_ch <- fread('metadata_ch.csv')
ch <- metadata_ch[, 22:31]

metadata_connectivity <- fread('metadata_connectivity.csv')
connectivity <- metadata_connectivity[, 22:31]

metadata_db <- fread('metadata_db.csv')
db <- metadata_db[, 22:31]

metadata_dunn <- fread('metadata_dunn.csv')
dunn <- metadata_dunn[, 22:31]

metadata_hub <- fread('metadata_hub.csv')
hub <- metadata_hub[, 22:31]

metadata_rl <- fread('metadata_rl.csv')
rl <- metadata_rl[, 22:31]

metadata_sd <- fread('metadata_sd.csv')
sd <- metadata_sd[, 22:31]

metadata_stability_ad <- fread('metadata_stability_ad.csv')
stability_ad <- metadata_stability_ad[, 22:31]

metadata_stability_adm <- fread('metadata_stability_adm.csv')
stability_adm <- metadata_stability_adm[, 22:31]

metadata_stability_apn <- fread('metadata_stability_apn.csv')
stability_apn <- metadata_stability_apn[, 22:31]

metadata_stability_fom <- fread('metadata_stability_fom.csv')
stability_fom <- metadata_stability_fom[, 22:31]

## ranking
rank_max <- function(x){ # maximizar
  return(rank(x))
}

rank_min <- function(x){ # minimizar
  return(rank(-x))
}

bh_ranking <- as.data.frame((t(apply(bh, 1, rank_max))))
ch_ranking <- as.data.frame((t(apply(ch, 1, rank_max))))
connectivity_ranking <- as.data.frame((t(apply(connectivity, 1, rank_min))))
db_ranking <- as.data.frame((t(apply(db, 1, rank_min))))
dunn_ranking <- as.data.frame((t(apply(dunn, 1, rank_max))))
hub_ranking <- as.data.frame((t(apply(hub, 1, rank_max))))
rl_ranking <- as.data.frame((t(apply(rl, 1, rank_max))))
sd_ranking <- as.data.frame((t(apply(sd, 1, rank_min))))
stability_ad_ranking <- as.data.frame((t(apply(stability_ad, 1, rank_min))))
stability_adm_ranking <- as.data.frame((t(apply(stability_adm, 1, rank_min))))
stability_apn_ranking <- as.data.frame((t(apply(stability_apn, 1, rank_min))))
stability_fom_ranking <- as.data.frame((t(apply(stability_fom, 1, rank_min))))

bh <- apply(bh_ranking, 2, mean)
conn <- apply(connectivity_ranking, 2, mean)
rl <- apply(rl_ranking, 2, mean)
ch <- apply(ch_ranking, 2, mean)
db <- apply(db_ranking, 2, mean)
dunn <- apply(dunn_ranking, 2, mean)
sd <- apply(sd_ranking, 2, mean)
apn <- apply(stability_apn_ranking, 2, mean)
ad <- apply(stability_ad_ranking, 2, mean)
adm <- apply(stability_adm_ranking, 2, mean)
fom <- apply(stability_fom_ranking, 2, mean)
hub <- apply(hub_ranking, 2, mean)

indexes <- data.frame(bh, conn, rl, ch, db, dunn, sd, apn, ad, adm, fom, hub)
colnames(indexes) <- c('BH', 'C', 'RL', 'CH', 'DB', 'D', 'SD', 'APN', 'AD', 'ADM', 'FOM', 'HUB')

# Heatmap 
algo <- rep( c("KME", "KMD", "KMO", "SL", "CL", "AL", "WM", "CC", "MBK", "FCM"), each = 12)
rank <- c(indexes$BH, indexes$C, indexes$RL, indexes$CH, indexes$DB, indexes$D,
          indexes$SD, indexes$APN, indexes$AD, indexes$ADM, indexes$FOM, indexes$HUB)
data <- data.frame(algo, rank)

ggplot(data, aes(algo, rep(names(indexes), 10), fill = rank)) + 
  geom_tile() +  labs(x = "") +
  scale_fill_gradientn(colours = as.vector(parula(24))) +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank())

# kmeans
kmeans_mean <- (bh_ranking[, 1] + ch_ranking[, 1] + connectivity_ranking[, 1] + 
                 db_ranking[, 1] + dunn_ranking[, 1] + hub_ranking[, 1] +
                 rl_ranking[, 1] + sd_ranking[, 1] + stability_ad_ranking[, 1] +
                 stability_adm_ranking[, 1]+ stability_apn_ranking[, 1] + 
                 stability_fom_ranking[, 1])/12

kmeans_std <- sqrt(((bh_ranking[, 1] - kmeans_mean)^2 + (ch_ranking[, 1] - kmeans_mean)^2 + (connectivity_ranking[, 1] - kmeans_mean)^2 + 
                 (db_ranking[, 1] - kmeans_mean)^2 + (dunn_ranking[, 1] - kmeans_mean)^2 + (hub_ranking[, 1] - kmeans_mean)^2 +
                 (rl_ranking[, 1] - kmeans_mean)^2 + (sd_ranking[, 1] - kmeans_mean)^2 + (stability_ad_ranking[, 1] - kmeans_mean)^2 +
                 (stability_adm_ranking[, 1] - kmeans_mean)^2 + (stability_apn_ranking[, 1] - kmeans_mean)^2 + 
                 (stability_fom_ranking[, 1] - kmeans_mean)^2)/12)

# kmedians
kmedians_mean <- (bh_ranking[, 2] + ch_ranking[, 2] + connectivity_ranking[, 2] + 
                  db_ranking[, 2] + dunn_ranking[, 2] + hub_ranking[, 2] +
                  rl_ranking[, 2] + sd_ranking[, 2] + stability_ad_ranking[, 2] +
                  stability_adm_ranking[, 2]+ stability_apn_ranking[, 2] + 
                  stability_fom_ranking[, 2])/12

kmedians_std <- sqrt(((bh_ranking[, 2] - kmeans_mean)^2 + (ch_ranking[, 2] - kmeans_mean)^2 + (connectivity_ranking[, 2] - kmeans_mean)^2 + 
                      (db_ranking[, 2] - kmeans_mean)^2 + (dunn_ranking[, 2] - kmeans_mean)^2 + (hub_ranking[, 2] - kmeans_mean)^2 +
                      (rl_ranking[, 2] - kmeans_mean)^2 + (sd_ranking[, 2] - kmeans_mean)^2 + (stability_ad_ranking[, 2] - kmeans_mean)^2 +
                      (stability_adm_ranking[, 2] - kmeans_mean)^2 + (stability_apn_ranking[, 2] - kmeans_mean)^2 + 
                      (stability_fom_ranking[, 2] - kmeans_mean)^2)/12)

# kmedoids
kmedoids_mean <- (bh_ranking[, 3] + ch_ranking[, 3] + connectivity_ranking[, 3] + 
                    db_ranking[, 3] + dunn_ranking[, 3] + hub_ranking[, 3] +
                    rl_ranking[, 3] + sd_ranking[, 3] + stability_ad_ranking[, 3] +
                    stability_adm_ranking[, 3]+ stability_apn_ranking[, 3] + 
                    stability_fom_ranking[, 3])/12

kmedoids_std <- sqrt(((bh_ranking[, 3] - kmeans_mean)^2 + (ch_ranking[, 3] - kmeans_mean)^2 + (connectivity_ranking[, 3] - kmeans_mean)^2 + 
                        (db_ranking[, 3] - kmeans_mean)^2 + (dunn_ranking[, 3] - kmeans_mean)^2 + (hub_ranking[, 3] - kmeans_mean)^2 +
                        (rl_ranking[, 3] - kmeans_mean)^2 + (sd_ranking[, 3] - kmeans_mean)^2 + (stability_ad_ranking[, 3] - kmeans_mean)^2 +
                        (stability_adm_ranking[, 3] - kmeans_mean)^2 + (stability_apn_ranking[, 3] - kmeans_mean)^2 + 
                        (stability_fom_ranking[, 3] - kmeans_mean)^2)/12)


# single
single_mean <- (bh_ranking[, 4] + ch_ranking[, 4] + connectivity_ranking[, 4] + 
                    db_ranking[, 4] + dunn_ranking[, 4] + hub_ranking[, 4] +
                    rl_ranking[, 4] + sd_ranking[, 4] + stability_ad_ranking[, 4] +
                    stability_adm_ranking[, 4]+ stability_apn_ranking[, 4] + 
                    stability_fom_ranking[, 4])/12

single_std <- sqrt(((bh_ranking[, 4] - kmeans_mean)^2 + (ch_ranking[, 4] - kmeans_mean)^2 + (connectivity_ranking[, 4] - kmeans_mean)^2 + 
                        (db_ranking[, 4] - kmeans_mean)^2 + (dunn_ranking[, 4] - kmeans_mean)^2 + (hub_ranking[, 4] - kmeans_mean)^2 +
                        (rl_ranking[, 4] - kmeans_mean)^2 + (sd_ranking[, 4] - kmeans_mean)^2 + (stability_ad_ranking[, 4] - kmeans_mean)^2 +
                        (stability_adm_ranking[, 4] - kmeans_mean)^2 + (stability_apn_ranking[, 4] - kmeans_mean)^2 + 
                        (stability_fom_ranking[, 4] - kmeans_mean)^2)/12)

# complete
complete_mean <- (bh_ranking[, 5] + ch_ranking[, 5] + connectivity_ranking[, 5] + 
                  db_ranking[, 5] + dunn_ranking[, 5] + hub_ranking[, 5] +
                  rl_ranking[, 5] + sd_ranking[, 5] + stability_ad_ranking[, 5] +
                  stability_adm_ranking[, 5]+ stability_apn_ranking[, 5] + 
                  stability_fom_ranking[, 5])/12

complete_std <- sqrt(((bh_ranking[, 5] - kmeans_mean)^2 + (ch_ranking[, 5] - kmeans_mean)^2 + (connectivity_ranking[, 5] - kmeans_mean)^2 + 
                      (db_ranking[, 5] - kmeans_mean)^2 + (dunn_ranking[, 5] - kmeans_mean)^2 + (hub_ranking[, 5] - kmeans_mean)^2 +
                      (rl_ranking[, 5] - kmeans_mean)^2 + (sd_ranking[, 5] - kmeans_mean)^2 + (stability_ad_ranking[, 5] - kmeans_mean)^2 +
                      (stability_adm_ranking[, 5] - kmeans_mean)^2 + (stability_apn_ranking[, 5] - kmeans_mean)^2 + 
                      (stability_fom_ranking[, 5] - kmeans_mean)^2)/12)

# average
average_mean <- (bh_ranking[, 6] + ch_ranking[, 6] + connectivity_ranking[, 6] + 
                    db_ranking[, 6] + dunn_ranking[, 6] + hub_ranking[, 6] +
                    rl_ranking[, 6] + sd_ranking[, 6] + stability_ad_ranking[, 6] +
                    stability_adm_ranking[, 6]+ stability_apn_ranking[, 6] + 
                    stability_fom_ranking[, 6])/12

average_std <- sqrt(((bh_ranking[, 6] - kmeans_mean)^2 + (ch_ranking[, 6] - kmeans_mean)^2 + (connectivity_ranking[, 6] - kmeans_mean)^2 + 
                        (db_ranking[, 6] - kmeans_mean)^2 + (dunn_ranking[, 6] - kmeans_mean)^2 + (hub_ranking[, 6] - kmeans_mean)^2 +
                        (rl_ranking[, 6] - kmeans_mean)^2 + (sd_ranking[, 6] - kmeans_mean)^2 + (stability_ad_ranking[, 6] - kmeans_mean)^2 +
                        (stability_adm_ranking[, 6] - kmeans_mean)^2 + (stability_apn_ranking[, 6] - kmeans_mean)^2 + 
                        (stability_fom_ranking[, 6] - kmeans_mean)^2)/12)

# ward
ward_mean <- (bh_ranking[, 7] + ch_ranking[, 7] + connectivity_ranking[, 7] + 
                   db_ranking[, 7] + dunn_ranking[, 7] + hub_ranking[, 7] +
                   rl_ranking[, 7] + sd_ranking[, 7] + stability_ad_ranking[, 7] +
                   stability_adm_ranking[, 7]+ stability_apn_ranking[, 7] + 
                   stability_fom_ranking[, 7])/12

ward_std <- sqrt(((bh_ranking[, 7] - kmeans_mean)^2 + (ch_ranking[, 7] - kmeans_mean)^2 + (connectivity_ranking[, 7] - kmeans_mean)^2 + 
                       (db_ranking[, 7] - kmeans_mean)^2 + (dunn_ranking[, 7] - kmeans_mean)^2 + (hub_ranking[, 7] - kmeans_mean)^2 +
                       (rl_ranking[, 7] - kmeans_mean)^2 + (sd_ranking[, 7] - kmeans_mean)^2 + (stability_ad_ranking[, 7] - kmeans_mean)^2 +
                       (stability_adm_ranking[, 7] - kmeans_mean)^2 + (stability_apn_ranking[, 7] - kmeans_mean)^2 + 
                       (stability_fom_ranking[, 7] - kmeans_mean)^2)/12)

# closest
closest_mean <- (bh_ranking[, 8] + ch_ranking[, 8] + connectivity_ranking[, 8] + 
                db_ranking[, 8] + dunn_ranking[, 8] + hub_ranking[, 8] +
                rl_ranking[, 8] + sd_ranking[, 8] + stability_ad_ranking[, 8] +
                stability_adm_ranking[, 8]+ stability_apn_ranking[, 8] + 
                stability_fom_ranking[, 8])/12

closest_std <- sqrt(((bh_ranking[, 8] - kmeans_mean)^2 + (ch_ranking[, 8] - kmeans_mean)^2 + (connectivity_ranking[, 8] - kmeans_mean)^2 + 
                    (db_ranking[, 8] - kmeans_mean)^2 + (dunn_ranking[, 8] - kmeans_mean)^2 + (hub_ranking[, 8] - kmeans_mean)^2 +
                    (rl_ranking[, 8] - kmeans_mean)^2 + (sd_ranking[, 8] - kmeans_mean)^2 + (stability_ad_ranking[, 8] - kmeans_mean)^2 +
                    (stability_adm_ranking[, 8] - kmeans_mean)^2 + (stability_apn_ranking[, 8] - kmeans_mean)^2 + 
                    (stability_fom_ranking[, 8] - kmeans_mean)^2)/12)

# mbkmeans
mbkmeans_mean <- (bh_ranking[, 9] + ch_ranking[, 9] + connectivity_ranking[, 9] + 
                   db_ranking[, 9] + dunn_ranking[, 9] + hub_ranking[, 9] +
                   rl_ranking[, 9] + sd_ranking[, 9] + stability_ad_ranking[, 9] +
                   stability_adm_ranking[, 9]+ stability_apn_ranking[, 9] + 
                   stability_fom_ranking[, 9])/12

mbkmeans_std <- sqrt(((bh_ranking[, 9] - kmeans_mean)^2 + (ch_ranking[, 9] - kmeans_mean)^2 + (connectivity_ranking[, 9] - kmeans_mean)^2 + 
                       (db_ranking[, 9] - kmeans_mean)^2 + (dunn_ranking[, 9] - kmeans_mean)^2 + (hub_ranking[, 9] - kmeans_mean)^2 +
                       (rl_ranking[, 9] - kmeans_mean)^2 + (sd_ranking[, 9] - kmeans_mean)^2 + (stability_ad_ranking[, 9] - kmeans_mean)^2 +
                       (stability_adm_ranking[, 9] - kmeans_mean)^2 + (stability_apn_ranking[, 9] - kmeans_mean)^2 + 
                       (stability_fom_ranking[, 9] - kmeans_mean)^2)/12)

# fcmeans
fcmeans_mean <- (bh_ranking[, 10] + ch_ranking[, 10] + connectivity_ranking[, 10] + 
                    db_ranking[, 10] + dunn_ranking[, 10] + hub_ranking[, 10] +
                    rl_ranking[, 10] + sd_ranking[, 10] + stability_ad_ranking[, 10] +
                    stability_adm_ranking[, 10]+ stability_apn_ranking[, 10] + 
                    stability_fom_ranking[, 10])/12

fcmeans_std <- sqrt(((bh_ranking[, 10] - kmeans_mean)^2 + (ch_ranking[, 10] - kmeans_mean)^2 + (connectivity_ranking[, 10] - kmeans_mean)^2 + 
                        (db_ranking[, 10] - kmeans_mean)^2 + (dunn_ranking[, 10] - kmeans_mean)^2 + (hub_ranking[, 10] - kmeans_mean)^2 +
                        (rl_ranking[, 10] - kmeans_mean)^2 + (sd_ranking[, 10] - kmeans_mean)^2 + (stability_ad_ranking[, 10] - kmeans_mean)^2 +
                        (stability_adm_ranking[, 10] - kmeans_mean)^2 + (stability_apn_ranking[, 10] - kmeans_mean)^2 + 
                        (stability_fom_ranking[, 10] - kmeans_mean)^2)/12)

ranking <- data.frame(kmeans_mean, kmedians_mean, kmedoids_mean, single_mean, complete_mean,
                      average_mean, ward_mean, closest_mean, mbkmeans_mean, fcmeans_mean)
colnames(ranking) <- colnames(bh)

# data standardization and normalization

ranking_std <- as.data.frame(apply(ranking, 2, robStandardize)) # median and median absolute deviation

min_max <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

ranking_norm <- as.data.frame(apply(ranking_std, 2, min_max)) # min-max
metadata_ranking <- cbind(metadata_bh[, 1:21], ranking_norm)

write.csv(metadata_ranking, 'metadata_ranking.csv', row.names = FALSE)





