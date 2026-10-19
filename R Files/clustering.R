# 10_clustering.R
# K-Means on district-disease profiles (one row per district + disease).
# Needs: install.packages(c("dplyr", "ggplot2", "cluster"))

library(dplyr)
library(ggplot2)
library(cluster)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures/clustering", recursive = TRUE, showWarnings = FALSE)

d <- read.csv("data/final/modeling_data.csv", stringsAsFactors = FALSE)
d$surge <- as.integer(d$high_outbreak %in% c("High", 1, "1", TRUE, "TRUE"))
d$is_monsoon <- as.numeric(d$is_monsoon)

# Unsupervised, so all splits are used. Pairs with fewer than 5 reports are dropped.
profile <- d %>%
  group_by(Disease, state_ut, district, zone) %>%
  summarise(n_reports     = n(),
            mean_log_cases = mean(log_cases),
            surge_rate     = mean(surge),
            mean_temp      = mean(Temp_C, na.rm = TRUE),
            mean_rain      = mean(preci_log, na.rm = TRUE),
            mean_lai       = mean(LAI, na.rm = TRUE),
            monsoon_share  = mean(is_monsoon),
            .groups = "drop") %>%
  filter(n_reports >= 5) %>%
  filter(complete.cases(.))

feat <- c("mean_log_cases", "surge_rate", "mean_temp", "mean_rain",
          "mean_lai", "monsoon_share")
X <- scale(profile[, feat])
cat("Profiles clustered:", nrow(X), "\n")

# ---------- choose k: elbow + silhouette ----------
set.seed(42)
dist_mat <- dist(X)
ks <- 2:8
sel <- do.call(rbind, lapply(ks, function(k) {
  km <- kmeans(X, centers = k, nstart = 25)
  data.frame(k = k, wss = km$tot.withinss,
             silhouette = mean(silhouette(km$cluster, dist_mat)[, 3]))
}))
print(sel)
write.csv(sel, "outputs/tables/kmeans_k_selection.csv", row.names = FALSE)

p_sel <- ggplot(sel, aes(k)) +
  geom_line(aes(y = wss)) + geom_point(aes(y = wss)) +
  labs(title = "Elbow: total within-cluster SS", y = "WSS") + theme_minimal()
p_sil <- ggplot(sel, aes(k, silhouette)) + geom_line() + geom_point() +
  labs(title = "Average silhouette width", y = "Silhouette") + theme_minimal()
ggsave("outputs/figures/clustering/kmeans_elbow.png", p_sel, width = 6, height = 4, dpi = 300)
ggsave("outputs/figures/clustering/kmeans_silhouette.png", p_sil, width = 6, height = 4, dpi = 300)

# Best silhouette by default; set k_final <- 4 (for example) to override after viewing the plots
k_final <- sel$k[which.max(sel$silhouette)]
cat("Chosen k:", k_final, "\n")

set.seed(42)
km <- kmeans(X, centers = k_final, nstart = 50)
profile$cluster <- factor(km$cluster)

# ---------- profile the clusters ----------
cluster_means <- profile %>%
  group_by(cluster) %>%
  summarise(n = n(), across(all_of(feat), ~ round(mean(.x), 2)), .groups = "drop")
print(cluster_means)
write.csv(cluster_means, "outputs/tables/cluster_profile_means.csv", row.names = FALSE)

disease_by_cluster <- round(100 * prop.table(table(profile$Disease, profile$cluster), 2), 1)
zone_by_cluster    <- round(100 * prop.table(table(profile$zone, profile$cluster), 2), 1)
cat("\nDisease mix per cluster (% of cluster):\n"); print(disease_by_cluster)
cat("\nZone mix per cluster (% of cluster):\n");    print(zone_by_cluster)
write.csv(as.data.frame.matrix(disease_by_cluster), "outputs/tables/cluster_disease_mix.csv")
write.csv(as.data.frame.matrix(zone_by_cluster), "outputs/tables/cluster_zone_mix.csv")
write.csv(profile, "outputs/tables/cluster_assignments.csv", row.names = FALSE)

# ---------- PCA view ----------
pca <- prcomp(X)
pc <- data.frame(pca$x[, 1:2], cluster = profile$cluster, Disease = profile$Disease)
p_pca <- ggplot(pc, aes(PC1, PC2, colour = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = paste("K-Means clusters (k =", k_final, ") on PCA axes")) +
  theme_minimal()
ggsave("outputs/figures/clustering/kmeans_pca.png", p_pca, width = 7, height = 5, dpi = 300)