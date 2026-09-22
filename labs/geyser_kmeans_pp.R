#-------------------------------------------------------------------------------
# Compare ordinary K-means and K-means++
#-------------------------------------------------------------------------------

rm(list = ls())

library(MASS)
library(TreeDist)
library(ggplot2)
library(cowplot)

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

data("geyser", package = "MASS")

X <- geyser[, c(
  "duration",
  "waiting"
)]

# Standardize variables
X_scaled <- scale(X)
X_scaled <- as.data.frame(X_scaled)

# Fixed number of clusters
K <- 4

#-------------------------------------------------------------------------------
# Ordinary K-means with nstart=1
#-------------------------------------------------------------------------------

set.seed(100)

fit_kmeans_nstart1 <- kmeans(
  x = X_scaled,
  centers = K,
  nstart = 1,
  iter.max = 100,
  algorithm = "Lloyd"
)

#-------------------------------------------------------------------------------
# Ordinary K-means with nstart=10
#-------------------------------------------------------------------------------

set.seed(100)

fit_kmeans_nstart10  <- kmeans(
  x = X_scaled,
  centers = K,
  nstart = 10,
  iter.max = 100,
  algorithm = "Lloyd"
)


#-------------------------------------------------------------------------------
# K-means++
#-------------------------------------------------------------------------------

set.seed(100)

fit_kmeans_pp <- KMeansPP(
  x = as.matrix(X_scaled),
  k = K,
  nstart = 1
)

#-------------------------------------------------------------------------------
# Compare SSE
#-------------------------------------------------------------------------------

sse_results <- data.frame(
  Method = c(
    "K-means, nstart=1",
    "K-means, nstart=10",
    "K-means++"
  ),
  SSE = c(
    fit_kmeans_nstart1$tot.withinss,
    fit_kmeans_nstart10$tot.withinss,
    fit_kmeans_pp$tot.withinss
  )
)

print(sse_results)

# hur blir skillnaden i de olika fallen?


#-------------------------------------------------------------------------------
# Prepare plotting data
#-------------------------------------------------------------------------------

plot_kmeans_nstart1 <- X_scaled
plot_kmeans_nstart1$cluster <- factor(
  fit_kmeans_nstart1$cluster
)

plot_kmeans_nstart10 <- X_scaled
plot_kmeans_nstart10$cluster <- factor(
  fit_kmeans_nstart10$cluster
)

plot_kmeans_pp <- X_scaled
plot_kmeans_pp$cluster <- factor(
  fit_kmeans_pp$cluster
)

cluster_colors <- c(
  "1" = "#0072B2",
  "2" = "#D55E00",
  "3" = "#009E73",
  "4" = "#CC79A7"
)

#-------------------------------------------------------------------------------
# Plot: K-means, nstart=1
#-------------------------------------------------------------------------------

p1 <- ggplot(
  data = plot_kmeans_nstart1,
  aes(
    x = duration,
    y = waiting,
    color = cluster
  )
) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  geom_point(
    data = as.data.frame(fit_kmeans_nstart1$centers),
    aes(
      x = duration,
      y = waiting
    ),
    inherit.aes = FALSE,
    shape = 8,
    size = 5,
    color = "black"
  ) +
  scale_color_manual(
    values = cluster_colors
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Vanlig K-means, nstart=1",
    subtitle = paste(
      "SSE =",
      round(fit_kmeans_nstart1$tot.withinss, 2)
    ),
    x = "Duration",
    y = "Waiting"
  )

#-------------------------------------------------------------------------------
# Plot: K-means, nstart=10
#-------------------------------------------------------------------------------

p2 <- ggplot(
  data = plot_kmeans_nstart10,
  aes(
    x = duration,
    y = waiting,
    color = cluster
  )
) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  geom_point(
    data = as.data.frame(fit_kmeans_nstart10$centers),
    aes(
      x = duration,
      y = waiting
    ),
    inherit.aes = FALSE,
    shape = 8,
    size = 5,
    color = "black"
  ) +
  scale_color_manual(
    values = cluster_colors
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Vanlig K-means, nstart=10",
    subtitle = paste(
      "SSE =",
      round(fit_kmeans_nstart10$tot.withinss, 2)
    ),
    x = "Duration",
    y = "Waiting"
  )


#-------------------------------------------------------------------------------
# Plot: K-means++
#-------------------------------------------------------------------------------

p3 <- ggplot(
  data = plot_kmeans_pp,
  aes(
    x = duration,
    y = waiting,
    color = cluster
  )
) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  geom_point(
    data = as.data.frame(fit_kmeans_pp$centers),
    aes(
      x = duration,
      y = waiting
    ),
    inherit.aes = FALSE,
    shape = 8,
    size = 5,
    color = "black"
  ) +
  scale_color_manual(
    values = cluster_colors
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "K-means++",
    subtitle = paste(
      "SSE =",
      round(fit_kmeans_pp$tot.withinss, 2)
    ),
    x = "Duration",
    y = "Waiting"
  )

#-------------------------------------------------------------------------------
# Combine plots
#-------------------------------------------------------------------------------

cowplot::plot_grid(
  p1,p2,p3,
  labels = c("A", "B"),
  nrow = 1
)

#-------------------------------------------------------------------------------
# Frequency tables
#-------------------------------------------------------------------------------

table(fit_kmeans_nstart1$cluster)
table(fit_kmeans_nstart10$cluster)
table(fit_kmeans_pp$cluster)



#-------------------------------------------------------------------------------
# Comment
#-------------------------------------------------------------------------------
# Note: geyser is a relatively simple dataset. As the data become more
# complex, the advantage of K-means++ over standard K-means is expected
# to become more noticeable.

