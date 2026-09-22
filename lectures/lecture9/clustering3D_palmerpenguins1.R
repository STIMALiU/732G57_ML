# Load packages
library(palmerpenguins)
library(plotly)

# Select three continuous variables and remove missing observations
clustering_data <- penguins[, c(
  "bill_length_mm",
  "bill_depth_mm",
  "flipper_length_mm"
)]

clustering_data <- na.omit(clustering_data)

# Inspect the data
str(clustering_data)
summary(clustering_data)

# Standardize the variables
clustering_data_scaled <- scale(clustering_data)
clustering_data_scaled <- as.data.frame(clustering_data_scaled)

# 3D scatter plot before clustering
plot_ly(
  data = clustering_data_scaled,
  x = ~bill_length_mm,
  y = ~bill_depth_mm,
  z = ~flipper_length_mm,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 4,
    color = "steelblue"
  ),
  name = "Observationer"
) |>
  layout(
    title = "Standardiserade pingvindata",
    scene = list(
      xaxis = list(title = "Näbblängd"),
      yaxis = list(title = "Näbbdjup"),
      zaxis = list(title = "Vinglängd")
    )
  )

# Perform K-means clustering
set.seed(73257)

kmeans_result <- stats::kmeans(
  x = clustering_data_scaled,
  centers = 3,
  nstart = 50
)

# Add cluster membership to the data
clustering_data_scaled$cluster <- factor(kmeans_result$cluster)
clustering_data$cluster <- factor(kmeans_result$cluster)

# Frequency table of cluster membership
cluster_frequency <- table(clustering_data$cluster)
cluster_frequency

# Extract the estimated cluster centroids
centroids <- as.data.frame(kmeans_result$centers)
centroids$cluster <- factor(seq_len(nrow(centroids)))

# 3D scatter plot with clusters and centroids
plot_ly(
  data = clustering_data_scaled,
  x = ~bill_length_mm,
  y = ~bill_depth_mm,
  z = ~flipper_length_mm,
  color = ~cluster,
  colors = c("steelblue", "darkorange", "darkgreen"),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4),
  name = "Observationer"
) |>
  add_trace(
    data = centroids,
    x = ~bill_length_mm,
    y = ~bill_depth_mm,
    z = ~flipper_length_mm,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 7,
      color = "black",
      symbol = "diamond",
      line = list(
        color = "white",
        width = 2
      )
    ),
    name = "Centroider",
    inherit = FALSE
  ) |>
  layout(
    title = "K-means-klustring av standardiserade pingvindata",
    legend = list(
      title = list(text = "Kluster")
    ),
    scene = list(
      xaxis = list(title = "Näbblängd"),
      yaxis = list(title = "Näbbdjup"),
      zaxis = list(title = "Vinglängd")
    )
  )

# Descriptive statistics for the original variables,
# calculated separately for each cluster
cluster_summaries <- lapply(
  split(clustering_data[, 1:3], clustering_data$cluster),
  summary
)

cluster_summaries