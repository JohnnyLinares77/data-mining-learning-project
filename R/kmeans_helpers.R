# R/kmeans_helpers.R
# ---- KMeans + métricas (silueta, WSS)
run_kmeans <- function(scores, k){
  stopifnot(k >= 2)
  km <- kmeans(scores, centers = k, nstart = 10)
  # Silueta
  if (nrow(scores) > k){
    sil <- silhouette(km$cluster, dist(scores))
    sil_mean <- mean(sil[, "sil_width"])
  } else {
    sil_mean <- NA_real_
  }
  list(
    clusters = km$cluster,
    wss = sum(km$withinss),
    inertia_total = km$tot.withinss,
    silueta = sil_mean
  )
}

# ---- Evalua rango de K (para graficos de Codo y Silueta)
evaluate_k_range <- function(scores, k_min = 2L, k_max = 10L){
  stopifnot(k_min >= 2, k_max > k_min)
  out <- data.frame(k = integer(), wss = numeric(), silueta = numeric())
  D <- dist(scores)
  for(k in k_min:k_max){
    km <- kmeans(scores, centers = k, nstart = 10)
    sil_mean <- NA_real_
    if(nrow(scores) > k){
      sil <- silhouette(km$cluster, D)
      sil_mean <- mean(sil[, "sil_width"])
    }
    out <- rbind(out, data.frame(k = k, wss = sum(km$withinss), silueta = sil_mean))
  }
  out
}
