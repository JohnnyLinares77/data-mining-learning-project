# R/persistencia.R
dir.ensure <- function(path) if(!dir.exists(path)) dir.create(path, recursive = TRUE)

persist_eval_m1 <- function(id_sim, varianza_pca, k, silueta, inercia_total, criterio_k, path="data"){
  dir.ensure(path)
  f <- file.path(path, "eval_m1.csv")
  df <- data.frame(id_sim, varianza_pca, k, silueta, inercia_total, criterio_k, stringsAsFactors = FALSE)
  if(file.exists(f)) write.table(df, f, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  else write.table(df, f, sep=",", row.names=FALSE, col.names=TRUE)
}

persist_variables_modelos <- function(id_sim, modulo, vars, path="data"){
  dir.ensure(path)
  f <- file.path(path, "variables_modelos.csv")
  df <- data.frame(id_sim, modulo, variable = vars, stringsAsFactors = FALSE)
  if(file.exists(f)) write.table(df, f, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  else write.table(df, f, sep=",", row.names=FALSE, col.names=TRUE)
}

persist_clientes_clusters <- function(id_sim, id_cliente, cluster_id, path="data"){
  dir.ensure(path)
  f <- file.path(path, "clientes_clusters.csv")
  df <- data.frame(
    id_sim = id_sim,
    id_cliente = as.character(id_cliente),
    cluster_id = cluster_id,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  tryCatch({
    if(file.exists(f)) write.table(df, f, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
    else write.table(df, f, sep=",", row.names=FALSE, col.names=TRUE)
    TRUE
  }, error = function(e) {
    message("Error guardando clusters: ", conditionMessage(e))
    FALSE
  })
}
