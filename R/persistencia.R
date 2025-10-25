# R/persistencia.R
dir.ensure <- function(path) if(!dir.exists(path)) dir.create(path, recursive = TRUE)

# ---- Función auxiliar para marcar modo independiente
get_persistence_suffix <- function(execution_mode = "sequential") {
  if (execution_mode == "independent") "_independent" else ""
}

persist_eval_m1 <- function(id_sim, varianza_pca, k, silueta, inercia_total, criterio_k, path="data", execution_mode="sequential"){
  dir.ensure(path)
  suffix <- get_persistence_suffix(execution_mode)
  f <- file.path(path, paste0("eval_m1", suffix, ".csv"))
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

persist_clientes_clusters <- function(id_sim, id_cliente, cluster_id, path="data", execution_mode="sequential"){
  dir.ensure(path)
  suffix <- get_persistence_suffix(execution_mode)
  f <- file.path(path, paste0("clientes_clusters", suffix, ".csv"))
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
