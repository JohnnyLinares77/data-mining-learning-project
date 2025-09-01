# R/export_helpers.R
exportar_resultados <- function(id_sim, path_in="data", path_out="data"){
  # Empaqueta CSVs de M1 (simple: asegura existencia)
  files <- c("variables_modelos.csv","eval_m1.csv","clientes_clusters.csv")
  ok <- all(file.exists(file.path(path_in, files)))
  if(!ok) return(FALSE)
  # Aquí podrías crear un zip; para empezar, devolvemos TRUE si existen.
  TRUE
}
