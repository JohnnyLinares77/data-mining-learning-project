# R/persistencia_m4.R

.ensure_dir <- function(path = "data") {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

.write_append_safe <- function(df, file) {
  .ensure_dir(dirname(file))
  tryCatch({
    if (file.exists(file)) {
      old <- utils::read.csv(file, stringsAsFactors = FALSE)
      cols <- union(names(old), names(df))
      for (nm in setdiff(cols, names(old))) old[[nm]] <- NA
      for (nm in setdiff(cols, names(df))) df[[nm]] <- NA
      old <- old[, cols, drop = FALSE]
      df <- df[, cols, drop = FALSE]
      df <- rbind(old, df)
    }
    utils::write.csv(df, file, row.names = FALSE)
    TRUE
  }, error = function(e) {
    message("persistencia_m4: error al escribir: ", conditionMessage(e))
    FALSE
  })
}

persist_eval_m4 <- function(id_sim, accuracy, macro_f1, n_nodos, vars_usadas, execution_mode = "sequential") {
  suffix <- if (execution_mode == "independent") "_independent" else ""
  df <- data.frame(
    timestamp = as.character(Sys.time()),
    id_sim = id_sim,
    accuracy = accuracy,
    macro_f1 = macro_f1,
    n_nodos = n_nodos,
    vars_usadas = vars_usadas
  )
  .write_append_safe(df, file.path("data", paste0("eval_m4", suffix, ".csv")))
}

persist_clasificaciones_m4 <- function(id_sim, clasificaciones, execution_mode = "sequential") {
  suffix <- if (execution_mode == "independent") "_independent" else ""
  df <- data.frame(
    timestamp = as.character(Sys.time()),
    id_sim = id_sim,
    id_cliente = clasificaciones$id_cliente,
    clase_predicha = clasificaciones$clase_predicha,
    prob_bajo = clasificaciones$prob_bajo,
    prob_medio = clasificaciones$prob_medio,
    prob_alto = clasificaciones$prob_alto,
    nivel_alerta = clasificaciones$nivel_alerta
  )
  .write_append_safe(df, file.path("data", paste0("clasificaciones_m4", suffix, ".csv")))
}