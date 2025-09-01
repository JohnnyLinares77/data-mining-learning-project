# R/persistencia_m2.R
# -------------------------------------------------------------------
# Persistencia para Módulo 2 – Scoring
# - persist_eval_m2(): almacena métricas de validación del módulo
# - persist_clientes_scores(): almacena scores/probabilidades por cliente
# -------------------------------------------------------------------

.dir.ensure <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Guarda métricas del módulo 2 (una fila por ejecución confirmada)
persist_eval_m2 <- function(id_sim,
                            auc_accept, auc_mora,
                            thr,
                            accuracy, sensibilidad, especificidad, precision, f1,
                            path = "data"){
  .dir.ensure(path)
  file <- file.path(path, "eval_m2.csv")
  row <- data.frame(
    id_sim = id_sim,
    auc_accept = round(auc_accept, 4),
    auc_mora   = round(auc_mora, 4),
    thr        = thr,
    accuracy   = round(accuracy, 4),
    sensibilidad = round(sensibilidad, 4),
    especificidad = round(especificidad, 4),
    precision  = round(precision, 4),
    f1         = round(f1, 4),
    fecha      = Sys.time(),
    stringsAsFactors = FALSE
  )
  if (file.exists(file)) {
    old <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
    row <- if (is.null(old)) row else rbind(old, row)
  }
  utils::write.csv(row, file, row.names = FALSE)
  invisible(TRUE)
}

# Guarda resultados por cliente (probabilidades, score y decisión)
persist_clientes_scores <- function(id_sim,
                                    df_clientes, # data.frame con: id_cliente, p_accept, p_mora, score, decision
                                    path = "data"){
  .dir.ensure(path)
  file <- file.path(path, "clientes_scores.csv")
  df_clientes$id_sim <- id_sim
  if (file.exists(file)) {
    old <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
    df_clientes <- if (is.null(old)) df_clientes else rbind(old, df_clientes)
  }
  utils::write.csv(df_clientes, file, row.names = FALSE)
  invisible(TRUE)
}
