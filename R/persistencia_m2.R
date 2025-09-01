# R/persistencia_m2.R
# -------------------------------------------------------------------
# Persistencia de resultados del Módulo 2
# - Guarda métricas agregadas (eval_m2)
# - Guarda resultados por cliente (clientes_scores)
# - Guarda variables seleccionadas para cada modelo
# Todo redondeado a 4 decimales
# -------------------------------------------------------------------

.ensure_dir <- function(path = "data"){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# Append seguro
.write_append <- function(df, file){
  if (file.exists(file)) {
    old <- utils::read.csv(file, stringsAsFactors = FALSE)
    df  <- rbind(old, df)
  }
  utils::write.csv(df, file, row.names = FALSE)
}

persist_eval_m2 <- function(id_sim, auc_accept, auc_mora,
                            thr, accuracy, sensibilidad, especificidad, precision, f1){
  .ensure_dir("data")
  file <- file.path("data","eval_m2.csv")
  now  <- Sys.time()

  num <- function(x) { if (is.null(x) || is.na(x)) NA_real_ else round(x, 4) }

  df <- data.frame(
    id_sim = id_sim,
    fecha  = as.character(now),
    auc_accept = num(auc_accept),
    auc_mora   = num(auc_mora),
    thr = num(thr),
    accuracy = num(accuracy),
    sensibilidad = num(sensibilidad),
    especificidad = num(especificidad),
    precision = num(precision),
    f1 = num(f1),
    stringsAsFactors = FALSE
  )
  .write_append(df, file)
}

persist_clientes_scores <- function(id_sim, df_clientes){
  .ensure_dir("data")
  file <- file.path("data","clientes_scores.csv")

  # Redondeo 4 decimales en probs/score
  for (nm in c("p_accept","p_mora","score")) {
    if (nm %in% names(df_clientes)) df_clientes[[nm]] <- round(df_clientes[[nm]], 4)
  }

  df_clientes$id_sim <- id_sim
  .write_append(df_clientes, file)
}

persist_variables_m2 <- function(id_sim,
                                 vars_candidatas,
                                 vars_accept_keep,
                                 vars_mora_keep,
                                 alpha_sig){
  .ensure_dir("data")
  file <- file.path("data","variables_modelos.csv")
  df <- data.frame(
    id_sim = id_sim,
    modulo = "M2",
    vars_candidatas = paste(vars_candidatas, collapse = "|"),
    vars_accept_keep = paste(vars_accept_keep, collapse = "|"),
    vars_mora_keep   = paste(vars_mora_keep, collapse = "|"),
    alpha = alpha_sig,
    fecha = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  .write_append(df, file)
}
