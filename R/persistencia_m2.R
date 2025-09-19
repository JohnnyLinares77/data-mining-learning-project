# R/persistencia_m2.R
# -------------------------------------------------------------------
# Persistencia de resultados del Módulo 2 – Scoring (Regresión Logística)
# - Guarda métricas agregadas (eval_m2.csv)
# - Guarda resultados por cliente (clientes_scores.csv)
# - Guarda variables seleccionadas/candidatas (variables_modelos.csv)
# Notas:
#   * Se redondea a 4 decimales donde aplica.
#   * Escritura robusta ante cambios de columnas (alineación + NA).
# -------------------------------------------------------------------

.ensure_dir <- function(path = "data"){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# Append SEGURO a CSV:
# - Alinea columnas de 'old' y 'df' si difieren.
# - Captura errores para no tumbar la sesión Shiny.
.write_append_safe <- function(df, file){
  .ensure_dir(dirname(file))
  tryCatch({
    if (file.exists(file)) {
      old <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
      # Unificar esquema de columnas
      cols <- union(names(old), names(df))
      for (nm in setdiff(cols, names(old))) old[[nm]] <- NA
      for (nm in setdiff(cols, names(df)))  df[[nm]]  <- NA
      # Orden consistente de columnas
      old <- old[, cols, drop = FALSE]
      df  <- df[,  cols, drop = FALSE]
      df  <- rbind(old, df)
    }
    utils::write.csv(df, file, row.names = FALSE)
    TRUE
  }, error = function(e){
    message("persistencia_m2: error al escribir en ", file, " -> ", conditionMessage(e))
    FALSE
  })
}

# -------------------------------------------------------------------
# 1) Métricas agregadas al umbral (AUCs + métricas en el thr actual)
#    Salida: data/eval_m2.csv
# -------------------------------------------------------------------
persist_eval_m2 <- function(id_sim, auc_accept, auc_mora,
                            thr, accuracy, sensibilidad, especificidad, precision, f1){
  .ensure_dir("data")
  file <- file.path("data","eval_m2.csv")

  num4 <- function(x) {
    if (is.null(x) || is.na(x)) NA_real_ else round(as.numeric(x), 4)
  }

  df <- data.frame(
    id_sim        = id_sim,
    fecha         = as.character(Sys.time()),
    auc_accept    = num4(auc_accept),
    auc_mora      = num4(auc_mora),
    thr           = num4(thr),
    accuracy      = num4(accuracy),
    sensibilidad  = num4(sensibilidad),
    especificidad = num4(especificidad),
    precision     = num4(precision),
    f1            = num4(f1),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  .write_append_safe(df, file)
}

# -------------------------------------------------------------------
# 2) Resultados por cliente (probabilidades, score, decisión)
#    Salida: data/clientes_scores.csv
# -------------------------------------------------------------------
persist_clientes_scores <- function(id_sim, df_clientes){
  .ensure_dir("data")
  file <- file.path("data","clientes_scores.csv")

  # Asegurar tipos y columnas esperadas
  if (!is.data.frame(df_clientes)) {
    stop("persist_clientes_scores: 'df_clientes' debe ser data.frame.")
  }

  # Asegurar id_cliente como character
  if ("id_cliente" %in% names(df_clientes)) {
    df_clientes$id_cliente <- as.character(df_clientes$id_cliente)
  }

  # Redondeo a 4 decimales
  for (nm in c("p_accept","p_mora","score")) {
    if (nm %in% names(df_clientes)) {
      df_clientes[[nm]] <- round(as.numeric(df_clientes[[nm]]), 4)
    }
  }

  # Agregar columnas requeridas
  df_clientes$id_sim <- id_sim
  df_clientes$timestamp <- as.character(Sys.time())

  # Evitar que se re-encodeen nombres de columnas
  attr(df_clientes, "stringsAsFactors") <- FALSE

  .write_append_safe(df_clientes, file)
}

# -------------------------------------------------------------------
# 3) Variables candidatas/seleccionadas con α de significancia
#    Salida: data/variables_modelos.csv
# -------------------------------------------------------------------
persist_variables_m2 <- function(id_sim,
                                 vars_candidatas,
                                 vars_accept_keep,
                                 vars_mora_keep,
                                 alpha_sig){
  .ensure_dir("data")
  file <- file.path("data","variables_modelos.csv")

  # Normalizar NULLs y a vector de caracteres
  `%||%` <- function(a, b) if (is.null(a)) b else a
  vars_candidatas  <- as.character(vars_candidatas  %||% character(0))
  vars_accept_keep <- as.character(vars_accept_keep %||% character(0))
  vars_mora_keep   <- as.character(vars_mora_keep   %||% character(0))

  df <- data.frame(
    id_sim            = id_sim,
    modulo            = "M2",
    vars_candidatas   = paste(vars_candidatas,  collapse = "|"),
    vars_accept_keep  = paste(vars_accept_keep, collapse = "|"),
    vars_mora_keep    = paste(vars_mora_keep,   collapse = "|"),
    alpha             = alpha_sig,
    fecha             = as.character(Sys.time()),
    stringsAsFactors  = FALSE,
    check.names       = FALSE
  )

  .write_append_safe(df, file)
}
