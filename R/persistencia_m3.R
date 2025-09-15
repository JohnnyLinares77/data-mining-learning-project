.ensure_dir <- function(path = "data"){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

.write_append_safe <- function(df, file){
  .ensure_dir(dirname(file))
  tryCatch({
    if (file.exists(file)) {
      old <- utils::read.csv(file, stringsAsFactors = FALSE)
      cols <- union(names(old), names(df))
      for (nm in setdiff(cols, names(old))) old[[nm]] <- NA
      for (nm in setdiff(cols, names(df)))  df[[nm]]  <- NA
      old <- old[, cols, drop = FALSE]
      df  <- df[,  cols, drop = FALSE]
      df  <- rbind(old, df)
    }
    utils::write.csv(df, file, row.names = FALSE)
    TRUE
  }, error = function(e){
    message("persistencia_m3: error al escribir: ", conditionMessage(e))
    FALSE
  })
}

persist_eval_m3 <- function(id_sim, modelo, rmse, r2_adj, alpha, vars){
  df <- data.frame(
    timestamp = as.character(Sys.time()),
    id_sim    = id_sim,
    modelo    = modelo,
    rmse      = rmse,
    r2_adj    = r2_adj,
    alpha     = alpha,
    vars      = paste(vars, collapse = ",")
  )
  .write_append_safe(df, file.path("data","eval_m3.csv"))
}

persist_simulacion_m3 <- function(id_sim, modelo, monto, plazo, score, cluster,
                                  rate_vec, me_vec, elasticidad_vec){
  df <- data.frame(
    timestamp = as.character(Sys.time()),
    id_sim    = id_sim,
    modelo    = modelo,
    monto     = monto,
    plazo     = plazo,
    score     = score,
    cluster   = cluster,
    rate      = rate_vec,
    ME        = me_vec,
    elasticidad = elasticidad_vec
  )
  .write_append_safe(df, file.path("data","simulacion_m3.csv"))
}
