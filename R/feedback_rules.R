# R/feedback_rules.R
feedback_metricas <- function(silueta, wss, var_acum){
  msg <- character()
  if(!is.na(silueta)){
    if(silueta < 0.25) msg <- c(msg, "Silueta baja: considere ajustar K o revisar variables.")
    else if(silueta < 0.4) msg <- c(msg, "Silueta media: puede mejorar probando otro K.")
    else msg <- c(msg, "Silueta adecuada.")
  } else {
    msg <- c(msg, "No se pudo calcular silueta (demasiados clusters o pocos puntos por cluster).")
  }
  if(var_acum < 0.7) msg <- c(msg, "Varianza explicada por PCA menor a 70%: incremente componentes.")
  paste(msg, collapse = " ")
}
