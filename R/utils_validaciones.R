# R/utils_validaciones.R
validar_variables <- function(vars){
  if(length(vars) < 1) return(list(ok = FALSE, msg = "Seleccione al menos 1 variable."))
  list(ok = TRUE, msg = "")
}

validar_k <- function(k){
  if(is.na(k) || k < 2) return(list(ok = FALSE, msg = "K debe ser un entero >= 2."))
  list(ok = TRUE, msg = "")
}
