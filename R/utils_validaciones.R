# R/utils_validaciones.R
validar_variables <- function(vars){
  # Validar que se seleccione exactamente 8 variables.
  if (length(vars) != 8) {
    return(list(ok = FALSE, msg = "Seleccione exactamente 8 variables para entrenar el árbol."))
  }
  list(ok = TRUE, msg = "")
}

validar_k <- function(k){
  if(is.na(k) || k < 2) return(list(ok = FALSE, msg = "K debe ser un entero >= 2."))
  list(ok = TRUE, msg = "")
}
