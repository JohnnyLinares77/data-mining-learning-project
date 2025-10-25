# R/utils_validaciones.R
validar_variables <- function(vars){
  # Validar que se seleccione exactamente 12 variables.
  if (length(vars) != 12) {
    return(list(ok = FALSE, msg = "Seleccione exactamente 12 variables para entrenar el árbol."))
  }
  list(ok = TRUE, msg = "")
}

validar_k <- function(k){
  if(is.na(k) || k < 2) return(list(ok = FALSE, msg = "K debe ser un entero >= 2."))
  list(ok = TRUE, msg = "")
}
