# R/utils_validaciones.R
validar_variables <- function(vars){
  # Validar que se seleccione al menos 3 variables (para M4 específicamente)
  if (length(vars) < 3) {
    return(list(ok = FALSE, msg = "Seleccione al menos 3 variables para entrenar el árbol."))
  }
  list(ok = TRUE, msg = "")
}

validar_variables_m1 <- function(vars){
  # Validar que se seleccione exactamente 12 variables (para M1 específicamente)
  if (length(vars) != 12) {
    return(list(ok = FALSE, msg = "Seleccione exactamente 12 variables para entrenar el modelo."))
  }
  list(ok = TRUE, msg = "")
}

validar_k <- function(k){
  if(is.na(k) || k < 2) return(list(ok = FALSE, msg = "K debe ser un entero >= 2."))
  list(ok = TRUE, msg = "")
}
