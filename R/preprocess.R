# R/preprocess.R
# ---- Escalado y dummies según selección
preprocess_inputs <- function(df_list, vars_seleccionadas){
  # Unimos tablas relevantes por id_cliente
  base <- Reduce(function(a,b) merge(a,b, by="id_cliente", all=TRUE),
                 df_list[c("demograficas","comp_historico","financieras")])

  # Subset de columnas
  cols <- c("id_cliente", vars_seleccionadas)
  X <- base[, intersect(cols, names(base)), drop = FALSE]

  # Detectar tipos
  numericas <- names(X)[sapply(X, is.numeric)]
  numericas <- setdiff(numericas, "id_cliente")
  categoricas <- setdiff(names(X), c("id_cliente", numericas))

  # Dummies para categóricas (si las hay)
  X_cat <- NULL
  if(length(categoricas) > 0){
    for(v in categoricas){
      X[[v]] <- factor(X[[v]])
    }
    mm <- model.matrix(~ . - 1, data = X[, categoricas, drop = FALSE])
    X_cat <- as.matrix(mm)
  }

  # Escalado z-score para numéricas
  X_num <- NULL
  if(length(numericas) > 0){
    X_num <- scale(as.matrix(X[, numericas, drop = FALSE]))
  }

  # Unión final
  if(!is.null(X_num) && !is.null(X_cat)){
    X_final <- cbind(X_num, X_cat)
  } else if(!is.null(X_num)) {
    X_final <- X_num
  } else {
    X_final <- X_cat
  }

  list(
    X_final = X_final,
    id_cliente = X$id_cliente
  )
}
