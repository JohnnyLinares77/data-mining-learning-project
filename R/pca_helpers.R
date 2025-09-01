# R/pca_helpers.R
# ---- PCA con prcomp; selección por n_comp o varianza objetivo
run_pca <- function(X_final, n_comp = NULL, var_obj = NULL){
  stopifnot(!is.null(X_final), nrow(X_final) > 1)
  pc <- prcomp(X_final, center = FALSE, scale. = FALSE)
  var_exp <- pc$sdev^2 / sum(pc$sdev^2)
  var_acum <- cumsum(var_exp)

  if(!is.null(var_obj)){
    n_comp_used <- which(var_acum >= var_obj)[1]
  } else if(!is.null(n_comp)){
    n_comp_used <- min(n_comp, ncol(pc$x))
  } else {
    n_comp_used <- min(2L, ncol(pc$x))
  }

  scores <- pc$x[, 1:n_comp_used, drop = FALSE]
  list(
    pca_obj = pc,
    scores = scores,
    varianza = data.frame(PC = seq_along(var_exp), var_exp = var_exp, var_acum = var_acum),
    n_comp_usadas = n_comp_used
  )
}
