# R/logit_helpers.R
# -------------------------------------------------------------------
# Helpers para Scoring con Regresión Logística
# - run_logit(): entrena GLM binomial y devuelve p-valores de coeficientes
# - evaluate_metrics(): calcula métricas para un umbral dado
# - evaluate_thresholds(): calcula métricas a lo largo de un grid de umbrales
# - compute_integrated_score(): combina p(aceptar) * (1 - p(mora))
# - utilidades de formateo/redondeo a 4 decimales y mapeo termino->variable base
# -------------------------------------------------------------------

# Redondeo consistente a 4 decimales
round4 <- function(x) {
  if (is.numeric(x)) return(round(x, 4))
  x
}

# AUC (opcional si está pROC)
.try_auc <- function(y, prob){
  out <- NA_real_
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc <- pROC::roc(response = y, predictor = prob, quiet = TRUE)
    out <- as.numeric(pROC::auc(roc))
  }
  out
}

# Tabla de coeficientes con p-valor y "stars" (4 decimales)
.format_coef_table <- function(coefs_mat, term_map = NULL){
  tb <- as.data.frame(coefs_mat)
  tb$Termino <- rownames(coefs_mat)
  rownames(tb) <- NULL
  names(tb) <- c("Estimado","ErrorStd","z","p_value","Termino")

  # variable base (si se provee term_map)
  if (!is.null(term_map)) {
    base <- term_map[match(tb$Termino, names(term_map))]
    tb$Variable <- ifelse(is.na(base), tb$Termino, base)
  } else {
    tb$Variable <- tb$Termino
  }

  tb$Signif <- cut(
    tb$p_value,
    breaks = c(-Inf, .001, .01, .05, .1, Inf),
    labels = c("***","**","*",".","")
  )
  tb <- tb[, c("Termino","Variable","Estimado","ErrorStd","z","p_value","Signif")]

  # 4 decimales
  tb$Estimado <- round4(tb$Estimado)
  tb$ErrorStd <- round4(tb$ErrorStd)
  tb$z        <- round4(tb$z)
  tb$p_value  <- round4(tb$p_value)

  tb[order(tb$p_value), ]
}

# Resumen de p-valores por variable base (mínimo p entre términos)
summarize_p_by_var <- function(coef_tbl){
  agg <- aggregate(p_value ~ Variable, coef_tbl, function(v) min(v, na.rm = TRUE))
  names(agg) <- c("Variable","p_min")
  agg$p_min <- round4(agg$p_min)
  agg[order(agg$p_min, decreasing = FALSE), ]
}

# Entrena un GLM binomial (logístico) y devuelve tabla de coeficientes + probabilidades
run_logit <- function(X_final, y, term_map = NULL){
  stopifnot(length(y) == nrow(X_final))
  df <- as.data.frame(X_final)
  df$.y <- as.numeric(y)

  # NAs fuera para evitar cuelgues y convergencia
  df <- stats::na.omit(df)

  fit <- stats::glm(.y ~ ., data = df, family = stats::binomial(),
                    control = list(maxit = 50))
  s   <- summary(fit)
  coef_tbl <- .format_coef_table(s$coefficients, term_map = term_map[colnames(df)[colnames(df) != ".y"]])
  prob <- stats::predict(fit, type = "response")
  auc  <- .try_auc(df$.y, prob)

  list(model = fit, probs = prob, coefs = coef_tbl, auc = auc)
}

# Métricas para un umbral específico
evaluate_metrics <- function(probs, y, thr = 0.5){
  yhat <- as.integer(probs >= thr)
  y    <- as.integer(y)
  TP <- sum(yhat == 1 & y == 1)
  TN <- sum(yhat == 0 & y == 0)
  FP <- sum(yhat == 1 & y == 0)
  FN <- sum(yhat == 0 & y == 1)
  acc <- (TP + TN) / length(y)
  sens <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)  # Recall
  spec <- ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_)
  prec <- ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_)
  f1 <- ifelse((prec + sens) > 0, 2 * prec * sens / (prec + sens), NA_real_)

  list(
    thr = round4(thr),
    accuracy = round4(acc),
    sensibilidad = round4(sens),
    especificidad = round4(spec),
    precision = round4(prec),
    f1 = round4(f1),
    TP = TP, TN = TN, FP = FP, FN = FN
  )
}

# Curva de métricas vs umbral
evaluate_thresholds <- function(probs, y, thrs = seq(0.05, 0.95, by = 0.05)){
  y <- as.integer(y)
  out <- lapply(thrs, function(t) evaluate_metrics(probs, y, t))
  do.call(rbind, lapply(out, function(z) as.data.frame(z)))
}

# Score integrado: maximiza aceptación y minimiza mora
# Por defecto: score = p(aceptar) * (1 - p(mora))
compute_integrated_score <- function(p_accept, p_mora, w_accept = 1, w_no_mora = 1){
  s <- (p_accept ^ w_accept) * ((1 - p_mora) ^ w_no_mora)
  pmin(pmax(s, 0), 1)
}
