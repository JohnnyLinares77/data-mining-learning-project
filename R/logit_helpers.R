# R/logit_helpers.R
# -------------------------------------------------------------------
# Helpers para Scoring con Regresión Logística
# - run_logit(): entrena GLM binomial y devuelve coefs + probabilidades + AUC
# - evaluate_metrics(): calcula métricas para un umbral dado
# - evaluate_thresholds(): calcula métricas a lo largo de un grid de umbrales
# - compute_integrated_score(): p(aceptar) * (1 - p(mora))  [Tesis §2.3.2]
# - compute_auc_bin(): AUC binario (respaldo si no hay pROC)
# Todas las salidas destinadas a reporte se redondean a 4 decimales.
# -------------------------------------------------------------------

# --- Utilitarios internos -------------------------------------------------------

# Redondeo seguro a 4 decimales (mantiene NAs)
.round4 <- function(x) {
  if (is.null(x)) return(x)
  if (is.list(x)) return(lapply(x, .round4))
  if (is.data.frame(x)) {
    num <- sapply(x, is.numeric)
    x[num] <- lapply(x[num], function(v) round(v, 4))
    return(x)
  }
  if (is.numeric(x)) return(round(x, 4))
  x
}

# AUC con pROC si existe; si no, usa respaldo (Mann–Whitney / Wilcoxon rank-sum)
.try_auc <- function(y, prob){
  y <- as.integer(y)
  prob <- as.numeric(prob)
  if (length(unique(y)) < 2L) return(NA_real_)
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc <- pROC::roc(response = y, predictor = prob, quiet = TRUE)
    return(as.numeric(pROC::auc(roc)))
  }
  # Respaldo: probabilidad de que un positivo tenga score > negativo (c-statistic)
  pos <- prob[y == 1]
  neg <- prob[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  ranks <- rank(c(pos, neg))
  n1 <- length(pos); n0 <- length(neg)
  r1 <- sum(ranks[seq_len(n1)])
  auc <- (r1 - n1*(n1 + 1)/2) / (n1*n0)
  as.numeric(auc)
}

# Tabla de coeficientes (acepta summary(fit)$coefficients)
# Devuelve columnas: Variable, Est., E.E., z, p-value, Signif (máx. 4 decimales)
.format_coef_table <- function(coefs_mat){
  stopifnot(is.matrix(coefs_mat) || is.data.frame(coefs_mat))
  tb <- as.data.frame(coefs_mat, stringsAsFactors = FALSE)
  # Soporta tanto "z value" como "t value" (glm binomial usa z)
  colnames(tb) <- c("Estimado","ErrorStd", "z", "p_value")[seq_len(ncol(tb))]
  tb$Variable <- rownames(coefs_mat)
  rownames(tb) <- NULL
  tb$Signif <- cut(
    tb$p_value,
    breaks = c(-Inf, .001, .01, .05, .1, Inf),
    labels = c("***","**","*",".","")
  )
  tb <- tb[, c("Variable","Estimado","ErrorStd","z","p_value","Signif")]
  names(tb) <- c("Variable","Est.","E.E.","z","p-value","Signif")
  .round4(tb)
}

# --- Modelo logístico -----------------------------------------------------------

# Entrena un GLM binomial (logístico) y devuelve lista: model, probs, coefs, auc
run_logit <- function(X_final, y){
  stopifnot(nrow(X_final) == length(y))
  df <- as.data.frame(X_final)
  df$.y <- as.numeric(y)
  fit <- stats::glm(.y ~ ., data = df, family = stats::binomial())
  s   <- summary(fit)
  coef_tbl <- .format_coef_table(s$coefficients)
  prob <- as.numeric(stats::predict(fit, type = "response"))
  auc  <- .try_auc(df$.y, prob)
  list(
    model = fit,
    probs = prob,
    coefs = coef_tbl,
    auc   = as.numeric(round(auc, 4))
  )
}

# --- Métricas y umbrales --------------------------------------------------------

# Métricas para un umbral específico (devuelve lista con valores a 4 decimales)
evaluate_metrics <- function(probs, y, thr = 0.5){
  y    <- as.integer(y)
  yhat <- as.integer(as.numeric(probs) >= thr)
  TP <- sum(yhat == 1 & y == 1, na.rm = TRUE)
  TN <- sum(yhat == 0 & y == 0, na.rm = TRUE)
  FP <- sum(yhat == 1 & y == 0, na.rm = TRUE)
  FN <- sum(yhat == 0 & y == 1, na.rm = TRUE)
  acc <- (TP + TN) / length(y)
  sens <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)  # Recall
  spec <- ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_)
  prec <- ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_)
  f1   <- ifelse((prec + sens) > 0, 2 * prec * sens / (prec + sens), NA_real_)
  .round4(list(
    thr = thr, accuracy = acc, sensibilidad = sens, especificidad = spec,
    precision = prec, f1 = f1, TP = TP, TN = TN, FP = FP, FN = FN
  ))
}

# Curva de métricas vs umbral (data.frame con 4 decimales)
evaluate_thresholds <- function(probs, y, thrs = seq(0.05, 0.95, by = 0.05)){
  y <- as.integer(y)
  out <- lapply(thrs, function(t) evaluate_metrics(probs, y, t))
  df  <- do.call(rbind, lapply(out, function(z) as.data.frame(z, stringsAsFactors = FALSE)))
  .round4(df)
}

# --- Score integrado ------------------------------------------------------------

# Score integrado (Tesis §2.3.2): por defecto p(aceptar) * (1 - p(mora))
# Pesos como potencias para flexibilizar la importancia relativa.
compute_integrated_score <- function(p_accept, p_mora, w_accept = 1, w_no_mora = 1){
  s <- (as.numeric(p_accept) ^ w_accept) * ((1 - as.numeric(p_mora)) ^ w_no_mora)
  pmin(pmax(s, 0), 1)
}

# --- AUC binario expuesto (usado en re-entrenos del server) ---------------------

# Versión pública para re-entrenamientos cuando no se usa pROC directamente.
compute_auc_bin <- function(probs, y){
  .try_auc(y, probs) |> round(4)
}
