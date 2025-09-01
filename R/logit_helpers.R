# R/logit_helpers.R
# -------------------------------------------------------------------
# Helpers para Scoring con Regresión Logística
# - run_logit(): entrena GLM binomial y devuelve p-valores de coeficientes
# - evaluate_metrics(): calcula métricas para un umbral dado
# - evaluate_thresholds(): calcula métricas a lo largo de un grid de umbrales
# - compute_integrated_score(): combina p(aceptar) * (1 - p(mora))
# -------------------------------------------------------------------

# AUC
.try_auc <- function(y, prob){
  out <- NA_real_
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc <- pROC::roc(response = y, predictor = prob, quiet = TRUE)
    out <- as.numeric(pROC::auc(roc))
  }
  out
}

# Tabla de coeficientes con p-valor y "stars"
.format_coef_table <- function(coefs_mat){
  tb <- as.data.frame(coefs_mat)
  tb$Termino <- rownames(coefs_mat)
  rownames(tb) <- NULL
  names(tb) <- c("Estimado","ErrorStd","z","p_value","Termino")
  tb$Signif <- cut(
    tb$p_value,
    breaks = c(-Inf, .001, .01, .05, .1, Inf),
    labels = c("***","**","*",".","")
  )
  tb <- tb[, c("Termino","Estimado","ErrorStd","z","p_value","Signif")]
  tb[order(tb$p_value), ]
}

# Entrena un GLM binomial (logístico) y devuelve tabla de coeficientes + probabilidades
run_logit <- function(X_final, y){
  stopifnot(length(y) == nrow(X_final))
  df <- as.data.frame(X_final)
  df$.y <- as.numeric(y)
  fit <- stats::glm(.y ~ ., data = df, family = stats::binomial())
  s   <- summary(fit)
  coef_tbl <- .format_coef_table(s$coefficients)
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
  list(thr = thr, accuracy = acc, sensibilidad = sens, especificidad = spec,
       precision = prec, f1 = f1, TP = TP, TN = TN, FP = FP, FN = FN)
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
  # Forma multiplicativa con pesos como potencias (flexible)
  s <- (p_accept ^ w_accept) * ((1 - p_mora) ^ w_no_mora)
  # Asegura rango [0,1]
  pmin(pmax(s, 0), 1)
}
