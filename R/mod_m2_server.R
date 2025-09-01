# R/mod_m2_server.R
# -------------------------------------------------------------------
# Server del Módulo 2 – Scoring (Regresión Logística)
# Requiere: logit_helpers.R, persistencia_m2.R y (opcionalmente) preprocess_inputs() del Módulo 1
# Argumentos:
#   - datos_reactivos: reactive() que retorna una lista con data.frames base
#     (demograficas, financieras, comp_historico, clientes, etc.)
#   - id_sim: identificador de simulación (string o número)
# -------------------------------------------------------------------

mod_m2_server <- function(input, output, session, datos_reactivos, id_sim){
  ns <- session$ns

  # ----------------------------
  # Utilitarios internos
  # ----------------------------
  get_base_df <- function(d){
    # Si ya existe un data.frame unificado, úsalo; si no, intenta unir por id_cliente
    if (!is.null(d$base)) return(d$base)
    keys <- c("demograficas","financieras","comp_historico","clientes")  # <-- incluye clientes (cluster_id)
    tabs <- Filter(Negate(is.null), d[keys])
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    base <- Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)

    # Forzar cluster_id como factor si existe
    if ("cluster_id" %in% names(base)) base$cluster_id <- factor(base$cluster_id)
    base
  }

  # construye matriz de diseño (numéricas estandarizadas + dummies)
  make_design <- function(df, vars, id_col = "id_cliente"){
    stopifnot(id_col %in% names(df))
    keep <- intersect(vars, names(df))
    if (length(keep) == 0) stop("No hay variables disponibles en los datos con los nombres seleccionados.")

    X <- df[, keep, drop = FALSE]

    # Asegurar que cluster_id (si está) sea factor para generar dummies
    if ("cluster_id" %in% names(X)) X$cluster_id <- factor(X$cluster_id)

    # numéricas -> escalar; categóricas -> factor y one-hot
    is_num <- sapply(X, is.numeric)
    X_num <- X[, is_num, drop = FALSE]
    X_cat <- X[, !is_num, drop = FALSE]

    if (ncol(X_num) > 0) {
      X_num <- scale(X_num)
      X_num <- as.data.frame(X_num)
    }
    if (ncol(X_cat) > 0) {
      X_cat <- lapply(X_cat, function(z) as.factor(z))
      X_cat <- stats::model.matrix(~ . - 1, data = as.data.frame(X_cat))
      X_cat <- as.data.frame(X_cat)
    }

    X_final <- cbind(X_num, X_cat)
    attr(X_final, "id_cliente") <- df[[id_col]]
    X_final
  }

  # Si no existen etiquetas reales, simula aceptación y mora con una relación plausible
  simular_etiquetas <- function(df){
    # heurística simple usando variables frecuentes
    s_buro   <- if ("score_buro" %in% names(df)) scale(df$score_buro) else 0
    ingreso  <- if ("ingreso_verificado" %in% names(df)) scale(df$ingreso_verificado) else 0
    moras_p  <- if ("n_moras_previas" %in% names(df)) scale(df$n_moras_previas) else 0
    rfm_v    <- if ("rfm" %in% names(df)) scale(df$rfm) else 0

    # prob aceptar ↑ con score e ingreso; ↓ con moras
    lin_acc <-  0.6 * s_buro + 0.4 * ingreso - 0.2 * moras_p + 0.2 * rfm_v
    p_acc   <- 1/(1 + exp(-lin_acc))
    # prob mora ↓ con score; ↑ con moras
    lin_mor <- -0.7 * s_buro + 0.6 * moras_p - 0.2 * ingreso - 0.1 * rfm_v
    p_mor   <- 1/(1 + exp(-lin_mor))

    acepta <- rbinom(nrow(df), 1, pmin(pmax(p_acc, 0.02), 0.98))
    mora   <- rbinom(nrow(df), 1, pmin(pmax(p_mor, 0.02), 0.98))
    list(acepta = acepta, mora = mora)
  }

  # ----------------------------
  # Estado reactivo
  # ----------------------------
  rv <- shiny::reactiveValues(
    base = NULL,
    X_final = NULL, id_cliente = NULL,
    y_accept = NULL, y_mora = NULL,
    fit_accept = NULL, fit_mora = NULL,
    probs_accept = NULL, probs_mora = NULL,
    auc_accept = NA_real_, auc_mora = NA_real_,
    score = NULL, thr_grid = NULL,
    thr_current = 0.5, metrics_current = NULL,
    df_scores = NULL
  )

  # ----------------------------
  # Entrenamiento de modelos
  # ----------------------------
  observeEvent(input$train_models, {
    d <- datos_reactivos()
    shiny::req(d)
    base <- get_base_df(d)
    rv$base <- base

    # Etiquetas reales si existen, sino simuladas
    if (!all(c("acepta","mora") %in% names(base))) {
      labs <- simular_etiquetas(base)
      base$acepta <- labs$acepta
      base$mora   <- labs$mora
    }

    shiny::req(input$vars)
    X_final <- make_design(base, input$vars)
    rv$X_final    <- X_final
    rv$id_cliente <- attr(X_final, "id_cliente")
    rv$y_accept   <- base$acepta
    rv$y_mora     <- base$mora

    # Modelos logísticos (helpers deben devolver lista con model, probs, auc)
    acc <- run_logit(X_final, rv$y_accept)
    mor <- run_logit(X_final, rv$y_mora)

    rv$fit_accept   <- acc$model
    rv$fit_mora     <- mor$model
    rv$probs_accept <- acc$probs
    rv$probs_mora   <- mor$probs
    rv$auc_accept   <- acc$auc
    rv$auc_mora     <- mor$auc

    # Score integrado y evaluación inicial (etiqueta "buena" = no mora)
    rv$score          <- compute_integrated_score(rv$probs_accept, rv$probs_mora)
    rv$thr_grid       <- evaluate_thresholds(rv$score, 1 - rv$y_mora)
    rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, rv$thr_current)

    shiny::showNotification("Modelos entrenados y score integrado calculado.", type = "message")
    shiny::updateTabsetPanel(session, "tabs", selected = "Modelos")
  }, ignoreInit = TRUE)

  # ----------------------------
  # Selección interactiva por p-valor y re-entrenamiento
  # (Requiere inputs en UI: alpha_accept, alpha_mora, keep_vars_accept, keep_vars_mora,
  #  retrain_accept, retrain_mora)
  # ----------------------------
  observe({
    shiny::req(rv$fit_accept)
    tb <- broom::tidy(rv$fit_accept)
    vars <- tb$term[!tb$term %in% "(Intercept)"]
    pval <- tb$p.value[match(vars, tb$term)]
    sel  <- vars[pval <= (input$alpha_accept %||% 0.05)]
    shiny::updateCheckboxGroupInput(session, "keep_vars_accept",
      choices = vars, selected = sel)
  })

  observe({
    shiny::req(rv$fit_mora)
    tb <- broom::tidy(rv$fit_mora)
    vars <- tb$term[!tb$term %in% "(Intercept)"]
    pval <- tb$p.value[match(vars, tb$term)]
    sel  <- vars[pval <= (input$alpha_mora %||% 0.05)]
    shiny::updateCheckboxGroupInput(session, "keep_vars_mora",
      choices = vars, selected = sel)
  })

  # Re-entrenar ACEPTACIÓN con selección del alumno
  observeEvent(input$retrain_accept, {
    shiny::req(rv$base, input$keep_vars_accept)
    # Los términos de coeficientes son nombres de columnas de la matriz de diseño (dummies incluidas).
    # Para regenerar X con solo los términos elegidos, detectamos la base de nombres originales:
    # Como simplificación educativa, permitimos re-entrenar usando directamente las columnas seleccionadas
    # desde la matriz ya creada si existen:
    if (!is.null(rv$X_final) && all(input$keep_vars_accept %in% colnames(rv$X_final))) {
      X_new <- as.matrix(rv$X_final[, unique(input$keep_vars_accept), drop = FALSE])
      attr(X_new, "id_cliente") <- rv$id_cliente
    } else {
      # Si no calzan, volvemos a construir desde variables originales seleccionadas en UI
      X_new <- make_design(rv$base, input$vars)
    }
    fit <- glm(rv$y_accept ~ X_new, family = binomial())
    # Actualiza probabilidades y AUC
    probs <- as.numeric(stats::predict(fit, type = "response"))
    aucv  <- compute_auc_bin(probs, rv$y_accept)  # helper simple si no usas pROC directamente

    rv$fit_accept   <- fit
    rv$probs_accept <- probs
    rv$auc_accept   <- aucv

    # Recalcular score/metricas
    shiny::req(rv$probs_mora)
    rv$score           <- compute_integrated_score(rv$probs_accept, rv$probs_mora)
    rv$thr_grid        <- evaluate_thresholds(rv$score, 1 - rv$y_mora)
    rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, rv$thr_current)
  }, ignoreInit = TRUE)

  # Re-entrenar MORA con selección del alumno
  observeEvent(input$retrain_mora, {
    shiny::req(rv$base, input$keep_vars_mora)
    if (!is.null(rv$X_final) && all(input$keep_vars_mora %in% colnames(rv$X_final))) {
      X_new <- as.matrix(rv$X_final[, unique(input$keep_vars_mora), drop = FALSE])
      attr(X_new, "id_cliente") <- rv$id_cliente
    } else {
      X_new <- make_design(rv$base, input$vars)
    }
    fit <- glm(rv$y_mora ~ X_new, family = binomial())
    probs <- as.numeric(stats::predict(fit, type = "response"))
    aucv  <- compute_auc_bin(probs, rv$y_mora)

    rv$fit_mora   <- fit
    rv$probs_mora <- probs
    rv$auc_mora   <- aucv

    # Recalcular score/metricas
    shiny::req(rv$probs_accept)
    rv$score           <- compute_integrated_score(rv$probs_accept, rv$probs_mora)
    rv$thr_grid        <- evaluate_thresholds(rv$score, 1 - rv$y_mora)
    rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, rv$thr_current)
  }, ignoreInit = TRUE)

  # ----------------------------
  # Salidas: tablas de coeficientes y AUC (4 decimales)
  # ----------------------------
  output$tbl_coefs_accept <- DT::renderDT({
    shiny::req(rv$fit_accept)
    s <- summary(rv$fit_accept)
    DT::datatable(.format_coef_table(s$coefficients),
                  rownames = FALSE, options = list(pageLength = 8)) |>
      DT::formatRound(c("Est.","E.E.","z","p-value"), 4)
  })

  output$tbl_coefs_mora <- DT::renderDT({
    shiny::req(rv$fit_mora)
    s <- summary(rv$fit_mora)
    DT::datatable(.format_coef_table(s$coefficients),
                  rownames = FALSE, options = list(pageLength = 8)) |>
      DT::formatRound(c("Est.","E.E.","z","p-value"), 4)
  })

  output$auc_accept <- shiny::renderText({ sprintf("%.4f", rv$auc_accept) })
  output$auc_mora   <- shiny::renderText({ sprintf("%.4f", rv$auc_mora) })

  # ----------------------------
  # Evaluación de umbrales
  # ----------------------------
  observeEvent(input$eval_thresholds, {
    shiny::req(rv$score, rv$y_mora)
    rv$thr_grid <- evaluate_thresholds(rv$score, 1 - rv$y_mora)
    shiny::showNotification("Evaluación de umbrales completada.", type = "message")
    shiny::updateTabsetPanel(session, "tabs", selected = "Umbral")
  }, ignoreInit = TRUE)

  observeEvent(input$apply_thr, {
    shiny::req(rv$score, rv$y_mora)
    rv$thr_current    <- input$thr
    rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, input$thr)
  }, ignoreInit = TRUE)

  output$plot_metrics <- shiny::renderPlot({
    shiny::req(rv$thr_grid)
    par(mar = c(4,4,1,1))
    df <- rv$thr_grid
    plot(df$thr, df$accuracy, type = "l", ylab = "Métrica", xlab = "Umbral",
         ylim = range(df[,c("accuracy","sensibilidad","especificidad","f1")], na.rm = TRUE))
    lines(df$thr, df$sensibilidad)
    lines(df$thr, df$especificidad)
    lines(df$thr, df$f1)
    abline(v = rv$thr_current, lty = 2)
    legend("bottomleft", bty = "n",
           legend = c("Accuracy","Sensibilidad","Especificidad","F1","Umbral actual"),
           lty = c(1,1,1,1,2))
  })

  output$tbl_thr_metrics <- DT::renderDT({
    shiny::req(rv$thr_grid)
    # Redondeo a 4 decimales para consistencia
    df <- rv$thr_grid
    num_cols <- names(df)[sapply(df, is.numeric)]
    df[num_cols] <- lapply(df[num_cols], function(x) round(x, 4))
    DT::datatable(df, options = list(pageLength = 10)) |>
      DT::formatRound(num_cols, 4)
  })

  # ----------------------------
  # Resultados finales por cliente (4 decimales)
  # ----------------------------
  observeEvent(list(rv$score, rv$thr_current, rv$probs_accept, rv$probs_mora), {
    shiny::req(rv$score, rv$id_cliente, rv$probs_accept, rv$probs_mora)
    decision <- as.integer(rv$score >= rv$thr_current)
    rv$df_scores <- data.frame(
      id_cliente = rv$id_cliente,
      p_accept   = round(rv$probs_accept, 4),
      p_mora     = round(rv$probs_mora, 4),
      score      = round(rv$score, 4),
      decision   = decision
    )
  })

  output$plot_scores <- shiny::renderPlot({
    shiny::req(rv$score)
    hist(rv$score, breaks = 30, main = "Distribución del score integrado", xlab = "Score")
    abline(v = rv$thr_current, col = 2, lty = 2)
  })

  output$tbl_scores <- DT::renderDT({
    shiny::req(rv$df_scores)
    DT::datatable(rv$df_scores, options = list(pageLength = 10)) |>
      DT::formatRound(c("p_accept","p_mora","score"), 4)
  })

  # ----------------------------
  # Persistencia / Exportación
  # ----------------------------
  observeEvent(input$confirmar, {
    shiny::req(rv$metrics_current, rv$df_scores)
    m <- rv$metrics_current
    persist_eval_m2(
      id_sim = id_sim,
      auc_accept   = rv$auc_accept, auc_mora = rv$auc_mora,
      thr          = m$thr,
      accuracy     = m$accuracy, sensibilidad = m$sensibilidad,
      especificidad = m$especificidad, precision = m$precision, f1 = m$f1
    )
    persist_clientes_scores(
      id_sim = id_sim,
      df_clientes = rv$df_scores
    )
    shiny::showNotification("Resultados guardados en data/eval_m2.csv y data/clientes_scores.csv", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$reiniciar, {
    rv$base <- NULL
    rv$X_final <- NULL; rv$id_cliente <- NULL
    rv$y_accept <- NULL; rv$y_mora <- NULL
    rv$fit_accept <- NULL; rv$fit_mora <- NULL
    rv$probs_accept <- NULL; rv$probs_mora <- NULL
    rv$auc_accept <- NA_real_; rv$auc_mora <- NA_real_
    rv$score <- NULL; rv$thr_grid <- NULL
    rv$thr_current <- 0.5; rv$metrics_current <- NULL
    rv$df_scores <- NULL
    shiny::showNotification("Módulo 2 reiniciado.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$exportar, {
    shiny::req(rv$df_scores)
    utils::write.csv(rv$df_scores, file.path("data","clientes_scores_export.csv"), row.names = FALSE)
    shiny::showNotification("Exportado data/clientes_scores_export.csv", type = "message")
  }, ignoreInit = TRUE)
}
