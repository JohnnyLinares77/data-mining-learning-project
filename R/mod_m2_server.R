# R/mod_m2_server.R
# -------------------------------------------------------------------
# Server del Módulo 2 – Scoring (Regresión Logística)
# Requiere: logit_helpers.R, persistencia_m2.R y (opcionalmente) preprocess_inputs() del Módulo 1
# Argumentos:
#   - datos_reactivos: reactive() que retorna una lista con data.frames base (demograficas, financieras, comp_historico, clientes, etc.)
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
    keys <- c("demograficas","financieras","comp_historico")
    tabs <- Filter(Negate(is.null), d[keys])
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)
  }

  # construye matriz de diseño (numéricas estandarizadas + dummies)
  make_design <- function(df, vars, id_col = "id_cliente"){
    stopifnot(id_col %in% names(df))
    keep <- intersect(vars, names(df))
    if (length(keep) == 0) stop("No hay variables disponibles en los datos con los nombres seleccionados.")
    X <- df[, keep, drop = FALSE]

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
    s_buro <- if ("score_buro" %in% names(df)) scale(df$score_buro) else 0
    ingreso <- if ("ingreso_verificado" %in% names(df)) scale(df$ingreso_verificado) else 0
    moras_prev <- if ("n_moras_previas" %in% names(df)) scale(df$n_moras_previas) else 0
    rfm <- if ("rfm" %in% names(df)) scale(df$rfm) else 0

    # prob aceptar ↑ con score e ingreso; ↓ con moras
    lin_acc <-  0.6 * s_buro + 0.4 * ingreso - 0.2 * moras_prev + 0.2 * rfm
    p_acc <- 1/(1 + exp(-lin_acc))
    # prob mora ↓ con score; ↑ con moras
    lin_mor <- -0.7 * s_buro + 0.6 * moras_prev - 0.2 * ingreso - 0.1 * rfm
    p_mor <- 1/(1 + exp(-lin_mor))

    acepta <- rbinom(nrow(df), 1, pmin(pmax(p_acc, 0.02), 0.98))
    mora   <- rbinom(nrow(df), 1, pmin(pmax(p_mor, 0.02), 0.98))
    list(acepta = acepta, mora = mora)
  }

  # ----------------------------
  # Estado reactivo
  # ----------------------------
  rv <- shiny::reactiveValues(
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

    # Etiquetas reales si existen, sino simuladas
    if (!all(c("acepta","mora") %in% names(base))) {
      labs <- simular_etiquetas(base)
      base$acepta <- labs$acepta
      base$mora   <- labs$mora
    }

    shiny::req(input$vars)
    X_final <- make_design(base, input$vars)
    rv$X_final <- X_final
    rv$id_cliente <- attr(X_final, "id_cliente")
    rv$y_accept <- base$acepta
    rv$y_mora   <- base$mora

    # Modelos logísticos
    acc <- run_logit(X_final, rv$y_accept)
    mor <- run_logit(X_final, rv$y_mora)

    rv$fit_accept <- acc$model
    rv$fit_mora   <- mor$model
    rv$probs_accept <- acc$probs
    rv$probs_mora   <- mor$probs
    rv$auc_accept   <- acc$auc
    rv$auc_mora     <- mor$auc

    # Score integrado y evaluación inicial
    rv$score <- compute_integrated_score(rv$probs_accept, rv$probs_mora)
    rv$thr_grid <- evaluate_thresholds(rv$score, 1 - rv$y_mora) # etiqueta "buena" = no mora
    rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, rv$thr_current)

    shiny::showNotification("Modelos entrenados y score integrado calculado.", type = "message")
    shiny::updateTabsetPanel(session, "tabs", selected = "Modelos")
  }, ignoreInit = TRUE)

  # ----------------------------
  # Salidas: tablas de coeficientes y AUC
  # ----------------------------
  output$tbl_coefs_accept <- DT::renderDT({
    shiny::req(rv$fit_accept)
    s <- summary(rv$fit_accept)
    DT::datatable(.format_coef_table(s$coefficients),
                  rownames = FALSE, options = list(pageLength = 8))
  })

  output$tbl_coefs_mora <- DT::renderDT({
    shiny::req(rv$fit_mora)
    s <- summary(rv$fit_mora)
    DT::datatable(.format_coef_table(s$coefficients),
                  rownames = FALSE, options = list(pageLength = 8))
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
    rv$thr_current <- input$thr
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
    DT::datatable(rv$thr_grid, options = list(pageLength = 10))
  })

  # ----------------------------
  # Resultados finales por cliente
  # ----------------------------
  observeEvent(list(rv$score, rv$thr_current), {
    shiny::req(rv$score, rv$id_cliente)
    decision <- as.integer(rv$score >= rv$thr_current)
    rv$df_scores <- data.frame(
      id_cliente = rv$id_cliente,
      p_accept = round(rv$probs_accept, 6),
      p_mora   = round(rv$probs_mora, 6),
      score    = round(rv$score, 6),
      decision = decision
    )
  })

  output$plot_scores <- shiny::renderPlot({
    shiny::req(rv$score)
    hist(rv$score, breaks = 30, main = "Distribución del score integrado", xlab = "Score")
    abline(v = rv$thr_current, col = 2, lty = 2)
  })

  output$tbl_scores <- DT::renderDT({
    shiny::req(rv$df_scores)
    DT::datatable(rv$df_scores, options = list(pageLength = 10))
  })

  # ----------------------------
  # Persistencia / Exportación
  # ----------------------------
  observeEvent(input$confirmar, {
    shiny::req(rv$metrics_current, rv$df_scores)
    # guardar métricas agregadas (sobre score integrado vs no-mora)
    m <- rv$metrics_current
    persist_eval_m2(
      id_sim = id_sim,
      auc_accept = rv$auc_accept, auc_mora = rv$auc_mora,
      thr = m$thr,
      accuracy = m$accuracy, sensibilidad = m$sensibilidad,
      especificidad = m$especificidad, precision = m$precision, f1 = m$f1
    )
    # guardar resultados por cliente
    persist_clientes_scores(
      id_sim = id_sim,
      df_clientes = rv$df_scores
    )
    shiny::showNotification("Resultados guardados en data/eval_m2.csv y data/clientes_scores.csv", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$reiniciar, {
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
    # Exportación simple a CSV (usuario descarga manual desde el directorio del proyecto)
    utils::write.csv(rv$df_scores, file.path("data","clientes_scores_export.csv"), row.names = FALSE)
    shiny::showNotification("Exportado data/clientes_scores_export.csv", type = "message")
  }, ignoreInit = TRUE)
}
