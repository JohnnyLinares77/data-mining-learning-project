# R/mod_m2_server.R
# -------------------------------------------------------------------
# Server del Módulo 2 – Scoring (Regresión Logística)
# -------------------------------------------------------------------

mod_m2_server <- function(input, output, session, datos_reactivos, id_sim, execution_mode = reactive("sequential")){
  ns <- session$ns

  # ----------------------------
  # Utilitarios internos (idénticos a tu versión)
  # ----------------------------
  get_base_df <- function(d){
    if (!is.null(d$base)) return(d$base)
    keys <- c("demograficas","financieras","comp_historico","clientes")
    tabs <- Filter(Negate(is.null), d[keys])
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)
  }

  make_design <- function(df, vars, id_col = "id_cliente"){
    stopifnot(id_col %in% names(df))
    keep <- intersect(vars, names(df))
    if (length(keep) == 0) stop("No hay variables disponibles en los datos con los nombres seleccionados.")
    X <- df[, keep, drop = FALSE]

    is_num <- sapply(X, is.numeric)
    X_num <- X[, is_num, drop = FALSE]
    X_cat <- X[, !is_num, drop = FALSE]

    if (ncol(X_num) > 0) {
      const <- vapply(X_num, function(z) length(unique(z[!is.na(z)])) <= 1, logical(1))
      if (any(const)) X_num <- X_num[, !const, drop = FALSE]
      if (ncol(X_num) > 0) {
        X_num <- scale(X_num)
        X_num <- as.data.frame(X_num)
      }
    }
    if (ncol(X_cat) > 0) {
      X_cat <- lapply(X_cat, function(z) as.factor(z))
      M <- stats::model.matrix(~ . - 1, data = as.data.frame(X_cat))
      X_cat <- as.data.frame(M)
    }

    X_final <- cbind(X_num, X_cat)

    term_map <- setNames(rep(colnames(X_num), times = 1), colnames(X_num))
    if (ncol(X_cat) > 0) {
      for (v in names(df[, keep, drop = FALSE])) {
        if (!is.numeric(df[[v]])) {
          ind <- grep(paste0("^", v), colnames(X_cat))
          if (length(ind) > 0) {
            term_map <- c(term_map, setNames(rep(v, length(ind)), colnames(X_cat)[ind]))
          }
        }
      }
    }

    attr(X_final, "id_cliente") <- df[[id_col]]
    attr(X_final, "term_map")   <- term_map
    X_final
  }

  simular_etiquetas <- function(df){
    s_buro <- if ("score_buro" %in% names(df)) scale(df$score_buro) else 0
    ingreso <- if ("ingreso_verificado" %in% names(df)) scale(df$ingreso_verificado) else 0
    moras_prev <- if ("n_moras_previas" %in% names(df)) scale(df$n_moras_previas) else 0
    rfm <- if ("rfm" %in% names(df)) scale(df$rfm) else 0

    lin_acc <-  0.6 * s_buro + 0.4 * ingreso - 0.2 * moras_prev + 0.2 * rfm
    p_acc <- 1/(1 + exp(-lin_acc))
    lin_mor <- -0.7 * s_buro + 0.6 * moras_prev - 0.2 * ingreso - 0.1 * rfm
    p_mor <- 1/(1 + exp(-lin_mor))

    acepta <- rbinom(nrow(df), 1, pmin(pmax(p_acc, 0.02), 0.98))
    mora   <- rbinom(nrow(df), 1, pmin(pmax(p_mor, 0.02), 0.98))
    list(acepta = acepta, mora = mora)
  }

  .signos_por_variable <- function(model, term_map){
    s <- summary(model)
    tb <- .format_coef_table(s$coefficients, term_map = term_map)
    tb <- tb[tb$Variable != "(Intercept)", , drop = FALSE]
    if (!nrow(tb)) return(setNames(character(0), character(0)))
    tb$idx_min_p <- ave(tb$p_value, tb$Variable, FUN = function(x) as.integer(rank(x, ties.method = "first") == 1))
    tb_min <- tb[tb$idx_min_p == 1, c("Variable","Estimado")]
    setNames(ifelse(tb_min$Estimado >= 0, "+", "−"), tb_min$Variable)
  }

  # ----------------------------
  # Estado reactivo
  # ----------------------------
  rv <- shiny::reactiveValues(
    base = NULL,
    X_accept = NULL, X_mora = NULL,
    id_cliente = NULL,
    y_accept = NULL, y_mora = NULL,
    fit_accept = NULL, fit_mora = NULL,
    probs_accept = NULL, probs_mora = NULL,
    auc_accept = NA_real_, auc_mora = NA_real_,
    score = NULL, thr_grid = NULL,
    thr_current = 0.5, metrics_current = NULL,
    df_scores = NULL,
    var_rank_accept = NULL, var_rank_mora = NULL,
    vars_accept_keep = NULL, vars_mora_keep = NULL,
    term_map_accept = NULL, term_map_mora = NULL,
    # Interpretación
    interp_ok = FALSE,
    interp_target = NULL      # <<< NUEVO: variable asignada aleatoriamente
  )

  # ----------------------------
  # Actualizar variables disponibles dinámicamente
  # ----------------------------
  observe({
    # Verificar si hay datos de clusters filtrados
    cluster_file <- file.path("data", "clientes_clusters.csv")
    if (file.exists(cluster_file)) {
      clusters <- read.csv(cluster_file, stringsAsFactors = FALSE)
      if (!is.null(clusters$cluster_id) && length(unique(clusters$cluster_id)) == 1) {
        # Todos tienen el mismo cluster, remover cluster_id de las opciones disponibles
        vars_originales <- c(
          "edad","estado_civil","ubicacion","nivel_educativo","tipo_ocupacion","rubro_laboral","n_dependientes",
          "antiguedad_cliente","n_moras_previas","dias_atraso_max","n_moras_leves",
          "ingreso_declarado","ingreso_verificado","cuota_ingreso",
          "capacidad_endeudamiento","endeudamiento_total","rfm","score_buro","tendencia_ingresos",
          "cluster_id"
        )
        vars_filtradas <- setdiff(vars_originales, "cluster_id")

        # Actualizar las choices del checkbox
        shiny::updateCheckboxGroupInput(session, "vars",
                                      choices = vars_filtradas,
                                      selected = intersect(vars_filtradas, c("edad","ingreso_verificado","rfm","score_buro")))
      }
    }
  })

  # ----------------------------
  # Entrenamiento inicial
  # ----------------------------
  observeEvent(input$train_models, {
    shiny::req(datos_reactivos)

    # En modo independiente, generar datos simulados
    if (execution_mode() == "independent") {
      d <- generate_independent_data_m1(n_clientes = 1000, seed = 123)
      # Simular clusters previos
      simulated_clusters <- generate_simulated_clusters(n_clientes = 1000, seed = 456)
      session$userData$clusters <- simulated_clusters
      shiny::showNotification("Modo independiente: Usando datos simulados con clusters generados", type = "info", duration = 3)
    } else {
      d <- datos_reactivos()
      shiny::req(d)

      # VALIDACIÓN: Verificar que hay datos de M1 (clusters)
      if (is.null(session$userData$clusters) || nrow(session$userData$clusters) == 0) {
        shiny::showNotification(
          "No hay datos de clusters del Módulo 1. Complete el Módulo 1 primero.",
          type = "warning"
        )
      }
    }

    shiny::withProgress(message = "Entrenando modelos...", value = 0, {
      incProgress(0.1)

      base <- get_base_df(d)

      # FILTRADO DE EMBUDO: Usar datos filtrados de M1 desde session$userData$clusters
      if (!is.null(session$userData$clusters) && nrow(session$userData$clusters) > 0) {
        n_original <- nrow(base)
        base <- base[base$id_cliente %in% session$userData$clusters$id_cliente, ]
        n_filtrados <- nrow(base)

        if (n_filtrados > 0) {
          shiny::showNotification(
            sprintf("Filtrado por cluster M1: %d/%d clientes retenidos (%.1f%%)",
                    n_filtrados, n_original, 100 * n_filtrados / n_original),
            type = "message", duration = 5
          )

          # Agregar cluster_id para consistencia
          base <- merge(base, session$userData$clusters[, c("id_cliente", "cluster_id")], by = "id_cliente", all.x = TRUE)

          # Si todos los clientes tienen el mismo cluster_id (filtrado automático), removerlo de variables disponibles
          if (length(unique(base$cluster_id)) == 1) {
            shiny::showNotification(
              sprintf("Cluster %d seleccionado en M1. Todos los clientes son del mismo cluster - cluster_id removido de variables disponibles.",
                      unique(base$cluster_id)),
              type = "message", duration = 5
            )
          }
        } else {
          shiny::showNotification("No hay clientes coincidentes con los clusters de M1.", type = "error")
          return(NULL)
        }
      } else {
        shiny::showNotification(
          "No hay datos de clusters del Módulo 1. Complete el Módulo 1 primero.",
          type = "warning"
        )
        return(NULL)
      }

      rv$base <- base

      if (!all(c("acepta","mora") %in% names(base))) {
        labs <- simular_etiquetas(base)
        base$acepta <- labs$acepta
        base$mora   <- labs$mora
      }

      # Filtrar variables disponibles: excluir cluster_id si todos tienen el mismo valor
      vars_disponibles <- input$vars
      if (!is.null(base$cluster_id) && length(unique(base$cluster_id)) == 1) {
        vars_disponibles <- setdiff(vars_disponibles, "cluster_id")
        if (length(vars_disponibles) == 0) {
          shiny::showNotification("No hay variables disponibles después de filtrar cluster_id", type = "error")
          return(NULL)
        }
      }

      shiny::validate(
        shiny::need(length(vars_disponibles) > 0, "Selecciona al menos una variable.")
      )

      Xacc <- make_design(base, vars_disponibles)
      Xm   <- make_design(base, vars_disponibles)

      rv$id_cliente <- attr(Xacc, "id_cliente")
      rv$X_accept   <- Xacc
      rv$X_mora     <- Xm
      rv$y_accept   <- base$acepta
      rv$y_mora     <- base$mora
      rv$term_map_accept <- attr(Xacc, "term_map")
      rv$term_map_mora   <- attr(Xm, "term_map")

      incProgress(0.2)

      acc <- try(run_logit(Xacc, rv$y_accept, term_map = rv$term_map_accept), silent = TRUE)
      mor <- try(run_logit(Xm,   rv$y_mora,   term_map = rv$term_map_mora),   silent = TRUE)

      if (inherits(acc, "try-error") || inherits(mor, "try-error")) {
        shiny::showNotification("Ocurrió un error entrenando GLM. Revisa NAs/colinealidad en las variables.", type = "error")
        return(NULL)
      }

      rv$fit_accept   <- acc$model
      rv$fit_mora     <- mor$model
      rv$probs_accept <- acc$probs
      rv$probs_mora   <- mor$probs
      rv$auc_accept   <- acc$auc
      rv$auc_mora     <- mor$auc

      rv$var_rank_accept <- summarize_p_by_var(acc$coefs)
      rv$var_rank_mora   <- summarize_p_by_var(mor$coefs)

      alpha <- if (is.null(input$alpha)) 0.05 else input$alpha
      rv$vars_accept_keep <- rv$var_rank_accept$Variable[rv$var_rank_accept$p_min <= alpha]
      rv$vars_mora_keep   <- rv$var_rank_mora$Variable[rv$var_rank_mora$p_min <= alpha]

      rv$score <- compute_integrated_score(rv$probs_accept, rv$probs_mora)
      rv$thr_grid <- evaluate_thresholds(rv$score, 1 - rv$y_mora)
      rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, rv$thr_current)

      incProgress(0.7)
      shiny::showNotification("Modelos entrenados y score integrado calculado.", type = "message")
      shiny::updateTabsetPanel(session, "tabs", selected = "Modelos")

      # ===== Poblar opciones de la pestaña Interpretación (SOLO ACEPTACIÓN) =====
      vars_acc <- sort(unique(rv$var_rank_accept$Variable))
      vars_acc <- vars_acc[vars_acc != "(Intercept)"]

      shiny::updateCheckboxGroupInput(session, "interp_sig_vars",
                                      choices = vars_acc, selected = character(0))
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Asignación aleatoria de variable para interpretación (se fija por sesión)
  # ----------------------------
  observeEvent(list(rv$fit_accept, rv$var_rank_accept), {
    shiny::req(rv$fit_accept, rv$var_rank_accept)

    candidatas <- sort(unique(rv$var_rank_accept$Variable))
    candidatas <- candidatas[candidatas != "(Intercept)"]
    shiny::validate(shiny::need(length(candidatas) > 0, "No hay variables disponibles para asignar."))

    if (is.null(rv$interp_target) || !(rv$interp_target %in% candidatas)) {
      set.seed(as.integer(as.numeric(Sys.time())) %% .Machine$integer.max)
      rv$interp_target <- sample(candidatas, 1)
    }

    output$interp_var_target <- shiny::renderUI({
      shiny::tagList(
        shiny::p(shiny::strong(rv$interp_target)),
      )
    })

    shiny::updateTextAreaInput(
      session, inputId = ns("interp_text"),
      placeholder = sprintf("Redacta tu interpretación centrada en: '%s'…", rv$interp_target)
    )
  })

  # ----------------------------
  # Re-entrenar con selección interactiva (igual que antes)
  # ----------------------------
  observeEvent(input$retrain_selected, {
    shiny::req(rv$base, rv$y_accept, rv$y_mora)

    shiny::withProgress(message = "Reentrenando con selección...", value = 0, {
      incProgress(0.1)

      cand <- intersect(input$vars, names(rv$base))

      keep_acc <- unique(c(
        intersect(input$keep_vars_accept, cand),
        intersect(input$force_keep_accept, cand)
      ))
      if (length(keep_acc) == 0) keep_acc <- cand

      Xacc <- make_design(rv$base, keep_acc)
      rv$X_accept <- Xacc
      rv$term_map_accept <- attr(Xacc, "term_map")

      keep_mor <- unique(c(
        intersect(input$keep_vars_mora, cand),
        intersect(input$force_keep_mora, cand)
      ))
      if (length(keep_mor) == 0) keep_mor <- cand

      Xm <- make_design(rv$base, keep_mor)
      rv$X_mora <- Xm
      rv$term_map_mora <- attr(Xm, "term_map")

      incProgress(0.3)

      acc <- try(run_logit(Xacc, rv$y_accept, term_map = rv$term_map_accept), silent = TRUE)
      mor <- try(run_logit(Xm,   rv$y_mora,   term_map = rv$term_map_mora),   silent = TRUE)
      if (inherits(acc, "try-error") || inherits(mor, "try-error")) {
        shiny::showNotification("Error al reentrenar. Revisa variables seleccionadas.", type = "error")
        return(NULL)
      }

      rv$fit_accept   <- acc$model
      rv$fit_mora     <- mor$model
      rv$probs_accept <- acc$probs
      rv$probs_mora   <- mor$probs
      rv$auc_accept   <- acc$auc
      rv$auc_mora     <- mor$auc

      rv$var_rank_accept <- summarize_p_by_var(acc$coefs)
      rv$var_rank_mora   <- summarize_p_by_var(mor$coefs)

      rv$score <- compute_integrated_score(rv$probs_accept, rv$probs_mora)
      rv$thr_grid <- evaluate_thresholds(rv$score, 1 - rv$y_mora)
      rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, rv$thr_current)

      shiny::showNotification("Selección aplicada y modelos reentrenados.", type = "message")
      shiny::updateTabsetPanel(session, "tabs", selected = "Modelos")
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Tablas y AUC
  # ----------------------------
  output$tbl_coefs_accept_an  <- output$tbl_coefs_accept_sel <- DT::renderDT({
    req(rv$fit_accept)
    s  <- summary(rv$fit_accept)
    tb <- .format_coef_table(s$coefficients, term_map = rv$term_map_accept)
    DT::datatable(tb, rownames = FALSE, options = list(pageLength = 8)) |>
      DT::formatRound(c("Estimado","ErrorStd","z","p_value"), 4)
  })

  output$tbl_coefs_mora <- DT::renderDT({
    shiny::req(rv$fit_mora)
    s <- summary(rv$fit_mora)
    tb <- .format_coef_table(s$coefficients, term_map = rv$term_map_mora)
    DT::datatable(tb, rownames = FALSE, options = list(pageLength = 8)) |>
      DT::formatRound(c("Estimado","ErrorStd","z","p_value"), 4)
  })

  output$auc_accept <- shiny::renderText({ sprintf("%.4f", rv$auc_accept) })
  output$auc_mora   <- shiny::renderText({ sprintf("%.4f", rv$auc_mora) })

  # Listas por p-valor (igual)
  observe({
    shiny::req(rv$var_rank_accept, rv$var_rank_mora)
    alpha <- if (is.null(input$alpha)) 0.05 else input$alpha

    choices_acc <- rv$var_rank_accept$Variable
    sel_acc     <- rv$var_rank_accept$Variable[rv$var_rank_accept$p_min <= alpha]
    shiny::updateCheckboxGroupInput(session, "keep_vars_accept", choices = choices_acc, selected = sel_acc)

    choices_mor <- rv$var_rank_mora$Variable
    sel_mor     <- rv$var_rank_mora$Variable[rv$var_rank_mora$p_min <= alpha]
    shiny::updateCheckboxGroupInput(session, "keep_vars_mora", choices = choices_mor, selected = sel_mor)
  })

  # ============================
  # HELPER: interpretación automática/correcta para la variable asignada
  # ============================
  .build_interpretacion_var <- function(model, term_map, var, alpha = 0.05){
    s  <- summary(model)
    tb <- .format_coef_table(s$coefficients, term_map = term_map)
    tb <- tb[tb$Variable != "(Intercept)", , drop = FALSE]

    sub <- tb[tb$Variable == var, , drop = FALSE]
    if (!nrow(sub)) {
      return(list(texto = sprintf("No se encontró información de coeficientes para '%s'.", var),
                  ok = FALSE))
    }

    # Tomamos el término más representativo (menor p-valor)
    i    <- which.min(sub$p_value)
    fila <- sub[i, ]

    beta <- as.numeric(fila$Estimado)
    pv   <- as.numeric(fila$p_value)
    or   <- exp(beta)
    dir  <- if (beta >= 0) "aumenta" else "disminuye"
    sig  <- if (is.finite(pv) && pv < alpha) "es significativa" else "no es significativa"

    # Categórica si hay >1 término o el término != nombre de la Variable
    es_cat <- nrow(sub) > 1 || (!identical(fila$Termino, var) && grepl(paste0("^", var), fila$Termino))

    if (es_cat) {
      texto <- sprintf(
        "Para '%s' (categórica), el término '%s' tiene coeficiente %.4f (p = %.4f) y %s: respecto a la categoría base, %s las odds de aceptación en %.1f%% (OR = %.3f).",
        var, fila$Termino, beta, pv, sig, dir, (or - 1)*100, or
      )
    } else {
      texto <- sprintf(
        "Para '%s' (numérica estandarizada), el coeficiente es %.4f (p = %.4f) y %s: un incremento de 1 desviación estándar en '%s' %s las odds de aceptación en %.1f%% (OR = %.3f).",
        var, beta, pv, sig, var, dir, (or - 1)*100, or
      )
    }

    list(texto = texto, ok = is.finite(pv))
  }

  # ----------------------------
  # Pestaña INTERPRETACIÓN: validación (SIN puntaje, SIN regla de negocio, SIN salto de pestaña)
  # ----------------------------
  observeEvent(input$interp_enviar, {
    shiny::req(rv$fit_accept, rv$var_rank_accept, rv$interp_target)

    alpha <- if (is.null(input$alpha)) 0.05 else input$alpha

    # (A) Significancia (feedback general)
    clave_sig <- sort(rv$var_rank_accept$Variable[rv$var_rank_accept$p_min < alpha])
    clave_sig <- clave_sig[clave_sig != "(Intercept)"]

    marcadas <- input$interp_sig_vars
    if (is.null(marcadas)) marcadas <- character(0)
    marcadas <- sort(unique(marcadas))
    ok_sig <- identical(marcadas, clave_sig)

    # (B) Interpretación escrita SOLO para la variable asignada
    v_target <- rv$interp_target
    texto <- trimws(input$interp_text %||% "")
    texto_ok <- (nchar(texto) >= 40)  # solo validamos longitud mínima

    # (C) Interpretación AUTOMÁTICA (correcta) para comparación
    auto <- .build_interpretacion_var(
      model    = rv$fit_accept,
      term_map = rv$term_map_accept,
      var      = v_target,
      alpha    = alpha
    )
    ref_text <- auto$texto   # <<-- La "Interpretación de referencia" ahora es la correcta

    output$interp_feedback <- shiny::renderUI({
      shiny::tagList(
        shiny::h5("Reporte de aciertos/errores"),
        shiny::p(if (ok_sig) "✓ Significancia correcta" else "✗ Revisa p < α"),
        shiny::p(if (texto_ok) sprintf("✓ Interpretación de '%s' recibida", v_target)
                 else sprintf("✗ Amplía tu interpretación de '%s' (mín. 40 caracteres)", v_target)),
        shiny::hr(),
        shiny::strong("Interpretación de referencia (correcta)"),
        shiny::p(ref_text),
        shiny::hr(),
        shiny::strong("Variables con p < α"),
        shiny::p(if (length(clave_sig)) paste(clave_sig, collapse = ", ") else "ninguna")
      )
    })
    # Sin habilitar ni saltar a la pestaña "Selección".
  })

  # ----------------------------
  # Umbrales, resultados y persistencia (idéntico a tu versión)
  # ----------------------------
  observeEvent(input$apply_thr, {
    shiny::req(rv$score, rv$y_mora)

    # 1) Recalcula la malla de métricas para todos los umbrales
    rv$thr_grid <- evaluate_thresholds(rv$score, 1 - rv$y_mora)

    # 2) Aplica el umbral elegido en el slider
    rv$thr_current <- input$thr

    # 3) Calcula las métricas en el umbral seleccionado
    rv$metrics_current <- evaluate_metrics(rv$score, 1 - rv$y_mora, input$thr)

    # (opcional) feedback UI
    shiny::showNotification("Umbral evaluado y aplicado.", type = "message")
    shiny::updateTabsetPanel(session, "tabs", selected = "Umbral")
  }, ignoreInit = TRUE)

  output$plot_metrics <- shiny::renderPlot({
    shiny::req(rv$thr_grid)
    par(mar = c(4,4,1,1))
    df <- rv$thr_grid

    # Colores para cada curva
    col_acc  <- "blue"
    col_sens <- "red"
    col_esp  <- "green"
    col_f1   <- "purple"

    plot(df$thr, df$accuracy, type = "l", ylab = "Métrica", xlab = "Umbral",
        ylim = range(df[,c("accuracy","sensibilidad","especificidad","f1")], na.rm = TRUE),
        col = col_acc, lwd = 2)
    lines(df$thr, df$sensibilidad, col = col_sens, lwd = 2)
    lines(df$thr, df$especificidad, col = col_esp, lwd = 2)
    lines(df$thr, df$f1, col = col_f1, lwd = 2)
    abline(v = rv$thr_current, lty = 2, col = "black")

    legend("bottomleft", bty = "n",
          legend = c("Accuracy","Sensibilidad","Especificidad","F1","Umbral actual"),
          col = c(col_acc, col_sens, col_esp, col_f1, "black"),
          lty = c(1,1,1,1,2),
          lwd = c(2,2,2,2,1))
  })

  output$tbl_thr_metrics <- DT::renderDT({
    shiny::req(rv$thr_grid)
    DT::datatable(rv$thr_grid, options = list(pageLength = 10)) |>
      DT::formatRound(c("thr","accuracy","sensibilidad","especificidad","precision","f1"), 4)
  })

  observeEvent(list(rv$score, rv$thr_current), {
    shiny::req(rv$score, rv$id_cliente)
    decision <- as.integer(rv$score >= rv$thr_current)
    rv$df_scores <- data.frame(
      id_cliente = as.character(rv$id_cliente),
      p_accept = round(rv$probs_accept, 4),
      p_mora   = round(rv$probs_mora, 4),
      score    = round(rv$score, 4),
      decision = decision,
      stringsAsFactors = FALSE
    )
    # Store scores in session for M3
    session$userData$scores <- rv$df_scores
  })

  output$plot_scores <- shiny::renderPlot({
    shiny::req(rv$score)
    hist(rv$score, breaks = 30, main = "Distribución del score integrado", xlab = "Score")
    abline(v = rv$thr_current, col = 2, lty = 2)
  })

  # Gráfico de torta: distribución Aprobados vs Rechazados
  output$plot_pie <- shiny::renderPlot({
    shiny::req(rv$df_scores)
    decision_counts <- table(rv$df_scores$decision)   # 0 = Rechazado, 1 = Aprobado

    # Asegura orden y etiquetas
    # Si alguna clase no aparece, garantizamos ambos niveles 0 y 1
    all_levels <- c("0","1")
    decision_counts <- decision_counts[all_levels[all_levels %in% names(decision_counts)]]
    names(decision_counts) <- c("Rechazados","Aprobados")[match(names(decision_counts), c("0","1"))]

    # Etiquetas con porcentajes
    total <- sum(decision_counts)
    pct_labels <- paste0(names(decision_counts), " (", round(100*decision_counts/total, 1), "%)")

    pie(
      decision_counts,
      labels = pct_labels,
      main = "Distribución de decisiones",
      clockwise = TRUE
    )
  })

  output$tbl_scores <- DT::renderDT({
    shiny::req(rv$df_scores)
    DT::datatable(rv$df_scores, options = list(pageLength = 10)) |>
      DT::formatRound(c("p_accept","p_mora","score"), 4)
  })

  observeEvent(input$confirmar, {
    shiny::req(rv$metrics_current, rv$df_scores)
    m <- rv$metrics_current

    # VALIDACIÓN: Verificar que hay datos de scores
    if (is.null(rv$df_scores) || nrow(rv$df_scores) == 0) {
      shiny::showNotification("No hay datos de scores disponibles para confirmar.", type = "error")
      return(NULL)
    }

    # VALIDACIÓN: Verificar que la columna decision existe
    if (!"decision" %in% names(rv$df_scores)) {
      shiny::showNotification("Los datos de scores no contienen columna 'decision'.", type = "error")
      return(NULL)
    }

    # FILTRADO DE EMBUDO: Aplicar umbral y filtrar solo clientes APROBADOS
    df_scores_filtrados <- rv$df_scores[rv$df_scores$decision == 1, ]

    n_total <- nrow(rv$df_scores)
    n_aprobados <- nrow(df_scores_filtrados)
    pct_aprobados <- round(100 * n_aprobados / n_total, 1)

    # VALIDACIÓN: Verificar que quedan clientes después del filtrado
    if (n_aprobados == 0) {
      shiny::showNotification("Ningún cliente fue aprobado con el umbral actual. Ajuste el umbral.", type = "warning")
      return(NULL)
    }

    # VALIDACIÓN: Verificar que no se pierden demasiados clientes (advertencia)
    if (pct_aprobados < 5) {
      shiny::showNotification(
        sprintf("Solo %.1f%% de clientes aprobados. Considere bajar el umbral.", pct_aprobados),
        type = "warning", duration = 8
      )
    }

    # Persist evaluation metrics
    persist_eval_m2(
      id_sim = id_sim,
      auc_accept = rv$auc_accept, auc_mora = rv$auc_mora,
      thr = m$thr,
      accuracy = m$accuracy, sensibilidad = m$sensibilidad,
      especificidad = m$especificidad, precision = m$precision, f1 = m$f1
    )

    # Persist filtered client scores (only approved clients)
    scores_saved <- persist_clientes_scores(id_sim = id_sim, df_clientes = df_scores_filtrados)

    # Persist variables
    persist_variables_m2(
      id_sim = id_sim,
      vars_candidatas = input$vars,
      vars_accept_keep = input$keep_vars_accept,
      vars_mora_keep   = input$keep_vars_mora,
      alpha_sig = input$alpha
    )

    if (scores_saved) {
      # Store filtered scores in session for M3 (only approved clients)
      session$userData$scores <- df_scores_filtrados

      shiny::showNotification(
        sprintf("Umbral aplicado: %d/%d clientes aprobados (%.1f%%). Scores guardados en data/clientes_scores.csv",
                n_aprobados, n_total, pct_aprobados),
        type = "message", duration = 6
      )
    } else {
      # Still store filtered data in session as fallback
      session$userData$scores <- df_scores_filtrados
      shiny::showNotification(
        sprintf("Umbral aplicado: %d/%d clientes aprobados (%.1f%%). Error guardando CSV, pero datos disponibles en sesión",
                n_aprobados, n_total, pct_aprobados),
        type = "warning", duration = 6
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$reiniciar, {
    rv$base <- NULL
    rv$X_accept <- NULL; rv$X_mora <- NULL; rv$id_cliente <- NULL
    rv$y_accept <- NULL; rv$y_mora <- NULL
    rv$fit_accept <- NULL; rv$fit_mora <- NULL
    rv$probs_accept <- NULL; rv$probs_mora <- NULL
    rv$auc_accept <- NA_real_; rv$auc_mora <- NA_real_
    rv$score <- NULL; rv$thr_grid <- NULL
    rv$thr_current <- 0.5; rv$metrics_current <- NULL
    rv$df_scores <- NULL
    rv$var_rank_accept <- NULL; rv$var_rank_mora <- NULL
    rv$vars_accept_keep <- NULL; rv$vars_mora_keep <- NULL
    rv$term_map_accept <- NULL; rv$term_map_mora <- NULL
    rv$interp_ok <- FALSE
    rv$interp_target <- NULL
    shiny::showNotification("Módulo 2 reiniciado.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$exportar, {
    shiny::req(rv$df_scores)
    utils::write.csv(rv$df_scores, file.path("data","clientes_scores_export.csv"), row.names = FALSE)
    shiny::showNotification("Exportado data/clientes_scores_export.csv", type = "message")
  }, ignoreInit = TRUE)
}
