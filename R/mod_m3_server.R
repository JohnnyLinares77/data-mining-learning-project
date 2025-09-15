# R/mod_m3_server.R
# ---------------------------------------------------------------
# Server del Módulo 3 – Pricing y Elasticidad
# Firma: callModule(..., datos_reactivos=..., id_sim=..., cluster_levels=NULL)
# ---------------------------------------------------------------

mod_m3_server <- function(input, output, session, datos_reactivos, id_sim, cluster_levels = NULL){
  ns <- session$ns

  # Operador "o si no" (útil para valores por defecto)
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # ----------------------------
  # Estado reactivo
  # ----------------------------
  rv <- shiny::reactiveValues(
    metrics = NULL,     # lista con $manual y/o $auto (rmse, r2_adj, alpha, vars)
    sim_results = NULL  # lista con resultados de simulación (para persistir)
  )

  # ----------------------------
  # Helpers (compatibles con M2)
  # ----------------------------
  .get_base_df <- function(d){
    # 1) Si ya es data.frame (como en app.R con datos_para_m3), úsalo directo
    if (is.data.frame(d)) return(d)

    # 2) Si viene como lista con $base, úsalo
    if (!is.null(d$base)) return(d$base)

    # 3) Si viene como lista de tablas, combínalas por id_cliente
    keys <- c("demograficas","financieras","comp_historico","clientes","post_desembolso")
    tabs <- tryCatch(Filter(Negate(is.null), d[keys]), error = function(e) list())
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)
  }

  .ensure_probs <- function(df){
    # Si M2 ya dejó p_accept / p_mora se usan; si no, simulamos (patrón M2)
    if (!all(c("p_accept","p_mora") %in% names(df))) {
      s_buro    <- if ("score_buro" %in% names(df)) scale(df$score_buro) else 0
      ingreso   <- if ("ingreso_verificado" %in% names(df)) scale(df$ingreso_verificado) else 0
      moras_prev<- if ("n_moras_previas" %in% names(df)) scale(df$n_moras_previas) else 0
      rfm       <- if ("rfm" %in% names(df)) scale(df$rfm) else 0
      lin_acc <-  0.6 * s_buro + 0.4 * ingreso - 0.2 * moras_prev + 0.2 * rfm
      lin_mor <- -0.7 * s_buro + 0.6 * moras_prev - 0.2 * ingreso - 0.1 * rfm
      df$p_accept <- 1/(1 + exp(-lin_acc))
      df$p_mora   <- 1/(1 + exp(-lin_mor))
    }
    df
  }

  .ensure_pricing_pillars <- function(df){
    # Si no hay rate/amount/term, creamos columnas plausibles
    if (!("rate" %in% names(df)))   df$rate   <- runif(nrow(df), 0.03, 0.09)
    if (!("amount" %in% names(df))) df$amount <- round(runif(nrow(df), 2000, 20000), 0)
    if (!("term" %in% names(df)))   df$term   <- sample(c(6,12,18,24,36), nrow(df), TRUE)
    df
  }

  # ----------------------------
  # Base con ME
  # ----------------------------
  base_df <- shiny::reactive({
    shiny::req(datos_reactivos())
    df <- .get_base_df(datos_reactivos())
    df <- .ensure_probs(df)
    df <- .ensure_pricing_pillars(df)

    # Ingreso bruto y Margen Esperado "contable"
    ingreso_bruto <- df$rate * df$amount * (df$term / 12)
    df$ME <- ingreso_bruto * df$p_accept * (1 - df$p_mora)

    # Poblar niveles de cluster para simulación
    if (is.null(cluster_levels)) {
      if ("cluster_id" %in% names(df)) {
        shiny::updateSelectInput(session, "cluster_sim",
                                 choices = sort(unique(as.character(df$cluster_id))))
      } else if ("cluster" %in% names(df)) {
        shiny::updateSelectInput(session, "cluster_sim",
                                 choices = sort(unique(as.character(df$cluster))))
      } else {
        shiny::updateSelectInput(session, "cluster_sim", choices = NULL)
      }
    } else {
      shiny::updateSelectInput(session, "cluster_sim", choices = cluster_levels)
    }

    # Poblar lista de numéricas (excluye id y respuesta)
    num_vars <- names(df)[vapply(df, is.numeric, TRUE)]
    num_vars <- setdiff(num_vars, c("ME","id_cliente"))
    shiny::updateCheckboxGroupInput(
      session, "vars_numeric",
      choices  = num_vars,
      selected = intersect(num_vars, c("rate","amount","term","score_buro","rfm","ingreso_verificado"))
    )

    df
  })

  # ----------------------------
  # Exploración: matriz de correlaciones
  # ----------------------------
  observeEvent(input$calcular_cor, {
    df   <- base_df()
    vars <- input$vars_numeric
    shiny::validate(shiny::need(length(vars) >= 1, "Selecciona al menos una variable numérica."))

    # Selección segura de columnas
    cols   <- intersect(unique(c(vars, "ME")), names(df))
    df_cor <- df[, cols, drop = FALSE]
    keep   <- vapply(df_cor, is.numeric, TRUE)
    df_cor <- df_cor[, keep, drop = FALSE]
    shiny::validate(shiny::need(ncol(df_cor) >= 2, "Datos insuficientes para correlación."))

    cor_mat <- stats::cor(df_cor, use = "pairwise.complete.obs")

    output$cor_plot <- shiny::renderPlot({
      if (requireNamespace("corrplot", quietly = TRUE)) {
        corrplot::corrplot(cor_mat, method = "color", tl.cex = 0.8)
      } else {
        image(1:ncol(cor_mat), 1:nrow(cor_mat), t(cor_mat)[, ncol(cor_mat):1],
              xlab = "", ylab = "", axes = FALSE)
        axis(1, at = 1:ncol(cor_mat), labels = colnames(cor_mat), las = 2, cex.axis = 0.7)
        axis(2, at = 1:nrow(cor_mat), labels = rev(rownames(cor_mat)), las = 2, cex.axis = 0.7)
      }
    })

    output$cor_table <- DT::renderDT({
      DT::datatable(round(cor_mat, 3), options = list(pageLength = 8))
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Modelo lineal (manual) con variables del panel izquierdo
  # ----------------------------
  modelo_manual <- shiny::reactiveVal(NULL)

  observeEvent(input$ajustar_modelo, {
    df   <- base_df()
    vars <- input$vars_numeric
    shiny::validate(shiny::need(length(vars) >= 1, "Selecciona variables para el modelo."))

    form_str <- paste("ME ~", paste(vars, collapse = " + "))
    fit      <- stats::lm(stats::as.formula(form_str), data = df)
    modelo_manual(fit)

    output$model_summary <- shiny::renderPrint({ summary(fit) })
    output$resid_plot <- shiny::renderPlot({
      par(mfrow = c(1,2))
      plot(fit, which = 1)
      plot(fit, which = 2)
      par(mfrow = c(1,1))
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Modelo automático (stepAIC)
  # ----------------------------
  modelo_auto <- shiny::reactiveVal(NULL)

  observeEvent(input$ajustar_auto, {
    df <- base_df()
    num_vars <- setdiff(names(df)[vapply(df, is.numeric, TRUE)], "ME")
    full_form <- stats::as.formula(paste("ME ~", paste(num_vars, collapse = " + ")))
    full_fit  <- stats::lm(full_form, data = df)
    auto_fit  <- MASS::stepAIC(full_fit, direction = "both", trace = FALSE)
    modelo_auto(auto_fit)

    output$auto_summary <- shiny::renderPrint({ summary(auto_fit) })

    # Comparación con hold-out 30%
    set.seed(123)
    n   <- nrow(df)
    idx <- sample.int(n, size = floor(0.3*n))
    tr  <- df[-idx, ]; te <- df[idx, ]

    rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

    # Métricas a mostrar y a persistir
    metrics_df   <- data.frame(Modelo = character(), RMSE = numeric(), R2_adj = numeric(), stringsAsFactors = FALSE)
    metrics_list <- list()

    # Si existe modelo manual, reentrenarlo en train y evaluar en test
    if (!is.null(modelo_manual())) {
      manual_fit_tr <- stats::lm(stats::formula(modelo_manual()), data = tr)
      pred_man <- predict(manual_fit_tr, newdata = te)
      rmse_man <- rmse(te$ME, pred_man)
      r2_man   <- summary(manual_fit_tr)$adj.r.squared
      metrics_df <- rbind(metrics_df, data.frame(Modelo = "Manual", RMSE = rmse_man, R2_adj = r2_man))
      metrics_list$manual <- list(rmse = rmse_man, r2_adj = r2_man, alpha = input$alpha, vars = input$vars_numeric %||% character(0))
    }

    # Modelo automático entrenado en train, evaluado en test
    auto_fit_tr <- stats::lm(stats::formula(auto_fit), data = tr)
    pred_auto   <- predict(auto_fit_tr, newdata = te)
    rmse_auto <- rmse(te$ME, pred_auto)
    r2_auto   <- summary(auto_fit_tr)$adj.r.squared
    metrics_df <- rbind(metrics_df, data.frame(Modelo = "Automático", RMSE = rmse_auto, R2_adj = r2_auto))
    auto_vars <- attr(terms(auto_fit), "term.labels")
    metrics_list$auto <- list(rmse = rmse_auto, r2_adj = r2_auto, alpha = input$alpha, vars = auto_vars %||% character(0))

    rv$metrics <- metrics_list

    output$comp_table <- DT::renderDT({
      DT::datatable(metrics_df, options = list(dom = "t", paging = FALSE)) |>
        DT::formatRound(c("RMSE","R2_adj"), 4)
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Simulación y elasticidad
  # ----------------------------
  observeEvent(input$simular, {
    m_final <- switch(input$modelo_final,
                      manual = modelo_manual(),
                      automatico = modelo_auto())
    shiny::req(m_final)

    df0   <- base_df()
    rango <- input$rango_tasa
    r_seq <- seq(rango[1], rango[2], length.out = 60)

    monto   <- input$monto_sim
    plazo   <- input$plazo_sim
    score   <- input$score_sim
    cluster_val <- input$cluster_sim

    # plantilla base (primera fila) y expandimos sobre tasa
    tpl <- df0[1, , drop = FALSE]
    df_sim <- do.call(rbind, lapply(r_seq, function(r){
      tmp <- tpl
      tmp$rate   <- r
      tmp$amount <- monto
      tmp$term   <- plazo
      if ("score_buro" %in% names(tmp)) tmp$score_buro <- score
      if ("score" %in% names(tmp))      tmp$score      <- score
      if ("cluster_id" %in% names(tmp)) tmp$cluster_id <- cluster_val
      if ("cluster" %in% names(tmp))    tmp$cluster    <- cluster_val
      # ME "contable" para referencia visual (la predicción vendrá del modelo)
      ingreso_bruto <- tmp$rate * tmp$amount * (tmp$term/12)
      tmp$ME <- ingreso_bruto * tmp$p_accept * (1 - tmp$p_mora)
      tmp
    }))

    me_hat <- as.numeric(predict(m_final, newdata = df_sim))

    # derivada parcial de ME_hat respecto a rate (si existen términos relevantes)
    coefs <- stats::coef(m_final)
    dME_dr <- numeric(length(r_seq))
    for (i in seq_along(r_seq)) {
      r <- r_seq[i]; d <- 0
      if ("rate" %in% names(coefs))             d <- d + coefs["rate"]
      if ("I(rate^2)" %in% names(coefs))        d <- d + 2*coefs["I(rate^2)"]*r
      if ("rate:score" %in% names(coefs))       d <- d + coefs["rate:score"]*score
      if ("rate:score_buro" %in% names(coefs))  d <- d + coefs["rate:score_buro"]*score
      dME_dr[i] <- d
    }

    elasticidad <- dME_dr * (r_seq / pmax(me_hat, .Machine$double.eps))

    df_plot <- data.frame(rate = r_seq, ME = me_hat, Elasticidad = elasticidad)

    # guardar resultados de simulación para persistir
    rv$sim_results <- list(
      modelo = input$modelo_final,
      monto  = monto,
      plazo  = plazo,
      score  = score,
      cluster = cluster_val,
      rate = df_plot$rate,
      ME   = df_plot$ME,
      Elasticidad = df_plot$Elasticidad
    )

    output$margen_plot <- shiny::renderPlot({
      plot(df_plot$rate, df_plot$ME, type = "l", lwd = 2,
           xlab = "Tasa de interés", ylab = "Margen esperado",
           main = "Curva ME vs Tasa")
      i_max <- which.max(df_plot$ME)
      points(df_plot$rate[i_max], df_plot$ME[i_max], pch = 19, col = 2)
      text(df_plot$rate[i_max], df_plot$ME[i_max],
           labels = sprintf("Max ME @ %.3f", df_plot$rate[i_max]), pos = 4)
    })

    output$elasticidad_plot <- shiny::renderPlot({
      plot(df_plot$rate, df_plot$Elasticidad, type = "l", lwd = 2,
           xlab = "Tasa de interés", ylab = "Elasticidad",
           main = "Elasticidad (∂ME/∂r · r/ME)")
      abline(h = c(-1, 0), lty = c(2,3), col = c("blue","gray40"))
      legend("topright", inset = 0.01, bty = "n",
             legend = c("E = -1 (punto elástico)","E = 0"),
             lty = c(2,3), col = c("blue","gray40"))
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Persistencia de resultados M3 (sin cerrar la app)
  # ----------------------------
  observeEvent(input$confirmar, {
    shiny::req(modelo_manual() %||% modelo_auto())
    shiny::req(rv$sim_results, rv$metrics)

    # Elegir bloque de métricas según modelo final
    met <- switch(input$modelo_final,
                  manual     = rv$metrics$manual,
                  automatico = rv$metrics$auto,
                  NULL)

    ok1 <- ok2 <- TRUE
    err <- NULL

    # Guardar métricas de modelo final
    tryCatch({
      persist_eval_m3(
        id_sim = id_sim,
        modelo = input$modelo_final,
        rmse   = met$rmse %||% NA_real_,
        r2_adj = met$r2_adj %||% NA_real_,
        alpha  = met$alpha %||% input$alpha %||% NA_real_,
        vars   = met$vars %||% character(0)
      )
    }, error = function(e) { ok1 <<- FALSE; err <<- conditionMessage(e) })

    # Guardar curva de simulación
    tryCatch({
      persist_simulacion_m3(
        id_sim = id_sim,
        modelo = input$modelo_final,
        monto  = input$monto_sim,
        plazo  = input$plazo_sim,
        score  = input$score_sim,
        cluster = input$cluster_sim,
        rate_vec = rv$sim_results$rate,
        me_vec   = rv$sim_results$ME,
        elasticidad_vec = rv$sim_results$Elasticidad
      )
    }, error = function(e) { ok2 <<- FALSE; err <<- conditionMessage(e) })

    if (isTRUE(ok1 && ok2)) {
      shiny::showNotification("Resultados M3 guardados en data/*.csv", type = "message")
    } else {
      shiny::showNotification(paste("No se pudo guardar (M3):", err), type = "error")
    }
  }, ignoreInit = TRUE)
}
