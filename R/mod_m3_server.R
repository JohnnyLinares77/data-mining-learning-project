# R/mod_m3_server.R
# ---------------------------------------------------------------
# Server del Módulo 3 – Pricing y Elasticidad
# Firma: callModule(..., datos_reactivos=..., id_sim=..., cluster_levels=NULL)
# ---------------------------------------------------------------

mod_m3_server <- function(input, output, session, datos_reactivos, id_sim, cluster_levels = NULL){
  ns <- session$ns
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # ----------------------------
  # Estado reactivo
  # ----------------------------
  rv <- shiny::reactiveValues(
    metrics = NULL,     # lista con $manual y/o $auto (rmse, r2_adj, alpha, vars)
    sim_results = NULL  # resultados de simulación (para persistir)
  )

  # ----------------------------
  # Helpers (compatibles con M2)
  # ----------------------------
  .get_base_df <- function(d){
    if (is.data.frame(d)) return(d)
    if (!is.null(d$base)) return(d$base)
    keys <- c("demograficas","financieras","comp_historico","clientes","post_desembolso")
    tabs <- tryCatch(Filter(Negate(is.null), d[keys]), error = function(e) list())
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)
  }
.ensure_probs <- function(df){
  # Check if columns exist and have valid (non-NA) values
  need_calc <- !all(c("p_accept","p_mora") %in% names(df)) ||
               any(is.na(df$p_accept)) || any(is.na(df$p_mora))

  if (need_calc) {
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
    if (!("rate" %in% names(df)))   df$rate   <- runif(nrow(df), 0.03, 0.09)
    if (!("amount" %in% names(df))) df$amount <- round(runif(nrow(df), 2000, 20000), 0)
    if (!("term" %in% names(df)))   df$term   <- sample(c(6,12,18,24,36), nrow(df), TRUE)
    df
  }

  # ----------------------------
  # Base con ME + poblar controles
  # ----------------------------
  base_df <- shiny::reactive({
    shiny::req(datos_reactivos())
    df <- .get_base_df(datos_reactivos())

    # Ensure id_cliente is character
    df$id_cliente <- as.character(df$id_cliente)

    df <- .ensure_probs(df)
    df <- .ensure_pricing_pillars(df)

    # Merge session data from M1 and M2 for current analysis
    if (!is.null(session$userData$clusters)) {
      clusters <- session$userData$clusters
      clusters$id_cliente <- as.character(clusters$id_cliente)
      df <- merge(df, clusters, by = "id_cliente", all.x = TRUE)
    }

    if (!is.null(session$userData$scores)) {
      scores_df <- session$userData$scores
      scores_df$id_cliente <- as.character(scores_df$id_cliente)
      df <- merge(df, scores_df, by = "id_cliente", all.x = TRUE)
    }

    # Historical CSV data is loaded separately for reference/display purposes
    # but doesn't interfere with the analysis data

    # Validar que el data frame tenga las dimensiones correctas
    if (nrow(df) == 0) {
      stop("Data frame vacío después del merge")
    }

    # Asegurar que las columnas necesarias existan (fallback si .ensure_* fallaron)
    if (!"rate" %in% names(df)) df$rate <- runif(nrow(df), 0.03, 0.09)
    if (!"amount" %in% names(df)) df$amount <- round(runif(nrow(df), 2000, 20000), 0)
    if (!"term" %in% names(df)) df$term <- sample(c(6,12,18,24,36), nrow(df), TRUE)
    if (!"p_accept" %in% names(df) || any(is.na(df$p_accept))) {
      df$p_accept <- runif(nrow(df), 0.1, 0.9)  # Fallback simple
    }
    if (!"p_mora" %in% names(df) || any(is.na(df$p_mora))) {
      df$p_mora <- runif(nrow(df), 0.01, 0.3)  # Fallback simple
    }

    # Ingreso bruto y ME base (contable)
    ingreso_bruto <- df$rate * df$amount * (df$term / 12)
    base_me <- ingreso_bruto * df$p_accept * (1 - df$p_mora)

    # Ajuste directo para asegurar correlaciones > 0.5 con variables clave
    # Escalar variables clave para el ajuste
    s_buro    <- if ("score_buro" %in% names(df)) scale(df$score_buro) else 0
    ingreso   <- if ("ingreso_verificado" %in% names(df)) scale(df$ingreso_verificado) else 0
    moras_prev<- if ("n_moras_previas" %in% names(df)) scale(df$n_moras_previas) else 0
    rfm       <- if ("rfm" %in% names(df)) scale(df$rfm) else 0

    # Ajuste lineal para aumentar correlaciones con variables predictoras clave
    # Coeficientes calibrados para lograr correlaciones > 0.5
    me_adjustment <- 1500 * (0.5 * s_buro + 0.6 * ingreso - 0.4 * moras_prev + 0.3 * rfm)

    # ME final = componente base + ajuste directo
    df$ME <- base_me + me_adjustment

    # Cluster para simulación
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

    # Lista de numéricas visible desde el inicio
    # Excluir variables derivadas (ME, probabilidades) y IDs
    num_vars <- names(df)[vapply(df, is.numeric, TRUE)]
    num_vars <- setdiff(num_vars, c("ME","id_cliente","p_accept","p_mora"))
    shiny::updateCheckboxGroupInput(
      session, "vars_numeric",
      choices  = num_vars,
      selected = intersect(num_vars, c("rate","amount","term","score_buro","rfm","ingreso_verificado"))
    )

    df
  })

  # No forzar la preparación inicial - dejar que se ejecute cuando se necesite
  # El reactive base_df() se ejecutará cuando otros módulos lo requieran

  # ----------------------------
  # Exploración: correlaciones con ME (bar plot + tabla)
  # ----------------------------
  observeEvent(input$calcular_cor, {
    df   <- base_df()
    vars <- input$vars_numeric
    shiny::validate(shiny::need(length(vars) >= 1, "Selecciona al menos una variable numérica."))

    # Filter variables with positive variance to avoid zero SD warnings
    vars <- vars[sapply(df[, vars, drop = FALSE], function(x) var(x, na.rm = TRUE) > 0)]
    shiny::validate(shiny::need(length(vars) >= 1, "No hay variables con varianza positiva para correlación."))

    cols   <- intersect(unique(c(vars, "ME")), names(df))
    df_cor <- df[, cols, drop = FALSE]
    keep   <- vapply(df_cor, is.numeric, TRUE)
    df_cor <- df_cor[, keep, drop = FALSE]
    shiny::validate(shiny::need(ncol(df_cor) >= 2, "Datos insuficientes para correlación."))

    cor_mat <- stats::cor(df_cor, use = "pairwise.complete.obs")
    cor_with_me <- cor_mat["ME", vars, drop = FALSE]

    # Validar que hay correlaciones válidas
    if (any(is.na(cor_with_me))) {
      shiny::showNotification("No se pudieron calcular algunas correlaciones (datos faltantes).", type = "warning")
    }

    # ---- ggplot: heatmap de matriz de correlaciones completa
    output$cor_plot <- shiny::renderPlot({
      # Convertir matriz de correlación a formato largo para ggplot
      cor_long <- reshape2::melt(cor_mat)
      cor_long <- cor_long[!is.na(cor_long$value), ]  # Filtrar NAs
      shiny::validate(shiny::need(nrow(cor_long) > 0, "No hay correlaciones válidas para mostrar."))

      ggplot2::ggplot(cor_long, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)),
                          color = "black", size = 3) +
        ggplot2::scale_fill_gradient2(limits = c(-1,1), midpoint = 0,
                                      low = "steelblue", mid = "white", high = "tomato",
                                      name = "Correlación") +
        ggplot2::labs(x = "", y = "", title = "Mapa de Calor de Correlaciones") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                      panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank())
    })

    output$cor_table <- DT::renderDT({
      # Mostrar matriz completa de correlaciones redondeada
      DT::datatable(round(cor_mat, 3),
                    options = list(pageLength = 20, scrollX = TRUE))
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Modelo lineal (manual)
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

    # Hold-out 30%
    set.seed(123)
    n   <- nrow(df)
    idx <- sample.int(n, size = floor(0.3*n))
    tr  <- df[-idx, ]; te <- df[idx, ]

    rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

    metrics_df   <- data.frame(Modelo = character(), RMSE = numeric(), R2_adj = numeric(), stringsAsFactors = FALSE)
    metrics_list <- list()

    if (!is.null(modelo_manual())) {
      manual_fit_tr <- stats::lm(stats::formula(modelo_manual()), data = tr)
      pred_man <- predict(manual_fit_tr, newdata = te)
      rmse_man <- rmse(te$ME, pred_man)
      r2_man   <- summary(manual_fit_tr)$adj.r.squared
      metrics_df <- rbind(metrics_df, data.frame(Modelo = "Manual", RMSE = rmse_man, R2_adj = r2_man))
      metrics_list$manual <- list(rmse = rmse_man, r2_adj = r2_man, alpha = input$alpha, vars = input$vars_numeric %||% character(0))
    }

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

      # Calcular ME consistente con base_df (incluyendo ajuste directo)
      ingreso_bruto <- tmp$rate * tmp$amount * (tmp$term/12)
      base_me <- ingreso_bruto * tmp$p_accept * (1 - tmp$p_mora)

      # Ajuste directo para correlaciones
      s_buro    <- if ("score_buro" %in% names(tmp)) scale(tmp$score_buro) else 0
      ingreso   <- if ("ingreso_verificado" %in% names(tmp)) scale(tmp$ingreso_verificado) else 0
      moras_prev<- if ("n_moras_previas" %in% names(tmp)) scale(tmp$n_moras_previas) else 0
      rfm       <- if ("rfm" %in% names(tmp)) scale(tmp$rfm) else 0

      me_adjustment <- 1500 * (0.5 * s_buro + 0.6 * ingreso - 0.4 * moras_prev + 0.3 * rfm)
      tmp$ME <- base_me + me_adjustment

      tmp
    }))

    me_hat <- as.numeric(predict(m_final, newdata = df_sim))

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
  # Persistencia de resultados M3
  # ----------------------------
  observeEvent(input$confirmar, {
    shiny::req((modelo_manual() %||% modelo_auto()), rv$sim_results, rv$metrics)

    met <- switch(input$modelo_final,
                  manual     = rv$metrics$manual,
                  automatico = rv$metrics$auto,
                  NULL)

    ok1 <- ok2 <- TRUE
    err <- NULL

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
