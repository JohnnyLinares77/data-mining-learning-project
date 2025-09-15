# R/mod_m3_server.R
# Server del Módulo 3 – Pricing y Elasticidad
#
# Esta función implementa la lógica reactiva del módulo de pricing. Recibe un
# objeto reactivo `base_data` que devuelve un data.frame con las variables
# necesarias: las probabilidades de aceptación y de mora (p_accept y p_mora),
# así como las palancas de pricing (rate, amount, term) y cualquier otra
# variable numérica o categórica relevante. La función calcula el margen
# esperado (ME), permite explorar correlaciones, ajustar modelos lineales
# manuales y automáticos, y simular márgenes y elasticidades.
#
mod_m3_server <- function(input, output, session, base_data, cluster_levels = NULL){
  ns <- session$ns

  # 1. Reactive: calcula el margen esperado y añade columna ME al data.frame
  datos_con_margen <- shiny::reactive({
    shiny::req(base_data())
    df <- base_data()
    # Verificar que existan las columnas requeridas
    required_cols <- c("p_accept", "p_mora", "rate", "amount", "term")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("Las siguientes columnas faltan en base_data:", paste(missing_cols, collapse = ", ")))
    }
    # Ingreso bruto: tasa * monto * plazo/12
    ingreso_bruto <- df$rate * df$amount * (df$term / 12)
    # Margen esperado: ingreso bruto * p_accept * (1 - p_mora)
    df$ME <- ingreso_bruto * df$p_accept * (1 - df$p_mora)
    df
  })

  # 1A. Actualizar dinámicamente selectores en UI
  shiny::observeEvent(datos_con_margen(), {
    df <- datos_con_margen()
    # Variables numéricas excluyendo la respuesta
    num_vars <- names(df)[sapply(df, is.numeric)]
    num_vars <- setdiff(num_vars, "ME")
    # Update de selectizeInput para correlaciones
    output$var_select_ui <- shiny::renderUI({
      shiny::selectizeInput(ns("vars_corr"), "Variables numéricas:",
                            choices = num_vars, selected = num_vars, multiple = TRUE)
    })
    # Update de checkboxGroupInput para modelo lineal manual
    output$model_var_select <- shiny::renderUI({
      shiny::checkboxGroupInput(ns("vars_model"), "Variables explicativas:",
                                choices = num_vars, selected = num_vars)
    })
    # Actualizar opciones de cluster para simulación
    clusters <- cluster_levels
    if (is.null(clusters)) {
      if ("cluster" %in% names(df)) {
        clusters <- unique(as.character(df$cluster))
      } else {
        clusters <- NULL
      }
    }
    shiny::updateSelectInput(session, "cluster_sim", choices = clusters)
  }, ignoreInit = TRUE)

  # 1B. Calcular la matriz de correlaciones
  shiny::observeEvent(input$calcular_cor, {
    shiny::req(input$vars_corr)
    df <- datos_con_margen()[, c(input$vars_corr, "ME"), drop = FALSE]
    cor_mat <- stats::cor(df, use = "pairwise.complete.obs")
    output$cor_plot <- shiny::renderPlot({
      if (requireNamespace("corrplot", quietly = TRUE)) {
        corrplot::corrplot(cor_mat, method = "color", tl.cex = 0.8)
      } else {
        # Fallback simple heatmap si corrplot no está disponible
        image(1:ncol(cor_mat), 1:nrow(cor_mat), t(cor_mat)[, ncol(cor_mat):1],
              xlab = "", ylab = "", axes = FALSE)
        axis(1, at = 1:ncol(cor_mat), labels = colnames(cor_mat), las = 2, cex.axis = 0.7)
        axis(2, at = 1:nrow(cor_mat), labels = rev(rownames(cor_mat)), las = 2, cex.axis = 0.7)
      }
    })
    output$cor_table <- shiny::renderDataTable({
      round(cor_mat, 3)
    }, options = list(pageLength = 5))
  })

  # 2. Ajustar modelo manual
  modelo_manual <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$ajustar_modelo, {
    shiny::req(input$vars_model)
    df <- datos_con_margen()
    vars <- input$vars_model
    formula_str <- paste("ME ~", paste(vars, collapse = "+"))
    fit <- stats::lm(stats::as.formula(formula_str), data = df)
    modelo_manual(fit)
    output$model_summary <- shiny::renderPrint({
      summary(fit)
    })
    output$resid_plot <- shiny::renderPlot({
      par(mfrow = c(1, 2))
      plot(fit, which = 1)
      plot(fit, which = 2)
    })
  })

  # 3. Ajustar modelo automático (stepwise AIC)
  modelo_auto <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$ajustar_auto, {
    df <- datos_con_margen()
    num_vars <- names(df)[sapply(df, is.numeric)]
    num_vars <- setdiff(num_vars, "ME")
    full_form <- stats::as.formula(paste("ME ~", paste(num_vars, collapse = "+")))
    full_fit <- stats::lm(full_form, data = df)
    auto_fit <- MASS::stepAIC(full_fit, direction = "both", trace = FALSE)
    modelo_auto(auto_fit)
    output$auto_summary <- shiny::renderPrint({
      summary(auto_fit)
    })
    # Comparación de desempeño con hold-out 30%
    set.seed(123)
    n <- nrow(df)
    idx <- sample(seq_len(n), size = floor(0.3 * n))
    df_train <- df[-idx, ]
    df_test <- df[idx, ]
    # Ajustar modelos en train
    if (!is.null(modelo_manual())) {
      manual_fit <- stats::lm(formula(modelo_manual()), data = df_train)
    } else {
      manual_fit <- NULL
    }
    auto_fit_train <- stats::lm(formula(auto_fit), data = df_train)
    # Función RMSE
    rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
    metrics <- data.frame(Modelo = character(), RMSE = numeric(), R2_adj = numeric())
    if (!is.null(manual_fit)) {
      pred_man <- predict(manual_fit, newdata = df_test)
      r2_man <- summary(manual_fit)$adj.r.squared
      metrics <- rbind(metrics, data.frame(Modelo = "Manual", RMSE = rmse(df_test$ME, pred_man), R2_adj = r2_man))
    }
    pred_auto <- predict(auto_fit_train, newdata = df_test)
    r2_auto <- summary(auto_fit_train)$adj.r.squared
    metrics <- rbind(metrics, data.frame(Modelo = "Automático", RMSE = rmse(df_test$ME, pred_auto), R2_adj = r2_auto))
    output$comp_table <- shiny::renderDataTable({
      metrics
    }, options = list(dom = "t", paging = FALSE))
  })

  # 4. Simulación de márgenes y elasticidad
  shiny::observeEvent(input$simular, {
    # Seleccionar el modelo según input$modelo_final
    selected_model <- switch(input$modelo_final,
                             manual = modelo_manual(),
                             automatico = modelo_auto())
    shiny::req(!is.null(selected_model))
    # Crear secuencia de tasas
    rango <- input$rango_tasa
    r_seq <- seq(from = rango[1], to = rango[2], length.out = 50)
    # Valores para monto, plazo, score y cluster
    monto <- input$monto_sim
    plazo <- input$plazo_sim
    score <- input$score_sim
    cluster_val <- input$cluster_sim
    # Plantilla con primer registro
    df_template <- datos_con_margen()[1, ]
    df_sim <- do.call(rbind, lapply(r_seq, function(r) {
      tmp <- df_template
      tmp$rate <- r
      tmp$amount <- monto
      tmp$term <- plazo
      tmp$score <- score
      if (!is.null(tmp$cluster)) tmp$cluster <- cluster_val
      # Recalcular ME para coherencia (no se usa en predicción)
      ingreso_bruto_tmp <- tmp$rate * tmp$amount * (tmp$term / 12)
      tmp$ME <- ingreso_bruto_tmp * tmp$p_accept * (1 - tmp$p_mora)
      tmp
    }))
    # Predicción del margen con el modelo seleccionado
    me_hat <- predict(selected_model, newdata = df_sim)
    # Calcular derivada dME/dr
    coefs <- stats::coef(selected_model)
    dME_dr <- numeric(length(r_seq))
    for (i in seq_along(r_seq)) {
      r <- r_seq[i]
      d <- 0
      if ("rate" %in% names(coefs)) d <- d + coefs["rate"]
      if ("I(rate^2)" %in% names(coefs)) d <- d + 2 * coefs["I(rate^2)"] * r
      if ("rate:score" %in% names(coefs)) d <- d + coefs["rate:score"] * score
      dME_dr[i] <- d
    }
    # Elasticidad: E = (dME/dr) * (rate / ME)
    elasticidad <- dME_dr * (r_seq / me_hat)
    df_plot <- data.frame(rate = r_seq, ME = me_hat, Elasticidad = elasticidad)
    # Gráfico de margen esperado
    output$margen_plot <- shiny::renderPlot({
      plot(df_plot$rate, df_plot$ME, type = "l", lwd = 2,
           xlab = "Tasa de interés", ylab = "Margen esperado",
           main = "Curva de margen esperado vs tasa")
      idx_max <- which.max(df_plot$ME)
      points(df_plot$rate[idx_max], df_plot$ME[idx_max], pch = 19, col = "red")
      text(df_plot$rate[idx_max], df_plot$ME[idx_max],
           labels = sprintf("\nMax: %.3f", df_plot$rate[idx_max]), pos = 4)
    })
    # Gráfico de elasticidad
    output$elasticidad_plot <- shiny::renderPlot({
      plot(df_plot$rate, df_plot$Elasticidad, type = "l", lwd = 2,
           xlab = "Tasa de interés", ylab = "Elasticidad",
           main = "Elasticidad de la demanda vs tasa")
      abline(h = c(-1, 0), lty = c(2, 3), col = c("blue", "darkgrey"))
      legend("topright", legend = c("E=-1 (punto elástico)", "E=0"),
             lty = c(2, 3), col = c("blue", "darkgrey"), bty = "n", cex = 0.8)
    })
  })
}
