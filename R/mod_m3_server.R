# R/mod_m3_server.R
# ---------------------------------------------------------------
# Server del Módulo 3 – Pricing y Elasticidad
# Firma: callModule(..., datos_reactivos=..., id_sim=..., cluster_levels=NULL)
# ---------------------------------------------------------------

mod_m3_server <- function(input, output, session, datos_reactivos, id_sim, execution_mode = reactive("sequential"), cluster_levels = NULL){
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
  # Actualizar variables disponibles cuando cambien los datos
  # ----------------------------
  observe({
    # Obtener datos para mostrar variables disponibles
    datos <- datos_reactivos()
    if (!is.null(datos)) {
      df <- .get_base_df(datos)

      # Lista de numéricas disponible
      num_vars <- names(df)[vapply(df, is.numeric, TRUE)]
      exclude_vars <- c("margen","id_cliente","p_accept","p_mora",
                        grep("\\.x$|\\.y$|\\.cluster$|\\.score$", num_vars, value = TRUE))
      num_vars <- setdiff(num_vars, exclude_vars)

      # VALIDACIÓN: Verificar que hay variables numéricas disponibles
      if (length(num_vars) == 0) {
        shiny::showNotification("No hay variables numéricas disponibles en los datos.", type = "error")
        shiny::updateCheckboxGroupInput(session, "vars_numeric", choices = character(0), selected = character(0))
        return(NULL)
      }

      # Excluir variables que no varían (todos los valores iguales) - típico cuando hay filtrado
      vars_con_variacion <- num_vars[sapply(df[, num_vars, drop = FALSE], function(x) {
        vals <- x[!is.na(x)]
        length(unique(vals)) > 1 && length(vals) > 0
      })]

      # VALIDACIÓN: Verificar que quedan variables con variación
      if (length(vars_con_variacion) == 0) {
        shiny::showNotification(
          "Todas las variables numéricas tienen valores constantes. Esto puede deberse a un filtrado muy restrictivo en módulos anteriores.",
          type = "warning", duration = 10
        )
        # Mantener al menos las variables básicas si existen
        vars_basicas <- intersect(c("rate","amount","term"), num_vars)
        if (length(vars_basicas) > 0) {
          vars_con_variacion <- vars_basicas
          shiny::showNotification(
            sprintf("Usando variables básicas para continuar: %s", paste(vars_basicas, collapse = ", ")),
            type = "message", duration = 5
          )
        } else {
          shiny::updateCheckboxGroupInput(session, "vars_numeric", choices = character(0), selected = character(0))
          return(NULL)
        }
      }

      # Variables por defecto disponibles
      selected_default <- c("tasa","monto_oferta","plazo","score_buro","rfm","ingreso_verificado",
                           "cluster_id","score")
      selected_available <- intersect(selected_default, vars_con_variacion)

      # Asegurar que hay al menos una variable seleccionada por defecto
      if (length(selected_available) == 0 && length(vars_con_variacion) > 0) {
        selected_available <- vars_con_variacion[1:min(3, length(vars_con_variacion))]
      }

      # Mostrar mensaje si se excluyeron variables por falta de variación (solo en modo secuencial)
      if (execution_mode() != "independent") {
        vars_excluidas <- setdiff(num_vars, vars_con_variacion)
        if (length(vars_excluidas) > 0) {
          shiny::showNotification(
            sprintf("Variables excluidas por falta de variación: %s",
                   paste(vars_excluidas, collapse = ", ")),
            type = "message", duration = 5
          )
        }
      }

      shiny::updateCheckboxGroupInput(
        session, "vars_numeric",
        choices  = vars_con_variacion,
        selected = selected_available
      )
    }
  })

  # ----------------------------
  # Helpers (compatibles con M2)
  # ----------------------------
  .get_base_df <- function(d){
    if (is.data.frame(d)) return(d)
    if (!is.null(d$base)) return(d$base)
    keys <- c("demograficas","financieras","comp_historico","clientes","post_desembolso","ofertas_historicas")
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
    # En modo independiente, usar datos simulados completos
    if (execution_mode() == "independent") {
      df <- generate_complete_data_m3(n_clientes = 1000, seed = 131415)
      # Simular scores previos
      simulated_scores <- generate_simulated_scores(n_clientes = 1000, seed = 789)
      session$userData$scores <- simulated_scores
      shiny::showNotification("Modo independiente: Usando datos simulados con scores generados", type = "info", duration = 3)
    } else {
      shiny::req(datos_reactivos())
      df <- .get_base_df(datos_reactivos())

      # Debug: Check initial data
      if (nrow(df) == 0) {
        stop("Datos iniciales vacíos de gen_datos()")
      }
    }

    # Ensure id_cliente is character
    df$id_cliente <- as.character(df$id_cliente)

    df <- .ensure_probs(df)
    df <- .ensure_pricing_pillars(df)

    # Debug: Check after ensuring columns
    if (nrow(df) == 0) {
      stop("Data frame vacío después de .ensure_* functions")
    }

    # Merge session data from M1 and M2 for current analysis
    # Evitar duplicados de columnas usando suffixes específicos
    if (!is.null(session$userData$clusters)) {
      clusters <- session$userData$clusters
      clusters$id_cliente <- as.character(clusters$id_cliente)
      # Solo incluir columnas de clusters, excluir id_cliente del merge
      cluster_cols <- setdiff(names(clusters), "id_cliente")
      df <- merge(df, clusters[, c("id_cliente", cluster_cols), drop = FALSE],
                  by = "id_cliente", all.x = TRUE, suffixes = c("", ".cluster"))
      # Debug: Check after cluster merge
      if (nrow(df) == 0) {
        stop("Data frame vacío después del merge con clusters")
      }
    }

    if (!is.null(session$userData$scores)) {
      scores_df <- session$userData$scores
      scores_df$id_cliente <- as.character(scores_df$id_cliente)
      # Solo incluir columnas de scores, excluir id_cliente del merge
      score_cols <- setdiff(names(scores_df), "id_cliente")
      df <- merge(df, scores_df[, c("id_cliente", score_cols), drop = FALSE],
                  by = "id_cliente", all.x = TRUE, suffixes = c("", ".score"))
      # Debug: Check after scores merge
      if (nrow(df) == 0) {
        stop("Data frame vacío después del merge con scores")
      }
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

    # Usar Margen Histórico como variable respuesta margen
    if ("margen" %in% names(df)) {
      df$margen <- df$margen
    } else {
      # Fallback si no existe (no debería ocurrir)
      df$margen <- 5000
    }

    # VALIDACIÓN: Verificar que ME tiene valores razonables
    me_range <- range(df$ME, na.rm = TRUE)
    if (any(is.na(df$ME)) || any(is.infinite(df$ME))) {
      shiny::showNotification("Algunos valores de ME son NA o infinitos. Verifique los datos.", type = "warning")
    }
    if (me_range[1] < -10000 || me_range[2] > 100000) {
      shiny::showNotification(
        sprintf("Valores de ME extremos detectados: [%.0f, %.0f]. Verifique los cálculos.", me_range[1], me_range[2]),
        type = "warning"
      )
    }

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

    # Nota: Las variables se actualizan en el observer de inicialización
    # para que estén disponibles desde que se abre la pestaña

    df
  })

  # No forzar la preparación inicial - dejar que se ejecute cuando se necesite
  # El reactive base_df() se ejecutará cuando otros módulos lo requieran

  # ----------------------------
  # Exploración: correlaciones con ME (bar plot + tabla)
  # ----------------------------
  observeEvent(input$calcular_cor, {
    # En modo independiente, no verificar dependencias
    if (execution_mode() != "independent") {
      # VALIDACIÓN: Verificar dependencias de módulos anteriores
      if (is.null(session$userData$clusters) || nrow(session$userData$clusters) == 0) {
        shiny::showNotification(
          "No hay datos de clusters del Módulo 1. Complete el Módulo 1 primero.",
          type = "warning"
        )
      }
      if (is.null(session$userData$scores) || nrow(session$userData$scores) == 0) {
        shiny::showNotification(
          "No hay datos de scores del Módulo 2. Complete el Módulo 2 primero.",
          type = "warning"
        )
      }
    }

    df   <- base_df()
    vars <- input$vars_numeric
    shiny::validate(shiny::need(length(vars) >= 1, "Selecciona al menos una variable numérica."))
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos disponibles para calcular correlaciones."))
    shiny::validate(shiny::need("margen" %in% names(df), "La variable respuesta margen no está disponible."))

    # VALIDACIÓN: Verificar que hay datos suficientes
    if (nrow(df) < 3) {
      shiny::showNotification(
        sprintf("Insuficientes datos para correlación (%d filas). Se necesitan al menos 3 observaciones.", nrow(df)),
        type = "error"
      )
      return(NULL)
    }

    # Verificar que las variables existen en el dataframe
    vars_available <- intersect(vars, names(df))
    shiny::validate(shiny::need(length(vars_available) >= 1,
      sprintf("Ninguna de las variables seleccionadas está disponible. Variables disponibles: %s",
              paste(names(df)[vapply(df, is.numeric, TRUE)], collapse = ", "))))

    # Filter variables with positive variance to avoid zero SD warnings
    vars_valid <- vars_available[sapply(df[, vars_available, drop = FALSE], function(x) {
      vals <- x[!is.na(x)]
      length(vals) >= 3 && var(vals) > 0
    })]
    shiny::validate(shiny::need(length(vars_valid) >= 1,
      sprintf("No hay variables con varianza positiva y suficientes datos. Variables con problemas: %s",
              paste(setdiff(vars_available, vars_valid), collapse = ", "))))

    cols   <- intersect(unique(c(vars_valid, "margen")), names(df))
    df_cor <- df[, cols, drop = FALSE]
    keep   <- vapply(df_cor, is.numeric, TRUE)
    df_cor <- df_cor[, keep, drop = FALSE]
    shiny::validate(shiny::need(ncol(df_cor) >= 2, "Datos insuficientes para correlación."))

    # Check for sufficient complete cases for correlation
    complete_rows <- sum(complete.cases(df_cor))
    shiny::validate(shiny::need(complete_rows >= 3,
      sprintf("Insuficientes casos completos para correlación (%d casos requeridos, %d disponibles).",
              3, complete_rows)))

    cor_mat <- stats::cor(df_cor, use = "pairwise.complete.obs")

    # Verificar que margen existe en la matriz de correlación
    if (!"margen" %in% rownames(cor_mat)) {
      shiny::showNotification("La variable margen no está disponible para calcular correlaciones.", type = "error")
      return(NULL)
    }

    # Solo incluir variables que existen en la matriz de correlación
    vars_in_cor <- intersect(vars_valid, colnames(cor_mat))
    if (length(vars_in_cor) == 0) {
      shiny::showNotification("No hay variables válidas para calcular correlaciones con ME.", type = "error")
      return(NULL)
    }

    cor_with_me <- cor_mat["margen", vars_in_cor, drop = FALSE]

    # Guardar datos procesados para usar en VIF
    rv$cor_data <- list(
      df = df,
      vars_valid = vars_valid,
      vars_in_cor = vars_in_cor,
      df_cor = df_cor
    )

    # Actualizar choices para selección de variables
    shiny::updateCheckboxGroupInput(session, "selected_vars",
                                    choices = vars_in_cor,
                                    selected = character(0))

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
                          color = "black", size = 4, fontface = "bold") +
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

    # Calcular VIF para detectar multicolinealidad
    output$vif_table <- DT::renderDT({
      # Usar datos procesados de correlaciones
      if (is.null(rv$cor_data)) {
        return(DT::datatable(data.frame(Variable = "Sin datos", VIF = "Calcule correlaciones primero"),
                            options = list(dom = "t", paging = FALSE)))
      }

      cor_data <- rv$cor_data
      vars_in_cor <- cor_data$vars_in_cor
      df_vif_model <- cor_data$df_cor

      if (length(vars_in_cor) < 2) {
        return(DT::datatable(data.frame(Variable = "Insuficientes variables", VIF = "Se necesitan al menos 2 variables"),
                            options = list(dom = "t", paging = FALSE)))
      }

      if (nrow(df_vif_model) < 3) {
        return(DT::datatable(data.frame(Variable = "Pocos casos", VIF = "Insuficientes casos completos para VIF"),
                            options = list(dom = "t", paging = FALSE)))
      }

      tryCatch({
        # Crear fórmula sin margen usando las mismas variables que en correlaciones
        form_str <- paste("margen ~", paste(vars_in_cor, collapse = " + "))
        fit_vif <- stats::lm(stats::as.formula(form_str), data = df_vif_model)
        vif_values <- car::vif(fit_vif)
        vif_df <- data.frame(Variable = names(vif_values), VIF = as.numeric(vif_values))
        DT::datatable(vif_df, options = list(dom = "t", paging = FALSE)) |>
          DT::formatRound("VIF", 2)
      }, error = function(e) {
        # Si hay error en VIF, mostrar mensaje
        DT::datatable(data.frame(Variable = "Error", VIF = paste("Error calculando VIF:", e$message)),
                      options = list(dom = "t", paging = FALSE))
      })
    })
  }, ignoreInit = TRUE)

  # ----------------------------
  # Confirmar selección de variables
  # ----------------------------
  observeEvent(input$confirmar_seleccion, {
    selected <- input$selected_vars
    shiny::validate(shiny::need(length(selected) >= 1, "Selecciona al menos una variable."))
    rv$selected_vars <- selected
    shiny::showNotification(sprintf("Variables seleccionadas: %s", paste(selected, collapse = ", ")), type = "message")
    shiny::updateTabsetPanel(session, "tabs", selected = "Análisis Modelo Regresión Lineal")
  })

  # ----------------------------
  # Modelo lineal (manual)
  # ----------------------------
  modelo_manual <- shiny::reactiveVal(NULL)

  observeEvent(input$ajustar_modelo, {
    df   <- base_df()
    vars <- rv$selected_vars %||% input$vars_numeric
    shiny::validate(shiny::need(length(vars) >= 1, "Selecciona variables para el modelo."))

    # Debug: Check data validity
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos disponibles para entrenar el modelo."))
    shiny::validate(shiny::need("margen" %in% names(df), "La variable respuesta margen no está disponible."))

    # Check for complete cases
    model_vars <- c("margen", vars)
    complete_cases <- complete.cases(df[, model_vars, drop = FALSE])
    shiny::validate(shiny::need(sum(complete_cases) > 0,
      sprintf("No hay casos completos para modelar. Verifica que las variables seleccionadas no tengan valores faltantes.")))

    # Use only complete cases
    df_model <- df[complete_cases, model_vars, drop = FALSE]
    shiny::validate(shiny::need(nrow(df_model) > 1, "Insuficientes casos completos para el modelo."))

    form_str <- paste("margen ~", paste(vars, collapse = " + "))
    fit      <- stats::lm(stats::as.formula(form_str), data = df_model)
    modelo_manual(fit)

    output$model_summary <- shiny::renderPrint({ summary(fit) })

    output$model_metrics <- DT::renderDT({
      summ <- summary(fit)
      mse <- mean(residuals(fit)^2)
      metrics_df <- data.frame(
        Métrica = c("R²", "R² Ajustado", "MSE", "F-statistic", "p-valor F"),
        Valor = c(
          round(summ$r.squared, 4),
          round(summ$adj.r.squared, 4),
          round(mse, 2),
          round(summ$fstatistic[1], 2),
          format.pval(pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE), digits = 4)
        )
      )
      DT::datatable(metrics_df, options = list(dom = "t", paging = FALSE))
    })

    # Tabla de coeficientes para análisis detallado
    output$coef_table <- DT::renderDT({
      summ <- summary(fit)
      coef_df <- as.data.frame(summ$coefficients)
      colnames(coef_df) <- c("Coeficiente", "Error Estándar", "t-value", "p-valor")
      coef_df$Variable <- rownames(coef_df)
      coef_df <- coef_df[, c("Variable", "Coeficiente", "Error Estándar", "t-value", "p-valor")]
      coef_df$Significativo <- coef_df$`p-valor` < input$alpha

      # Actualizar checkbox de variables a mantener
      shiny::updateCheckboxGroupInput(session, "keep_vars",
                                      choices = setdiff(rownames(coef_df), "(Intercept)"),
                                      selected = setdiff(rownames(coef_df)[coef_df$Significativo], "(Intercept)"))

      DT::datatable(coef_df, options = list(dom = "t", paging = FALSE)) |>
        DT::formatRound(c("Coeficiente", "Error Estándar", "t-value"), 4) |>
        DT::formatSignif("p-valor", 4) |>
        DT::formatStyle("Significativo", target = "row",
                        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da")))
    })

  }, ignoreInit = TRUE)

  # ----------------------------
  # Validar significancia del modelo
  # ----------------------------
  observeEvent(input$validar_significancia, {
    fit <- modelo_manual()
    shiny::req(fit)

    summ <- summary(fit)
    p_f <- pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE)
    alpha <- input$alpha
    is_significant <- p_f < alpha

    student_choice <- input$model_significance == "significant"

    correct <- student_choice == is_significant

    feedback <- if (correct) {
      shiny::HTML("<div class='alert alert-success'>¡Correcto! El modelo es ", ifelse(is_significant, "significativo", "no significativo"),
                  " (p-valor F = ", format.pval(p_f, digits = 4), ").</div>")
    } else {
      shiny::HTML("<div class='alert alert-warning'>Incorrecto. El modelo es ", ifelse(is_significant, "significativo", "no significativo"),
                  " (p-valor F = ", format.pval(p_f, digits = 4), "). Vuelve a la pestaña de correlaciones y selecciona variables diferentes.</div>")
    }

    output$significance_feedback <- shiny::renderUI({
      feedback
    })
  })

  # ----------------------------
  # Confirmar modelo final
  # ----------------------------
  observeEvent(input$confirmar_modelo_final, {
    shiny::req(input$modelo_final)
    modelo_elegido <- input$modelo_final
    shiny::showNotification(sprintf("Modelo final confirmado: %s. Puedes proceder a simulación y predicción.", modelo_elegido), type = "message")
    shiny::updateTabsetPanel(session, "tabs", selected = "Predicción")
  })

  # ----------------------------
  # Reentrenar modelo con variables seleccionadas
  # ----------------------------
  observeEvent(input$retrain_model, {
    df <- base_df()
    vars_keep <- input$keep_vars
    shiny::validate(shiny::need(length(vars_keep) >= 1, "Selecciona al menos una variable para reentrenar."))

    # Debug: Check data validity
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos disponibles para reentrenar el modelo."))
    shiny::validate(shiny::need("margen" %in% names(df), "La variable respuesta margen no está disponible."))

    # Check for complete cases
    model_vars <- c("margen", vars_keep)
    complete_cases <- complete.cases(df[, model_vars, drop = FALSE])
    shiny::validate(shiny::need(sum(complete_cases) > 0,
      sprintf("No hay casos completos para reentrenar. Verifica que las variables seleccionadas no tengan valores faltantes.")))

    # Use only complete cases
    df_model <- df[complete_cases, model_vars, drop = FALSE]
    shiny::validate(shiny::need(nrow(df_model) > 1, "Insuficientes casos completos para reentrenar el modelo."))

    form_str <- paste("margen ~", paste(vars_keep, collapse = " + "))
    fit_new <- stats::lm(stats::as.formula(form_str), data = df_model)
    modelo_manual(fit_new)

    output$model_final_summary <- shiny::renderPrint({ summary(fit_new) })

    # Actualizar métricas del modelo reentrenado
    output$model_metrics <- DT::renderDT({
      summ <- summary(fit_new)
      mse <- mean(residuals(fit_new)^2)
      metrics_df <- data.frame(
        Métrica = c("R²", "R² Ajustado", "MSE", "F-statistic", "p-valor F"),
        Valor = c(
          round(summ$r.squared, 4),
          round(summ$adj.r.squared, 4),
          round(mse, 2),
          round(summ$fstatistic[1], 2),
          format.pval(pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3], lower.tail = FALSE), digits = 4)
        )
      )
      DT::datatable(metrics_df, options = list(dom = "t", paging = FALSE))
    })

    shiny::showNotification("Modelo reentrenado con variables seleccionadas", type = "message")
  }, ignoreInit = TRUE)


  # ----------------------------
  # Modelo automático (stepAIC)
  # ----------------------------
  modelo_auto <- shiny::reactiveVal(NULL)

  observeEvent(input$ajustar_auto, {
    # Mostrar indicador de progreso
    progress <- shiny::Progress$new()
    progress$set(message = "Generando modelo automático...", value = 0)
    on.exit(progress$close())

    df <- base_df()
    progress$set(value = 0.1, detail = "Verificando datos...")

    # VALIDACIÓN: Verificar datos básicos
    if (is.null(df) || nrow(df) == 0) {
      shiny::showNotification("No hay datos disponibles para entrenar el modelo automático.", type = "error")
      return(NULL)
    }
    if (!"margen" %in% names(df)) {
      shiny::showNotification("La variable respuesta margen no está disponible.", type = "error")
      return(NULL)
    }

    # VALIDACIÓN: Suficientes datos para modelado
    n_datos <- nrow(df)
    if (n_datos < 10) {
      shiny::showNotification(
        sprintf("Insuficientes datos para modelado (%d observaciones). Se necesitan al menos 10.", n_datos),
        type = "error"
      )
      return(NULL)
    }

    # 1. Identificar variables numéricas con suficientes datos no-NA (>80%)
    progress$set(value = 0.2, detail = "Seleccionando variables...")
    num_vars <- names(df)[vapply(df, is.numeric, TRUE)]
    num_vars <- setdiff(num_vars, "margen")

    if (length(num_vars) == 0) {
      shiny::showNotification("No hay variables predictoras numéricas disponibles.", type = "error")
      return(NULL)
    }

    # Debug: Mostrar variables disponibles
    message(sprintf("Variables numéricas disponibles: %s", paste(num_vars, collapse = ", ")))

    # Filtrar variables con >80% de datos no-NA y limitar a máximo 10 para velocidad
    vars_valid <- num_vars[sapply(num_vars, function(v) {
      completeness <- mean(!is.na(df[[v]]))
      message(sprintf("Variable %s: %.1f%% completa", v, completeness * 100))
      completeness > 0.8
    })]

    message(sprintf("Variables válidas (>80%% completas): %s", paste(vars_valid, collapse = ", ")))

    # Limitar a las 10 variables más correlacionadas con ME para optimizar velocidad
    if (length(vars_valid) > 10) {
      cor_with_me <- sapply(vars_valid, function(v) {
        cor_val <- tryCatch(abs(cor(df[[v]], df$margen, use = "complete.obs")), error = function(e) 0)
        message(sprintf("Correlación %s-margen: %.3f", v, cor_val))
        cor_val
      })
      vars_valid <- names(sort(cor_with_me, decreasing = TRUE))[1:10]
      message(sprintf("Variables seleccionadas (top 10 por correlación): %s", paste(vars_valid, collapse = ", ")))
    }

    if (length(vars_valid) < 2) {
      shiny::showNotification("Insuficientes variables con datos completos (>80% no-NA).", type = "error")
      return(NULL)
    }

    # 2. Usar solo casos completos para variables seleccionadas
    progress$set(value = 0.3, detail = "Preparando datos...")
    model_vars <- c("margen", vars_valid)
    complete_cases <- complete.cases(df[, model_vars, drop = FALSE])
    df_model <- df[complete_cases, model_vars, drop = FALSE]

    if (nrow(df_model) < 10) {
      shiny::showNotification(sprintf("Insuficientes casos completos (%d). Se necesitan al menos 10.", nrow(df_model)), type = "error")
      return(NULL)
    }

    # 3. Proceder con stepAIC optimizado
    progress$set(value = 0.5, detail = "Entrenando modelo...")
    tryCatch({
      full_form <- stats::as.formula(paste("margen ~", paste(vars_valid, collapse = " + ")))
      full_fit  <- stats::lm(full_form, data = df_model)

      # Optimizar stepAIC: usar dirección backward primero, luego both
      progress$set(value = 0.7, detail = "Optimizando variables...")
      auto_fit  <- MASS::stepAIC(full_fit, direction = "backward", trace = FALSE, steps = 5)
      auto_fit  <- MASS::stepAIC(auto_fit, direction = "both", trace = FALSE, steps = 3)

      modelo_auto(auto_fit)

      output$auto_summary <- shiny::renderPrint({ summary(auto_fit) })

      progress$set(value = 0.9, detail = "Evaluando modelo...")

      # Hold-out simplificado (solo para el modelo automático)
      set.seed(123)
      n   <- nrow(df_model)
      idx <- sample.int(n, size = floor(0.3*n))
      tr  <- df_model[-idx, ]; te <- df_model[idx, ]

      rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

      metrics_df   <- data.frame(Modelo = character(), RMSE = numeric(), R2_adj = numeric(), stringsAsFactors = FALSE)
      metrics_list <- list()

      # Evaluar modelo automático
      auto_fit_tr <- stats::lm(stats::formula(auto_fit), data = tr)
      pred_auto   <- predict(auto_fit_tr, newdata = te)
      rmse_auto <- rmse(te$margen, pred_auto)
      r2_auto   <- summary(auto_fit_tr)$adj.r.squared
      metrics_df <- rbind(metrics_df, data.frame(Modelo = "Automático", RMSE = rmse_auto, R2_adj = r2_auto))
      auto_vars <- attr(terms(auto_fit), "term.labels")
      metrics_list$auto <- list(rmse = rmse_auto, r2_adj = r2_auto, alpha = input$alpha, vars = auto_vars %||% character(0))

      # Evaluar modelo manual si existe
      if (!is.null(modelo_manual())) {
        manual_fit_tr <- stats::lm(stats::formula(modelo_manual()), data = tr)
        pred_man <- predict(manual_fit_tr, newdata = te)
        rmse_man <- rmse(te$margen, pred_man)
        r2_man   <- summary(manual_fit_tr)$adj.r.squared
        metrics_df <- rbind(metrics_df, data.frame(Modelo = "Manual", RMSE = rmse_man, R2_adj = r2_man))
        metrics_list$manual <- list(rmse = rmse_man, r2_adj = r2_man, alpha = input$alpha, vars = input$vars_numeric %||% character(0))
      }

      rv$metrics <- metrics_list

      output$comp_table <- DT::renderDT({
        DT::datatable(metrics_df, options = list(dom = "t", paging = FALSE)) |>
          DT::formatRound(c("RMSE","R2_adj"), 4)
      })

      progress$set(value = 1.0, detail = "Completado")
      shiny::showNotification(sprintf("Modelo automático creado exitosamente con %d variables y %d casos.",
                                    length(vars_valid), nrow(df_model)), type = "message")

    }, error = function(e) {
      shiny::showNotification(paste("Error al crear modelo automático:", conditionMessage(e)), type = "error")
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
      tmp$tasa   <- r
      tmp$monto_oferta <- monto
      tmp$plazo   <- plazo

      # Usar score_buro si existe, sino score
      if ("score_buro" %in% names(tmp)) {
        tmp$score_buro <- score
      } else if ("score" %in% names(tmp)) {
        tmp$score <- score
      }

      # Usar cluster_id si existe, sino cluster
      if ("cluster_id" %in% names(tmp)) {
        tmp$cluster_id <- cluster_val
      } else if ("cluster" %in% names(tmp)) {
        tmp$cluster <- cluster_val
      }

      # Calcular ME consistente (no usado en simulación actual)
      ingreso_bruto <- tmp$tasa * tmp$amount * (tmp$plazo/12)

      # Usar p_accept y p_mora disponibles (sin sufijos)
      p_accept_val <- if ("p_accept" %in% names(tmp) && !is.na(tmp$p_accept)) tmp$p_accept else 0.5
      p_mora_val <- if ("p_mora" %in% names(tmp) && !is.na(tmp$p_mora)) tmp$p_mora else 0.1

      base_me <- ingreso_bruto * p_accept_val * (1 - p_mora_val)

      # Ajuste directo para correlaciones usando las columnas disponibles
      s_buro    <- if ("score_buro" %in% names(tmp) && !is.na(tmp$score_buro)) scale(tmp$score_buro) else 0
      ingreso   <- if ("ingreso_verificado" %in% names(tmp) && !is.na(tmp$ingreso_verificado)) scale(tmp$ingreso_verificado) else 0
      moras_prev<- if ("n_moras_previas" %in% names(tmp) && !is.na(tmp$n_moras_previas)) scale(tmp$n_moras_previas) else 0
      rfm       <- if ("rfm" %in% names(tmp) && !is.na(tmp$rfm)) scale(tmp$rfm) else 0

      me_adjustment <- 1500 * (0.5 * s_buro + 0.6 * ingreso - 0.4 * moras_prev + 0.3 * rfm)
      tmp$ME <- base_me + me_adjustment

      tmp
    }))

    me_hat <- as.numeric(predict(m_final, newdata = df_sim))

    coefs <- stats::coef(m_final)
    dME_dr <- numeric(length(r_seq))
    for (i in seq_along(r_seq)) {
      r <- r_seq[i]; d <- 0
      if ("tasa" %in% names(coefs))             d <- d + coefs["tasa"]
      if ("I(tasa^2)" %in% names(coefs))        d <- d + 2*coefs["I(tasa^2)"]*r
      if ("tasa:score" %in% names(coefs))       d <- d + coefs["tasa:score"]*score
      if ("tasa:score_buro" %in% names(coefs))  d <- d + coefs["tasa:score_buro"]*score
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
  # Predicción con nuevos datos
  # ----------------------------
  observeEvent(input$predecir, {
    m_final <- switch(input$modelo_final,
                      manual = modelo_manual(),
                      automatico = modelo_auto())
    shiny::req(m_final)

    # Crear data frame con los datos del cliente
    new_client <- data.frame(
      score_buro = input$pred_score_buro,
      ingreso_verificado = input$pred_ingreso,
      n_moras_previas = input$pred_moras,
      rfm = input$pred_rfm,
      tasa = input$pred_tasa,
      monto_oferta = input$pred_monto,
      plazo = input$pred_plazo,
      p_accept = input$pred_p_accept,
      p_mora = input$pred_p_mora
    )

    # Predicción usando el modelo
    pred_me <- predict(m_final, newdata = new_client)

    output$pred_result <- shiny::renderPrint({
      cat("Datos del cliente:\n")
      cat(sprintf("Score buro: %.0f\n", new_client$score_buro))
      cat(sprintf("Ingreso verificado: %.0f\n", new_client$ingreso_verificado))
      cat(sprintf("N° moras previas: %.0f\n", new_client$n_moras_previas))
      cat(sprintf("RFM: %.0f\n", new_client$rfm))
      cat("\n")
      cat(sprintf("Margen histórico predicho por modelo: %.2f\n", pred_me))
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
