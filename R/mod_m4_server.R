# R/mod_m4_server.R
# Server del Módulo 4 – Árboles de Clasificación

mod_m4_server <- function(input, output, session, datos_reactivos, id_sim, execution_mode = reactive("sequential")) {

  ns <- session$ns

  # -------------------------
  # Estado reactivo del módulo
  # -------------------------
  rv <- reactiveValues(
    # Dataset histórico (train/test)
    df_historico = NULL,
    train_data = NULL,
    test_data = NULL,

    # Modelos
    tree_model = NULL,
    pruned_model = NULL,
    poda_info = NULL,

    # Resultados
    metrics = NULL,
    test_predictions = NULL,
    n3_predictions = NULL,

    # UI state
    modelo_entrenado = FALSE,
    poda_aplicada = FALSE,
    nodo_aleatorio = NULL,
    ejercicio_actual = NULL
  )

  # -------------------------
  # Helper: Obtener datos base
  # -------------------------
  .get_base_df_m4 <- function(d) {
    if (is.data.frame(d)) return(d)
    if (!is.null(d$base)) return(d$base)
    keys <- c("demograficas","financieras","comp_historico","clientes","post_desembolso")
    tabs <- tryCatch(Filter(Negate(is.null), d[keys]), error = function(e) list())
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)
  }

  # -------------------------
  # Preparar dataset histórico (interno)
  # -------------------------
  prepare_historic_data <- reactive({
    # En modo independiente, usar datos históricos simulados completos
    if (execution_mode() == "independent") {
      tryCatch({
        df <- generate_historic_data_m4(n_clientes = 2000, seed = 101112)
        showNotification("Modo independiente: Usando datos históricos simulados para M4", type = "info", duration = 3)
      }, error = function(e) {
        showNotification("❌ Error generando datos históricos simulados", type = "error")
        return(NULL)
      })
    } else {
      # Usar datos simulados como "históricos"
      df <- .get_base_df_m4(datos_reactivos())

      # Asegurar que id_cliente sea character
      df$id_cliente <- as.character(df$id_cliente)

      # Crear variable dependiente de alerta de riesgo
      tryCatch({
        df$alerta_riesgo <- .create_alerta_riesgo(df)
      }, error = function(e) {
        # Fallback si .create_alerta_riesgo no está disponible
        df$alerta_riesgo <- factor(ifelse(df$score_buro < 500 | df$n_moras_previas > 2, "alto",
                                         ifelse(df$score_buro < 700 | df$n_moras_previas > 0, "medio", "bajo")),
                                  levels = c("bajo", "medio", "alto"))
      })
    }

    # Filtrar columnas relevantes - incluir todas las variables predictoras disponibles
    # para que el usuario pueda seleccionarlas
    all_predictor_vars <- c(
      "edad","estado_civil","ubicacion","nivel_educativo",
      "tipo_ocupacion","rubro_laboral","n_dependientes",
      "antiguedad_cliente","n_moras_previas","dias_atraso_max",
      "productos_activos","frecuencia_uso","cancelaciones_anticip",
      "rfm","ingreso_declarado","ingreso_verificado","capacidad_endeud",
      "endeudamiento_total","score_buro","tendencia_ingresos"
    )

    vars_disponibles <- c("id_cliente", all_predictor_vars, "alerta_riesgo")
    df <- df[, intersect(vars_disponibles, names(df)), drop = FALSE]

    # Remover filas con NA
    df <- na.omit(df)

    # VALIDACIÓN: Asegurar que tenemos suficientes datos
    if (nrow(df) < 100) {
      showNotification("Datos insuficientes para M4. Se necesitan al menos 100 observaciones.", type = "error")
      return(NULL)
    }

    # VALIDACIÓN: Asegurar que alerta_riesgo existe y tiene variación
    if (!"alerta_riesgo" %in% names(df)) {
      showNotification("Variable dependiente 'alerta_riesgo' no encontrada.", type = "error")
      return(NULL)
    }

    if (length(unique(df$alerta_riesgo)) < 2) {
      showNotification("La variable 'alerta_riesgo' no tiene suficiente variación.", type = "error")
      return(NULL)
    }

    rv$df_historico <- df
    df
  })

  # -------------------------
  # Entrenar modelo (Paso 1)
  # -------------------------
  observeEvent(input$entrenar_modelo, {
    # Mostrar progreso
    progress <- shiny::Progress$new()
    progress$set(message = "Entrenando modelo...", value = 0.1)
    on.exit(progress$close())

    # Preparar datos históricos con las variables seleccionadas
    progress$set(value = 0.2, detail = "Preparando datos...")
    df_hist <- prepare_historic_data()

    # VALIDACIÓN: Verificar que prepare_historic_data retornó datos válidos
    if (is.null(df_hist) || nrow(df_hist) < 100) {
      showNotification("❌ Error en preparación de datos históricos. Verifica la configuración.", type = "error")
      return(NULL)
    }

    # En modo independiente, actualizar las opciones de variables disponibles
    if (execution_mode() == "independent") {
      available_vars <- names(df_hist)[!names(df_hist) %in% c("id_cliente", "alerta_riesgo")]
      updateCheckboxGroupInput(session, "vars_predictoras",
                              choices = available_vars,
                              selected = intersect(input$vars_predictoras, available_vars))
    }

    # Verificar que las variables seleccionadas están disponibles
    selected_vars <- intersect(input$vars_predictoras, names(df_hist))
    if (length(selected_vars) == 0) {
      showNotification("❌ Ninguna de las variables seleccionadas está disponible en los datos.", type = "error")
      return(NULL)
    }

    val <- validar_variables(selected_vars)
    if(!val$ok){
      showNotification(val$msg, type = "error")
      return(invisible(NULL))
    }

    # VALIDACIÓN: Asegurar que tenemos la variable dependiente
    if (!input$var_dependiente %in% names(df_hist)) {
      showNotification(sprintf("Variable dependiente '%s' no encontrada en los datos.", input$var_dependiente), type = "error")
      return(NULL)
    }

    # Crear dataset solo con variables seleccionadas + target
    vars_para_modelo <- c("id_cliente", selected_vars, input$var_dependiente)
    df_modelo <- df_hist[, vars_para_modelo, drop = FALSE]

    # VALIDACIÓN: Verificar que no hay NAs en el dataset final
    if (any(is.na(df_modelo))) {
      showNotification("Hay valores faltantes en el dataset. Limpiando datos...", type = "warning")
      df_modelo <- na.omit(df_modelo)
      if (nrow(df_modelo) < 100) {
        showNotification("Después de remover NAs, quedan muy pocos datos.", type = "error")
        return(NULL)
      }
    }

    # Dividir en train (80%) y test (20%)
    set.seed(123)
    train_idx <- sample(1:nrow(df_modelo), size = 0.8 * nrow(df_modelo))
    rv$train_data <- df_modelo[train_idx, ]
    rv$test_data <- df_modelo[-train_idx, ]

    # VALIDACIÓN: Verificar que tenemos datos en train y test
    if (nrow(rv$train_data) == 0 || nrow(rv$test_data) == 0) {
      showNotification("Error al dividir datos en train/test.", type = "error")
      return(NULL)
    }

    # Entrenar árbol con las variables verificadas
    progress$set(value = 0.5, detail = "Entrenando modelo...")
    tryCatch({
      rv$tree_model <- train_tree(rv$train_data, selected_vars, input$var_dependiente)
      if (is.null(rv$tree_model)) {
        showNotification("❌ Error al entrenar el modelo de árbol.", type = "error")
        return(NULL)
      }

      # VALIDACIÓN: Verificar que el modelo se entrenó correctamente
      if (is.null(rv$tree_model$frame) || nrow(rv$tree_model$frame) == 0) {
        showNotification("❌ El modelo entrenado no tiene estructura válida.", type = "error")
        return(NULL)
      }

      # Verificar que hay al menos algunos nodos terminales
      n_terminal <- sum(rv$tree_model$frame$var == "<leaf>")
      if (n_terminal == 0) {
        showNotification("❌ El modelo no generó nodos terminales.", type = "error")
        return(NULL)
      }

    }, error = function(e) {
      showNotification(sprintf("❌ Error en train_tree: %s", substr(e$message, 1, 100)), type = "error")
      return(NULL)
    })

    progress$set(value = 0.9, detail = "Finalizando...")

    rv$modelo_entrenado <- TRUE

    output$mensaje_entrenamiento <- renderUI({
      # Obtener información del modelo entrenado
      n_vars_used <- length(unique(rv$tree_model$frame$var[rv$tree_model$frame$var != "<leaf>"]))
      n_terminal <- sum(rv$tree_model$frame$var == "<leaf>")

      div(class = "alert alert-success",
          h4("✅ Modelo Entrenado Exitosamente"),
          p(sprintf("• Observaciones de entrenamiento: %d", nrow(rv$train_data))),
          p(sprintf("• Observaciones de prueba: %d", nrow(rv$test_data))),
          p(sprintf("• Variables seleccionadas por usuario: %d", length(selected_vars))),
          p(sprintf("• Variables efectivamente usadas en el modelo: %d", n_vars_used)),
          p(sprintf("• Nodos terminales generados: %d", n_terminal)),
          hr(),
          strong("Variables usadas en el modelo: "), paste(selected_vars, collapse = ", "))
    })

    progress$set(value = 1.0, detail = "Completado")
    showNotification("✅ Modelo entrenado exitosamente. Procede a interpretar los nodos.", type = "message")
    updateTabsetPanel(session, "tabs", selected = "Interpretación de Nodos")
  })

  # -------------------------
  # Tab 2: Interpretación de Nodos
  # -------------------------

  # Visualización del árbol
  output$plot_arbol <- renderPlot({
    req(rv$tree_model)

    # VALIDACIÓN: Verificar que el modelo tiene la estructura esperada
    if (is.null(rv$tree_model$frame) || nrow(rv$tree_model$frame) == 0) {
      plot.new()
      text(0.5, 0.5, "Modelo de árbol no válido o vacío", cex = 1.2)
      return()
    }

    tryCatch({
      # MOSTRAR SIEMPRE EL ÁRBOL ORIGINAL PRIMERO
      # Solo mostrar podado si ya fue aplicado
      if (!is.null(rv$pruned_model) && rv$poda_aplicada) {
        rpart.plot::rpart.plot(rv$pruned_model, main = "Árbol de Clasificación Podado",
                              extra = 104, box.palette = "RdYlGn", shadow.col = "gray", roundint = FALSE)
      } else {
        rpart.plot::rpart.plot(rv$tree_model, main = "Árbol de Clasificación Original",
                              extra = 104, box.palette = "RdYlGn", shadow.col = "gray", roundint = FALSE)
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error al graficar árbol:\n", substr(e$message, 1, 100)), cex = 1.0)
    })
  })

  # Validar pregunta teórica
  observeEvent(input$validar_pregunta, {
    correcta <- input$pregunta_nodo == "prediccion"
    feedback <- if (correcta) {
      "¡Correcto! Un nodo terminal representa la predicción final de clase."
    } else {
      "Incorrecto. Un nodo terminal contiene la predicción final de clase para un subconjunto de observaciones."
    }

    output$feedback_pregunta <- renderUI({
      div(class = if (correcta) "alert alert-success" else "alert alert-warning", feedback)
    })
  })

  # Validar interpretación de gráfico de poda
  observeEvent(input$validar_grafico, {
    req(rv$tree_model)

    cv_results <- rpart::printcp(rv$tree_model)
    optimal_point <- which.min(cv_results[, "xerror"]) + 1

    # Verificar si menciona el punto óptimo y explica por qué
    mencion_optimo <- grepl(as.character(optimal_point), input$interpretacion_grafico) ||
                     grepl("óptimo|punto mínimo|donde error deja", tolower(input$interpretacion_grafico))
    explica_razon <- grepl("error|disminuir|mejor|óptimo", tolower(input$interpretacion_grafico))

    correcta <- mencion_optimo && explica_razon

    feedback <- if (correcta) {
      paste0("¡Excelente interpretación! El punto óptimo está en ", optimal_point,
             " nodos donde el error de validación cruzada es mínimo.")
    } else {
      paste0("Revisa el gráfico. El punto óptimo está donde el error deja de disminuir significativamente (alrededor de ",
             optimal_point, " nodos). Después de ese punto, agregar más nodos no mejora el rendimiento.")
    }

    output$feedback_grafico <- renderUI({
      div(class = if (correcta) "alert alert-success" else "alert alert-info", feedback)
    })
  })

  # Validar interpretación de nodo
  observeEvent(input$guardar_interpretacion, {
    req(rv$nodo_aleatorio)

    node <- rv$nodo_aleatorio
    clase <- c("bajo", "medio", "alto")[node$yval]

    # Interpretación esperada básica
    interpretacion_correcta <- grepl(tolower(clase), tolower(input$interpretacion_nodo)) &&
                              (grepl("predice|predicción|clasifica", tolower(input$interpretacion_nodo)) ||
                               grepl("riesgo", tolower(input$interpretacion_nodo)))

    feedback <- if (interpretacion_correcta) {
      paste0("¡Buena interpretación! El nodo predice riesgo ", clase,
             " para el ", node$n, "% de las observaciones que llegan a él.")
    } else {
      paste0("Revisa tu interpretación. El nodo predice riesgo ", clase,
             ". Considera mencionar qué tipo de riesgo predice y qué porcentaje de observaciones representa.")
    }

    output$feedback_interpretacion <- renderUI({
      div(class = if (interpretacion_correcta) "alert alert-success" else "alert alert-info", feedback)
    })
  })

  # Mostrar información de nodo aleatorio
  output$info_nodo_aleatorio <- renderUI({
    req(rv$tree_model)

    if (is.null(rv$nodo_aleatorio)) {
      rv$nodo_aleatorio <- select_random_node(rv$tree_model)
    }

    if (is.null(rv$nodo_aleatorio)) return(NULL)

    node <- rv$nodo_aleatorio
    clase <- c("bajo", "medio", "alto")[node$yval]

    div(
      h5("Información del Nodo Seleccionado:"),
      p(strong("ID del Nodo:"), node$node_id),
      p(strong("Clase Predicha:"), clase),
      p(strong("Número de observaciones:"), node$n),
      p(strong("Regla:"), node$rule)
    )
  })

  # -------------------------
  # Tab 3: Poda del Árbol
  # -------------------------

  # Curva de error vs tamaño
  output$plot_error_vs_size <- renderPlot({
    req(rv$tree_model)

    cv_results <- rpart::printcp(rv$tree_model)
    plot(cv_results[, "nsplit"] + 1, cv_results[, "xerror"],
         type = "b", xlab = "Tamaño del Árbol (nodos terminales)",
         ylab = "Error de Validación Cruzada", main = "Error vs Tamaño del Árbol")
    abline(v = which.min(cv_results[, "xerror"]) + 1, col = "red", lty = 2)
    text(which.min(cv_results[, "xerror"]) + 1, min(cv_results[, "xerror"]),
         "Tamaño Óptimo", pos = 4, col = "red")
  })

  # Información de poda
  output$info_poda <- renderUI({
    req(rv$tree_model)

    cv_results <- rpart::printcp(rv$tree_model)
    optimal_size <- which.min(cv_results[, "xerror"]) + 1
    current_size <- sum(rv$tree_model$frame$var == "<leaf>")

    div(
      p(strong("Tamaño actual del árbol:"), current_size, "nodos terminales"),
      p(strong("Tamaño óptimo recomendado:"), optimal_size, "nodos terminales"),
      p("Aplicar poda reducirá la complejidad del modelo y puede mejorar su capacidad de generalización.")
    )
  })

  # Aplicar poda
  observeEvent(input$aplicar_poda, {
    req(rv$tree_model)

    poda_result <- prune_tree(rv$tree_model, rv$train_data)
    rv$pruned_model <- poda_result$pruned
    rv$poda_info <- poda_result

    rv$poda_aplicada <- TRUE

    showNotification("Poda aplicada exitosamente.", type = "message")
  })

  # Visualización árbol original
  output$plot_arbol_original <- renderPlot({
    req(rv$tree_model)
    tryCatch({
      rpart.plot::rpart.plot(rv$tree_model, main = "Árbol Original",
                            extra = 104, box.palette = "RdYlGn", shadow.col = "gray", roundint = FALSE)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error al graficar árbol original:\n", substr(e$message, 1, 100)), cex = 1.0)
    })
  })

  # Visualización árbol podado
  output$plot_arbol_podado <- renderPlot({
    req(rv$pruned_model)
    tryCatch({
      rpart.plot::rpart.plot(rv$pruned_model, main = "Árbol Podado",
                            extra = 104, box.palette = "RdYlGn", shadow.col = "gray", roundint = FALSE)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error al graficar árbol podado:\n", substr(e$message, 1, 100)), cex = 1.0)
    })
  })

  # Tabla comparación
  output$tabla_comparacion_arboles <- DT::renderDT({
    req(rv$tree_model, rv$pruned_model)

    original_size <- sum(rv$tree_model$frame$var == "<leaf>")
    pruned_size <- sum(rv$pruned_model$frame$var == "<leaf>")

    # Calcular accuracy en test set
    if (!is.null(rv$test_data)) {
      pred_original <- predict(rv$tree_model, rv$test_data, type = "class")
      pred_pruned <- predict(rv$pruned_model, rv$test_data, type = "class")

      acc_original <- mean(pred_original == rv$test_data$alerta_riesgo)
      acc_pruned <- mean(pred_pruned == rv$test_data$alerta_riesgo)
    } else {
      acc_original <- acc_pruned <- NA
    }

    df_comp <- data.frame(
      Árbol = c("Original", "Podado"),
      "Nodos Terminales" = c(original_size, pruned_size),
      "Accuracy Test" = c(acc_original, acc_pruned)
    )

    DT::datatable(df_comp, options = list(dom = "t", paging = FALSE)) %>%
      DT::formatRound("Accuracy Test", 3)
  })

  # Validar reflexiones
  observeEvent(input$validar_reflexiones, {
    req(rv$tree_model, rv$pruned_model)

    original_size <- sum(rv$tree_model$frame$var == "<leaf>")
    pruned_size <- sum(rv$pruned_model$frame$var == "<leaf>")
    nodos_eliminados <- original_size - pruned_size

    rendimiento_similar <- abs(input$pregunta_rendimiento == "si")
    nodos_correctos <- abs(input$nodos_eliminados == nodos_eliminados)
    ventaja_mencionada <- grepl("simple|interpretable|generaliza", tolower(input$ventaja_podado))

    score <- sum(c(rendimiento_similar, nodos_correctos, ventaja_mencionada))

    feedback <- paste0("Puntuación: ", score, "/3. ",
                      "Nodos eliminados: ", nodos_eliminados, ".")

    showNotification(feedback, type = "message")
  })

  # -------------------------
  # Tab 4: Matriz de Confusión y Métricas
  # -------------------------

  # Calcular métricas cuando se cambia el modelo
  observe({
    req(rv$pruned_model, rv$test_data)

    pred <- predict(rv$pruned_model, rv$test_data, type = "class")
    rv$test_predictions <- pred
    rv$metrics <- calculate_metrics(pred, rv$test_data$alerta_riesgo, input$umbral_clasificacion)
  })

  # Matriz de confusión
  output$plot_matriz_confusion <- renderPlot({
    req(rv$metrics)

    conf_mat <- rv$metrics$confusion_matrix
    ggplot2::ggplot(as.data.frame(conf_mat), ggplot2::aes(x = Actual, y = Predicted, fill = Freq)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 1) +
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::labs(title = "Matriz de Confusión", x = "Real", y = "Predicho") +
      ggplot2::theme_minimal()
  })

  # Tabla matriz de confusión
  output$tabla_matriz_confusion <- DT::renderDT({
    req(rv$metrics)
    DT::datatable(as.data.frame(rv$metrics$confusion_matrix),
                  options = list(dom = "t", paging = FALSE))
  })

  # Mostrar métricas actuales del modelo
  output$metricas_actuales <- renderUI({
    req(rv$metrics)

    div(
      h5("Métricas actuales del modelo:"),
      p(strong("Accuracy:"), round(rv$metrics$accuracy, 3)),
      p(strong("Sensibilidad:"), round(rv$metrics$macro_recall, 3)),
      p(strong("Especificidad:"), round(rv$metrics$macro_specificity, 3)),
      p(strong("F1-Score:"), round(rv$metrics$macro_f1, 3))
    )
  })

  # Validar respuesta métrica
  observeEvent(input$validar_metrica, {
    req(rv$metrics, input$metrica_interpretar)

    metrica_valor <- switch(input$metrica_interpretar,
                           "sensibilidad" = rv$metrics$macro_recall,
                           "especificidad" = rv$metrics$macro_specificity,
                           "accuracy" = rv$metrics$accuracy,
                           "f1" = rv$metrics$macro_f1)

    # Respuestas esperadas según la métrica
    respuestas_esperadas <- list(
      "sensibilidad" = c("detecta casos positivos", "identifica alto riesgo", "casos de riesgo alto"),
      "especificidad" = c("detecta casos negativos", "identifica bajo riesgo", "casos de riesgo bajo"),
      "accuracy" = c("predicciones correctas", "total correctas", "porcentaje correctas"),
      "f1" = c("balance precisión sensibilidad", "media armónica", "precisión y sensibilidad")
    )

    respuesta_correcta <- any(sapply(respuestas_esperadas[[input$metrica_interpretar]],
                                    function(palabra) grepl(tolower(palabra), tolower(input$respuesta_metrica))))

    feedback <- if (respuesta_correcta) {
      paste0("¡Buena interpretación! El valor de ", round(metrica_valor, 3),
             " indica que el modelo tiene un buen rendimiento en esta métrica.")
    } else {
      paste0("Revisa tu interpretación. Un valor de ", round(metrica_valor, 3),
             " para ", input$metrica_interpretar, " significa que el modelo...",
             switch(input$metrica_interpretar,
                   "sensibilidad" = "detecta correctamente esa proporción de casos de alto riesgo.",
                   "especificidad" = "identifica correctamente esa proporción de casos de bajo riesgo.",
                   "accuracy" = "realiza esa proporción de predicciones correctas en total.",
                   "f1" = "balancea precisión y sensibilidad con ese valor."))
    }

    output$feedback_metrica <- renderUI({
      div(class = if (respuesta_correcta) "alert alert-success" else "alert alert-info", feedback)
    })
  })

  # Pregunta Verdadero/Falso 1
  output$pregunta_vf_1 <- renderUI({
    preguntas_vf <- list(
      "Si aumenta el umbral de clasificación, la sensibilidad del modelo aumenta." = FALSE,
      "Si aumenta el número de Verdaderos Negativos (TN), la especificidad aumenta." = TRUE,
      "Un accuracy de 0.95 significa que el 95% de las predicciones son correctas." = TRUE,
      "La sensibilidad mide la capacidad de detectar casos positivos." = TRUE,
      "Si disminuye el umbral, aumenta la especificidad del modelo." = FALSE,
      "El F1-Score es útil cuando queremos balancear precisión y sensibilidad." = TRUE,
      "Un modelo con alta sensibilidad comete pocos Falsos Negativos." = TRUE,
      "La especificidad mide la capacidad de identificar casos negativos." = TRUE
    )

    pregunta <- sample(names(preguntas_vf), 1)
    rv$respuesta_vf_correcta_1 <- preguntas_vf[[pregunta]]

    div(
      h5("Pregunta 1 - Verdadero o Falso:"),
      p(pregunta)
    )
  })

  # Pregunta Verdadero/Falso 2
  output$pregunta_vf_2 <- renderUI({
    preguntas_vf <- list(
      "Si aumenta el umbral de clasificación, disminuye la sensibilidad." = TRUE,
      "Un aumento en Falsos Positivos (FP) mejora la especificidad." = FALSE,
      "La precisión mide la calidad de las predicciones positivas." = TRUE,
      "Un modelo con alta especificidad comete pocos Falsos Positivos." = TRUE,
      "Si disminuye el umbral, aumenta el número de Falsos Positivos." = TRUE,
      "El accuracy incluye tanto positivos como negativos correctamente clasificados." = TRUE,
      "La sensibilidad es igual al recall en problemas de clasificación binaria." = TRUE,
      "Un F1-Score de 1.0 indica un modelo perfecto." = TRUE
    )

    pregunta <- sample(names(preguntas_vf), 1)
    rv$respuesta_vf_correcta_2 <- preguntas_vf[[pregunta]]

    div(
      h5("Pregunta 2 - Verdadero o Falso:"),
      p(pregunta)
    )
  })

  # Validar V/F
  observeEvent(input$validar_vf, {
    correcta_1 <- (input$respuesta_vf_1 == "verdadero") == rv$respuesta_vf_correcta_1
    correcta_2 <- (input$respuesta_vf_2 == "verdadero") == rv$respuesta_vf_correcta_2

    score <- sum(correcta_1, correcta_2)

    feedback <- paste0("Puntuación: ", score, "/2. ",
                      if (correcta_1) "Pregunta 1 correcta. " else "Pregunta 1 incorrecta. ",
                      if (correcta_2) "Pregunta 2 correcta." else "Pregunta 2 incorrecta.")

    output$feedback_vf <- renderUI({
      div(class = if (score == 2) "alert alert-success" else
                  if (score == 1) "alert alert-warning" else "alert alert-danger",
          feedback)
    })
  })

  # -------------------------
  # Tab 5: Clasificación de Alertas
  # -------------------------

  # Clasificar datos N3 (simulados como nuevos)
  observe({
    req(rv$pruned_model)

    # Usar un subset de datos históricos como "N3" (nuevas observaciones)
    df_n3 <- rv$df_historico[sample(1:nrow(rv$df_historico), 100), ]

    rv$n3_predictions <- classify_new_data(rv$pruned_model, df_n3)
  })

  # Tabla de clasificación
  output$tabla_clasificacion <- DT::renderDT({
    req(rv$n3_predictions)

    df_display <- rv$n3_predictions[, c("id_cliente", "clase_predicha", "prob_bajo", "prob_medio", "prob_alto", "nivel_alerta")]
    colnames(df_display) <- c("ID Cliente", "Clase Predicha", "Prob Baja", "Prob Media", "Prob Alta", "Nivel Alerta")

    DT::datatable(df_display, options = list(pageLength = 10)) %>%
      DT::formatRound(c("Prob Baja", "Prob Media", "Prob Alta"), 3)
  })

  # Gráfico de pie
  output$plot_pie_clasificacion <- renderPlot({
    req(rv$n3_predictions)

    dist_clases <- table(rv$n3_predictions$clase_predicha)
    pie(dist_clases,
        main = "Distribución de Alertas de Riesgo",
        col = c("green", "yellow", "red"),
        labels = paste(names(dist_clases), "\n", dist_clases))
  })

  # -------------------------
  # Persistencia y finalización
  # -------------------------
  observeEvent(input$finalizar_modulo, {
    req(rv$pruned_model, rv$metrics, rv$n3_predictions)

    # Persistir evaluación
    persist_eval_m4(
      id_sim = id_sim,
      accuracy = rv$metrics$accuracy,
      macro_f1 = rv$metrics$macro_f1,
      n_nodos = sum(rv$pruned_model$frame$var == "<leaf>"),
      vars_usadas = paste(input$vars_predictoras, collapse = ",")
    )

    # Persistir clasificaciones N3
    persist_clasificaciones_m4(
      id_sim = id_sim,
      clasificaciones = rv$n3_predictions
    )

    showNotification("Módulo 4 completado y resultados guardados.", type = "message")
  })

}