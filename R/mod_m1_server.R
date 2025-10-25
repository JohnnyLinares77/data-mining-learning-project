# R/mod_m1_server.R
# Módulo 1 - Perfilamiento del Cliente
# Server: orquesta PCA, evaluación de K y clustering; muestra métricas y persiste resultados.

mod_m1_server <- function(input, output, session, datos_reactivos, id_sim, execution_mode = reactive("sequential")){

  ns <- session$ns

  # Helper function similar to M3's .get_base_df
  .get_base_df_m1 <- function(d){
    if (is.data.frame(d)) return(d)
    if (!is.null(d$base)) return(d$base)
    keys <- c("demograficas","financieras","comp_historico","clientes","post_desembolso","ofertas_historicas")
    tabs <- tryCatch(Filter(Negate(is.null), d[keys]), error = function(e) list())
    if (length(tabs) == 0) stop("No se encontraron tablas base en datos_reactivos().")
    Reduce(function(a,b) merge(a,b, by = "id_cliente", all = TRUE), tabs)
  }

  # -------------------------
  # Estado reactivo del módulo
  # -------------------------
  rv <- reactiveValues(
    # PCA
    pc = NULL,          # prcomp completo (todas las componentes)
    scores = NULL,      # matriz de scores reducidos (primeras ncomp)
    varianza = NULL,    # data.frame con var_exp y var_acum
    ncomp = NULL,       # n° componentes elegidas para clustering

    # Clustering
    clusters = NULL,
    silueta = NA_real_,
    wss = NA_real_,
    inertia = NA_real_,

    # Evaluación de rango de K
    kgrid = NULL,

    # Selección automática
    auto_selected_cluster = NULL,  # ID del cluster seleccionado automáticamente
    cluster_potential = NULL       # DataFrame con métricas de potencial por cluster
  )

  # ----------------------------------------------------------
  # Paso 1: Analizar Varianza -> calcula PCA completo y grafica
  # ----------------------------------------------------------
  observeEvent(input$analyze_var, {
    val <- validar_variables(input$vars)
    if(!val$ok){
      showNotification(val$msg, type = "error")
      return(invisible(NULL))
    }

    # Usar datos independientes si es modo independiente
    if (execution_mode() == "independent") {
      datos_para_usar <- generate_independent_data_m1(n_clientes = 1000, seed = 123)
      showNotification("Modo independiente: Usando datos simulados para M1", type = "info", duration = 3)
    } else {
      datos_para_usar <- datos_reactivos()
    }

    # Preprocesamiento según selección
    prep <- preprocess_inputs(
      df_list = datos_para_usar,
      vars_seleccionadas = input$vars
    )

    # PCA completo (todas las componentes)
    pc_obj <- prcomp(prep$X_final, center = FALSE, scale. = FALSE)
    var_exp  <- pc_obj$sdev^2 / sum(pc_obj$sdev^2)
    var_acum <- cumsum(var_exp)

    rv$pc       <- pc_obj
    rv$varianza <- data.frame(PC = seq_along(var_exp), var_exp = var_exp, var_acum = var_acum)

    # Reset de dependencias
    rv$scores <- NULL
    rv$ncomp  <- NULL
    rv$clusters <- NULL
    rv$silueta  <- NA_real_
    rv$wss      <- NA_real_
    rv$inertia  <- NA_real_
    rv$kgrid    <- NULL

    # Limpiar biplot (aún no se han elegido componentes)
    output$plot_pca <- renderPlot({})

    # Graficar Scree + Varianza acumulada
    output$plot_varianza <- renderPlot({
      v <- rv$varianza
      if(is.null(v)) return(invisible(NULL))
      par(mfrow = c(1,2))
      plot(v$PC, v$var_exp, type = "b",
           xlab = "Componente", ylab = "Varianza explicada",
           main = "Scree plot")
      plot(v$PC, v$var_acum, type = "b",
           xlab = "Componente", ylab = "Varianza acumulada",
           main = "Varianza acumulada")
      abline(h = 0.7, lty = 3); abline(h = 0.9, lty = 3)
      par(mfrow = c(1,1))
    })

    # Limpiar paneles de K/Clusters
    output$plot_elbow      <- renderPlot({})
    output$plot_sil_k      <- renderPlot({})
    output$plot_clusters   <- renderPlot({})
    output$tabla_metricas  <- DT::renderDT(NULL)
    output$feedback        <- renderUI(NULL)

    showNotification("PCA completo generado. Revise Scree/Varianza y elija Nº de componentes.", type = "message")
    updateTabsetPanel(session, inputId = "tabs", selected = "PCA")
  })

  # Utilidad: fijar scores según input$ncomp sobre el PCA ya calculado
  set_scores_from_ncomp <- function(){
    if(is.null(rv$pc)) return(FALSE)
    nmax <- ncol(rv$pc$x)
    nsel <- max(1L, min(input$ncomp, nmax))
    rv$scores <- rv$pc$x[, 1:nsel, drop = FALSE]
    rv$ncomp  <- nsel
    TRUE
  }

  # ----------------------------------------------------------
  # Paso 2: Ejecutar PCA (usar N componentes) -> biplot con scores reducidos
  # ----------------------------------------------------------
  observeEvent(input$run_pca, {
    if(is.null(rv$pc)){
      showNotification("Primero pulse 'Analizar Varianza'.", type = "warning")
      return(invisible(NULL))
    }
    ok <- set_scores_from_ncomp()
    if(!ok){
      showNotification("No hay componentes disponibles. Revise el PCA.", type = "error")
      return(invisible(NULL))
    }

  # Biplot con scores y loadings
  output$plot_pca <- renderPlot({
    # Reset gráfico parameters para evitar interferencia con otros plots
    par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

    sc <- rv$scores
    if(is.null(sc)) return(invisible(NULL))

    # % de varianza explicada
    var_exp <- (rv$pc$sdev^2 / sum(rv$pc$sdev^2)) * 100

    if(ncol(sc) >= 2){
      plot(sc[,1], sc[,2],
          xlab = paste0("PC1 (", round(var_exp[1],1), "%)"),
          ylab = paste0("PC2 (", round(var_exp[2],1), "%)"),
          main = paste0("Biplot PCA (scores + loadings) — ", rv$ncomp, " comp."),
          pch = 19, cex = 0.6, col = "gray30")
      abline(h = 0, v = 0, lty = 3)

      # Dibujar vectores de carga (loadings) - mejorados para visibilidad
      loadings <- rv$pc$rotation[,1:2]
      # Escalar loadings para mejor visibilidad
      scale_factor <- 1.5
      arrows(0, 0, loadings[,1]*scale_factor, loadings[,2]*scale_factor,
             col = "red", length = 0.15, lwd = 2, angle = 20)
      text(loadings[,1]*(scale_factor + 0.3), loadings[,2]*(scale_factor + 0.3),
           labels = rownames(loadings), col = "red", cex = 0.9, font = 2)

    } else {
      plot(sc[,1], rep(0, nrow(sc)),
          xlab = paste0("PC1 (", round(var_exp[1],1), "%)"),
          ylab = "",
          main = paste0("Scores PC1 — ", rv$ncomp, " comp."),
          pch = 19, cex = 0.6, col = "gray30")
      abline(h = 0, lty = 3)
    }
  }, height = 350, width = 550)  # Mismas dimensiones que el gráfico de clusters

    showNotification(paste0("PCA preparado con ", rv$ncomp, " componentes para clustering."), type = "message")
    updateTabsetPanel(session, inputId = "tabs", selected = "PCA")
  })

  # -------------------------
  # Evaluación de rango de K (2..10) con las componentes elegidas
  # -------------------------
  observeEvent(input$eval_k, {
    if(is.null(rv$pc)){
      showNotification("Primero analice varianza y ejecute PCA con N componentes.", type = "warning")
      return(invisible(NULL))
    }
    if(is.null(rv$scores)){
      # si aún no fijaron ncomp, fijarlo ahora
      ok <- set_scores_from_ncomp()
      if(!ok){
        showNotification("No hay scores disponibles. Revise el PCA.", type = "error")
        return(invisible(NULL))
      }
    }

    dfk <- evaluate_k_range(rv$scores, k_min = 2, k_max = 10)
    rv$kgrid <- dfk

    output$plot_elbow <- renderPlot({
      plot(dfk$k, dfk$wss, type = "b",
           xlab = "K", ylab = "WSS",
           main = paste0("Método del codo (WSS vs K) — ", rv$ncomp, " comp."))
    })
    output$plot_sil_k <- renderPlot({
      plot(dfk$k, dfk$silueta, type = "b",
           xlab = "K", ylab = "Silueta media",
           main = paste0("Silueta vs K — ", rv$ncomp, " comp."))
      abline(h = 0.25, lty = 3); abline(h = 0.40, lty = 3)
    })

    showNotification(paste0("Evaluación de K (2..10) con ", rv$ncomp, " componentes."), type = "message")
    updateTabsetPanel(session, inputId = "tabs", selected = "Elegir K")
  })

  # -------------------------
  # K-means con K elegido (usa rv$scores de ncomp seleccionadas)
  # -------------------------
  observeEvent(input$run_km, {
    if(is.null(rv$pc)){
      showNotification("Primero analice varianza y ejecute PCA con N componentes.", type = "warning")
      return(invisible(NULL))
    }
    vk <- validar_k(input$k)
    if(!vk$ok){
      showNotification(vk$msg, type = "error")
      return(invisible(NULL))
    }

    # Asegura scores actualizados
    if(is.null(rv$scores)){
      ok <- set_scores_from_ncomp()
      if(!ok){
        showNotification("No hay scores disponibles. Revise el PCA.", type = "error")
        return(invisible(NULL))
      }
    }

    km <- run_kmeans(rv$scores, k = input$k)
    rv$clusters <- km$clusters
    rv$wss      <- km$wss
    rv$inertia  <- km$inertia_total
    rv$silueta  <- km$silueta

    # Scatter de clusters
    output$plot_clusters <- renderPlot({
      # Reset gráfico parameters para evitar interferencia con otros plots
      par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

      sc  <- rv$scores
      cls <- factor(rv$clusters)
      if(is.null(sc) || is.null(cls)) return(invisible(NULL))

      if(ncol(sc) >= 2){
        plot(sc[,1], sc[,2],
              col = cls, pch = 19, cex = 0.7,
              xlab = "PC1", ylab = "PC2",
              main = paste("Clusters (K =", input$k, ") —", rv$ncomp, "comp."))
        legend("topright", legend = levels(cls),
                col = seq_along(levels(cls)), pch = 19, title = "Cluster")
      } else {
        plot(sc[,1], rep(0, nrow(sc)),
              col = cls, pch = 19, cex = 0.7,
              xlab = "PC1", ylab = "",
              main = paste("Clusters (K =", input$k, ") —", rv$ncomp, "comp."))
      }
    }, height = 350, width = 550)  # Rectangular para mejor presentación

    # Tabla de métricas
    output$tabla_metricas <- DT::renderDT({
      DT::datatable(data.frame(
        k             = input$k,
        n_componentes = rv$ncomp,
        varianza_acum = if(!is.null(rv$varianza)) max(rv$varianza$var_acum[seq_len(rv$ncomp)]) else NA_real_,
        silueta       = round(rv$silueta, 4),
        wss           = round(rv$wss, 2),
        inercia_total = round(rv$inertia, 2)
      ), rownames = FALSE, options = list(dom = "t"))
    })

    # Feedback
    output$feedback <- renderUI({
      var_acum_sel <- if(!is.null(rv$varianza)) max(rv$varianza$var_acum[seq_len(rv$ncomp)]) else NA_real_
      htmltools::p(htmltools::em(
        feedback_metricas(rv$silueta, rv$wss, var_acum_sel)
      ))
    })

    showNotification("Clustering ejecutado.", type = "message")
    updateTabsetPanel(session, inputId = "tabs", selected = "Clusters")
  })

  # -------------------------
  # Selección automática de cluster óptimo
  # -------------------------
  observeEvent(input$auto_select_cluster, {
    req(rv$clusters)

    # Obtener datos originales para calcular métricas de potencial
    datos <- datos_reactivos()
    df <- .get_base_df_m1(datos)

    # Ensure id_cliente is character (consistent with M3)
    df$id_cliente <- as.character(df$id_cliente)

    # Agregar clusters a los datos
    df$cluster_id <- rv$clusters

    # VALIDACIÓN: Verificar que hay datos suficientes
    if (nrow(df) == 0) {
      shiny::showNotification("No hay datos disponibles para calcular potencial de clusters.", type = "error")
      return(NULL)
    }

    if (length(unique(rv$clusters)) < 2) {
      shiny::showNotification("Se necesita al menos 2 clusters para selección automática.", type = "warning")
      return(NULL)
    }

    # Calcular métricas de potencial por cluster
    cluster_metrics <- data.frame()

    for (cluster_id in unique(rv$clusters)) {
      cluster_data <- df[df$cluster_id == cluster_id, ]

      # VALIDACIÓN: Cluster no vacío
      if (nrow(cluster_data) == 0) {
        shiny::showNotification(sprintf("Cluster %s está vacío, omitiendo.", cluster_id), type = "warning")
        next
      }

      # Métricas básicas con validación
      n_clientes <- nrow(cluster_data)

      # Calcular promedios de forma segura
      score_promedio <- if ("score_buro" %in% names(cluster_data) &&
                           any(!is.na(cluster_data$score_buro))) {
        mean(cluster_data$score_buro, na.rm = TRUE)
      } else { 600 }  # Valor por defecto razonable

      ingreso_promedio <- if ("ingreso_verificado" %in% names(cluster_data) &&
                             any(!is.na(cluster_data$ingreso_verificado))) {
        mean(cluster_data$ingreso_verificado, na.rm = TRUE)
      } else { 3000 }  # Valor por defecto

      rfm_promedio <- if ("rfm" %in% names(cluster_data) &&
                         any(!is.na(cluster_data$rfm))) {
        mean(cluster_data$rfm, na.rm = TRUE)
      } else { 3 }  # Valor por defecto

      # Margen Esperado simulado (basado en datos generados)
      me_potencial <- if ("margen_esperado" %in% names(cluster_data) &&
                         any(!is.na(cluster_data$margen_esperado))) {
        mean(cluster_data$margen_esperado, na.rm = TRUE)
      } else {
        NA_real_
      }

      # VALIDACIÓN: Evitar valores NaN o Inf
      if (is.na(score_promedio) || is.nan(score_promedio) || is.infinite(score_promedio)) {
        score_promedio <- 600
      }
      if (is.na(ingreso_promedio) || is.nan(ingreso_promedio) || is.infinite(ingreso_promedio)) {
        ingreso_promedio <- 3000
      }
      if (is.na(rfm_promedio) || is.nan(rfm_promedio) || is.infinite(rfm_promedio)) {
        rfm_promedio <- 3
      }

      # Calcular score compuesto de potencial (0-100)
      # Normalizar métricas y combinar
      score_norm <- (score_promedio - 300) / (950 - 300)  # Normalizar score_buro
      ingreso_norm <- pmin(ingreso_promedio / 10000, 1)   # Capear ingreso alto
      rfm_norm <- rfm_promedio / 100                       # RFM ya está en 0-100

      # VALIDACIÓN: Asegurar valores en rango [0,1]
      score_norm <- pmax(0, pmin(1, score_norm))
      ingreso_norm <- pmax(0, pmin(1, ingreso_norm))
      rfm_norm <- pmax(0, pmin(1, rfm_norm))

      # Peso: 40% score, 30% ingreso, 20% RFM, 10% tamaño cluster
      size_weight <- min(n_clientes / 500, 1)  # Bonus por tamaño (máx 500 clientes)
      potencial_score <- (0.4 * score_norm + 0.3 * ingreso_norm +
                         0.2 * rfm_norm + 0.1 * size_weight) * 100

      # VALIDACIÓN: Score final en rango válido
      potencial_score <- pmax(0, pmin(100, potencial_score))

      cluster_metrics <- rbind(cluster_metrics, data.frame(
        cluster_id = cluster_id,
        n_clientes = n_clientes,
        score_buro_promedio = round(score_promedio, 1),
        ingreso_promedio = round(ingreso_promedio, 0),
        rfm_promedio = round(rfm_promedio, 1),
        me_potencial = round(me_potencial, 2),
        potencial_score = round(potencial_score, 1)
      ))
    }

    # VALIDACIÓN: Verificar que hay al menos un cluster válido
    if (nrow(cluster_metrics) == 0) {
      shiny::showNotification("No se pudieron calcular métricas para ningún cluster.", type = "error")
      return(NULL)
    }

    # Seleccionar cluster con mayor potencial
    best_cluster <- cluster_metrics[which.max(cluster_metrics$potencial_score), ]

    # Guardar en estado reactivo
    rv$auto_selected_cluster <- best_cluster$cluster_id
    rv$cluster_potential <- cluster_metrics

    # Mostrar resultado
    output$auto_cluster_result <- renderUI({
      div(
        class = "alert alert-success",
        h4("¡Cluster Óptimo Seleccionado!"),
        p(sprintf("La computadora ha seleccionado el Cluster %d como el más prometedor.", best_cluster$cluster_id)),
        p(sprintf("Este cluster tiene %d clientes con un score de potencial de %.1f/100.",
                 best_cluster$n_clientes, best_cluster$potencial_score)),
        p("El cluster seleccionado tiene el mejor balance entre score crediticio, capacidad de pago y comportamiento histórico.")
      )
    })

    # Mostrar tabla de comparación
    output$cluster_potential_table <- DT::renderDT({
      DT::datatable(
        cluster_metrics[order(cluster_metrics$potencial_score, decreasing = TRUE), ],
        colnames = c("Cluster", "N° Clientes", "Score Buró Promedio",
                    "Ingreso Promedio", "RFM Promedio", "Margen Esperado", "Score Potencial"),
        options = list(dom = "t", paging = FALSE)
      ) |>
        DT::formatStyle("potencial_score",
                       backgroundColor = DT::styleInterval(
                         c(70, 85),
                         c("lightcoral", "lightyellow", "lightgreen")
                       ))
    })

    # Actualizar input de K para reflejar la selección automática
    updateNumericInput(session, "k", value = best_cluster$cluster_id)

    showNotification(sprintf("Cluster %d seleccionado automáticamente como óptimo (potencial: %.1f/100)",
                           best_cluster$cluster_id, best_cluster$potencial_score),
                    type = "message", duration = 8)
  })

  # -------------------------
  # Confirmar decisión (persistencia)
  # -------------------------
  observeEvent(input$confirmar, {
    req(rv$clusters)

    persist_variables_modelos(id_sim, "M1", input$vars)

    var_acum_sel <- if(!is.null(rv$varianza)) max(rv$varianza$var_acum[seq_len(rv$ncomp)]) else NA_real_
    persist_eval_m1(
      id_sim         = id_sim,
      varianza_pca   = var_acum_sel,
      k              = input$k,
      silueta        = rv$silueta,
      inercia_total  = rv$inertia,
      criterio_k     = "mixto"
    )

    # Usar IDs reales de los datos originales en lugar de IDs secuenciales
    datos <- datos_reactivos()
    df_base <- .get_base_df_m1(datos)
    df_base$id_cliente <- as.character(df_base$id_cliente)
    id_cliente <- df_base$id_cliente
    success <- persist_clientes_clusters(
      id_sim     = id_sim,
      id_cliente = id_cliente,
      cluster_id = as.integer(rv$clusters)
    )

    if (success) {
      # FILTRADO DE EMBUDO: Si hay cluster seleccionado automáticamente, filtrar solo ese cluster
      if (!is.null(rv$auto_selected_cluster)) {
        selected_cluster <- rv$auto_selected_cluster
        cluster_filter <- rv$clusters == selected_cluster
        filtered_id_cliente <- id_cliente[cluster_filter]
        filtered_clusters <- rv$clusters[cluster_filter]

        # Store filtered clusters in session for M2/M3
        session$userData$clusters <- data.frame(
          id_cliente = filtered_id_cliente,
          cluster_id = as.integer(rep(selected_cluster, length(filtered_id_cliente)))
        )

        n_total <- length(id_cliente)
        n_filtrados <- length(filtered_id_cliente)
        pct_filtrados <- round(100 * n_filtrados / n_total, 1)

        showNotification(
          sprintf("Cluster %d seleccionado automáticamente. Filtrados %d/%d clientes (%.1f%%). Módulo 1 completado.",
                  selected_cluster, n_filtrados, n_total, pct_filtrados),
          type = "message", duration = 6
        )
      } else {
        # No hay filtrado automático, guardar todos los clusters
        session$userData$clusters <- data.frame(id_cliente = id_cliente, cluster_id = as.integer(rv$clusters))
        showNotification("Clusters guardados en data/clientes_clusters.csv. Módulo 1 completado.", type = "message")
      }
    } else {
      showNotification("Error guardando clusters en CSV, pero datos disponibles en sesión.", type = "warning")
      # Still store in session as fallback (con filtrado si aplica)
      if (!is.null(rv$auto_selected_cluster)) {
        selected_cluster <- rv$auto_selected_cluster
        cluster_filter <- rv$clusters == selected_cluster
        session$userData$clusters <- data.frame(
          id_cliente = id_cliente[cluster_filter],
          cluster_id = as.integer(rep(selected_cluster, sum(cluster_filter)))
        )
      } else {
        session$userData$clusters <- data.frame(id_cliente = id_cliente, cluster_id = as.integer(rv$clusters))
      }
    }
  })

  # -------------------------
  # Reiniciar módulo (estado en memoria)
  # -------------------------
  observeEvent(input$reiniciar, {
    rv$pc <- rv$scores <- rv$varianza <- rv$kgrid <- NULL
    rv$ncomp <- NULL
    rv$clusters <- NULL
    rv$silueta <- rv$wss <- rv$inertia <- NA_real_
    rv$auto_selected_cluster <- NULL
    rv$cluster_potential <- NULL

    output$plot_pca        <- renderPlot({})
    output$plot_varianza   <- renderPlot({})
    output$plot_elbow      <- renderPlot({})
    output$plot_sil_k      <- renderPlot({})
    output$plot_clusters   <- renderPlot({})
    output$tabla_metricas  <- DT::renderDT(NULL)
    output$feedback        <- renderUI(NULL)
    output$auto_cluster_result <- renderUI(NULL)
    output$cluster_potential_table <- DT::renderDT(NULL)

    showNotification("Módulo 1 reiniciado.", type = "message")
    updateTabsetPanel(session, inputId = "tabs", selected = "PCA")
  })

  # -------------------------
  # Exportar
  # -------------------------
  observeEvent(input$exportar, {
    ok <- exportar_resultados(id_sim)
    if(ok) showNotification("Archivos listos en /data.", type = "message")
    else   showNotification("Aún no hay resultados para exportar.", type = "warning")
  })
}
