# R/mod_m1_server.R
# Módulo 1 - Perfilamiento del Cliente
# Server: orquesta PCA, evaluación de K y clustering; muestra métricas y persiste resultados.

mod_m1_server <- function(input, output, session, datos_reactivos, id_sim){

  ns <- session$ns

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
    kgrid = NULL
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

    # Preprocesamiento según selección
    prep <- preprocess_inputs(
      df_list = datos_reactivos(),
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

      # Dibujar vectores de carga (loadings)
      loadings <- rv$pc$rotation[,1:2]
      arrows(0, 0, loadings[,1]*2, loadings[,2]*2, col = "red", length = 0.1, lwd = 0.2)
      text(loadings[,1]*2.2, loadings[,2]*2.2, labels = rownames(loadings),
          col = "red", cex = 0.7)

    } else {
      plot(sc[,1], rep(0, nrow(sc)),
          xlab = paste0("PC1 (", round(var_exp[1],1), "%)"),
          ylab = "",
          main = paste0("Scores PC1 — ", rv$ncomp, " comp."),
          pch = 19, cex = 0.6, col = "gray30")
      abline(h = 0, lty = 3)
    }
  }, height = 500, width = 500)

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
    })

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

    id_cliente <- as.character(seq_len(nrow(rv$scores)))
    success <- persist_clientes_clusters(
      id_sim     = id_sim,
      id_cliente = id_cliente,
      cluster_id = as.integer(rv$clusters)
    )

    if (success) {
      # Store clusters in session for M2/M3
      session$userData$clusters <- data.frame(id_cliente = id_cliente, cluster_id = as.integer(rv$clusters))
      showNotification("Clusters guardados en data/clientes_clusters.csv. Módulo 1 completado.", type = "message")
    } else {
      showNotification("Error guardando clusters en CSV, pero datos disponibles en sesión.", type = "warning")
      # Still store in session as fallback
      session$userData$clusters <- data.frame(id_cliente = id_cliente, cluster_id = as.integer(rv$clusters))
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

    output$plot_pca        <- renderPlot({})
    output$plot_varianza   <- renderPlot({})
    output$plot_elbow      <- renderPlot({})
    output$plot_sil_k      <- renderPlot({})
    output$plot_clusters   <- renderPlot({})
    output$tabla_metricas  <- DT::renderDT(NULL)
    output$feedback        <- renderUI(NULL)

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
