
# ---- Dependencias principales
library(shiny)
library(DT)
library(shinyjs)

# ---- Dependencias específicas por módulo
library(cluster)      # M1: distancia/silueta
library(pROC)         # M2: AUC
library(ggplot2)      # M3: gráficos
library(reshape2)     # M3: melt() en heatmaps
library(MASS)         # M3: stepAIC
library(car)          # M3: VIF multicolinealidad
library(rpart)        # M4: árboles de clasificación
library(rpart.plot)   # M4: visualización de árboles

# ---- Sourcing de funciones auxiliares
source("R/utils_validaciones.R")
source("R/feedback_rules.R")
source("R/export_helpers.R")

# ---- Sourcing: Módulo 1 - Perfilamiento
source("R/gen_datos.R")
source("R/preprocess.R")
source("R/pca_helpers.R")
source("R/kmeans_helpers.R")
source("R/persistencia.R")
source("R/mod_m1_ui.R")
source("R/mod_m1_server.R")

# ---- Sourcing: Módulo 2 - Scoring (Regresión Logística)
source("R/logit_helpers.R")
source("R/persistencia_m2.R")
source("R/mod_m2_ui.R")
source("R/mod_m2_server.R")

# ---- Sourcing: Módulo 3 - Pricing y Elasticidad
source("R/persistencia_m3.R")
source("R/mod_m3_ui.R")
source("R/mod_m3_server.R")

# ---- Sourcing: Módulo 4 - Árboles de Clasificación
source("R/gen_datos_m4.R")  # Generador específico para M4
source("R/tree_helpers.R")
source("R/persistencia_m4.R")
source("R/mod_m4_ui.R")
source("R/mod_m4_server.R")

# ---------------- UI ----------------
ui <- navbarPage(
  title = "Simulación Campaña Préstamos",
  tabPanel("Configuración",
    fluidPage(
      h3("Configuración de Ejecución"),
      p("Selecciona el modo de ejecución de los módulos:"),
      radioButtons("execution_mode", "Modo de Ejecución:",
                  choices = c("Secuencial (Módulos dependen entre sí)" = "sequential",
                             "Independiente (Cada módulo funciona por separado)" = "independent"),
                  selected = "sequential"),
      hr(),
      h4("Modo Seleccionado:"),
      uiOutput("mode_description"),
      hr(),
      p(strong("Nota:"), "En modo independiente, cada módulo genera sus propios datos simulados y puede ejecutarse sin depender de los módulos anteriores."),
      hr(),
      actionButton("confirm_config", "Confirmar Configuración",
                  class = "btn-success btn-lg"),
      uiOutput("config_status")
    )
  ),
  tabPanel("Módulo 1: Perfilamiento", mod_m1_ui("m1")),
  tabPanel("Módulo 2: Scoring",       mod_m2_ui("m2")),
  tabPanel("Módulo 3: Pricing",       mod_m3_ui("m3")),
  tabPanel("Módulo 4: Árboles",       mod_m4_ui("m4")),
  tabPanel("Exportar Datos",
    fluidPage(
      h3("Exportar Datos Simulados"),
      p("Exporta los datos simulados base a un archivo CSV para análisis posterior."),
      actionButton("export_simulated_data", "Exportar Datos Simulados a CSV",
                   class = "btn-primary")
    )
  )
)

# --------------- Server --------------
server <- function(input, output, session){

  # 1) Inicializamos simulación (se comparte entre M1, M2 y M3)
  id_sim     <- paste0("SIM_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  seed       <- 12345L
  n_clientes <- 5000L  # Aumentado para mejor performance y más datos después del filtrado

  # 2) Estado de configuración
  config_confirmed <- reactiveVal(FALSE)

  # 3) Modo de ejecución reactivo (solo después de confirmar)
  execution_mode <- reactive({
    if (!config_confirmed()) return("sequential")
    input$execution_mode %||% "sequential"
  })

  # 3) Generación de datos (lista de data.frames) — base común
  #    gen_datos() devuelve: demograficas, financieras, comp_historico,
  #    post_desembolso, simulacion_meta
  datos <- gen_datos(n_clientes = n_clientes, seed = seed)

  # 4) Descripción del modo seleccionado
  output$mode_description <- renderUI({
    if (!config_confirmed()) {
      return(div(class = "alert alert-warning",
                 h4("⚠️ Configuración Pendiente"),
                 p("Selecciona un modo de ejecución y confirma la configuración para habilitar los módulos.")))
    }

    if (execution_mode() == "sequential") {
      div(class = "alert alert-info",
          h4("🔗 Modo Secuencial"),
          p("Los módulos se ejecutan en orden dependiente:"),
          tags$ul(
            tags$li("M1 → genera datos base"),
            tags$li("M2 → usa clusters de M1"),
            tags$li("M3 → usa scores de M2"),
            tags$li("M4 → usa datos históricos independientes")
          )
      )
    } else {
      div(class = "alert alert-success",
          h4("🔄 Modo Independiente"),
          p("Cada módulo genera sus propios datos simulados:"),
          tags$ul(
            tags$li("M1 → genera datos demográficos y financieros"),
            tags$li("M2 → simula clusters previos + genera scores"),
            tags$li("M3 → simula scores previos + genera márgenes"),
            tags$li("M4 → usa datos históricos simulados completos")
          )
      )
    }
  })

  # 5) Estado de configuración
  output$config_status <- renderUI({
    if (config_confirmed()) {
      div(class = "alert alert-success",
          h4("✅ Configuración Confirmada"),
          p("Ya puedes usar los módulos con el modo seleccionado. Los datos simulados están listos."))
    } else {
      div(class = "alert alert-info",
          h4("⏳ Configuración Pendiente"),
          p("Haz clic en 'Confirmar Configuración' para habilitar los módulos."))
    }
  })

  # 6) Observador para confirmar configuración
  observeEvent(input$confirm_config, {
    config_confirmed(TRUE)
    showNotification("✅ Configuración confirmada. Los módulos están listos para usar.",
                    type = "message", duration = 5)
  })

  # 5) Módulo 1 (Perfilamiento)
  callModule(
    module = mod_m1_server, id = "m1",
    datos_reactivos = reactive(datos),  # pasa la misma lista
    id_sim = id_sim,
    execution_mode = execution_mode
  )

  # 6) Módulo 2 (Scoring - Regresión Logística)
  #    Este módulo calcula probabilidades de aceptación y mora y persiste
  #    resultados (eval_m2, clientes_scores). Para enlazar con M3
  #    recomendamos (en mod_m2_server) exponer rv$df_scores en
  #    session$userData$df_scores.
  callModule(
    module = mod_m2_server, id = "m2",
    datos_reactivos = reactive(datos),  # misma base simulada
    id_sim = id_sim,
    execution_mode = execution_mode
  )

  # 5) Preparación del dataset para el Módulo 3 (Pricing)
  #    - Integra datos de cliente (M1) + probabilidades (M2) + ofertas históricas
  #    - Usa ofertas_historicas para datos de entrenamiento realistas
  datos_para_m3 <- reactive({
    # Base de cliente (un flat por id_cliente)
    base_cli <- Reduce(function(x, y) merge(x, y, by = "id_cliente", all = TRUE),
                      list(datos$demograficas, datos$financieras, datos$comp_historico))

    # Agregar ofertas históricas si existen (para datos de entrenamiento realistas)
    if (!is.null(datos$ofertas_historicas) && nrow(datos$ofertas_historicas) > 0) {
      # Merge con ofertas históricas - cada cliente tendrá múltiples filas (una por oferta histórica)
      base_cli <- merge(base_cli, datos$ofertas_historicas, by = "id_cliente", all.y = TRUE)
    }

    # Traer probabilidades de M2 si ya fueron calculadas (solo score, no p_accept/p_mora)
    if (!is.null(session$userData$df_scores)) {
      # Solo incluir score y cluster_id, excluir p_accept y p_mora para evitar multicolinealidad
      scores_filtered <- session$userData$df_scores[, c("id_cliente", "score"), drop = FALSE]
      base_cli <- merge(base_cli, scores_filtered, by = "id_cliente", all.x = TRUE)
    } else {
      # Si aún no hay df_scores, crea columna score vacía
      base_cli$score <- NA_real_
    }

    # Traer clusters de M1 si existen
    if (!is.null(session$userData$clusters)) {
      clusters_filtered <- session$userData$clusters[, c("id_cliente", "cluster_id"), drop = FALSE]
      base_cli <- merge(base_cli, clusters_filtered, by = "id_cliente", all.x = TRUE)
    } else {
      base_cli$cluster_id <- NA_character_
    }

    # Si no hay ofertas_historicas, generar valores por defecto para compatibilidad
    if (!"rate" %in% names(base_cli)) base_cli$rate <- 0.05
    if (!"amount" %in% names(base_cli)) base_cli$amount <- 5000
    if (!"term" %in% names(base_cli)) base_cli$term <- 12

    base_cli
  })

  # 7) Módulo 3 (Pricing y Elasticidad)
  callModule(
    module = mod_m3_server, id = "m3",
    datos_reactivos = datos_para_m3,  # df con cliente + p_accept/p_mora + oferta
    id_sim = id_sim,
    execution_mode = execution_mode
  )

  # 8) Módulo 4 (Árboles de Clasificación)
  callModule(
    module = mod_m4_server, id = "m4",
    datos_reactivos = reactive(datos),  # misma base simulada
    id_sim = id_sim,
    execution_mode = execution_mode
  )

  # 7) Exportar datos simulados a CSV
  observeEvent(input$export_simulated_data, {
    tryCatch({
      # Combinar todos los datos simulados en un solo data.frame
      simulated_data <- Reduce(function(x, y) merge(x, y, by = "id_cliente", all = TRUE),
                              list(datos$demograficas, datos$financieras,
                                   datos$comp_historico, datos$post_desembolso))

      # Agregar timestamp
      simulated_data$timestamp <- as.character(Sys.time())
      simulated_data$id_sim <- id_sim

      # Guardar en CSV
      dir.create("data", showWarnings = FALSE)
      file_path <- file.path("data", "datos_simulados.csv")
      write.csv(simulated_data, file_path, row.names = FALSE)

      showNotification(paste("Datos simulados guardados en", file_path), type = "message")
    }, error = function(e) {
      showNotification(paste("Error exportando datos simulados:", conditionMessage(e)), type = "error")
    })
  })
}

shinyApp(ui, server)