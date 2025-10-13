
# ---- Dependencias
library(shiny)
library(DT)
library(cluster)  # para distancia/silueta en M1
library(pROC)     # para AUC en M2
library(shinyjs)
library(ggplot2)  # para gráficos en M3
library(reshape2) # para melt() en heatmaps de correlación
library(MASS)     # para stepAIC en M3
library(car)      # para VIF en análisis de multicolinealidad
library(rpart)    # para árboles de clasificación en M4
library(rpart.plot) # para visualización de árboles en M4

# ---- Sourcing: Módulo 1
source("R/gen_datos.R")
source("R/preprocess.R")
source("R/pca_helpers.R")
source("R/kmeans_helpers.R")
source("R/persistencia.R")
source("R/utils_validaciones.R")
source("R/feedback_rules.R")
source("R/export_helpers.R")
source("R/mod_m1_ui.R")
source("R/mod_m1_server.R")

# ---- Sourcing: Módulo 2 (Scoring - Regresión Logística)
source("R/logit_helpers.R")
source("R/persistencia_m2.R")
source("R/mod_m2_ui.R")
source("R/mod_m2_server.R")

# ---- Sourcing: Módulo 3 (Pricing y Elasticidad)
# Asegúrate de colocar estos archivos en R/ tal como hiciste con M1 y M2
source("R/persistencia_m3.R")
source("R/mod_m3_ui.R")
source("R/mod_m3_server.R")

# ---- Sourcing: Módulo 4 (Árboles de Clasificación)
source("R/tree_helpers.R")
source("R/persistencia_m4.R")
source("R/mod_m4_ui.R")
source("R/mod_m4_server.R")

# ---------------- UI ----------------
ui <- navbarPage(
  title = "Simulación Campaña Préstamos",
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

  # 2) Generación de datos (lista de data.frames) — base común
  #    gen_datos() devuelve: demograficas, financieras, comp_historico,
  #    post_desembolso, simulacion_meta
  datos <- gen_datos(n_clientes = n_clientes, seed = seed)

  # 3) Módulo 1 (Perfilamiento)
  callModule(
    module = mod_m1_server, id = "m1",
    datos_reactivos = reactive(datos),  # pasa la misma lista
    id_sim = id_sim
  )

  # 4) Módulo 2 (Scoring - Regresión Logística)
  #    Este módulo calcula probabilidades de aceptación y mora y persiste
  #    resultados (eval_m2, clientes_scores). Para enlazar con M3
  #    recomendamos (en mod_m2_server) exponer rv$df_scores en
  #    session$userData$df_scores.
  callModule(
    module = mod_m2_server, id = "m2",
    datos_reactivos = reactive(datos),  # misma base simulada
    id_sim = id_sim
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

  # 6) Módulo 3 (Pricing y Elasticidad)
  callModule(
    module = mod_m3_server, id = "m3",
    datos_reactivos = datos_para_m3,  # df con cliente + p_accept/p_mora + oferta
    id_sim = id_sim
  )

  # 7) Módulo 4 (Árboles de Clasificación)
  callModule(
    module = mod_m4_server, id = "m4",
    datos_reactivos = reactive(datos),  # misma base simulada
    id_sim = id_sim
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