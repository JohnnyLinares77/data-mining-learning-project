# app.R (integrado con Módulo 3 - Pricing y Elasticidad)
# ---- Dependencias
library(shiny)
library(DT)
library(cluster)  # para distancia/silueta en M1
library(pROC)     # para AUC en M2
library(shinyjs)

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
source("R/mod_m3_ui.R")
source("R/mod_m3_server.R")

# ---------------- UI ----------------
ui <- navbarPage(
  title = "Simulación Campaña Préstamos",
  tabPanel("Módulo 1: Perfilamiento", mod_m1_ui("m1")),
  tabPanel("Módulo 2: Scoring",       mod_m2_ui("m2")),
  tabPanel("Módulo 3: Pricing",       mod_m3_ui("m3"))
)

# --------------- Server --------------
server <- function(input, output, session){

  # 1) Inicializamos simulación (se comparte entre M1, M2 y M3)
  id_sim     <- paste0("SIM_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  seed       <- 12345L
  n_clientes <- 1000L

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
  #    - Integra datos de cliente (M1) + probabilidades (M2)
  #    - Asegura la presencia de columnas de oferta: rate, amount, term
  datos_para_m3 <- reactive({
    # Base de cliente (un flat por id_cliente)
    base_cli <- Reduce(function(x, y) merge(x, y, by = "id_cliente", all = TRUE),
                       list(datos$demograficas, datos$financieras, datos$comp_historico))

    # Traer probabilidades de M2 si ya fueron calculadas
    if (!is.null(session$userData$df_scores)) {
      base_cli <- merge(base_cli, session$userData$df_scores, by = "id_cliente", all.x = TRUE)
    } else {
      # Si aún no hay df_scores, crea columnas vacías para que la UI de M3 avise al alumno
      base_cli$p_accept <- NA_real_
      base_cli$p_mora   <- NA_real_
      base_cli$score    <- NA_real_
    }

    # Incorporar parámetros de oferta desde simulacion_meta si existen
    # (ajusta los nombres según tu gen_datos)
    if (!is.null(datos$simulacion_meta)) {
      meta <- datos$simulacion_meta
      # intenta detectar columnas típicas
      posibles_nombres <- list(
        rate   = c("rate", "tasa", "tasa_ofrecida", "interes"),
        amount = c("amount", "monto", "monto_ofrecido"),
        term   = c("term", "plazo", "meses")
      )
      pick_col <- function(df, opciones) {
        nm <- opciones[opciones %in% names(df)]
        if (length(nm) >= 1) df[[nm[1]]] else NULL
      }
      meta_rate   <- pick_col(meta, posibles_nombres$rate)
      meta_amount <- pick_col(meta, posibles_nombres$amount)
      meta_term   <- pick_col(meta, posibles_nombres$term)

      # Unir por id_cliente si existe, si no, reciclar valores típicos
      if ("id_cliente" %in% names(meta)) {
        fuse <- data.frame(id_cliente = meta$id_cliente,
                           rate   = if (is.null(meta_rate))   NA_real_ else meta_rate,
                           amount = if (is.null(meta_amount)) NA_real_ else meta_amount,
                           term   = if (is.null(meta_term))   NA_real_ else meta_term)
        base_cli <- merge(base_cli, fuse, by = "id_cliente", all.x = TRUE)
      } else {
        # relleno por defecto si no hay id_cliente en meta
        if (!"rate"   %in% names(base_cli)) base_cli$rate   <- if (is.null(meta_rate))   0.40 else meta_rate[1]
        if (!"amount" %in% names(base_cli)) base_cli$amount <- if (is.null(meta_amount)) 5000 else meta_amount[1]
        if (!"term"   %in% names(base_cli)) base_cli$term   <- if (is.null(meta_term))   12    else meta_term[1]
      }
    } else {
      # Fallback si no existe simulacion_meta
      if (!"rate"   %in% names(base_cli)) base_cli$rate   <- 0.40
      if (!"amount" %in% names(base_cli)) base_cli$amount <- 5000
      if (!"term"   %in% names(base_cli)) base_cli$term   <- 12
    }

    base_cli
  })

  # 6) Módulo 3 (Pricing y Elasticidad)
  callModule(
    module = mod_m3_server, id = "m3",
    datos_reactivos = datos_para_m3,  # df con cliente + p_accept/p_mora + oferta
    id_sim = id_sim
  )
}

shinyApp(ui, server)
