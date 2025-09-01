# app.R
# ---- Dependencias
library(shiny)
library(DT)
library(cluster)  # para distancia/silueta en M1
library(pROC)     # para AUC en M2

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

# ---------------- UI ----------------
ui <- navbarPage(
  title = "Tesis - Módulos",
  tabPanel("Módulo 1: Perfilamiento", mod_m1_ui("m1")),
  tabPanel("Módulo 2: Scoring",       mod_m2_ui("m2"))
)

# --------------- Server --------------
server <- function(input, output, session){

  # 1) Inicializamos simulación (se comparte entre M1 y M2)
  id_sim     <- paste0("SIM_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  seed       <- 12345L
  n_clientes <- 1000L

  # 2) Generación de datos (lista de data.frames) — base común a ambos módulos
  #    gen_datos() devuelve: demograficas, financieras, comp_historico, post_desembolso, simulacion_meta
  datos <- gen_datos(n_clientes = n_clientes, seed = seed)

  # 3) Módulo 1 (Perfilamiento)
  callModule(
    module = mod_m1_server, id = "m1",
    datos_reactivos = reactive(datos),  # pasa la misma lista
    id_sim = id_sim
  )

  # 4) Módulo 2 (Scoring - Regresión Logística)
  #    Este módulo:
  #    - arma el dataset de predicción uniendo demograficas/financieras/comp_historico por id_cliente
  #    - usa variables seleccionadas (cuando exista variables_modelos de M1) o las escogidas en la UI de M2
  #    - entrena modelos logísticos de aceptación y mora, muestra p-valores y AUC
  #    - evalúa umbrales sobre el score integrado y persiste resultados (eval_m2, clientes_scores)
  callModule(
    module = mod_m2_server, id = "m2",
    datos_reactivos = reactive(datos),  # misma base simulada
    id_sim = id_sim
  )
}

shinyApp(ui, server)
