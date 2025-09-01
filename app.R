# app.R
# ---- Dependencias mínimas
# install.packages(c("shiny","DT","cluster"))
library(shiny)
library(DT)
library(cluster)

# Sourcing de módulos y utilidades
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

ui <- fluidPage(
  tags$head(tags$title("Módulo 1 - Perfilamiento")),
  mod_m1_ui("m1")
)

server <- function(input, output, session){
  # 1) Inicializamos simulación (id_sim, seed)
  id_sim <- paste0("SIM_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  seed   <- 12345L
  n_clientes <- 1000L

  # 2) Generación de datos (lista de data.frames)
  datos <- gen_datos(n_clientes = n_clientes, seed = seed)

  # 3) Llamada al servidor del módulo 1
  callModule(mod_m1_server, id = "m1",
             datos_reactivos = reactive(datos),
             id_sim = id_sim)
}

shinyApp(ui, server)
