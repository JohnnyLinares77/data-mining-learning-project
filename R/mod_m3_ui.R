# R/mod_m3_ui.R
# UI del Módulo 3 – Pricing y Elasticidad
#
# Estructura en pestañas: Exploración, Modelo lineal, Modelo automático,
# Simulación y Conclusión. Alineado al estilo de M1 y M2.
mod_m3_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h2("Módulo 3: Pricing y Elasticidad"),
    shiny::p("Se estima el margen esperado (ME) como función de la oferta (tasa, monto, plazo) y del perfil del cliente. 
              Luego se modela ME con regresión lineal y se derivan elasticidades."),
    shiny::tabsetPanel(
      id = ns("tabs"), type = "tabs",
      
      # Pestaña 1: Exploración
      shiny::tabPanel(
        title = "Exploración",
        shiny::h4("Matriz de correlaciones y selección de variables"),
        shiny::uiOutput(ns("var_select_ui")),
        shiny::actionButton(ns("calcular_cor"), "Calcular correlaciones"),
        shiny::br(),
        shiny::plotOutput(ns("cor_plot"), height = "400px"),
        DT::DTOutput(ns("cor_table"))
      ),
      
      # Pestaña 2: Modelo lineal (Alumno)
      shiny::tabPanel(
        title = "Modelo lineal",
        shiny::h4("Modelo lineal manual (Alumno)"),
        shiny::uiOutput(ns("model_var_select")),
        shiny::sliderInput(ns("alpha"), "Nivel de significancia (alpha)",
                           min = 0.001, max = 0.10, value = 0.05, step = 0.001),
        shiny::actionButton(ns("ajustar_modelo"), "Ajustar modelo"),
        shiny::br(),
        shiny::verbatimTextOutput(ns("model_summary")),
        shiny::plotOutput(ns("resid_plot"), height = "300px")
      ),
      
      # Pestaña 3: Modelo automático
      shiny::tabPanel(
        title = "Modelo automático",
        shiny::h4("Stepwise AIC y comparación"),
        shiny::actionButton(ns("ajustar_auto"), "Ajustar modelo automático"),
        shiny::br(),
        shiny::verbatimTextOutput(ns("auto_summary")),
        shiny::h4("Comparación (hold-out)"),
        DT::DTOutput(ns("comp_table")),
        shiny::radioButtons(ns("modelo_final"), "Modelo para simulación:",
                            choices = c("Manual" = "manual", "Automático" = "automatico"),
                            selected = "manual", inline = TRUE)
      ),
      
      # Pestaña 4: Simulación y elasticidad
      shiny::tabPanel(
        title = "Simulación",
        shiny::h4("Curvas de ME y Elasticidad (respecto a tasa)"),
        shiny::sliderInput(ns("rango_tasa"), "Rango de tasas", min = 0.05, max = 0.60,
                           value = c(0.20, 0.40), step = 0.01),
        shiny::numericInput(ns("monto_sim"),  "Monto simulado:",  value = 10000, min = 500),
        shiny::numericInput(ns("plazo_sim"),  "Plazo (meses):",   value = 12,    min = 3),
        shiny::numericInput(ns("score_sim"),  "Score simulado:",  value = 0.5,   min = 0, max = 1, step = 0.01),
        shiny::selectInput(ns("cluster_sim"), "Cluster simulado:", choices = NULL),
        shiny::actionButton(ns("simular"), "Simular"),
        shiny::br(),
        shiny::plotOutput(ns("margen_plot"),      height = "350px"),
        shiny::plotOutput(ns("elasticidad_plot"), height = "350px")
      ),
      
      # Pestaña 5: Conclusión
      shiny::tabPanel(
        title = "Conclusión",
        shiny::h4("Resumen y conclusiones"),
        shiny::p("Se integra Perfilamiento (M1) y Scoring (M2) para definir políticas de precio que maximizan el ME con aceptación y riesgo adecuados.")
      )
    )
  )
}
