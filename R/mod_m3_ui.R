# R/mod_m3_ui.R
# UI del Módulo 3 – Pricing y Elasticidad
#
# Esta función define la interfaz de usuario para el módulo de pricing.
# Se estructura en varias pestañas (Exploración, Modelo lineal, Modelo automático,
# Simulación y Conclusión) y se inspira en la organización de los módulos
# 1 y 2 del proyecto.
#
mod_m3_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::h2("Módulo 3: Pricing y Elasticidad"),
    shiny::p("En este módulo se estima el margen esperado de cada operación en función
             de las condiciones de la oferta (tasa, monto y plazo) y de las
             características del cliente. Se aplican técnicas de regresión
             lineal múltiple y se analiza la elasticidad de la demanda frente a
             variaciones en los parámetros de precio."),
    shiny::tabsetPanel(
      id = ns("tabs"), type = "tabs",
      
      # Pestaña 1: Exploración
      shiny::tabPanel(
        title = "Exploración",
        shiny::h4("Matriz de correlaciones y selección de variables"),
        shiny::p("Seleccione las variables numéricas para calcular la matriz de
                 correlaciones con el margen esperado. Esto ayuda a detectar
                 relaciones fuertes y colinealidad entre predictores."),
        shiny::uiOutput(ns("var_select_ui")),
        shiny::actionButton(ns("calcular_cor"), "Calcular correlaciones"),
        shiny::br(),
        shiny::plotOutput(ns("cor_plot"), height = "400px"),
        shiny::dataTableOutput(ns("cor_table"))
      ),
      
      # Pestaña 2: Modelo lineal manual
      shiny::tabPanel(
        title = "Modelo lineal",
        shiny::h4("Construcción de modelo lineal"),
        shiny::p("Seleccione las variables explicativas para el modelo de
                 regresión lineal. El margen esperado será la variable
                 respuesta. Se mostrará un resumen con R² ajustado y
                 p‑valores."),
        shiny::uiOutput(ns("model_var_select")),
        shiny::actionButton(ns("ajustar_modelo"), "Ajustar modelo"),
        shiny::br(),
        shiny::verbatimTextOutput(ns("model_summary")),
        shiny::plotOutput(ns("resid_plot"), height = "300px")
      ),
      
      # Pestaña 3: Modelo automático
      shiny::tabPanel(
        title = "Modelo automático",
        shiny::h4("Modelo automático y comparación"),
        shiny::p("Se ajustará un modelo de forma automática (stepwise AIC) y
                 se compararán las métricas con el modelo manual."),
        shiny::actionButton(ns("ajustar_auto"), "Ajustar modelo automático"),
        shiny::br(),
        shiny::verbatimTextOutput(ns("auto_summary")),
        shiny::h4("Comparación de desempeño"),
        shiny::dataTableOutput(ns("comp_table")),
        shiny::radioButtons(ns("modelo_final"), "Seleccione el modelo a utilizar:",
                            choices = c("Manual" = "manual", "Automático" = "automatico"),
                            selected = "manual")
      ),
      
      # Pestaña 4: Simulación y elasticidad
      shiny::tabPanel(
        title = "Simulación",
        shiny::h4("Simulación de márgenes y elasticidad"),
        shiny::p("Con el modelo seleccionado se simulan distintos niveles de
                 tasa para un cliente representativo y se grafican el margen
                 esperado y la elasticidad de la demanda."),
        shiny::sliderInput(ns("rango_tasa"), "Rango de tasas", min = 0.01, max = 0.1,
                          value = c(0.04, 0.08), step = 0.005),
        shiny::numericInput(ns("monto_sim"), "Monto simulado:", value = 10000, min = 1000),
        shiny::numericInput(ns("plazo_sim"), "Plazo simulado (meses):", value = 12, min = 3),
        shiny::numericInput(ns("score_sim"), "Score simulado:", value = 0.5, min = 0, max = 1,
                           step = 0.01),
        shiny::selectInput(ns("cluster_sim"), "Cluster simulado:", choices = NULL),
        shiny::actionButton(ns("simular"), "Simular"),
        shiny::br(),
        shiny::plotOutput(ns("margen_plot"), height = "350px"),
        shiny::plotOutput(ns("elasticidad_plot"), height = "350px")
      ),
      
      # Pestaña 5: Conclusión
      shiny::tabPanel(
        title = "Conclusión",
        shiny::h4("Resumen y conclusiones"),
        shiny::p("Esta sección resume los hallazgos del análisis de pricing y
                 elasticidad. Se destaca la importancia de integrar la
                 segmentación (módulo 1) y el scoring (módulo 2) para definir
                 políticas de precio que maximicen el margen esperado con
                 niveles de aceptación y riesgo aceptables.")
      )
    )
  )
}
