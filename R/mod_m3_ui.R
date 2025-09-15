# R/mod_m3_ui.R
# UI del Módulo 3 – Pricing y Elasticidad
# Replica el layout de M1/M2: panel izquierdo (inputs) + panel derecho (tabs)

mod_m3_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      # -------------------------
      # Panel izquierdo: Inputs
      # -------------------------
      shiny::column(
        width = 3,
        shiny::h3("Inputs"),
        shiny::tags$strong("Variables numéricas disponibles"),
        # Se llena dinámicamente desde el server (updateCheckboxGroupInput)
        shiny::checkboxGroupInput(
          inputId = ns("vars_numeric"),
          label   = NULL,
          choices = c(),
          selected = c()
        ),
        shiny::tags$hr(),
        shiny::tags$strong("Parámetros"),
        shiny::numericInput(ns("alpha"), "α (p-valor) para significancia", value = 0.05,
                            min = 0.001, max = 0.2, step = 0.001),

        shiny::tags$hr(),
        shiny::tags$strong("Palancas de simulación"),
        shiny::sliderInput(ns("rango_tasa"), "Rango de tasas", min = 0.01, max = 0.10,
                           value = c(0.04, 0.08), step = 0.005),
        shiny::numericInput(ns("monto_sim"), "Monto (S/)", value = 10000, min = 1000, step = 500),
        shiny::numericInput(ns("plazo_sim"), "Plazo (meses)", value = 12, min = 3, step = 1),
        shiny::numericInput(ns("score_sim"), "Score simulado (0-1)", value = 0.5, min = 0, max = 1, step = 0.01),
        shiny::selectInput(ns("cluster_sim"), "Cluster simulado", choices = NULL, selected = NULL)
      ),

      # -------------------------
      # Panel derecho: Tabs
      # -------------------------
      shiny::column(
        width = 9,
        shiny::h3("Módulo 3: Pricing y Elasticidad"),
        shiny::p("Se estima el margen esperado (ME) como función de la oferta (tasa, monto, plazo) y del perfil del cliente. 
                 Luego se modela ME con regresión lineal y se derivan elasticidades."),

        shiny::tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 1: Exploración
          shiny::tabPanel(
            title = "Exploración",
            shiny::h4("Matriz de correlaciones y selección de variables"),
            shiny::helpText("Selecciona variables numéricas en el panel izquierdo y calcula la matriz de correlación contra ME."),
            shiny::actionButton(ns("calcular_cor"), "Calcular correlaciones"),
            shiny::br(), shiny::br(),
            shiny::plotOutput(ns("cor_plot"), height = "420px"),
            DT::DTOutput(ns("cor_table"))
          ),

          # ---- Tab 2: Modelo lineal (manual)
          shiny::tabPanel(
            title = "Modelo lineal",
            shiny::h4("Construcción de modelo lineal"),
            shiny::helpText("Elige las variables explicativas (del panel izquierdo). La respuesta es ME."),
            shiny::actionButton(ns("ajustar_modelo"), "Ajustar modelo"),
            shiny::br(),
            shiny::verbatimTextOutput(ns("model_summary")),
            shiny::plotOutput(ns("resid_plot"), height = "320px")
          ),

          # ---- Tab 3: Modelo automático
          shiny::tabPanel(
            title = "Modelo automático",
            shiny::h4("Stepwise AIC y comparación"),
            shiny::actionButton(ns("ajustar_auto"), "Ajustar modelo automático"),
            shiny::br(),
            shiny::verbatimTextOutput(ns("auto_summary")),
            shiny::h4("Comparación (hold-out 30%)"),
            DT::DTOutput(ns("comp_table")),
            shiny::radioButtons(ns("modelo_final"), "Modelo para simulación:",
              choices = c("Manual" = "manual", "Automático" = "automatico"),
              selected = "manual", inline = TRUE
            )
          ),

          # ---- Tab 4: Simulación
          shiny::tabPanel(
            title = "Simulación",
            shiny::h4("Simulación de márgenes y elasticidad"),
            shiny::actionButton(ns("simular"), "Simular"),
            shiny::br(), shiny::br(),
            shiny::plotOutput(ns("margen_plot"), height = "360px"),
            shiny::plotOutput(ns("elasticidad_plot"), height = "360px")
          ),

          # ---- Tab 5: Conclusión
          shiny::tabPanel(
            title = "Conclusión",
            shiny::h4("Resumen y conclusiones"),
            shiny::p("Integrando M1 (segmentación) y M2 (scoring) con este M3 (pricing), 
                     se pueden fijar políticas de precio que maximicen el margen esperado con
                     niveles de aceptación y riesgo acordes a los objetivos.")
          )
        )
      )
    )
  )
}
