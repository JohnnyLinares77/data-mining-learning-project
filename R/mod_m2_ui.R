# R/mod_m2_ui.R
# -------------------------------------------------------------------
# UI del Módulo 2 – Scoring de Clientes (Regresión Logística)
# Tabs: Intro. | Modelos | Umbral | Resultados
# Mantiene la misma estructura visual del Módulo 1
# -------------------------------------------------------------------

mod_m2_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      # -------------------------
      # Panel izquierdo: Inputs
      # -------------------------
      shiny::column(
        width = 3,
        shiny::h3("Inputs"),
        shiny::tags$strong("Variables predictoras"),
        shiny::checkboxGroupInput(
          inputId = ns("vars"),
          label   = NULL,
          choices = c(
            # Demográficas / básicas
            "edad","estado_civil","ubicacion","nivel_educativo",
            "tipo_ocupacion","rubro_laboral","n_dependientes",
            # Historial / financieras
            "antiguedad_cliente","ingreso_declarado","ingreso_verificado",
            "rfm","score_buro","capacidad_endeud","endeudamiento_total"
          ),
          selected = c("edad","ingreso_declarado","rfm","score_buro")
        ),
      ),

      # -------------------------
      # Panel derecho: Resultados (tabs)
      # -------------------------
      shiny::column(
        width = 9,
        shiny::h3("Módulo 2: Scoring del cliente"),
        shiny::tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 0: Intro
          shiny::tabPanel(
            title = "Intro.",
            shiny::br(),
            shiny::h4("Propósito"),
            shiny::p("Este módulo estima, para cada cliente, la probabilidad de aceptar la oferta y la probabilidad de incurrir en mora mediante regresión logística. 
                     Se reportan los coeficientes con sus p-valores, se construye un score integrado que pondera aceptación y riesgo, y se selecciona un umbral de decisión."),
            shiny::h5("Flujo general"),
            shiny::tags$ul(
              shiny::tags$li("Selecciona variables predictoras."),
              shiny::tags$li("Entrena los modelos logísticos de aceptación y mora."),
              shiny::tags$li("Revisa la significancia de coeficientes (p-valores) y el AUC."),
              shiny::tags$li("Evalúa métricas por umbral y elige el que optimiza tu objetivo."),
              shiny::tags$li("Confirma y exporta resultados para módulos posteriores."),
            shiny::tags$hr(),
            shiny::actionButton(ns("train_models"), "Entrenar Modelos (Logit)")
            )
          ),

          # ---- Tab 1: Modelos
          shiny::tabPanel(
            title = "Modelos",
            shiny::h4("Regresión Logística"),
            shiny::p("Se muestran los coeficientes del modelo con su p-valor asociado. 
                     El AUC resume la capacidad discriminante de cada modelo."),
            shiny::tags$hr(),
            shiny::h5("Modelo: Probabilidad de Aceptación"),
            DT::DTOutput(ns("tbl_coefs_accept")),
            shiny::p(shiny::strong("AUC aceptación: "), shiny::textOutput(ns("auc_accept"), inline = TRUE)),
            shiny::tags$hr(),
            shiny::h5("Modelo: Probabilidad de Mora"),
            DT::DTOutput(ns("tbl_coefs_mora")),
            shiny::p(shiny::strong("AUC mora: "), shiny::textOutput(ns("auc_mora"), inline = TRUE))
          ),

          # ---- Tab 2: Umbral

          shiny::tabPanel(
            title = "Umbral",
            shiny::h4("Selección de umbral sobre el score integrado"),
            shiny::p("El score integrado favorece alta aceptación y bajo riesgo: score = p(aceptar) × (1 − p(mora)). 
                     Ajusta el umbral y compara métricas."),
            shiny::br(), shiny::br(),
            shiny::actionButton(ns("eval_thresholds"), "Evaluar Umbrales (score integrado)"),
            shiny::sliderInput(ns("thr"), "Umbral de decisión", min = 0, max = 1, value = 0.5, step = 0.01),
            shiny::actionButton(ns("apply_thr"), "Aplicar umbral"),
            shiny::br(), shiny::br(),
            shiny::plotOutput(ns("plot_metrics"), height = 260),
            DT::DTOutput(ns("tbl_thr_metrics"))
          ),

          # ---- Tab 3: Resultados
          shiny::tabPanel(
            title = "Resultados",
            shiny::br(),
            shiny::plotOutput(ns("plot_scores"), height = 240),
            DT::DTOutput(ns("tbl_scores")),
            shiny::br(),
            shiny::actionButton(ns("confirmar"), "Confirmar y Guardar"),
            shiny::actionButton(ns("reiniciar"), "Reiniciar Módulo 2", style = "margin-left:6px;"),
            shiny::actionButton(ns("exportar"),  "Exportar resultados", style = "margin-left:6px;"),
            shiny::uiOutput(ns("feedback"))
          )
        )
      )
    )
  )
}
