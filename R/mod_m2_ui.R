# R/mod_m2_ui.R
# -------------------------------------------------------------------
# UI del Módulo 2 – Scoring de Clientes (Regresión Logística)
# Tabs: Intro. | Modelos | Umbral | Resultados
# Mantiene la misma estructura visual del Módulo 1
# -------------------------------------------------------------------

mod_m2_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::withMathJax(),   # para fórmulas en Intro
    shiny::fluidRow(
      # -------------------------
      # Panel izquierdo: Inputs
      # -------------------------
      shiny::column(
        width = 3,
        shiny::h3("Inputs"),
        shiny::tags$strong("Variables predictoras"),
        shiny::helpText("Incluye el cluster del Módulo 1 si está disponible (cluster_id)."),
        shiny::checkboxGroupInput(
          inputId = ns("vars"),
          label   = NULL,
          choices = c(
            # Demográficas / básicas
            "edad","estado_civil","ubicacion","nivel_educativo",
            "tipo_ocupacion","rubro_laboral","n_dependientes",
            # Historial / financieras / derivadas
            "antiguedad_cliente","n_moras_previas","dias_atraso_max","n_moras_leves",
            "ingreso_declarado","ingreso_verificado","cuota_ingreso",
            "capacidad_endeudamiento","endeudamiento_total",
            "rfm","score_buro","tendencia_ingresos",
            # Cluster del Módulo 1
            "cluster_id"
          ),
          selected = c("edad","ingreso_verificado","rfm","score_buro","cluster_id")
        ),
        shiny::tags$hr(),
        shiny::numericInput(ns("alpha"), "\\(\\alpha\\) para significancia (p-valor)", value = 0.05, min = 0.001, max = 0.2, step = 0.001),
        shiny::actionButton(ns("train_models"), "Entrenar Modelos (Logit)")
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
            shiny::p("Este módulo estima, para cada cliente, la probabilidad de aceptar la oferta y la probabilidad de incurrir en mora mediante regresión logística. Se reportan los coeficientes con sus p-valores (4 decimales), se construye un score integrado que pondera aceptación y riesgo, y se selecciona un umbral de decisión."),
            shiny::h5("¿Por qué regresión logística aquí?"),
            shiny::p("Porque la variable respuesta es binaria en ambos casos: \\(Y_{acc}\\in\\{0,1\\}\\) para aceptar/no aceptar y \\(Y_{mor}\\in\\{0,1\\}\\) para mora/no mora. La función de enlace logística modela:"),
            shiny::helpText("$$\\Pr(Y=1 \\mid X)=\\frac{1}{1+e^{-(\\beta_0+\\beta_1 x_1+\\cdots+\\beta_p x_p)}}$$"),
            shiny::p("Se trata de aprendizaje supervisado: entrenamos con etiquetas (reales o simuladas) y evaluamos AUC y métricas por umbral."),
            shiny::h5("Flujo general"),
            shiny::tags$ul(
              shiny::tags$li("Selecciona variables predictoras (incluye el cluster del Módulo 1)."),
              shiny::tags$li("Entrena los modelos logísticos de Aceptación y Mora."),
              shiny::tags$li("Revisa significancia de coeficientes (p-valores) y el AUC."),
              shiny::tags$li("Usa la selección interactiva para (des)activar variables según p-valor o por criterio."),
              shiny::tags$li("Evalúa métricas por umbral sobre el score integrado y define el corte."),
              shiny::tags$li("Confirma y exporta resultados para módulos posteriores.")
            )
          ),

          # ---- Tab 1: Modelos
          shiny::tabPanel(
            title = "Modelos",
            shiny::h4("Regresión Logística"),
            shiny::p("Coeficientes con p-valor (4 decimales). Panel de la derecha para selección interactiva por p-valor; puedes forzar incluir variables."),
            shiny::tags$hr(),

            # ACEPTACIÓN
            shiny::h5("Modelo: Probabilidad de Aceptación"),
            shiny::fluidRow(
              shiny::column(8, DT::DTOutput(ns("tbl_coefs_accept"))),
              shiny::column(4,
                shiny::tags$strong("Mantener variables (aceptación)"),
                shiny::checkboxGroupInput(ns("keep_vars_accept"), label = NULL, choices = c()),
                shiny::tags$strong("Forzar incluir"),
                shiny::checkboxGroupInput(ns("force_keep_accept"), label = NULL, choices = c())
              )
            ),
            shiny::p(shiny::strong("AUC aceptación: "), shiny::textOutput(ns("auc_accept"), inline = TRUE)),

            shiny::tags$hr(),

            # MORA
            shiny::h5("Modelo: Probabilidad de Mora"),
            shiny::fluidRow(
              shiny::column(8, DT::DTOutput(ns("tbl_coefs_mora"))),
              shiny::column(4,
                shiny::tags$strong("Mantener variables (mora)"),
                shiny::checkboxGroupInput(ns("keep_vars_mora"), label = NULL, choices = c()),
                shiny::tags$strong("Forzar incluir"),
                shiny::checkboxGroupInput(ns("force_keep_mora"), label = NULL, choices = c())
              )
            ),
            shiny::p(shiny::strong("AUC mora: "), shiny::textOutput(ns("auc_mora"), inline = TRUE)),

            shiny::div(style="margin-top:8px;",
              shiny::actionButton(ns("retrain_selected"), "Aplicar selección y reentrenar")
            )
          ),

          # ---- Tab 2: Umbral
          shiny::tabPanel(
            title = "Umbral",
            shiny::h4("Selección de umbral sobre el score integrado"),
            shiny::p("El score integrado favorece alta aceptación y bajo riesgo: \\(\\text{score} = p(\\text{aceptar})\\times (1 - p(\\text{mora}))\\). Ajusta el umbral y compara métricas."),
            shiny::br(),
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
