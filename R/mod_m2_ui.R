# R/mod_m2_ui.R
# -------------------------------------------------------------------
# UI del Módulo 2 – Scoring de Clientes (Regresión Logística)
# Tabs: Introducción | Modelos | Umbral | Resultados
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
        shiny::helpText("Selecciona las variables que entrarán a los modelos de Aceptación y Mora."),
        shiny::checkboxGroupInput(
          inputId = ns("vars"),
          label   = NULL,
          choices = c(
            # Demográficas / básicas
            "edad","estado_civil","ubicacion","nivel_educativo",
            "tipo_ocupacion","rubro_laboral","n_dependientes",
            # Comportamiento histórico
            "antiguedad_cliente","n_moras_previas","dias_atraso_max","n_moras_leves",
            "productos_activos","frecuencia_uso","cancelaciones_anticipadas","rfm",
            # Financieras
            "ingreso_declarado","ingreso_verificado","cuota_ingreso","capacidad_endeudamiento",
            "endeudamiento_total","score_buro","tendencia_ingresos",
            # Segmentación (Módulo 1)
            "cluster_id"
          ),
          selected = c("edad","score_buro","n_moras_previas","rfm","ingreso_verificado","cuota_ingreso","cluster_id")
        ),
        shiny::br(),
        shiny::actionButton(ns("train_models"), "Entrenar Modelos (Logit)", class = "btn-primary")
      ),

      # -------------------------
      # Panel derecho: Resultados (tabs)
      # -------------------------
      shiny::column(
        width = 9,
        shiny::h3("Módulo 2: Selección de Clientes (Scoring)"),
        shiny::tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 0: Introducción
          shiny::tabPanel(
            title = "Introducción",
            shiny::withMathJax(
              shiny::br(),
              shiny::h4("¿De qué trata esta etapa?"),
              shiny::HTML("
                <p>En <strong>Selección de clientes</strong> estimamos dos modelos de aprendizaje
                <em>supervisado</em> mediante <strong>regresión logística</strong>:
                (i) probabilidad de <em>aceptación</em> y (ii) probabilidad de <em>mora</em>.
                La salida se combina en un <strong>score integrado</strong> para decidir a quién ofrecer.</p>
              "),
              shiny::h5("¿Por qué regresión logística?"),
              shiny::HTML("
                <p>La logística modela directamente la probabilidad de un evento binario
                a partir de una combinación lineal de predictores y la función sigmoide:</p>
              "),
              shiny::HTML("$$
                P(Y=1\\mid X)=\\frac{1}{1+e^{-(\\beta_0+\\beta_1x_1+\\cdots+\\beta_px_p)}}
              $$"),
              shiny::h5("Score integrado"),
              shiny::HTML("$$
                \\text{Score} = P(\\text{aceptar})\\times (1-P(\\text{mora}))
              $$"),
              shiny::p("Un umbral configurable (p.ej., 0.65) define la elegibilidad."),
              shiny::tags$hr(),
              shiny::h5("Flujo general"),
              shiny::tags$ul(
                shiny::tags$li("Selecciona variables predictoras (incluye el cluster del Módulo 1)."),
                shiny::tags$li("Entrena los modelos de Aceptación y Mora."),
                shiny::tags$li("Revisa coeficientes y p-valores; valida AUC."),
                shiny::tags$li("Evalúa métricas por umbral sobre el score integrado."),
                shiny::tags$li("Confirma y exporta resultados.")
              )
            )
          ),

          # ---- Tab 1: Modelos
          shiny::tabPanel(
            title = "Modelos",
            shiny::h4("Regresión Logística"),
            shiny::p("Coeficientes con p-valor y AUC por modelo."),
            shiny::tags$hr(),

            # Aceptación
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h5("Modelo: Probabilidad de Aceptación"),
                DT::DTOutput(ns("tbl_coefs_accept")),
                shiny::p(shiny::strong("AUC aceptación: "),
                         shiny::textOutput(ns("auc_accept"), inline = TRUE)),
                shiny::tags$hr(),
                shiny::h6("Selección interactiva por p-valor"),
                shiny::sliderInput(ns("alpha_accept"), "α sugerido (p-valor):",
                                   min = 0.001, max = 0.20, value = 0.05, step = 0.001),
                shiny::checkboxGroupInput(ns("keep_vars_accept"),
                                          "Variables a mantener (Aceptación):",
                                          choices = NULL, selected = NULL),
                shiny::actionButton(ns("retrain_accept"), "Re-entrenar (Aceptación)",
                                    class = "btn-secondary")
              ),

              # Mora
              shiny::column(
                width = 6,
                shiny::h5("Modelo: Probabilidad de Mora"),
                DT::DTOutput(ns("tbl_coefs_mora")),
                shiny::p(shiny::strong("AUC mora: "),
                         shiny::textOutput(ns("auc_mora"), inline = TRUE)),
                shiny::tags$hr(),
                shiny::h6("Selección interactiva por p-valor"),
                shiny::sliderInput(ns("alpha_mora"), "α sugerido (p-valor):",
                                   min = 0.001, max = 0.20, value = 0.05, step = 0.001),
                shiny::checkboxGroupInput(ns("keep_vars_mora"),
                                          "Variables a mantener (Mora):",
                                          choices = NULL, selected = NULL),
                shiny::actionButton(ns("retrain_mora"), "Re-entrenar (Mora)",
                                    class = "btn-secondary")
              )
            )
          ),

          # ---- Tab 2: Umbral
          shiny::tabPanel(
            title = "Umbral",
            shiny::h4("Selección de umbral sobre el score integrado"),
            shiny::p("El score integrado favorece alta aceptación y bajo riesgo: score = p(aceptar) × (1 − p(mora)). 
                     Ajusta el umbral y compara métricas."),
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
            shiny::actionButton(ns("confirmar"), "Confirmar y Guardar", class = "btn-success"),
            shiny::actionButton(ns("reiniciar"), "Reiniciar Módulo 2", style = "margin-left:6px;"),
            shiny::actionButton(ns("exportar"),  "Exportar resultados", style = "margin-left:6px;"),
            shiny::uiOutput(ns("feedback"))
          )
        )
      )
    )
  )
}
