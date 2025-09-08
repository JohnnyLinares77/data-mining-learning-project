mod_m2_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shinyjs::useShinyjs(),   # <— NUEVO: para habilitar/deshabilitar pestañas
    shiny::withMathJax(),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::h3("Inputs"),
        shiny::tags$strong("Variables predictoras"),
        shiny::helpText("Incluye el cluster del Módulo 1 si está disponible (cluster_id)."),
        shiny::checkboxGroupInput(
          inputId = ns("vars"),
          label   = NULL,
          choices = c(
            "edad","estado_civil","ubicacion","nivel_educativo","tipo_ocupacion","rubro_laboral","n_dependientes",
            "antiguedad_cliente","n_moras_previas","dias_atraso_max","n_moras_leves",
            "ingreso_declarado","ingreso_verificado","cuota_ingreso",
            "capacidad_endeudamiento","endeudamiento_total","rfm","score_buro","tendencia_ingresos",
            "cluster_id"
          ),
          selected = c("edad","ingreso_verificado","rfm","score_buro","cluster_id")
        )
      ),
      shiny::column(
        width = 9,
        shiny::h3("Módulo 2: Scoring del cliente"),

        shiny::tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 0: Intro (igual que tenías)
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
              shiny::tags$li("Revisa significancia de coeficientes (p-valores)."),
              shiny::tags$li("Realiza una interpretación guiada de los resultados."),
              shiny::tags$li("Selecciona variables y reentrena el modelo."),
              shiny::tags$li("Evalúa métricas por umbral y confirma resultados.")
            )
          ),

          # ---- Tab 1: Significancia
          shiny::tabPanel(
            title = "Significancia",
            shiny::h4("Significancia de Variables"),
            shiny::withMathJax( shiny::p("El nivel de significancia \\(\\alpha\\) cumple un papel esencial como umbral de decisión para la selección de variables 
            en regresión logística. Define cuánta evidencia estadística se exige para considerar que una variable tiene efecto sobre la respuesta y controla la 
            rigurosidad con la que se aceptan o descartan predictores."), 
            shiny::p("La evaluación se formula como una prueba de hipótesis para cada coeficiente:"), 
            shiny::p("\\(H_0: \\beta_j = 0\\)", style = "text-align:center"), 
            shiny::p("\\(H_a: \\beta_j \\neq 0\\)", style = "text-align:center"), 
            shiny::p("La regla de decisión compara el p-valor del estimador con \\(\\alpha\\): si el p-valor es menor que \\(\\alpha\\), se rechaza \\(H_0\\) y 
            la variable se considera estadísticamente significativa; si es mayor o igual, no se rechaza \\(H_0\\)."), 
            shiny::p("Reducir \\(\\alpha\\) (por ejemplo, de 0.05 a 0.01) hace más estricto el criterio: solo se retienen variables con evidencia muy sólida 
            (p-valores muy pequeños). Con ello se reduce la probabilidad de falsos positivos (error tipo I), pero aumenta el riesgo de excluir variables 
            realmente útiles (error tipo II)."), 
            shiny::p("Aumentar \\(\\alpha\\) (por ejemplo, de 0.05 a 0.10) relaja el criterio: se aceptan más variables como significativas, 
            incluso con menor evidencia estadística. Esto puede enriquecer el modelo con información adicional, aunque eleva la probabilidad de 
            incorporar predictores sin efecto real, incrementando el riesgo de error tipo I."), 
            shiny::p("La interpretación práctica exige equilibrar parsimonia, capacidad explicativa y objetivos de negocio. 
            Un \\(\\alpha\\) bajo favorece modelos más estables y concisos; un \\(\\alpha\\) alto produce modelos más inclusivos pero potencialmente 
            menos confiables."), 
            shiny::p("Además, la selección no depende solo del criterio estadístico: las reglas de negocio y el conocimiento del dominio pueden justificar 
            mantener variables no significativas (p-valor \\(\\geq\\) \\(\\alpha\\)) cuando aportan interpretabilidad, cumplimiento normativo, 
            trazabilidad de decisiones o son requisitos funcionales del despliegue. De igual modo, cuando hay restricciones operativas, puede priorizarse 
            la inclusión de variables con p-valores menores que \\(\\alpha\\) por su mayor evidencia estadística y robustez ante auditorías.") ),


            shiny::numericInput(ns("alpha"), "\\(\\alpha\\) para significancia (p-valor)", value = 0.05, min = 0.001, max = 0.2, step = 0.001),
            shiny::actionButton(ns("train_models"), "Entrenar Modelos (Logit)"),
            shiny::tags$hr()
          ),

          # ---- Tab 2: Interpretación
          shiny::tabPanel(
            title = "Análisis",
            shiny::h4("Análisis de variables del modelo"),
            shiny::helpText("Responde y envía para habilitar la pestaña 'Selección'."),

            shiny::h5("Modelo: Probabilidad de Aceptación de Crédito"),
            DT::DTOutput(ns("tbl_coefs_accept")),
            shiny::p(shiny::strong("AUC aceptación: "), shiny::textOutput(ns("auc_accept"), inline = TRUE)),
            shiny::tags$hr(),

            shiny::checkboxGroupInput(ns("interp_sig_vars"),
                                      label = "Marca las variables significativas con el α actual (p < α):",
                                      choices = c(), selected = c()),

            shiny::h5("Variable asignada para interpretar:"),
            shiny::uiOutput(ns("interp_var_target")),
            shiny::textAreaInput(ns("interp_text"),
                                 "Escriba la interpretación del coeficiente de la variable:",
                                 placeholder = "Redacta la interpretación del coeficiente, considere significancia y signo... ",
                                 width = "100%", height = "120px"),
            shiny::actionButton(ns("interp_enviar"), "Enviar interpretación"),
            shiny::tags$hr(),
            shiny::uiOutput(ns("interp_feedback"))
          ),

          # ---- Tab 3: Selección
          shiny::tabPanel(
            title = "Selección",
            shiny::h5("Selección de variables y reentrenamiento"),
            shiny::h4("Modelo de Probabilidad de Aceptación de Crédito"),
            shiny::fluidRow(
              shiny::column(6,
                DT::DTOutput(ns("tbl_coefs_accept")),
                shiny::tags$strong("Selección de variables a mantener"),
                shiny::checkboxGroupInput(ns("keep_vars_accept"), label = NULL, choices = c())
              ),
            ),
            shiny::h5("Mora"),
            shiny::fluidRow(
              shiny::column(6,
                DT::DTOutput(ns("tbl_coefs_mora")),
                shiny::tags$strong("Selección de variables a mantener"),
                shiny::checkboxGroupInput(ns("keep_vars_mora"), label = NULL, choices = c())
              ),
            ),
            shiny::div(style="margin-top:8px;",
              shiny::actionButton(ns("retrain_selected"), "Aplicar selección y reentrenar")
            )
          ),

          # ---- Tab 4: Umbral (igual)
          shiny::tabPanel(
            title = "Umbral",
            shiny::withMathJax(),

            shiny::p("En este módulo trabajamos con dos modelos: ",
                    "uno para la probabilidad de aceptación ",
                    "\\(p(aceptar)\\) y otro para la probabilidad de mora ",
                    "\\(p(mora)\\)."),

            shiny::p("Definimos el score integrado como:",
                    "\\[ \\text{score} = p(aceptar) \\times (1 - p(mora)) \\]"),

            shiny::p("El umbral (threshold) \\(\\tau\\) define la regla de decisión: ",
                    "si \\(\\text{score} \\geq \\tau\\), clasificamos al cliente como ",
                    "APROBADO; de lo contrario, como RECHAZADO."),

            shiny::p("Al entrenar el modelo con datos históricos (aprendizaje supervisado), ",
                    "podemos comparar las predicciones con los resultados reales ",
                    "y así calcular los aciertos y errores en una matriz de confusión."),

            shiny::p("De la matriz de confusión obtenemos varias métricas:"),
            shiny::p("• Accuracy: proporción de aciertos totales ",
                    "\\[ Accuracy = \\frac{TP + TN}{TP+TN+FP+FN} \\]"),
            shiny::p("• Sensibilidad (Recall): capacidad de detectar positivos ",
                    "\\[ Sensibilidad = \\frac{TP}{TP+FN} \\]"),
            shiny::p("• Especificidad: capacidad de detectar negativos ",
                    "\\[ Especificidad = \\frac{TN}{TN+FP} \\]"),
            shiny::p("• Precisión (Precision): de los aprobados, cuántos eran realmente buenos ",
                    "\\[ Precision = \\frac{TP}{TP+FP} \\]"),
            shiny::p("• F1 Score: equilibrio entre precisión y sensibilidad ",
                    "\\[ F1 = 2 \\cdot \\frac{Precision \\cdot Sensibilidad}{Precision + Sensibilidad} \\]"),

            shiny::p("El gráfico te muestra cómo varían estas métricas cuando cambias ",
                    "el valor del umbral \\(\\tau\\). Al bajar \\(\\tau\\), apruebas más clientes ",
                    "(mayor sensibilidad, menor especificidad). Al subirlo, apruebas menos ",
                    "clientes (mayor especificidad, menor sensibilidad)."),

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

          # ---- Tab 5: Resultados (igual)
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
