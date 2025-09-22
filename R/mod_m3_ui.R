# R/mod_m3_ui.R
# UI del Módulo 3 – Pricing y Elasticidad
# Replica el layout de M1/M2: panel izquierdo (inputs) + panel derecho (tabs)

mod_m3_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      # Panel izquierdo: Inputs
      shiny::column(
        width = 3,
        shiny::h3("Inputs"),
        shiny::tags$strong("Variables numéricas disponibles"),
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
      ),
      # Panel derecho: Tabs
      shiny::column(
        width = 9,
        shiny::h3("Módulo 3: Pricing y Elasticidad"),
        shiny::p("Se estima el margen esperado (ME) como función de la oferta (tasa, monto, plazo) y del perfil del cliente. \
                 Luego se modela ME con regresión lineal y se derivan elasticidades."),
        shiny::tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 1: Introducción
          shiny::tabPanel(
            title = "Introducción",
            shiny::br(),
            shiny::h4(shiny::strong("Propósito")),
            shiny::p("Este módulo estima el ", shiny::strong("margen esperado (ME)"), " de cada cliente como función de las características del cliente y los términos de la oferta de crédito (tasa, monto, plazo). El ME se calcula como el ingreso bruto esperado menos las pérdidas esperadas por mora. Con estos datos, se construye un modelo de regresión lineal que relaciona el ME con las variables predictoras, permitiendo identificar qué factores influyen en la rentabilidad de cada cliente y derivar elasticidades para optimizar el pricing."),

            shiny::h4(shiny::strong("Definiciones básicas")),
            shiny::tags$ul(
              shiny::tags$li("El ", shiny::strong("margen esperado (ME)"), " es la ganancia neta esperada por cliente: \\(ME = \\text{Ingreso bruto} \\times P(\\text{aceptar}) \\times (1 - P(\\text{mora}))\\)."),
              shiny::tags$li("La ", shiny::strong("regresión lineal"), " modela la relación entre una variable respuesta (ME) y variables predictoras mediante: \\(Y = \\beta_0 + \\beta_1 X_1 + \\cdots + \\beta_p X_p + \\epsilon\\)."),
              shiny::tags$li("Las ", shiny::strong("elasticidades"), " miden el cambio porcentual en ME ante un cambio porcentual en una variable predictora.")
            ),

            shiny::h4(shiny::strong("Funcionamiento del modelo")),
            shiny::p("La regresión lineal asume una relación lineal entre las variables predictoras y la respuesta. Los coeficientes \\(\\beta_j\\) indican cuánto cambia ME (en unidades) cuando la variable correspondiente aumenta en una unidad, manteniendo las demás constantes."),

            shiny::h4(shiny::strong("Contexto del problema de negocio")),
            shiny::p("En el pricing de préstamos, la entidad financiera busca maximizar el margen esperado considerando tanto la probabilidad de aceptación como el riesgo de mora. El modelo de regresión lineal permite:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Identificar factores clave:"), " qué variables del cliente y de la oferta afectan más el ME."),
              shiny::tags$li(shiny::strong("Optimizar precios:"), " ajustar tasa, monto y plazo para maximizar rentabilidad por cliente."),
              shiny::tags$li(shiny::strong("Segmentar clientes:"), " ofrecer términos diferenciados según perfil de riesgo y aceptación.")
            ),

            shiny::h4(shiny::strong("Flujo general")),
            shiny::tags$ul(
              shiny::tags$li("Explorar correlaciones entre variables y detectar multicolinealidad."),
              shiny::tags$li("Ajustar el nivel de significancia y validar el modelo global."),
              shiny::tags$li("Analizar significancia e interpretar coeficientes."),
              shiny::tags$li("Seleccionar variables y comparar modelos alternativos."),
              shiny::tags$li("Realizar predicciones con nuevos datos de clientes.")
            )
          ),

          # ---- Tab 2: Matriz de correlaciones
          shiny::tabPanel(
            title = "Matriz de correlaciones",
            shiny::h4(shiny::strong("¿Qué es el coeficiente de correlación?")),
            shiny::p("El coeficiente de correlación de Pearson mide la fuerza y dirección de la relación lineal entre dos variables. Toma valores entre -1 y 1:"),
            shiny::tags$ul(
              shiny::tags$li("+1: correlación positiva perfecta (ambas variables aumentan juntas)"),
              shiny::tags$li("-1: correlación negativa perfecta (una aumenta cuando la otra disminuye)"),
              shiny::tags$li("0: no hay relación lineal")
            ),
            shiny::p("En el contexto del modelo de regresión lineal, buscamos variables con correlación alta (en valor absoluto) con la variable respuesta (ME), ya que estas serán mejores predictoras."),

            shiny::h4(shiny::strong("Reconocer las variables con mayor asociación")),
            shiny::p("Selecciona las variables numéricas en el panel izquierdo y calcula la matriz de correlación. Identifica cuáles tienen la correlación más alta con ME."),

            shiny::h4(shiny::strong("¿Qué es la Multicolinealidad?")),
            shiny::p("La multicolinealidad ocurre cuando dos o más variables predictoras están altamente correlacionadas entre sí. Esto puede causar problemas como:"),
            shiny::tags$ul(
              shiny::tags$li("Coeficientes de regresión inestables (cambian mucho con pequeños cambios en los datos)"),
              shiny::tags$li("Sobreajuste del modelo (buen ajuste en entrenamiento pero malo en nuevos datos)"),
              shiny::tags$li("Dificultades para interpretar la importancia individual de cada variable")
            ),
            shiny::p("Si ves que dos variables están muy correlacionadas (ejemplo: correlación > 0.95), sospecha de multicolinealidad."),

            shiny::h4(shiny::strong("Medir VIF (Variance Inflation Factor)")),
            shiny::p("El VIF mide cuánto se infla la varianza de un coeficiente debido a multicolinealidad:"),
            shiny::helpText("$$VIF_j = \\frac{1}{1 - R_j^2}$$"),
            shiny::p("donde \\(R_j^2\\) es el coeficiente de determinación de la regresión de la variable j sobre las demás."),
            shiny::tags$ul(
              shiny::tags$li("VIF = 1: no hay multicolinealidad"),
              shiny::tags$li("VIF entre 1 y 5: multicolinealidad moderada"),
              shiny::tags$li("VIF > 5 o 10: multicolinealidad alta (considerar eliminar variables)")
            ),

            shiny::h4(shiny::strong("Selección preliminar de variables")),
            shiny::p("Basándote en las correlaciones con ME y los valores VIF, selecciona un conjunto inicial de variables para el modelo. Evita incluir variables altamente correlacionadas entre sí."),

            shiny::actionButton(ns("calcular_cor"), "Calcular correlaciones y VIF"),
            shiny::br(), shiny::br(),
            shiny::plotOutput(ns("cor_plot"), height = "420px"),
            DT::DTOutput(ns("cor_table")),
            shiny::br(),
            shiny::h5("Valores VIF:"),
            DT::DTOutput(ns("vif_table"))
          ),

          # ---- Tab 3: Análisis Modelo Regresión Lineal
          shiny::tabPanel(
            title = "Análisis Modelo Regresión Lineal",
            shiny::h4(shiny::strong("Ajuste del nivel de significancia α")),
            shiny::p("Similar al Módulo 2, el nivel de significancia α determina qué tan estrictos somos al considerar que una variable tiene efecto real sobre ME. Un α más bajo requiere más evidencia estadística."),

            shiny::h4(shiny::strong("Prueba de Hipótesis para el Modelo (p-valor)")),
            shiny::p("La prueba F evalúa si el modelo en conjunto explica significativamente la variabilidad en ME:"),
            shiny::helpText("\\(H_0: \\beta_1 = \\beta_2 = \\cdots = \\beta_p = 0\\) (ninguna variable explica ME)"),
            shiny::helpText("\\(H_a:\\) al menos un \\(\\beta_j \\neq 0\\)"),
            shiny::p("Si el p-valor de la prueba F es < α, rechazamos H₀ y concluimos que el modelo explica significativamente la variabilidad en ME."),

            shiny::h4(shiny::strong("Validar si el modelo se ajusta a la población")),
            shiny::p("Además de la significancia estadística, evaluamos qué tan bien el modelo se ajusta a los datos mediante:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("R²:"), " proporción de variabilidad en ME explicada por el modelo (0-1)"),
              shiny::tags$li(shiny::strong("R² ajustado:"), " R² penalizado por número de variables"),
              shiny::tags$li(shiny::strong("MSE:"), " error cuadrático medio (más bajo = mejor ajuste)"),
              shiny::tags$li(shiny::strong("Análisis de residuos:"), " verificar normalidad, homocedasticidad y ausencia de patrones")
            ),

            shiny::actionButton(ns("ajustar_modelo"), "Ajustar modelo de regresión lineal"),
            shiny::br(), shiny::br(),
            shiny::verbatimTextOutput(ns("model_summary")),
            shiny::br(),
            shiny::h5("Métricas de ajuste del modelo:"),
            DT::DTOutput(ns("model_metrics")),
            shiny::plotOutput(ns("resid_plot"), height = "320px")
          ),

          # ---- Tab 4: Análisis Coeficientes Regresión Lineal
          shiny::tabPanel(
            title = "Análisis Coeficientes Regresión Lineal",
            shiny::h4(shiny::strong("Prueba de Hipótesis para Coeficientes (p-valor)")),
            shiny::p("Para cada coeficiente \\(\\beta_j\\), realizamos una prueba t:"),
            shiny::helpText("\\(H_0: \\beta_j = 0\\) (la variable j no afecta ME)"),
            shiny::helpText("\\(H_a: \\beta_j \\neq 0\\)"),
            shiny::p("Si p-valor < α, la variable es estadísticamente significativa."),

            shiny::h4(shiny::strong("Interpretación de coeficientes del Modelo")),
            shiny::p("Cada coeficiente indica cuánto cambia ME (en unidades monetarias) cuando la variable aumenta en una unidad:"),
            shiny::tags$ul(
              shiny::tags$li("Coeficiente positivo: la variable aumenta ME"),
              shiny::tags$li("Coeficiente negativo: la variable disminuye ME"),
              shiny::tags$li("Magnitud: efecto absoluto (coeficientes más grandes = mayor impacto)")
            ),

            shiny::h4(shiny::strong("Seleccionar las variables y reentrenar")),
            shiny::p("Basándote en la significancia estadística y la importancia de negocio, selecciona qué variables mantener en el modelo final."),

            DT::DTOutput(ns("coef_table")),
            shiny::br(),
            shiny::checkboxGroupInput(ns("keep_vars"), "Variables a mantener en el modelo:",
                                    choices = c(), selected = c()),
            shiny::actionButton(ns("retrain_model"), "Reentrenar modelo con variables seleccionadas"),
            shiny::br(), shiny::br(),
            shiny::verbatimTextOutput(ns("model_final_summary"))
          ),

          # ---- Tab 5: Comparación de Modelos
          shiny::tabPanel(
            title = "Comparación de Modelos",
            shiny::h4(shiny::strong("Explicación sobre métricas")),
            shiny::p("Para comparar modelos de regresión lineal, usamos varias métricas:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("R²:"), " proporción de varianza explicada (más alto = mejor, máximo 1)"),
              shiny::tags$li(shiny::strong("AIC (Akaike Information Criterion):"), " mide calidad del ajuste penalizando complejidad (más bajo = mejor)"),
              shiny::tags$li(shiny::strong("MSE (Mean Squared Error):"), " error promedio al cuadrado (más bajo = mejor)")
            ),

            shiny::h4(shiny::strong("Modelo generado por la computadora")),
            shiny::p("El modelo automático utiliza el método stepwise AIC para seleccionar automáticamente las mejores variables, buscando el mejor balance entre ajuste y parsimonia."),

            shiny::actionButton(ns("ajustar_auto"), "Generar modelo automático"),
            shiny::br(), shiny::br(),
            shiny::h5("Resumen del modelo automático:"),
            shiny::verbatimTextOutput(ns("auto_summary")),
            shiny::br(),
            shiny::h5("Comparación de modelos:"),
            DT::DTOutput(ns("comp_table")),

            shiny::h4(shiny::strong("Elección del modelo final")),
            shiny::p("Considera las métricas y elige qué modelo usarás para las predicciones:"),
            shiny::radioButtons(ns("modelo_final"), "Modelo para predicción:",
                              choices = c("Manual (seleccionado)" = "manual", "Automático (stepwise)" = "automatico"),
                              selected = "manual")
          ),

          # ---- Tab 6: Predicción
          shiny::tabPanel(
            title = "Predicción",
            shiny::h4(shiny::strong("Predicción con nuevos datos")),
            shiny::p("Ingresa los datos de un nuevo cliente para predecir su margen esperado (ME) usando el modelo seleccionado."),

            shiny::h5("Datos del cliente:"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::numericInput(ns("pred_score_buro"), "Score buro", value = 600, min = 0, max = 1000),
                shiny::numericInput(ns("pred_ingreso"), "Ingreso verificado", value = 5000, min = 0),
                shiny::numericInput(ns("pred_moras"), "N° moras previas", value = 0, min = 0)
              ),
              shiny::column(4,
                shiny::numericInput(ns("pred_rfm"), "RFM", value = 3, min = 1, max = 5),
                shiny::numericInput(ns("pred_tasa"), "Tasa ofrecida", value = 0.06, min = 0.01, max = 0.15, step = 0.005),
                shiny::numericInput(ns("pred_monto"), "Monto", value = 10000, min = 1000)
              ),
              shiny::column(4,
                shiny::numericInput(ns("pred_plazo"), "Plazo (meses)", value = 12, min = 3, max = 60),
                shiny::numericInput(ns("pred_p_accept"), "P(aceptar)", value = 0.7, min = 0, max = 1, step = 0.01),
                shiny::numericInput(ns("pred_p_mora"), "P(mora)", value = 0.1, min = 0, max = 1, step = 0.01)
              )
            ),

            shiny::actionButton(ns("predecir"), "Predecir ME"),
            shiny::br(), shiny::br(),
            shiny::h5("Resultado de la predicción:"),
            shiny::verbatimTextOutput(ns("pred_result")),
            shiny::br(),
            shiny::h5("Análisis de elasticidades:"),
            shiny::p("La elasticidad mide cuánto cambia porcentualmente ME ante un cambio porcentual en cada variable."),
            DT::DTOutput(ns("elasticidades"))
          )
        )
      )
    )
  )
}