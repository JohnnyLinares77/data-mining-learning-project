# R/mod_m4_ui.R
# UI del Módulo 4 – Árboles de Clasificación
# Tabs: Introducción | Interpretación de Nodos | Poda del Árbol | Matriz de Confusión y Métricas | Clasificación de Alertas

mod_m4_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    withMathJax(),
    tags$head(tags$style(HTML("textarea.form-control{min-height:140px;}"))),
    fluidRow(
      # -------------------------------
      # Panel izquierdo: Inputs
      # -------------------------------
      column(
        width = 3,
        h3("Configuración del modelo"),
        tags$strong("Variables predictoras"),
        p("Este módulo carga un conjunto de variables predictoras preconfiguradas para asegurar consistencia entre sesiones y permitir comparaciones válidas. Si habilitas el modo manual, seleccionas variables y observas cómo cambia la complejidad del árbol y su desempeño."),
        # Bloque dinámico para modo demo / modo manual
        uiOutput(ns("vars_block")),
        tags$hr(),
        tags$strong("Variable dependiente"),
        selectInput(
          inputId = ns("var_dependiente"),
          label   = NULL,
          choices = c("alerta_riesgo" = "alerta_riesgo"),
          selected = "alerta_riesgo"
        ),
        p("La variable objetivo es multiclase y representa el nivel de alerta de riesgo: bajo, medio o alto. El sistema construye esta etiqueta de forma consistente y verifica su integridad antes de entrenar."),
        tags$hr(),
        actionButton(ns("entrenar_modelo"), "Entrenar árbol de clasificación",
                     class = "btn-primary"),
        br(), br(),
        uiOutput(ns("mensaje_entrenamiento"))
      ),

      # -------------------------------
      # Panel derecho: Resultados (tabs)
      # -------------------------------
      column(
        width = 9,
        h3("Módulo 4: Árboles de Clasificación"),
        tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 1: Introducción
          tabPanel(
            title = "Introducción",
            br(),
            h4("Contexto y objetivo"),
            p("Un árbol de clasificación detecta alertas de riesgo al traducir patrones de datos en reglas explícitas y trazables. Este enfoque apoya decisiones operativas porque explica el 'por qué' de cada predicción y prioriza intervenciones preventivas."),
            p("El módulo entrena un árbol para clasificar clientes en tres niveles de alerta (bajo, medio, alto) usando características demográficas, financieras e históricas. El estudiante interpreta la estructura del árbol, aplica poda para mejorar generalización, evalúa métricas y aplica el modelo a nuevos casos con registro de resultados."),
            br(),
            h4("Definición de Árbol de Clasificación"),
            p("Un árbol de clasificación segmenta el conjunto de datos con reglas de decisión hasta alcanzar hojas relativamente homogéneas. Cada división ocurre en un nodo interno y cada hoja representa una clase predicha."),
            tags$ul(
              tags$li(strong("Variable dependiente (multiclase):"), " bajo, medio y alto riesgo."),
              tags$li(strong("Variables predictoras:"), " características del cliente que aportan señales de riesgo."),
              tags$li(strong("Criterios de división:"), " impureza (p. ej., índice Gini = 1 - ∑ p_k^2, y entropía = -∑ p_k log p_k) y ganancia de información.")
            ),
            br(),
            h4("Variables del modelo"),
            p("El sistema carga variables predictoras preconfiguradas para lograr estabilidad entre corridas. En modo manual, el estudiante explora configuraciones alternativas y compara resultados para comprender el impacto de cada predictor.")
          ),

          # ---- Tab 2: Interpretación de Nodos
          tabPanel(
            title = "Interpretación de Nodos",
            br(),
            h4("Explicación de las Partes del Árbol"),
            p("El árbol de clasificación incluye elementos clave que orientan la lectura y la explicación de sus reglas:"),
            tags$ul(
              tags$li(strong("Raíz:"), " contiene todas las observaciones iniciales."),
              tags$li(strong("Nodos internos:"), " aplican reglas de decisión del tipo 'si condición entonces rama'."),
              tags$li(strong("Hojas (nodos terminales):"), " asignan una clase y muestran su distribución de probabilidades.")
            ),
            p("Cada regla divide los datos para aumentar la pureza de clase y conservar cobertura suficiente de casos."),
            br(),
            h4("Visualización del Árbol"),
            plotOutput(ns("plot_arbol"), height = "500px"),
            br(),
            h4("Pregunta teórica"),
            p("¿Qué representa un nodo terminal en un árbol de clasificación?"),
            radioButtons(
              ns("pregunta_nodo"),
              label = NULL,
              choices = c(
                "Un punto de decisión donde se divide el conjunto de datos" = "decision",
                "La clase final y su distribución de probabilidades para un subconjunto de observaciones" = "prediccion",
                "El cálculo de impureza del conjunto de datos" = "impureza"
              ),
              selected = character(0)
            ),
            actionButton(ns("validar_pregunta"), "Validar Respuesta"),
            uiOutput(ns("feedback_pregunta")),
            br(),
            h4("Guía de interpretación de nodos"),
            p("Cada hoja muestra información clave para su interpretación y el soporte de decisiones:"),
            tags$ul(
              tags$li(strong("Clase predominante:"), " categoría con mayor proporción en la hoja."),
              tags$li(strong("Probabilidades por clase:"), " mezcla de bajo, medio y alto en la hoja."),
              tags$li(strong("Cobertura:"), " porcentaje del conjunto de entrenamiento que llega a la hoja."),
              tags$li(strong("Confianza sugerida:"), " hojas con pureza ≥ 80% suelen ofrecer interpretaciones robustas.")
            ),
            p("Por ejemplo, un nodo que muestra 'bajo 0.79 0.21 0.00 47%' significa:"),
            tags$ul(
              tags$li("Predice riesgo BAJO"),
              tags$li("79% de las observaciones en este nodo tienen riesgo bajo"),
              tags$li("21% tienen riesgo medio, 0% riesgo alto"),
              tags$li("Representa el 47% de los datos de entrenamiento")
            ),
            br(),
            h4("Interpretación del alumno (nodo al azar)"),
            p("El sistema selecciona una hoja al azar. Analiza la información y escribe una interpretación útil para la operación:"),
            uiOutput(ns("info_nodo_aleatorio")),
            textAreaInput(ns("interpretacion_nodo"), "Tu interpretación:",
                         rows = 4, placeholder = "Describe el perfil dominante de la hoja, explica por qué el árbol llega a esta conclusión y sugiere una acción operativa (p. ej., monitoreo, contacto preventivo, oferta específica)."),
            actionButton(ns("guardar_interpretacion"), "Enviar"),
            uiOutput(ns("feedback_interpretacion"))
          ),

          # ---- Tab 3: Poda del Árbol
          tabPanel(
            title = "Poda del Árbol",
            br(),
            h4("Poda por complejidad del costo"),
            p("La poda reduce complejidad al eliminar ramas de aporte marginal. El objetivo es mantener el árbol lo bastante simple para explicarlo y lo bastante preciso para generalizar."),
            p("La función objetivo equilibra error y complejidad del modelo:"),
            helpText("$$\\mathrm{Error}(T) + \\alpha \\cdot |T|$$"),
            helpText("$$\\text{con } \\alpha \\propto cp \\quad \\text{y} \\quad |T| = \\text{número de hojas}$$"),
            p("A mayor cp, mayor penalización a la complejidad del árbol."),
            br(),
            h4("¿Cómo funciona el proceso de poda?"),
            p("La poda evalúa múltiples valores de cp con validación cruzada y selecciona el tamaño que optimiza el compromiso entre precisión e interpretabilidad:"),
            tags$ul(
              tags$li(strong("Validación cruzada:"), " estima el error fuera de muestra para distintos tamaños de árbol."),
              tags$li(strong("Curva Error vs Tamaño:"), " identifica el punto donde el error deja de mejorar de forma apreciable."),
              tags$li(strong("Regla 1-SE (sugerida):"), " elige el árbol más simple cuyo error está dentro de una desviación estándar del mínimo.")
            ),
            p("El proceso encuentra el mejor balance entre precisión y simplicidad con base en evidencia empírica de validación."),
            br(),
            h4("Interpretación del gráfico: Error vs Tamaño del Árbol"),
            p("Identifica el tamaño con menor error de validación. Si varios tamaños empatan dentro del margen 1-SE, elige el más simple para mejorar interpretabilidad:"),
            plotOutput(ns("plot_error_vs_size"), height = "300px"),
            p("Observa cómo cambia el error con el tamaño. Un árbol más grande añade complejidad si el error ya no disminuye."),
            textAreaInput(ns("interpretacion_grafico"), "¿Dónde está el punto óptimo en el gráfico? ¿Por qué?",
                        rows = 5, width = "100%", placeholder = "El punto óptimo se ubica en X nodos porque a partir de ese tamaño el error no disminuye de manera significativa, por lo que un árbol más grande solo añade complejidad."),
            actionButton(ns("validar_grafico"), "Validar Interpretación"),
            uiOutput(ns("feedback_grafico")),
            br(),
            hr(),
            actionButton(ns("aplicar_poda"), "Aplicar Poda", class = "btn-warning"),
            br(), br(),
            h4("Comparación antes y después de la poda"),
            fluidRow(
              column(6,
                     h5("Árbol Original"),
                     plotOutput(ns("plot_arbol_original"), height = "300px")
              ),
              column(6,
                     h5("Árbol Podado"),
                     plotOutput(ns("plot_arbol_podado"), height = "300px")
              )
            ),
            br(),
            h4("Métricas de rendimiento"),
            p("Comparamos el desempeño en el conjunto de prueba con medidas estándar:"),
            tags$ul(
              tags$li(strong("Accuracy:"), " aciertos totales / total de casos."),
              tags$li(strong("Error de clasificación:"), " errores totales / total de casos (1 - Accuracy).")
            ),
            DT::DTOutput(ns("tabla_comparacion_arboles")),
            br(),
            h4("Preguntas"),
            p("¿El árbol podado mantiene rendimiento similar al original?"),
            radioButtons(ns("pregunta_rendimiento"), NULL,
                        choices = c("Sí, mantiene rendimiento similar" = "si",
                                   "No, el rendimiento disminuye significativamente" = "no"),
                        selected = character(0)),
            p("¿Cuántos nodos elimina la poda?"),
            numericInput(ns("nodos_eliminados"), NULL, value = NA, min = 0),
            p("¿Qué ventaja tiene el árbol podado frente al original?"),
            textAreaInput(ns("ventaja_podado"), NULL, rows = 5, width = "100%",
                         placeholder = "El árbol podado conserva precisión, reduce complejidad y facilita explicar decisiones y auditar reglas."),
            actionButton(ns("validar_reflexiones"), "Validar Respuestas")
          ),

          # ---- Tab 4: Matriz de Confusión y Métricas
          tabPanel(
            title = "Matriz de Confusión y Métricas",
            br(),
            h4("Generar matriz de confusión (binaria)"),
            p("Selecciona la clase positiva y un umbral. La matriz muestra TN, TP, FN y FP para esa clase (uno versus resto)."),
            selectInput(ns("clase_positiva"), "Clase positiva:",
                        choices = c("alto","medio","bajo"), selected = "alto"),
            plotOutput(ns("plot_matriz_confusion"), height = "400px"),
            br(),
            DT::DTOutput(ns("tabla_matriz_confusion")),
            br(),
            # 5) Métricas con significado de negocio (estilo Módulo 2)
            h4("Métricas clave y su interpretación de negocio"),

            h5("Accuracy (Exactitud)"),
            p("Proporción total de aciertos. Útil como visión global, pero puede ser engañosa con clases desbalanceadas."),
            helpText("$$Accuracy = \\frac{TP + TN}{TP + TN + FP + FN}$$"),

            h5("Sensibilidad (Recall o TPR)"),
            p("De todos los casos realmente positivos (p. ej., clientes de alto riesgo), ¿qué proporción detecta el modelo? Si es baja, el sistema no activa alertas necesarias."),
            helpText("$$Sensibilidad = \\frac{TP}{TP + FN}$$"),

            h5("Especificidad (TNR)"),
            p("De todos los casos realmente negativos, ¿qué proporción descarta el modelo? Si es baja, se generan demasiadas falsas alertas."),
            helpText("$$Especificidad = \\frac{TN}{TN + FP}$$"),

            h5("Precisión (Precision o VPP)"),
            p("De los casos que el modelo predice como positivos, ¿qué proporción realmente lo es? Mide la calidad de las alertas emitidas."),
            helpText("$$Precisi\\acute{o}n = \\frac{TP}{TP + FP}$$"),

            h5("F1 Score"),
            p("Equilibrio entre Precisión y Sensibilidad. Útil para balancear captura de alertas con costos de falsas alarmas."),
            helpText("$$F1 = 2 \\cdot \\frac{Precisi\\acute{o}n \\cdot Sensibilidad}{Precisi\\acute{o}n + Sensibilidad}$$"),

            p("El umbral de decisión convierte probabilidades en decisión binaria para la clase positiva (p. ej., 'alto' vs no 'alto'). Umbrales bajos aumentan recall y falsos positivos; umbrales altos hacen lo contrario."),
            sliderInput(ns("umbral_clasificacion"), "Umbral de clasificación:",
                        min = 0.1, max = 0.9, value = 0.5, step = 0.1),
            br(),
            h4("Interpretación de métricas del modelo actual"),
            uiOutput(ns("metricas_actuales")),
            selectInput(ns("metrica_interpretar"), "Selecciona una métrica para interpretar:",
                        choices = c("Sensibilidad" = "sensibilidad",
                                   "Especificidad" = "especificidad",
                                   "Accuracy" = "accuracy",
                                   "F1-Score" = "f1")),
            textAreaInput(ns("respuesta_metrica"), "Interpreta esta métrica en el contexto del modelo:",
                         rows = 5, width = "100%", placeholder = "Explica qué implica este valor para priorizar 'alto' riesgo. Indica si conviene ganar recall aunque aumenten falsos positivos en contextos de prevención."),
            actionButton(ns("validar_metrica"), "Validar Respuesta"),
            uiOutput(ns("feedback_metrica")),
            br(),
            h4("Preguntas de Verdadero o Falso"),
            p("Responde y valida conceptos clave sobre métricas de clasificación:"),
            uiOutput(ns("pregunta_vf_1")),
            radioButtons(ns("respuesta_vf_1"), NULL,
                        choices = c("Verdadero" = "verdadero",
                                   "Falso" = "falso"),
                        selected = character(0)),
            br(),
            uiOutput(ns("pregunta_vf_2")),
            radioButtons(ns("respuesta_vf_2"), NULL,
                        choices = c("Verdadero" = "verdadero",
                                   "Falso" = "falso"),
                        selected = character(0)),
            actionButton(ns("validar_vf"), "Validar Respuestas"),
            uiOutput(ns("feedback_vf"))
          ),

          # ---- Tab 5: Clasificación de Alertas
          tabPanel(
            title = "Clasificación de Alertas",
            br(),
            h4("Visualizar resultados"),
            p("El modelo podado clasifica nuevas observaciones y muestra para cada cliente la clase estimada y sus probabilidades por nivel de alerta."),
            DT::DTOutput(ns("tabla_clasificacion")),
            br(),
            h4("Diagrama de Pie de la Clasificación"),
            plotOutput(ns("plot_pie_clasificacion"), height = "400px"),
            br(),
            h4("Contexto antes de la interpretación"),
            uiOutput(ns("resumen_prev")),
            tags$ul(
              tags$li("Identifica la clase predominante y posibles sesgos por desbalance."),
              tags$li("Ubica hojas con alta pureza y cobertura relevante; cita reglas clave."),
              tags$li("Relaciona patrones con acciones operativas (monitoreo, contactabilidad, ofertas preventivas).")
            ),
            br(),
            h4("Redactar interpretación de la clasificación"),
            p("Redacta una interpretación útil para la operación con base en los resultados:"),
            textAreaInput(ns("interpretacion_clasificacion"), NULL, rows = 7, width = "100%",
                         placeholder = "Resume patrones observados (p. ej., 'alto' aparece con score bajo y moras previas), propone acciones (monitoreo, contactabilidad, oferta preventiva) y sugiere salvaguardas (revisión de sesgos, auditoría de reglas, nueva evidencia)."),
            actionButton(ns("guardar_interpretacion_final"), "Guardar Interpretación Final"),
            br(), br(),
            actionButton(ns("finalizar_modulo"), "Finalizar Módulo 4",
                         class = "btn-success")
          )
        )
      )
    )
  )
}