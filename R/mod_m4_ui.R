# R/mod_m4_ui.R
# UI del Módulo 4 – Árboles de Clasificación
# Tabs: Introducción | Interpretación de Nodos | Poda del Árbol | Matriz de Confusión y Métricas | Clasificación de Alertas

mod_m4_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      # -------------------------------
      # Panel izquierdo: Inputs
      # -------------------------------
      column(
        width = 3,
        h3("Inputs"),
        tags$strong("Variables predictoras"),
        # Bloque dinámico para modo demo / modo manual
        uiOutput(ns("vars_block")),
        tags$hr(),
        checkboxInput(
          inputId = ns("demo_auto"),
          label   = "Usar modo demo (modelo preconfigurado por el equipo)",
          value   = TRUE
        ),
        tags$hr(),
        tags$strong("Variable dependiente"),
        selectInput(
          inputId = ns("var_dependiente"),
          label   = NULL,
          choices = c("alerta_riesgo" = "alerta_riesgo"),
          selected = "alerta_riesgo"
        ),
        tags$hr(),
        actionButton(ns("entrenar_modelo"), "Entrenar Modelo",
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
            h4("Contexto dentro del proceso de negocio"),
            p("Los árboles de clasificación son herramientas poderosas para la detección de alertas tempranas en el sector financiero. Permiten identificar patrones en los datos históricos de clientes que predicen comportamientos de riesgo, facilitando intervenciones preventivas antes de que ocurran problemas mayores."),
            p("En este módulo, aplicaremos un árbol de clasificación para predecir alertas de riesgo basadas en características demográficas, financieras e históricas del cliente. El modelo aprenderá de datos históricos para clasificar nuevos casos en categorías de riesgo."),
            br(),
            h4("Definición de Árbol de Clasificación"),
            p("Un árbol de clasificación es un modelo predictivo que divide recursivamente el conjunto de datos en subconjuntos más homogéneos basándose en reglas de decisión. Cada división se realiza en un nodo interno, y las hojas finales representan las clases predichas."),
            tags$ul(
              tags$li(strong("Variable dependiente multiclase:"), " La variable objetivo puede tener múltiples categorías (ej: bajo, medio, alto riesgo)"),
              tags$li(strong("Variables predictoras:"), " Características del cliente que ayudan a predecir el riesgo"),
              tags$li(strong("Parámetros del modelo:"), " Índice Gini (mide impureza) y entropía (mide desorden)")
            ),
            br(),
            h4("Selección de variables"),
            p("Selecciona las variables predictoras en el panel izquierdo que consideres relevantes para predecir alertas de riesgo. El sistema utilizará un dataset histórico interno para entrenar el modelo."),
            p("Recuerda que una buena selección de variables mejora la capacidad predictiva del árbol y facilita su interpretación.")
          ),

          # ---- Tab 2: Interpretación de Nodos
          tabPanel(
            title = "Interpretación de Nodos",
            br(),
            h4("Explicación de las Partes del Árbol"),
            p("El árbol de clasificación consta de varios elementos clave:"),
            tags$ul(
              tags$li(strong("Raíz:"), " El nodo inicial que representa todo el conjunto de datos"),
              tags$li(strong("Nodos internos:"), " Puntos de decisión donde se divide el conjunto de datos"),
              tags$li(strong("Hojas (nodos terminales):"), " Nodos finales que contienen la predicción de clase")
            ),
            p("Cada división se basa en una regla condicional que maximiza la separación entre clases."),
            br(),
            h4("Visualización del Árbol"),
            plotOutput(ns("plot_arbol"), height = "500px"),
            br(),
            h4("Pregunta teórica"),
            p("¿Qué representa un nodo terminal dentro del árbol de clasificación?"),
            radioButtons(
              ns("pregunta_nodo"),
              label = NULL,
              choices = c(
                "Un punto de decisión donde se divide el conjunto de datos" = "decision",
                "La predicción final de clase para un subconjunto de observaciones" = "prediccion",
                "El cálculo de impureza del conjunto de datos" = "impureza"
              ),
              selected = character(0)
            ),
            actionButton(ns("validar_pregunta"), "Validar Respuesta"),
            uiOutput(ns("feedback_pregunta")),
            br(),
            h4("Guía de interpretación de nodos"),
            p("Cada nodo terminal muestra información clave para su interpretación:"),
            tags$ul(
              tags$li(strong("Clase predominante:"), " La categoría de riesgo que predice el nodo (bajo, medio, alto)"),
              tags$li(strong("Probabilidades:"), " Los valores decimales muestran la proporción de cada clase en el nodo"),
              tags$li(strong("Porcentaje:"), " Indica qué fracción de las observaciones de entrenamiento llegan a este nodo"),
              tags$li(strong("Interpretación:"), " Los nodos con alta pureza (>80% de una clase) son más confiables")
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
            p("Se ha seleccionado un nodo terminal aleatorio del árbol. Analiza la información proporcionada y escribe tu interpretación:"),
            uiOutput(ns("info_nodo_aleatorio")),
            textAreaInput(ns("interpretacion_nodo"), "Tu interpretación:",
                         rows = 4, placeholder = "Describe qué características tienen los clientes en este nodo y qué alerta de riesgo predice..."),
            actionButton(ns("guardar_interpretacion"), "Enviar"),
            uiOutput(ns("feedback_interpretacion"))
          ),

          # ---- Tab 3: Poda del Árbol
          tabPanel(
            title = "Poda del Árbol",
            br(),
            h4("Definición de Poda y función objetivo"),
            p("La poda es el proceso de reducir la complejidad del árbol eliminando ramas que no aportan información significativa. Esto previene el sobreajuste (overfitting), donde el modelo se ajusta demasiado a los datos de entrenamiento pero falla en nuevos datos."),
            p("El proceso de poda utiliza una función objetivo que balancea el error de predicción con la complejidad del modelo:"),
            helpText("$$Error(T) + \\alpha \\cdot |T|$$"),
            p("donde T es el árbol, |T| es el número de nodos terminales, y α es un parámetro que controla el tradeoff entre precisión y simplicidad."),
            br(),
            h4("¿Cómo funciona el proceso de poda?"),
            p("La poda utiliza validación cruzada para encontrar el árbol óptimo:"),
            tags$ul(
              tags$li(strong("Validación cruzada:"), " Técnica que divide los datos en varios subconjuntos, entrena el modelo en algunos y lo valida en otros para evitar sobreajuste"),
              tags$li(strong("Criterio de selección:"), " Se prueban diferentes niveles de poda y se selecciona aquel que minimiza el error de validación cruzada"),
              tags$li(strong("Índice Gini:"), " Mide la impureza de los nodos (valores más bajos = mayor pureza). La poda busca nodos donde dividir no reduce significativamente la impureza")
            ),
            p("El proceso es automático pero se basa en optimización matemática para encontrar el mejor balance entre precisión y simplicidad."),
            br(),
            h4("Interpretación del gráfico: Error vs Tamaño del Árbol"),
            p("Antes de aplicar la poda, analiza el gráfico para identificar el punto óptimo:"),
            plotOutput(ns("plot_error_vs_size"), height = "300px"),
            p("El gráfico muestra cómo cambia el error de validación cruzada según el tamaño del árbol. El punto óptimo es donde el error deja de disminuir significativamente."),
            textAreaInput(ns("interpretacion_grafico"), "¿Dónde está el punto óptimo en el gráfico? ¿Por qué?",
                        rows = 3, placeholder = "El punto óptimo está en X nodos porque después de ese punto el error..."),
            actionButton(ns("validar_grafico"), "Validar Interpretación"),
            uiOutput(ns("feedback_grafico")),
            br(),
            hr(),
            actionButton(ns("aplicar_poda"), "Aplicar Poda", class = "btn-warning"),
            br(), br(),
            h4("Visualización Antes y Después + Métricas de Rendimiento"),
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
            p("Comparamos el rendimiento de ambos árboles usando métricas en el conjunto de prueba:"),
            tags$ul(
              tags$li(strong("Accuracy:"), " Proporción de predicciones correctas"),
              tags$li(strong("Error de clasificación:"), " Proporción de predicciones incorrectas (1 - Accuracy)")
            ),
            DT::DTOutput(ns("tabla_comparacion_arboles")),
            br(),
            h4("Preguntas"),
            p("¿El árbol podado mantiene rendimiento similar al original?"),
            radioButtons(ns("pregunta_rendimiento"), NULL,
                        choices = c("Sí, mantiene rendimiento similar" = "si",
                                   "No, el rendimiento disminuye significativamente" = "no"),
                        selected = character(0)),
            p("¿Cuántos nodos eliminó la poda?"),
            numericInput(ns("nodos_eliminados"), NULL, value = NA, min = 0),
            p("¿Qué ventaja tiene el árbol podado frente al original?"),
            textAreaInput(ns("ventaja_podado"), NULL, rows = 3,
                         placeholder = "El árbol podado es más simple, menos propenso a sobreajuste, más interpretable..."),
            actionButton(ns("validar_reflexiones"), "Validar Respuestas")
          ),

          # ---- Tab 4: Matriz de Confusión y Métricas
          tabPanel(
            title = "Matriz de Confusión y Métricas",
            br(),
            h4("Generar matriz de confusión"),
            p("La matriz de confusión muestra el rendimiento del modelo comparando las predicciones con los valores reales en una tabla 2x2."),
            plotOutput(ns("plot_matriz_confusion"), height = "400px"),
            br(),
            DT::DTOutput(ns("tabla_matriz_confusion")),
            br(),
            h4("Explicación detallada de métricas"),
            p("Las métricas se calculan a partir de los valores de la matriz de confusión:"),
            tags$ul(
              tags$li(strong("Verdaderos Positivos (TP):"), " Casos de alto riesgo correctamente identificados"),
              tags$li(strong("Verdaderos Negativos (TN):"), " Casos de bajo riesgo correctamente identificados"),
              tags$li(strong("Falsos Positivos (FP):"), " Casos de bajo riesgo incorrectamente clasificados como alto riesgo"),
              tags$li(strong("Falsos Negativos (FN):"), " Casos de alto riesgo incorrectamente clasificados como bajo riesgo")
            ),
            p("A partir de estos valores se calculan las métricas principales:"),
            tags$ul(
              tags$li(strong("Accuracy = (TP + TN) / (TP + TN + FP + FN):"), " Proporción total de predicciones correctas"),
              tags$li(strong("Sensibilidad = TP / (TP + FN):"), " Capacidad para detectar casos de alto riesgo (también llamado recall)"),
              tags$li(strong("Especificidad = TN / (TN + FP):"), " Capacidad para identificar correctamente casos de bajo riesgo"),
              tags$li(strong("F1-Score = 2 × (Precisión × Sensibilidad) / (Precisión + Sensibilidad):"), " Balance entre precisión y sensibilidad")
            ),
            p("El umbral de decisión determina cómo se clasifican las probabilidades en clases. Un umbral más bajo aumenta la sensibilidad pero puede reducir la especificidad."),
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
                         rows = 3, placeholder = "Explica qué significa este valor para la detección de alertas de riesgo..."),
            actionButton(ns("validar_metrica"), "Validar Respuesta"),
            uiOutput(ns("feedback_metrica")),
            br(),
            h4("Preguntas de Verdadero o Falso"),
            p("Responde las siguientes preguntas sobre conceptos de métricas de clasificación:"),
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
            p("El modelo podado ha sido aplicado a un conjunto de nuevas observaciones para predecir alertas de riesgo."),
            DT::DTOutput(ns("tabla_clasificacion")),
            br(),
            h4("Diagrama de Pie de la Clasificación"),
            plotOutput(ns("plot_pie_clasificacion"), height = "400px"),
            br(),
            h4("Redactar interpretación de la clasificación"),
            p("Basándote en los resultados de clasificación, redacta una interpretación de los patrones observados:"),
            textAreaInput(ns("interpretacion_clasificacion"), NULL, rows = 5,
                         placeholder = "Las alertas más frecuentes corresponden a clientes con bajo puntaje de estabilidad, lo que sugiere priorizar monitoreo preventivo..."),
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