# UI del Módulo 1 – Perfilamiento del Cliente
# Tabs: Teoría | PCA | Elegir K | Clusters

mod_m1_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      # -------------------------
      # Panel izquierdo: Inputs
      # -------------------------
      column(
        width = 3,
        h3("Inputs"),
        tags$strong("Variables para segmentación"),
        checkboxGroupInput(
          inputId = ns("vars"),
          label   = NULL,
          choices = c(
            # Demográficas / básicas
            "edad","estado_civil","ubicacion","nivel_educativo",
            "tipo_ocupacion","rubro_laboral","n_dependientes",
            # Historial / financieras simples
            "antiguedad_cliente","ingreso_declarado","ingreso_verificado",
            # Agregadas clave
            "rfm","score_buro","capacidad_endeud","endeudamiento_total"
          ),
          selected = c("edad","ingreso_declarado","rfm","score_buro")
        )
      ),

      # -------------------------
      # Panel derecho: Resultados (tabs)
      # -------------------------
      column(
        width = 9,
        h3("Módulo 1: Perfilamiento del cliente"),
        tabsetPanel(
          id = ns("tabs"),

          # ---- Tab 0: Teoría
          tabPanel(
            title = "Intro.",
            br(),

            h4("Introducción"),

            p("En el mercado financiero, los préstamos personales exigen que el banco comprenda a sus clientes para agruparlos en perfiles que orienten 
            mejor las ofertas y la gestión del riesgo. Este primer paso —el perfilamiento— analiza información demográfica, financiera e histórica para 
            descubrir grupos con comportamientos similares y así planificar campañas más efectivas."),

            p("Existen múltiples formas de segmentar y aplicar técnicas de clustering. En este módulo trabajaremos con un 
            enfoque de aprendizaje no supervisado —no contamos con etiquetas previas—: el banco explora los datos y define nuevos segmentos a partir de 
            patrones emergentes. Para ello, aplicaremos dos técnicas combinadas: Análisis de Componentes Principales (PCA) para reducir la 
            dimensionalidad y K-Medias para formar clusters de clientes."),

            h5("Objetivos del Módulo 1"),

            tags$ul(
              tags$li("Comprender el rol de la segmentación dentro de una campaña de préstamos personales."),
              tags$li("Aprender la aplicación e interpretación de técnicas de aprendizaje no supervisado en contexto bancario."),
              tags$li("Usar PCA para simplificar la información y resaltar ejes de variación relevantes."),
              tags$li("Aplicar K-Medias para identificar grupos de clientes homogéneos."),
              tags$li("Interpretar los resultados: qué caracteriza a cada cluster y cómo usar esos perfiles en decisiones comerciales.")
            ),

            p("Selecciona a continuación las variables que van a participar del algoritmo de clustering.")

          ),

          # ---- Tab 1: PCA
          tabPanel(
            title = "PCA",
            h4("¿Qué es PCA?"),
            withMathJax(
            p("El análisis de componentes principales es un método de análisis multivariado que permite reemplazar un 
            número \\(p\\) de variables numéricas por un número menor de variables no correlacionadas manteniendo gran parte 
            de la información inicial. Las variables que se obtienen son llamadas componentes
            principales y cada una de ellas es una combinación lineal de las variables originales."),

            p("Si las variables originales son \\(X_1, X_2, ..., X_p\\), entonces las componentes principales 
            para este conjunto de variables son las combinaciones lineales:"),

            "$$Y_1 = u_{11}X_1 + u_{12}X_2 + \\cdots + u_{1p}X_p$$",
            "$$Y_2 = u_{21}X_1 + u_{22}X_2 + \\cdots + u_{2p}X_p$$",
            "$$\\vdots$$",
            "$$Y_p = u_{p1}X_1 + u_{p2}X_2 + \\cdots + u_{pp}X_p$$",

            p("Estas nuevas variables se construyen de tal manera que la primera componente principal \\(Y_1\\) 
            capte la mayor variabilidad de los datos. La segunda componente principal \\(Y_2\\) debe ser no 
            correlacionada con la primera y debe captar la mayor variabilidad que no ha sido expresada 
            por ésta. La tercera componente principal debe ser no correlacionada con las dos primeras y debe 
            captar la mayor variabilidad que no ha sido expresada por las dos primeras, y así sucesivamente."),

            p("En la práctica, se sugiere considerar un número de componentes que retengan entre 70% y 
            90% de la variación total.")
            ),

            tags$hr(),
            # Ejecutar PCA Completo
            actionButton(ns("analyze_var"), "Analizar Varianza (PCA completo)"),
            

            h4("¿Cómo elegir la cantidad de componentes principales?"),

            p("Para identificar el número de componentes principales a analizar, se utilizan dos enfoques 
            complementarios. El Scree plot muestra la cantidad de varianza explicada por cada componente, 
            permitiendo detectar el punto donde la curva deja de descender de forma pronunciada.  
            Ese punto sugiere que a partir de ahí los componentes adicionales aportan muy poca información."),

            p("La varianza acumulada indica qué proporción total de la información original se conserva al 
            considerar un conjunto de componentes. Como referencia, se suele recomendar retener entre un 
            70% y un 90% de la variabilidad para lograr un buen equilibrio entre simplificación y 
            representatividad de los datos."),

            br(),
            plotOutput(ns("plot_varianza"), height = 220),     # Scree + Varianza acumulada (tras 'Analizar Varianza')
            
            tags$hr(),
            # Elegir nº de componentes y ejecutar PCA reducido (para clustering)
            numericInput(ns("ncomp"), "Nº componentes para clustering", value = 2, min = 1, step = 1),
            actionButton(ns("run_pca"), "Ejecutar PCA (usar N componentes)"),

            h5("Gráfico Biplot"),

            p("Para interpretar el biplot de PCA, cada punto representa un individuo proyectado en las nuevas componentes principales. 
            Los ejes muestran PC1 y PC2 junto con el porcentaje de varianza que cada uno explica, lo que indica cuánta información de 
            los datos originales se conserva. Además, las flechas (loadings) representan las variables originales y señalan la dirección 
            y magnitud de su aporte a cada componente. De esta manera, los alumnos pueden identificar tanto la similitud entre individuos 
            (cercanía de puntos) como la influencia de las variables en la formación de las componentes."),   


            # Biplot (solo tras 'Ejecutar PCA')
            plotOutput(ns("plot_pca"), height = 260), 

          ),

          # ---- Tab 2: Elegir K
          tabPanel(
            title = "Elegir K",


            h4("¿Qué es K-Medias?"),
            p("K-Medias es un algoritmo de agrupamiento o clustering que permite dividir un conjunto de observaciones en K grupos distintos, llamados clusters. 
            Cada cluster reúne individuos similares entre sí y diferentes respecto a los de otros grupos."),

            p("El algoritmo funciona de forma iterativa: inicialmente se eligen K centroides (puntos representativos de cada cluster) y cada observación 
            se asigna al cluster cuyo centroide esté más cercano. Luego, los centroides se recalculan como el promedio de los puntos asignados, y el proceso 
            se repite hasta que las asignaciones ya no cambian o se alcanza la convergencia. Este método busca minimizar la variabilidad interna dentro de 
            cada grupo y maximizar la separación entre los diferentes clusters. La métrica más utilizada para medir cercanía es la distancia euclidiana, 
            lo que explica por qué K-Medias requiere trabajar con variables numéricas o con variables categóricas previamente transformadas a numéricas."),

            p("Es importante tener en cuenta que K-Medias es un aprendizaje no supervisado: no conocemos las etiquetas ni los grupos de antemano. 
            El banco aplica este método para descubrir segmentos ocultos en su base de clientes y definir perfiles útiles para campañas comerciales."),

            h4("¿Cómo elegir la cantidad de clusters K?"),
            h5("Método del codo (WSS)"),

            p("En el gráfico del codo se observa cómo disminuye la inercia o WSS (Within Sum of Squares) a medida que 
            aumenta el número de clusters K. Al inicio la caída es pronunciada porque cada nuevo cluster mejora 
            bastante la compactación, pero después la mejora se vuelve marginal. El “codo” es el punto donde la 
            curva deja de caer abruptamente y se vuelve más plana: ese es un buen candidato para elegir K, 
            porque equilibra simplicidad del modelo con capacidad de representar la estructura de los datos."),

            h5("Método de la silueta"),
            p("El gráfico de la silueta muestra, para cada K, el valor promedio de la silueta, que mide qué tan 
            bien separados y compactos están los clusters (valores cercanos a 1 indican mejor separación). 
            Un valor alto de silueta significa que los datos dentro de un cluster son similares entre sí y 
            diferentes a los de otros clusters. Por ello, el K con mayor silueta promedio suele ser la mejor 
            elección."),

            br(),
            actionButton(ns("eval_k"), "Ejecutar Método del Codo y Silueta"),

            br(),
            fluidRow(
              column(6, plotOutput(ns("plot_elbow"), height = 260)),   # Codo (WSS vs K)
              column(6, plotOutput(ns("plot_sil_k"), height = 260))    # Silueta vs K
            ),

            tags$hr(),
            numericInput(ns("k"),     "Nº clusters K", value = 3, min = 2, step = 1),
            actionButton(ns("run_km"),  "Ejecutar Clustering"),
            p("")
          ),

          # ---- Tab 3: Clusters
          tabPanel(
            title = "Clusters",
            br(),
            # Gráfico de clusters arriba
            plotOutput(ns("plot_clusters")),
            br(),

            # Tabla de métricas debajo del gráfico
            h5("Métricas del Clustering"),
            DT::DTOutput(ns("tabla_metricas")),
            br(),

            # Feedback debajo de la tabla
            uiOutput(ns("feedback")),

            tags$hr(),
            h4("Selección Automática de Cluster Óptimo"),
            p("La computadora puede seleccionar automáticamente el cluster con mayor potencial comercial basado en métricas como score crediticio promedio, ingreso promedio y potencial de margen esperado."),

            actionButton(ns("auto_select_cluster"), "Seleccionar Cluster Óptimo Automáticamente",
                        class = "btn-warning"),
            br(), br(),

            # Resultado de selección automática
            uiOutput(ns("auto_cluster_result")),
            DT::DTOutput(ns("cluster_potential_table")),

            br(),
            actionButton(ns("confirmar"), "Confirmar decisión"),
            actionButton(ns("reiniciar"), "Reiniciar Módulo 1", style = "margin-left:6px;"),
            actionButton(ns("exportar"),  "Exportar resultados", style = "margin-left:6px;")
          )
        )
      )
    )
  )
}


