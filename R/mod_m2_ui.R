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
            shiny::h4(shiny::strong("Propósito")),
            shiny::p("Este módulo estima, para cada cliente, dos probabilidades clave: ",
                    shiny::strong("aceptar la oferta de crédito"), " y ",
                    shiny::strong("incurrir en mora"), " mediante regresión logística. 
                    Con estos resultados se construye un score integrado que pondera aceptación y riesgo, 
                    y finalmente se selecciona un umbral de decisión para apoyar la gestión comercial."),
            
            shiny::h4(shiny::strong("Definiciones básicas")),
            shiny::tags$ul(
              shiny::tags$li("La variable respuesta es binaria: \\(Y=1\\) indica que el evento ocurre (aceptación o mora), 
                            y \\(Y=0\\) indica que no ocurre."),
              shiny::tags$li("La ", shiny::strong("probabilidad de éxito"), " se denota como \\(p = \\Pr(Y=1)\\)."),
              shiny::tags$li("Las ", shiny::strong("odds"), " son la razón \\(\\tfrac{p}{1-p}\\), que compara éxito contra fracaso."),
              shiny::tags$li("El ", shiny::strong("logit"), " es el logaritmo natural de las odds: 
                            \\(\\text{logit}(p)=\\log\\tfrac{p}{1-p}\\). 
                            Es la escala lineal donde se estiman los coeficientes.")
            ),
            
            shiny::h4(shiny::strong("Funcionamiento del modelo")),
            shiny::p("La regresión logística utiliza la función sigmoide para mapear cualquier combinación lineal de variables predictoras 
                    a un valor entre 0 y 1, interpretable como probabilidad:"),
            shiny::helpText("$$\\Pr(Y=1 \\mid X)=\\frac{1}{1+e^{-(\\beta_0+\\beta_1 x_1+\\cdots+\\beta_p x_p)}}$$"),
            shiny::p("Cada coeficiente \\(\\beta_j\\) mide cómo cambia el logit (y, por tanto, las odds) 
                    cuando la variable correspondiente aumenta en una unidad, manteniendo las demás constantes."),

            shiny::h4(shiny::strong("Contexto del problema de negocio")),
            shiny::p("En la campaña de préstamos personales, la entidad financiera necesita balancear dos objetivos:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Maximizar aceptación:"), " identificar clientes con alta probabilidad de tomar el crédito."),
              shiny::tags$li(shiny::strong("Controlar riesgo de mora:"), " evitar otorgar créditos a clientes con alta probabilidad de incumplimiento.")
            ),
            shiny::p("El uso de regresión logística permite integrar ambas probabilidades en un ",
                    shiny::strong("score único"), 
                    " que facilita la segmentación, la toma de decisiones y la fijación de políticas de riesgo."),
            
            shiny::h4(shiny::strong("Flujo general")),
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
            shiny::h4(shiny::strong("Significancia de Variables")),
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

          # ---- Tab 2: Análisis
          shiny::tabPanel(
            title = "Análisis",
            shiny::h4(shiny::strong("Análisis de variables del modelo")),
            shiny::helpText("Responde y envía para habilitar la pestaña 'Selección'."),

            shiny::h5("Modelo: Probabilidad de Aceptación de Crédito"),
            DT::DTOutput(ns("tbl_coefs_accept_an")),          
            shiny::p(shiny::strong("AUC aceptación: "),
                    shiny::textOutput(ns("auc_accept"), inline = TRUE)),
            shiny::tags$hr(),

            shiny::checkboxGroupInput(ns("interp_sig_vars"),
              label   = "Marca las variables significativas con el α actual (p < α):",
              choices = c(), selected = c()
            ),



          # ---- Explicación didáctica (agrupada por tema con bullets y MathJax)
          shiny::h4(shiny::strong("Cómo interpretar los coeficientes")),

          # --- Tema 1: Idea general
          shiny::h5("Idea general"),
          shiny::tags$ul(
            shiny::tags$li("En regresión logística, los coeficientes actúan sobre el logit (logaritmo de las odds), no directamente sobre la probabilidad."),
            shiny::tags$li("Para una interpretación más intuitiva usamos el odds ratio (\\(OR = e^{\\beta}\\)), que indica cómo cambian las odds al aumentar una variable en una unidad.")
          ),

          # --- Tema 2: Odds y Odds Ratio
          shiny::h5("Odds y Odds Ratio"),
          shiny::tags$ul(
            shiny::tags$li("Las odds se definen como: \\( \\text{odds} = \\frac{p}{1-p} \\)."),
            shiny::tags$li("Ejemplo: si \\(p = 0.8\\), entonces \\(\\text{odds} = 4\\): por cada 1 'no', hay 4 'sí'."),
            shiny::tags$li("El OR muestra el cambio multiplicativo en las odds: si \\(\\beta = 0.20\\), entonces \\(OR = e^{0.20} \\approx 1.22\\) (aumento de 22%).")
          ),

          # --- Tema 3: Intercepto
          shiny::h5("Intercepto"),
          shiny::tags$ul(
            shiny::tags$li("Representa el logit y la probabilidad base cuando todas las variables valen 0 o están en la categoría de referencia."),
            shiny::tags$li("Sirve como punto de partida del modelo, pero rara vez es lo más importante en la interpretación práctica.")
          ),

          # --- Tema 4: Variables numéricas
          shiny::h5("Variables numéricas"),
          shiny::tags$ul(
            shiny::tags$li("El coeficiente \\(\\beta\\) indica el cambio en el logit por cada unidad adicional."),
            shiny::tags$li("En términos de OR: \\(OR = e^{\\beta}\\). Ejemplo: si \\(\\beta = 0.05\\), entonces \\(OR \\approx 1.05\\) (odds aumentan 5%).")
          ),

          # --- Tema 5: Variables categóricas
          shiny::h5("Variables categóricas"),
          shiny::tags$ul(
            shiny::tags$li("Se interpretan respecto a una categoría base."),
            shiny::tags$li("Si \\(OR > 1\\), la categoría tiene mayores odds que la base; si \\(OR < 1\\), menores."),
            shiny::tags$li("Ejemplo: frente a 'mujer' (base), si 'hombre' tiene OR = 1.35 ⇒ 35% más odds.")
          ),

          # --- Tema 6: Signo, magnitud y significancia
          shiny::h5("Signo, magnitud y significancia"),
          shiny::tags$ul(
            shiny::tags$li("Signo: positivo ⇒ aumenta odds; negativo ⇒ disminuye."),
            shiny::tags$li("Magnitud: OR cercano a 1 implica efecto pequeño."),
            shiny::tags$li("Evidencia estadística: revisar p < α y el intervalo de confianza (si incluye 1, el efecto puede no ser concluyente).")
          ),

          # --- Tema 7: Errores comunes
          shiny::h5("Errores comunes"),
          shiny::tags$ul(
            shiny::tags$li("No interpretar \\(\\beta\\) como cambio directo en la probabilidad."),
            shiny::tags$li("En variables numéricas, especificar siempre la unidad."),
            shiny::tags$li("En categóricas, mencionar la categoría de referencia."),
            shiny::tags$li("No concluir sin considerar p-valor e intervalo de confianza.")
          ),


            shiny::h4(shiny::strong("Variable Asignada:")),
            shiny::uiOutput(ns("interp_var_target")),
            shiny::textAreaInput(ns("interp_text"),
              "Escribe tu interpretación del coeficiente de la variable asignada:",
              placeholder = "Redacta tu interpretación...",
              width = "100%", height = "120px"
            ),
            shiny::actionButton(ns("interp_enviar"), "Enviar interpretación"),
            shiny::tags$hr(),
            shiny::uiOutput(ns("interp_feedback"))
          ),


          # ---- Tab 3: Selección
          shiny::tabPanel(
            title = "Selección",
            shiny::h4(shiny::strong("Selección de variables y reentrenamiento")),
            shiny::h5("Modelo de Probabilidad de Aceptación de Crédito"),
            shiny::fluidRow(
              shiny::column(6,
                DT::DTOutput(ns("tbl_coefs_accept_sel")),
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

            # 1) ¿Qué es el umbral?
            shiny::h4(shiny::strong("¿Qué es el umbral de clasificación?")),
            shiny::p("Cuando el modelo entrega probabilidades o un score, necesitamos una regla de decisión: 
                    si el valor supera un umbral \\(\\tau\\), clasificamos como ", shiny::strong("Aprobado"),
                    "; si no lo supera, como ", shiny::strong("Rechazado"), "."),

            # 2) Score integrado en el negocio
            shiny::h4(shiny::strong("Score integrado en el negocio")),
            shiny::p("Trabajamos con dos probabilidades: ",
                    "\\(p(\\text{aceptar})\\) y \\(p(\\text{mora})\\). Para combinarlas definimos el score integrado:"),
            shiny::helpText("$$\\text{score} = p(\\text{aceptar}) \\times \\big(1 - p(\\text{mora})\\big)$$"),
            shiny::tags$ul(
              shiny::tags$li("Score alto: alta probabilidad de aceptación y bajo riesgo de mora."),
              shiny::tags$li("Score bajo: baja aceptación y/o alto riesgo.")
            ),

            # 3) Regla de decisión
            shiny::h4("Regla de decisión con el umbral \\(\\tau\\)"),
            shiny::helpText("$$
              \\text{Si } \\text{score} \\ge \\tau \\Rightarrow \\; \\text{Aprobado}
              \\qquad\\qquad
              \\text{Si } \\text{score} < \\tau \\Rightarrow \\; \\text{Rechazado}
            $$"),

            # 4) Matriz de confusión
            shiny::h4("Cómo evaluamos la calidad de la decisión"),
            shiny::p("Comparamos las decisiones contra el resultado real con la ", shiny::strong("matriz de confusión"), ":"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("TP (verdadero positivo): "), "aprobado y realmente bueno."),
              shiny::tags$li(shiny::strong("FP (falso positivo): "), "aprobado pero era malo (riesgo que entró)."),
              shiny::tags$li(shiny::strong("TN (verdadero negativo): "), "rechazado y realmente malo (riesgo que evitamos)."),
              shiny::tags$li(shiny::strong("FN (falso negativo): "), "rechazado pero era bueno (venta perdida).")
            ),

            # 5) Métricas con significado de negocio
            shiny::h4("Métricas clave y su interpretación de negocio"),

            shiny::h5("Accuracy (Exactitud)"),
            shiny::p("Proporción total de aciertos. Útil como visión global, pero puede ser engañosa con clases desbalanceadas."),
            shiny::helpText("$$Accuracy = \\frac{TP + TN}{TP + TN + FP + FN}$$"),

            shiny::h5("Sensibilidad (Recall o TPR)"),
            shiny::p("De todos los clientes realmente buenos, ¿qué proporción aprobamos? Si es baja, perdemos ventas."),
            shiny::helpText("$$Sensibilidad = \\frac{TP}{TP + FN}$$"),

            shiny::h5("Especificidad (TNR)"),
            shiny::p("De todos los clientes realmente malos, ¿qué proporción rechazamos? Si es baja, se cuela más riesgo."),
            shiny::helpText("$$Especificidad = \\frac{TN}{TN + FP}$$"),

            shiny::h5("Precisión (Precision o VPP)"),
            shiny::p("De los clientes aprobados, ¿qué proporción resultó realmente buena? Mide la calidad de las aprobaciones."),
            shiny::helpText("$$Precisi\\acute{o}n = \\frac{TP}{TP + FP}$$"),

            shiny::h5("F1 Score"),
            shiny::p("Equilibrio entre Precisión y Sensibilidad. Útil para encontrar un punto medio entre colocar y controlar riesgo."),
            shiny::helpText("$$F1 = 2 \\cdot \\frac{Precisi\\acute{o}n \\cdot Sensibilidad}{Precisi\\acute{o}n + Sensibilidad}$$"),

            # 6) Efecto de mover el umbral
            shiny::h4("¿Qué pasa cuando muevo el umbral \\(\\tau\\)?"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Bajar \\(\\tau\\): "), "apruebas más clientes → sube la Sensibilidad; 
                              pero suelen bajar la Especificidad y la Precisión (entra más riesgo)."),
              shiny::tags$li(shiny::strong("Subir \\(\\tau\\): "), "apruebas menos → suben Especificidad y Precisión; 
                              pero baja la Sensibilidad (pierdes buenos clientes)."),
              shiny::tags$li(shiny::strong("Objetivo práctico: "), "elige \\(\\tau\\) según la estrategia (maximizar colocaciones, 
                              minimizar riesgo o un balance; por ejemplo, maximizando F1 o fijando una meta mínima de Precisión).")
            ),

            shiny::br(),

            shiny::sliderInput(ns("thr"), "Umbral de decisión", min = 0, max = 1, value = 0.5, step = 0.01),
            shiny::actionButton(ns("apply_thr"), "Evaluar y aplicar umbral"),

            shiny::br(), shiny::br(),
            shiny::plotOutput(ns("plot_metrics"), height = 260),
            DT::DTOutput(ns("tbl_thr_metrics"))
          ),

          # ---- Tab 5: Resultados (igual)
          shiny::tabPanel(
            title = "Resultados",
            shiny::br(),
            shiny::h4("Interpretación de los resultados"),
            shiny::p(
              "El histograma resume la distribución del score integrado y la línea marca el umbral actual. ",
              "El diagrama de torta a continuación muestra la proporción de clientes aprobados (score ≥ umbral) ",
              "y rechazados (score < umbral). Esto refleja un paso del \"funnel\" comercial del banco, ",
              "en el que cada clasificación ayuda a segmentar a los clientes para la oferta de créditos."
            ),
            shiny::plotOutput(ns("plot_scores"), height = 240),
            shiny::plotOutput(ns("plot_pie"), height = 240),
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
