# R/independent_mode_helpers.R
# -------------------------------------------------------------------
# Funciones para generar datos simulados en modo independiente
# Permite que cada módulo funcione sin depender de módulos anteriores
# -------------------------------------------------------------------

# ---- Función para generar datos base para M1 (Perfilamiento)
generate_independent_data_m1 <- function(n_clientes = 1000, seed = 123) {
  set.seed(seed)

  # Usar la misma lógica que gen_datos.R pero simplificada
  demograficas <- data.frame(
    id_cliente       = 1:n_clientes,
    edad             = rnorm(n_clientes, mean = 38.4, sd = 9.57),
    estado_civil     = sample(1:4, n_clientes, replace = TRUE),
    ubicacion        = sample(1:5, n_clientes, replace = TRUE),
    n_dependientes   = rpois(n_clientes, lambda = 1.48),
    nivel_educativo  = sample(1:4, n_clientes, replace = TRUE),
    tipo_ocupacion   = sample(1:4, n_clientes, replace = TRUE),
    rubro_laboral    = sample(1:5, n_clientes, replace = TRUE),
    stringsAsFactors = FALSE
  )

  comp_historico <- data.frame(
    id_cliente              = 1:n_clientes,
    antiguedad_cliente      = 10 + rgamma(n_clientes, shape = 2, scale = 24),
    n_moras_previas         = rpois(n_clientes, 0.248),
    dias_atraso_max         = rpois(n_clientes, 3.29),
    n_moras_leves           = rpois(n_clientes, 0.502),
    rfm                     = 19.5 + 81*rbeta(n_clientes, 2.55, 2.62),
    stringsAsFactors        = FALSE
  )

  financieras <- data.frame(
    id_cliente           = 1:n_clientes,
    ingreso_declarado    = 1200 + rexp(n_clientes, rate = 1/1680),
    ingreso_verificado   = 904 + 9820*rbeta(n_clientes, 0.906, 3.94),
    capacidad_endeud     = rexp(n_clientes, rate = 1/460),
    endeudamiento_total  = 58 + rweibull(n_clientes, shape = 1.17, scale = 602),
    score_buro           = 300 + 650*rbeta(n_clientes, 0.649, 0.56),
    stringsAsFactors     = FALSE
  )

  # Combinar en lista como gen_datos
  list(
    demograficas = demograficas,
    comp_historico = comp_historico,
    financieras = financieras
  )
}

# ---- Función para generar clusters simulados para M2
generate_simulated_clusters <- function(n_clientes = 1000, seed = 456) {
  set.seed(seed)

  # Simular clusters basados en características típicas
  # Cluster 1: Jóvenes con ingresos bajos (riesgo alto)
  # Cluster 2: Adultos con ingresos medios (riesgo medio)
  # Cluster 3: Adultos mayores con ingresos altos (riesgo bajo)

  clusters <- sample(1:3, n_clientes, replace = TRUE, prob = c(0.3, 0.4, 0.3))

  data.frame(
    id_cliente = 1:n_clientes,
    cluster_id = clusters,
    stringsAsFactors = FALSE
  )
}

# ---- Función para generar scores simulados para M3
generate_simulated_scores <- function(n_clientes = 1000, seed = 789) {
  set.seed(seed)

  # Generar probabilidades simuladas
  p_accept <- runif(n_clientes, 0.1, 0.9)
  p_mora <- runif(n_clientes, 0.01, 0.3)

  # Score integrado: p_accept * (1 - p_mora)
  score <- p_accept * (1 - p_mora)

  # Decisión basada en umbral 0.5
  decision <- as.integer(score >= 0.5)

  data.frame(
    id_cliente = 1:n_clientes,
    p_accept = round(p_accept, 4),
    p_mora = round(p_mora, 4),
    score = round(score, 4),
    decision = decision,
    stringsAsFactors = FALSE
  )
}

# ---- Función para generar datos históricos completos para M4
generate_historic_data_m4 <- function(n_clientes = 2000, seed = 101112) {
  set.seed(seed)

  # Generar datos con variabilidad garantizada para todas las variables
  df <- data.frame(
    id_cliente = 1:n_clientes,
    edad = round(rnorm(n_clientes, 40, 10), 1),
    estado_civil = sample(1:4, n_clientes, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
    ubicacion = sample(1:5, n_clientes, replace = TRUE, prob = c(0.2, 0.25, 0.2, 0.2, 0.15)),
    n_dependientes = sample(0:5, n_clientes, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.07, 0.03)),
    nivel_educativo = sample(1:4, n_clientes, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15)),
    tipo_ocupacion = sample(1:4, n_clientes, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
    rubro_laboral = sample(1:5, n_clientes, replace = TRUE, prob = c(0.25, 0.2, 0.2, 0.2, 0.15)),
    antiguedad_cliente = round(10 + rgamma(n_clientes, shape = 2, scale = 20), 1),
    n_moras_previas = sample(0:5, n_clientes, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.05, 0.03, 0.02)),
    dias_atraso_max = sample(0:30, n_clientes, replace = TRUE, prob = c(0.7, rep(0.03, 30))),
    productos_activos = sample(0:6, n_clientes, replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.15, 0.05)),
    frecuencia_uso = sample(1:20, n_clientes, replace = TRUE),
    cancelaciones_anticip = sample(0:3, n_clientes, replace = TRUE, prob = c(0.7, 0.2, 0.08, 0.02)),
    rfm = round(20 + 80 * rbeta(n_clientes, 2, 2), 1),
    ingreso_declarado = round(1000 + rexp(n_clientes, rate = 1/2000), 0),
    ingreso_verificado = round(800 + 10000 * rbeta(n_clientes, 1, 3), 0),
    capacidad_endeud = round(rexp(n_clientes, rate = 1/500), 0),
    endeudamiento_total = round(50 + rweibull(n_clientes, shape = 1.2, scale = 600), 0),
    score_buro = round(300 + 650 * rbeta(n_clientes, 0.6, 0.5), 0),
    stringsAsFactors = FALSE
  )

  # Crear variable dependiente de alerta de riesgo
  # Usar tryCatch para evitar errores si .create_alerta_riesgo no está disponible
  tryCatch({
    df$alerta_riesgo <- .create_alerta_riesgo(df)
  }, error = function(e) {
    # Fallback: crear alerta basada en score_buro y moras
    df$alerta_riesgo <- factor(ifelse(df$score_buro < 500 | df$n_moras_previas > 2, "alto",
                                     ifelse(df$score_buro < 700 | df$n_moras_previas > 0, "medio", "bajo")),
                              levels = c("bajo", "medio", "alto"))
  })

  # Verificar que todas las variables tienen variabilidad
  vars_numeric <- sapply(df, is.numeric)
  vars_to_check <- names(df)[vars_numeric & !names(df) %in% c("id_cliente")]

  for (var in vars_to_check) {
    if (length(unique(df[[var]])) < 2) {
      # Si no hay variabilidad, agregar ruido pequeño
      df[[var]] <- df[[var]] + rnorm(nrow(df), 0, sd(df[[var]], na.rm = TRUE) * 0.01)
    }
  }

  df
}

# ---- Función para generar datos completos para M3 (con márgenes simulados)
generate_complete_data_m3 <- function(n_clientes = 1000, seed = 131415) {
  set.seed(seed)

  # Base de datos similar a M1
  df <- data.frame(
    id_cliente = 1:n_clientes,
    edad = rnorm(n_clientes, 40, 10),
    ingreso_verificado = 1000 + 9000 * rbeta(n_clientes, 1, 2),
    score_buro = 300 + 650 * rbeta(n_clientes, 0.7, 0.6),
    n_moras_previas = rpois(n_clientes, 0.2),
    rfm = 20 + 80 * rbeta(n_clientes, 2.5, 2.5),
    stringsAsFactors = FALSE
  )

  # Agregar variables de pricing
  df$rate <- runif(n_clientes, 0.03, 0.12)
  df$amount <- round(runif(n_clientes, 1000, 25000), 0)
  df$term <- sample(c(6,12,18,24,36), n_clientes, replace = TRUE)

  # Simular scores si no existen
  if (!"score" %in% names(df)) {
    df$score <- runif(n_clientes, 0, 1)
  }

  # Calcular margen simulado basado en las variables
  df$margen <- with(df, {
    base_margin <- 3000 +
      1000 * (score_buro - 500)/500 +  # Score normalizado
      500 * (ingreso_verificado - 5000)/5000 +  # Ingreso normalizado
      2000 * score +  # Score de aceptación
      500 * (rfm - 50)/50 -  # RFM normalizado
      1000 * n_moras_previas  # Penalización por moras

    # Añadir ruido
    base_margin + rnorm(n_clientes, 0, 500)
  })

  # Asegurar márgenes positivos
  df$margen <- pmax(100, df$margen)

  df
}