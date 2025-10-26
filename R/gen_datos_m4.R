# R/gen_datos_m4.R
# Generador específico para Módulo 4 - Árboles de Clasificación
# Datos independientes con distribuciones realistas y etiquetas multiclase

library(MASS)  # Para mvrnorm

# Función principal para generar datos M4
gen_datos_m4 <- function(n = 2000, seed = 101112) {
  set.seed(seed)
  message(sprintf("[M4_GEN] Generando %d observaciones para M4 (seed=%d)", n, seed))

  # 1. Variables latentes correlacionadas (base para continuas)
  # Matriz de correlación realista entre variables financieras
  Sigma <- matrix(c(
    1.0, 0.6, 0.4, 0.3, 0.2,  # score_buro, ingreso_verificado, endeudamiento_total, cuota_ingreso, antiguedad_cliente
    0.6, 1.0, 0.5, 0.4, 0.3,
    0.4, 0.5, 1.0, 0.6, 0.2,
    0.3, 0.4, 0.6, 1.0, 0.1,
    0.2, 0.3, 0.2, 0.1, 1.0
  ), 5, 5)

  # Generar variables latentes normales correlacionadas
  latentes <- mvrnorm(n, mu = rep(0, 5), Sigma = Sigma)

  # 2. Variables continuas (transformadas de latentes)
  df <- data.frame(
    id_cliente = 1:n,

    # Score Bureau: mixtura logit-normal remapeada [300, 950]
    score_buro = {
      raw <- 0.8 * latentes[,1] + 0.2 * rnorm(n, 0, 0.5)
      prob <- 1 / (1 + exp(-raw))  # logit a prob
      300 + 650 * prob  # remapear a rango realista
    },

    # Ingreso verificado: lognormal
    ingreso_verificado = {
      raw <- 0.7 * latentes[,2] + 0.3 * rnorm(n, 0, 0.4)
      exp(7.5 + 0.8 * raw)  # media ~$2000, sd razonable
    },

    # Endeudamiento total: lognormal dependiente
    endeudamiento_total = {
      raw <- 0.6 * latentes[,3] + 0.4 * latentes[,1] + 0.2 * rnorm(n, 0, 0.3)
      exp(8.0 + 0.7 * raw)  # media ~$3000
    },

    # Cuota/ingreso: beta transformada [0,1] -> [0,0.8]
    cuota_ingreso = {
      raw <- 0.5 * latentes[,4] + 0.3 * latentes[,3] + 0.2 * rnorm(n, 0, 0.4)
      prob <- 1 / (1 + exp(-raw))  # logit
      0.8 * rbeta(n, 2 + 3*prob, 5 - 3*prob)  # beta sesgada
    },

    # Antiguedad cliente: gamma
    antiguedad_cliente = {
      raw <- 0.4 * latentes[,5] + 0.6 * rnorm(n, 0, 0.5)
      10 + rgamma(n, shape = 2, scale = 12) * (1 + 0.3 * raw)  # media ~34 meses
    }
  )

  # 3. Variables discretas/categóricas (con niveles fijos)
  df <- df |> dplyr::mutate(

    # Estado civil (4 niveles fijos)
    estado_civil = factor(sample(c("soltero", "casado", "divorciado", "viudo"),
                                n, replace = TRUE, prob = c(0.4, 0.4, 0.15, 0.05)),
                         levels = c("soltero", "casado", "divorciado", "viudo")),

    # Nivel educativo (4 niveles fijos)
    nivel_educativo = factor(sample(c("primaria", "secundaria", "tecnico", "universitario"),
                                   n, replace = TRUE, prob = c(0.2, 0.4, 0.25, 0.15)),
                            levels = c("primaria", "secundaria", "tecnico", "universitario")),

    # Número de dependientes (convertir a factor con niveles fijos)
    n_dependientes = factor(pmin(pmax(round(rpois(n, 1.2)), 0), 5),
                           levels = as.character(0:5)),

    # Cancelaciones anticipadas (convertir a factor)
    cancelaciones_anticip = factor(pmin(pmax(rpois(n, 0.3), 0), 3),
                                  levels = as.character(0:3)),

    # Número de moras previas (Poisson)
    n_moras_previas = pmin(pmax(rpois(n, 0.4), 0), 10),

    # Días de atraso máximo (Poisson con dependencia de moras)
    dias_atraso_max = {
      base <- rpois(n, 2)
      moras_factor <- ifelse(n_moras_previas > 0, 3, 1)
      pmin(base * moras_factor + rpois(n, 1), 180)  # máximo 180 días
    },

    # Frecuencia de uso (Poisson)
    frecuencia_uso = pmin(pmax(rpois(n, 4), 0), 20),

    # RFM Score (beta transformada)
    rfm = 20 + 80 * rbeta(n, 2.5, 2.5),

    # Ubicación (5 niveles fijos)
    ubicacion = factor(sample(1:5, n, replace = TRUE),
                      levels = 1:5),

    # Tipo ocupación (4 niveles fijos)
    tipo_ocupacion = factor(sample(1:4, n, replace = TRUE),
                           levels = 1:4),

    # Rubro laboral (5 niveles fijos)
    rubro_laboral = factor(sample(1:5, n, replace = TRUE),
                          levels = 1:5),

    # Productos activos (ordinal)
    productos_activos = factor(sample(0:6, n, replace = TRUE, prob = c(0.05, 0.15, 0.25, 0.25, 0.15, 0.1, 0.05)),
                              levels = 0:6),

    # Tendencia ingresos (3 niveles fijos)
    tendencia_ingresos = factor(sample(c("baja", "estable", "creciente"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
                               levels = c("baja", "estable", "creciente"))
  )

  # 4. Etiqueta multiclase con softmax (no cortes duros)
  df$alerta_riesgo <- .create_alerta_riesgo_m4(df)

  # 5. Limpiar y validar
  df <- na.omit(df)  # eliminar cualquier NA generado
  message(sprintf("[M4_GEN] Generación completada: %d filas, %d columnas", nrow(df), ncol(df)))

  # Verificar distribución de clases
  class_dist <- table(df$alerta_riesgo)
  message(sprintf("[M4_GEN] Distribución de clases: %s",
                  paste(names(class_dist), class_dist, sep = "=", collapse = ", ")))

  df
}

# Función para crear etiquetas multiclase con softmax
.create_alerta_riesgo_m4 <- function(df) {
  # Calcular logits para alto y medio riesgo
  z_alto <- with(df, {
    -1.1 * scale(score_buro) +
    0.8 * scale(cuota_ingreso) +
    0.6 * (n_moras_previas > 1) +
    0.5 * scale(log1p(endeudamiento_total)) +
    0.3 * scale(dias_atraso_max) +
    0.2 * (estado_civil == "divorciado") +
    0.15 * (frecuencia_uso < 3) +
    0.1 * scale(antiguedad_cliente) +
    rnorm(nrow(df), 0, 0.3)  # ruido
  })

  z_medio <- with(df, {
    -0.4 * scale(score_buro) +
    0.3 * scale(cuota_ingreso) +
    0.2 * (n_moras_previas > 0) +
    0.2 * scale(log1p(endeudamiento_total)) +
    -0.1 * scale(antiguedad_cliente) +
    0.1 * scale(rfm) +
    rnorm(nrow(df), 0, 0.25)  # ruido
  })

  # Softmax para probabilidades
  exp_alto <- exp(z_alto)
  exp_medio <- exp(z_medio)
  exp_bajo <- rep(1, nrow(df))  # baseline

  p_alto <- exp_alto / (exp_alto + exp_medio + exp_bajo)
  p_medio <- exp_medio / (exp_alto + exp_medio + exp_bajo)
  p_bajo <- exp_bajo / (exp_alto + exp_medio + exp_bajo)

  # Asignar clase por muestreo multinomial
  probs <- cbind(p_bajo, p_medio, p_alto)
  clases <- apply(probs, 1, function(p) sample(c("bajo", "medio", "alto"), 1, prob = p))

  factor(clases, levels = c("bajo", "medio", "alto"))
}

# Función auxiliar para alinear tipos en predicción (si no está en tree_helpers.R)
.align_types_for_rpart <- function(model, new_data) {
  # DataClasses del modelo
  data_classes <- attr(model$terms, "dataClasses")
  # xlevels para variables factor
  xlevels <- model$xlevels

  # Recorre predictores
  for (nm in setdiff(names(data_classes), names(model$ylevels))) {
    if (!nm %in% names(new_data)) next
    cls <- data_classes[[nm]]
    if (cls == "factor") {
      levs <- xlevels[[nm]]
      new_data[[nm]] <- factor(as.character(new_data[[nm]]), levels = levs)
    } else {
      new_data[[nm]] <- suppressWarnings(as.numeric(new_data[[nm]]))
    }
  }
  new_data
}