# R/gen_datos.R
# ---- Genera dataset sintético respetando distribuciones
# ---- Función principal unificada para todos los modos y módulos

gen_datos <- function(n_clientes = 1000L, seed = 123L, mode = "sequential", module = NULL){
  message(sprintf("[DATA_GEN] Generando datos (n=%d, seed=%d, mode=%s, module=%s)",
                  n_clientes, seed, mode, ifelse(is.null(module), "NULL", module)))

  # ---- MODO INDEPENDIENTE: Cada módulo genera sus propios datos
  if (mode == "independent") {
    if (module == "m1") {
      message("[DATA_GEN] Modo independiente: Generando datos para M1 (Perfilamiento)")
      return(generate_independent_data_m1(n_clientes, seed))
    }

    if (module == "m2") {
      message("[DATA_GEN] Modo independiente: Generando clusters simulados para M2")
      return(generate_simulated_clusters(n_clientes, seed))
    }

    if (module == "m3") {
      message("[DATA_GEN] Modo independiente: Generando scores simulados para M3")
      return(generate_simulated_scores(n_clientes, seed))
    }

    if (module == "m4") {
      message("[DATA_GEN] Modo independiente: Generando datos históricos para M4 (Árboles)")
      return(generate_historic_data_m4(n_clientes, seed))
    }

    stop(sprintf("[DATA_GEN] ERROR: Módulo '%s' no reconocido en modo independiente", module))
  }

  # ---- MODO SECUENCIAL: Generación tradicional para M1-M3
  message("[DATA_GEN] Modo secuencial: Generando datos completos para M1-M3")
  set.seed(seed)

  # Helpers de distribución
  rDISC <- function(n, breaks, values){
    # breaks: vector de probabilidades acumuladas (0-1)
    # values: categorías
    u <- runif(n)
    cut_idx <- findInterval(u, breaks)
    values[pmin(pmax(cut_idx, 1), length(values))]
  }
  rERLA <- function(n, k, scale){ # Erlang = Gamma(k, scale)
    rgamma(n, shape = k, scale = scale)
  }

  # --- DEMOGRÁFICAS
  demograficas <- data.frame(
    id_cliente       = 1:n_clientes,
    edad             = rnorm(n_clientes, mean = 38.4, sd = 9.57),
    sexo             = rDISC(n_clientes, c(0.50,1.00), c(1,0)),            # definir diccionario luego
    estado_civil     = rDISC(n_clientes, c(0.45,0.85,0.95,1.00), 1:4),
    ubicacion        = rDISC(n_clientes, c(0.50,0.65,0.75,0.90,1.00), 1:5),
    n_dependientes   = rpois(n_clientes, lambda = 1.48),
    nivel_educativo  = rDISC(n_clientes, c(0.25,0.55,0.90,1.00), 1:4),
    tipo_ocupacion   = rDISC(n_clientes, c(0.55,0.80,0.95,1.00), 1:4),
    rubro_laboral    = rDISC(n_clientes, c(0.30,0.65,0.80,0.90,1.00), 1:5),
    stringsAsFactors = FALSE
  )

  # --- COMPORTAMIENTO HISTÓRICO
  comp_historico <- data.frame(
    id_cliente              = 1:n_clientes,
    antiguedad_cliente      = 10 + rERLA(n_clientes, k = 2, scale = 24),  # 10 + ERLA(24,2)
    n_moras_previas         = rpois(n_clientes, 0.248),
    dias_atraso_max         = rpois(n_clientes, 3.29),
    n_moras_leves           = rpois(n_clientes, 0.502),
    productos_activos       = rDISC(n_clientes, c(0.10,0.35,0.65,0.85,0.95,0.99,1.00), 0:6),
    frecuencia_uso          = rpois(n_clientes, 5.13),
    cancelaciones_anticip   = rpois(n_clientes, 0.282),
    rfm                      = 19.5 + 81*rbeta(n_clientes, 2.55, 2.62),
    stringsAsFactors         = FALSE
  )

  # --- FINANCIERAS
  financieras <- data.frame(
    id_cliente           = 1:n_clientes,
    ingreso_declarado    = 1200 + rexp(n_clientes, rate = 1/1680),
    ingreso_verificado   = 904 + 9820*rbeta(n_clientes, 0.906, 3.94),
    cuota_ingreso        = rgamma(n_clientes, shape = 0.0658, rate = 3.52), # usar rate según convención
    capacidad_endeud     = rexp(n_clientes, rate = 1/460),
    endeudamiento_total  = 58 + rweibull(n_clientes, shape = 1.17, scale = 602),
    score_buro           = 300 + 650*rbeta(n_clientes, 0.649, 0.56),
    tendencia_ingresos   = rDISC(n_clientes, c(0.33,0.67,1.00), 1:3),
    stringsAsFactors     = FALSE
  )


  # --- POST DESEMBOLSO (para consistencia; no se usa en M1)
  post_desembolso <- data.frame(
    id_cliente          = 1:n_clientes,
    pago_primera_cuota  = rDISC(n_clientes, c(0.25,1.00), c(0,1)),
    saldo_promedio      = rexp(n_clientes, rate = 1/1150),
    n_rebotes_debito    = rpois(n_clientes, 0.746),
    accesos_digitales   = rpois(n_clientes, 5.93),
    consultas_credito   = rpois(n_clientes, 2.02),
    tiempo_respuesta    = 4 + rgamma(n_clientes, shape = 13, rate = 1/1.99),
    stringsAsFactors    = FALSE
  )

  # --- OFERTAS HISTÓRICAS (para M3 - simular datos históricos de ofertas)
  # Una oferta por cliente para evitar duplicación en merge
  n_ofertas_total <- n_clientes
  id_cliente_ofertas <- 1:n_clientes

  ofertas_historicas <- data.frame(
    id_cliente = id_cliente_ofertas,
    tasa   = runif(n_ofertas_total, 0.03, 0.12),  # tasas históricas ofrecidas
    monto_oferta = round(runif(n_ofertas_total, 1000, 25000), 0),  # montos históricos
    plazo   = sample(c(3,6,9,12,18,24,36,48), n_ofertas_total, replace = TRUE),  # plazos históricos
    stringsAsFactors = FALSE
  )

  # Hacer que las ofertas dependan del perfil del cliente (más conservadoras para clientes de riesgo)
  for(i in 1:n_clientes) {
    score_buro_i <- financieras$score_buro[i]
    ingreso_i <- financieras$ingreso_verificado[i]
    moras_i <- comp_historico$n_moras_previas[i]

    # Clientes con mejor score/income: ofertas más agresivas (tasas más bajas, montos más altos)
    tasa_adjust <- ifelse(score_buro_i > 700 & ingreso_i > 5000 & moras_i == 0, -0.02,
                          ifelse(score_buro_i < 500 | moras_i > 2, 0.03, 0))
    monto_adjust <- ifelse(score_buro_i > 700 & ingreso_i > 5000, 5000,
                           ifelse(score_buro_i < 500, -5000, 0))

    ofertas_historicas$tasa[i] <- pmax(0.03, pmin(0.15,
      ofertas_historicas$tasa[i] + tasa_adjust))
    ofertas_historicas$monto_oferta[i] <- pmax(500, pmin(30000,
      ofertas_historicas$monto_oferta[i] + monto_adjust))
  }

  # Simular Margen Histórico (margen) basado en score, ingreso, moras, RFM, monto_oferta y plazo
  financieras$margen <- with(financieras, {
    # Unir con moras, RFM, monto_oferta y plazo
    moras <- comp_historico$n_moras_previas[match(id_cliente, comp_historico$id_cliente)]
    rfm_val <- comp_historico$rfm[match(id_cliente, comp_historico$id_cliente)]
    monto_val <- ofertas_historicas$monto_oferta[match(id_cliente, ofertas_historicas$id_cliente)]
    plazo_val <- ofertas_historicas$plazo[match(id_cliente, ofertas_historicas$id_cliente)]

    # Factores de riesgo y potencial
    s_buro <- scale(score_buro)
    ingreso <- scale(ingreso_verificado)
    moras_scaled <- scale(moras)
    rfm_scaled <- scale(rfm_val)
    monto_scaled <- scale(monto_val)
    plazo_scaled <- scale(plazo_val)

    # Margen base positivo, con mayor influencia de monto y plazo
    base_margin <- 5000 + 2000 * (0.5 * s_buro + 0.6 * ingreso - 0.4 * moras_scaled + 0.3 * rfm_scaled) +
                   1000 * (0.4 * monto_scaled + 0.3 * plazo_scaled)  # Mayor correlación

    # Añadir ruido aleatorio para variabilidad histórica
    margin_with_noise <- base_margin + rnorm(n_clientes, mean = 0, sd = 500)

    pmax(100, margin_with_noise)  # asegurar positivo mínimo
  })

  result <- list(
    demograficas      = demograficas,
    comp_historico    = comp_historico,
    financieras       = financieras,
    post_desembolso   = post_desembolso,
    ofertas_historicas = ofertas_historicas,
    simulacion_meta   = data.frame(id_sim = NA, seed = seed, fecha = Sys.time())
  )

  message(sprintf("[DATA_GEN] Generación completada: %d tablas, %d registros totales",
                  length(result), n_clientes))
  result
}

# ---- FUNCIONES DE GENERACIÓN INDEPENDIENTE (movidas aquí para centralización)

# Función para generar datos base para M1 (Perfilamiento) - Modo Independiente
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

# Función para generar clusters simulados para M2
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

# Función para generar scores simulados para M3
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

# Función para generar datos históricos completos para M4
generate_historic_data_m4 <- function(n_clientes = 2000, seed = 101112) {
  message(sprintf("[M4_DATA_GEN] Generando datos históricos INDEPENDIENTES para M4 usando gen_datos() (n=%d, seed=%d)", n_clientes, seed))

  # Usar gen_datos() como base pero crear dataset independiente para M4
  datos_base <- gen_datos(n_clientes = n_clientes, seed = seed)

  message("[M4_DATA_GEN] Procesando datos de gen_datos() para M4...")

  # Combinar todas las tablas relevantes de gen_datos() en un solo dataframe
  df <- Reduce(function(x, y) merge(x, y, by = "id_cliente", all = TRUE),
               list(datos_base$demograficas,
                    datos_base$comp_historico,
                    datos_base$financieras))

  message(sprintf("[M4_DATA_GEN] Dataset combinado: %d filas, %d columnas", nrow(df), ncol(df)))

  # Asegurar que id_cliente sea character (consistencia)
  df$id_cliente <- as.character(df$id_cliente)

  # Crear variable dependiente específica para M4: alerta_riesgo
  # Sistema mejorado para distribución balanceada de clases
  message("[M4_DATA_GEN] Creando variable dependiente 'alerta_riesgo' con distribución balanceada...")

  # Calcular puntaje de riesgo para cada observación
  df$puntaje_riesgo <- with(df, {
    # Sistema de puntuación mejorado para balance de clases

    # 1. Score Bureau (peso alto: 35%)
    riesgo_score <- ifelse(score_buro < 450, 4,   # Muy alto riesgo
                          ifelse(score_buro < 550, 3,   # Alto riesgo
                                ifelse(score_buro < 650, 2,   # Medio riesgo
                                      ifelse(score_buro < 750, 1,   # Bajo riesgo
                                            0))))                # Muy bajo riesgo

    # 2. Historial de moras (peso alto: 30%)
    riesgo_moras <- ifelse(n_moras_previas >= 5, 4,   # Muy alto riesgo
                          ifelse(n_moras_previas >= 3, 3,   # Alto riesgo
                                ifelse(n_moras_previas >= 1, 2,   # Medio riesgo
                                      0)))                    # Sin moras

    # 3. Días de atraso máximo (peso medio: 20%)
    riesgo_dias <- ifelse(dias_atraso_max >= 60, 3,   # Alto riesgo
                         ifelse(dias_atraso_max >= 30, 2,   # Medio riesgo
                               ifelse(dias_atraso_max >= 10, 1,   # Bajo riesgo
                                     0)))                     # Sin atrasos

    # 4. RFM Score (peso medio: 15%)
    riesgo_rfm <- ifelse(rfm < 30, 3,   # Alto riesgo (muy bajo engagement)
                        ifelse(rfm < 50, 2,   # Medio riesgo
                              ifelse(rfm < 70, 1,   # Bajo riesgo
                                    0)))        # Bueno

    # Puntaje total ponderado (máximo: 4*4 + 4*3 + 3*2 + 3*1 = 35)
    (riesgo_score * 0.35) + (riesgo_moras * 0.30) +
    (riesgo_dias * 0.20) + (riesgo_rfm * 0.15)
  })

  # Crear variable dependiente basada en el puntaje
  df$alerta_riesgo <- with(df, {
    # Clasificación final con distribución objetivo: 45% bajo, 35% medio, 20% alto
    factor(ifelse(puntaje_riesgo >= 2.5, "alto",   # Alto riesgo: puntaje >= 2.5 (~20%)
                 ifelse(puntaje_riesgo >= 1.2, "medio",  # Medio riesgo: 1.2-2.5 (~35%)
                       "bajo")),                    # Bajo riesgo: < 1.2 (~45%)
           levels = c("bajo", "medio", "alto"))
  })

  # Verificar y ajustar distribución si es necesario
  class_dist <- table(df$alerta_riesgo)
  total_obs <- nrow(df)

  target_bajo <- round(total_obs * 0.45)   # 45%
  target_medio <- round(total_obs * 0.35)  # 35%
  target_alto <- total_obs - target_bajo - target_medio  # 20%

  message(sprintf("[M4_DATA_GEN] Distribución objetivo: bajo=%d, medio=%d, alto=%d",
                  target_bajo, target_medio, target_alto))
  message(sprintf("[M4_DATA_GEN] Distribución actual: %s",
                  paste(names(class_dist), class_dist, sep = "=", collapse = ", ")))

  # Si la distribución está muy desbalanceada, hacer ajuste fino
  if (length(class_dist) < 3 || (is.na(class_dist["alto"]) || class_dist["alto"] < target_alto * 0.5)) {  # Menos del 10% en alto riesgo
    message("[M4_DATA_GEN] WARNING: Muy pocos casos de alto riesgo, ajustando umbrales...")

    # Ajustar umbrales para aumentar casos de alto riesgo
    df$alerta_riesgo <- with(df, {
      # Recalcular con umbrales más permisivos para alto riesgo
      factor(ifelse(puntaje_riesgo >= 2.0, "alto",   # Bajar umbral de 2.5 a 2.0
                   ifelse(puntaje_riesgo >= 0.8, "medio",  # Ajustar medio
                         "bajo")),
             levels = c("bajo", "medio", "alto"))
    })

    class_dist <- table(df$alerta_riesgo)
    message(sprintf("[M4_DATA_GEN] Distribución ajustada: %s",
                    paste(names(class_dist), class_dist, sep = "=", collapse = ", ")))
  }

  # Verificación final: asegurar que tenemos las 3 clases
  if (length(class_dist) < 3) {
    message("[M4_DATA_GEN] ERROR: No se pudieron generar las 3 clases de riesgo")
    # Forzar distribución mínima
    n_per_class <- floor(total_obs / 3)
    indices <- sample(1:total_obs, total_obs)

    df$alerta_riesgo[indices[1:n_per_class]] <- "bajo"
    df$alerta_riesgo[indices[(n_per_class+1):(2*n_per_class)]] <- "medio"
    df$alerta_riesgo[indices[(2*n_per_class+1):total_obs]] <- "alto"

    df$alerta_riesgo <- factor(df$alerta_riesgo, levels = c("bajo", "medio", "alto"))

    class_dist <- table(df$alerta_riesgo)
    message(sprintf("[M4_DATA_GEN] Distribución forzada: %s",
                    paste(names(class_dist), class_dist, sep = "=", collapse = ", ")))
  }

  message("[M4_DATA_GEN] Datos base generados exitosamente")

  # Variables críticas para M4 - asegurar alta variabilidad y correlación con riesgo
  vars_criticas_m4 <- c("score_buro", "n_moras_previas", "dias_atraso_max", "endeudamiento_total",
                        "rfm", "antiguedad_cliente", "productos_activos", "ingreso_verificado")

  message("[M4_DATA_GEN] Asegurando variabilidad en variables críticas para M4...")

  for (var in vars_criticas_m4) {
    if (var %in% names(df)) {
      unique_vals <- length(unique(df[[var]]))
      message(sprintf("[M4_DATA_GEN] Variable crítica '%s': %d valores únicos", var, unique_vals))

      if (unique_vals < 5) {
        # Agregar más variabilidad a variables críticas
        if (is.numeric(df[[var]])) {
          sd_var <- sd(df[[var]], na.rm = TRUE)
          if (sd_var == 0 || is.na(sd_var)) sd_var <- abs(mean(df[[var]], na.rm = TRUE)) * 0.2
          # Agregar ruido más significativo para variables críticas
          df[[var]] <- df[[var]] + rnorm(nrow(df), 0, sd_var * 0.15)
          message(sprintf("[M4_DATA_GEN] Agregado ruido a '%s', nuevos únicos: %d",
                         var, length(unique(df[[var]]))))
        }
      }
    } else {
      message(sprintf("[M4_DATA_GEN] WARNING: Variable crítica '%s' no encontrada", var))
    }
  }

  # Verificar otras variables predictoras disponibles
  otras_vars <- setdiff(names(df), c(vars_criticas_m4, "id_cliente", "alerta_riesgo", "puntaje_riesgo"))
  for (var in otras_vars) {
    if (is.numeric(df[[var]])) {
      unique_vals <- length(unique(df[[var]]))
      if (unique_vals < 3) {
        sd_var <- sd(df[[var]], na.rm = TRUE)
        if (sd_var == 0 || is.na(sd_var)) sd_var <- abs(mean(df[[var]], na.rm = TRUE)) * 0.1
        df[[var]] <- df[[var]] + rnorm(nrow(df), 0, sd_var * 0.1)
      }
    }
  }

  # Variables críticas para M4 que estarán disponibles para selección
  vars_criticas_m4 <- c("score_buro", "n_moras_previas", "dias_atraso_max", "endeudamiento_total",
                        "rfm", "antiguedad_cliente", "productos_activos", "ingreso_verificado")

  # Log detallado de variables generadas
  message("[M4_DATA_GEN] Resumen de variables generadas:")
  for (var in vars_criticas_m4) {
    if (var %in% names(df)) {
      unique_vals <- length(unique(df[[var]]))
      tipo <- ifelse(is.numeric(df[[var]]), "numérico", "categórico")
      rango <- if (is.numeric(df[[var]])) {
        sprintf("[%.1f, %.1f]", min(df[[var]], na.rm = TRUE), max(df[[var]], na.rm = TRUE))
      } else {
        paste(unique(df[[var]]), collapse = ",")
      }
      message(sprintf("  - %s (%s): %d únicos, rango: %s", var, tipo, unique_vals, rango))
    }
  }

  # Log final
  message(sprintf("[M4_DATA_GEN] Generación completada: %d observaciones, %d variables predictoras con variabilidad >= 3 niveles",
                  nrow(df), sum(sapply(df[vars_criticas_m4], function(x) length(unique(x)) >= 3))))

  df

  df
}

# Función para generar datos completos para M3 (con márgenes simulados)
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
