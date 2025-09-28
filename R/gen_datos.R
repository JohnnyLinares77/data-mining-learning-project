# R/gen_datos.R
# ---- Genera dataset sintético respetando distribuciones
gen_datos <- function(n_clientes = 1000L, seed = 123L){
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

  # Simular Margen Esperado (ME) basado en score, ingreso y riesgo, con mayor variabilidad
  financieras$margen_esperado <- with(financieras, {
    # Unir con moras para riesgo
    moras <- comp_historico$n_moras_previas[match(id_cliente, comp_historico$id_cliente)]
    rfm <- comp_historico$rfm[match(id_cliente, comp_historico$id_cliente)]
    risk_factor <- 1 - (moras / 5) * 0.5  # Penalizar moras
    potential_factor <- rfm / 100  # Bonificar RFM alto
    base_me <- (score_buro / 100) * (ingreso_verificado / 100) * 100 * risk_factor * potential_factor
    pmax(0, base_me)  # asegurar positivo
  })

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
  # Generar múltiples ofertas por cliente para simular historial
  n_ofertas_total <- n_clientes * 3  # 3 ofertas promedio por cliente
  id_cliente_ofertas <- rep(1:n_clientes, each = 3)

  ofertas_historicas <- data.frame(
    id_cliente = id_cliente_ofertas,
    rate   = runif(n_ofertas_total, 0.03, 0.12),  # tasas históricas ofrecidas
    amount = round(runif(n_ofertas_total, 1000, 25000), 0),  # montos históricos
    term   = sample(c(3,6,9,12,18,24,36,48), n_ofertas_total, replace = TRUE),  # plazos históricos
    stringsAsFactors = FALSE
  )

  # Hacer que las ofertas dependan del perfil del cliente (más conservadoras para clientes de riesgo)
  for(i in 1:n_clientes) {
    cliente_rows <- ofertas_historicas$id_cliente == i
    score_buro_i <- financieras$score_buro[i]
    ingreso_i <- financieras$ingreso_verificado[i]
    moras_i <- comp_historico$n_moras_previas[i]

    # Clientes con mejor score/income: ofertas más agresivas (tasas más bajas, montos más altos)
    rate_adjust <- ifelse(score_buro_i > 700 & ingreso_i > 5000 & moras_i == 0, -0.02,
                         ifelse(score_buro_i < 500 | moras_i > 2, 0.03, 0))
    amount_adjust <- ifelse(score_buro_i > 700 & ingreso_i > 5000, 5000,
                           ifelse(score_buro_i < 500, -5000, 0))

    ofertas_historicas$rate[cliente_rows] <- pmax(0.03, pmin(0.15,
      ofertas_historicas$rate[cliente_rows] + rate_adjust))
    ofertas_historicas$amount[cliente_rows] <- pmax(500, pmin(30000,
      ofertas_historicas$amount[cliente_rows] + amount_adjust))
  }

  list(
    demograficas      = demograficas,
    comp_historico    = comp_historico,
    financieras       = financieras,
    post_desembolso   = post_desembolso,
    ofertas_historicas = ofertas_historicas,
    simulacion_meta   = data.frame(id_sim = NA, seed = seed, fecha = Sys.time())
  )
}
