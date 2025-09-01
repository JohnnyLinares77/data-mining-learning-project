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

  list(
    demograficas    = demograficas,
    comp_historico  = comp_historico,
    financieras     = financieras,
    post_desembolso = post_desembolso,
    simulacion_meta = data.frame(id_sim = NA, seed = seed, fecha = Sys.time())
  )
}
