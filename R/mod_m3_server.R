# R/mod_m3_server.R
# Server del Módulo 3 – Pricing y Elasticidad
#
# Compatible con callModule():
#   callModule(mod_m3_server, "m3", datos_reactivos = ..., id_sim = ...)
#
# Requisitos de datos en datos_reactivos():
#   - Columnas: p_accept, p_mora, rate, amount, term
#   - (Opcional) score, cluster y demás features numéricas/categóricas
#
mod_m3_server <- function(input, output, session, datos_reactivos, id_sim, cluster_levels = NULL){
  ns <- session$ns
  
  # ---------- Helpers ----------
  .calc_me <- function(df){
    # ME = (rate * amount * (term/12)) * p_accept * (1 - p_mora)
    if (!all(c("p_accept","p_mora","rate","amount","term") %in% names(df))) {
      return(rep(NA_real_, nrow(df)))
    }
    (df$rate * df$amount * (df$term/12)) * df$p_accept * (1 - df$p_mora)
  }
  
  # ---------- Base con ME ----------
  datos_con_margen <- shiny::reactive({
    shiny::req(datos_reactivos())
    df <- datos_reactivos()
    req_cols <- c("p_accept","p_mora","rate","amount","term")
    miss <- setdiff(req_cols, names(df))
    if (length(miss) > 0) {
      stop(sprintf("Faltan columnas en datos_reactivos(): %s", paste(miss, collapse=", ")))
    }
    df$ME <- .calc_me(df)
    df
  })
  
  # ---------- UI dinámica: selección de variables ----------
  shiny::observeEvent(datos_con_margen(), {
    df <- datos_con_margen()
    num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
    num_vars <- setdiff(num_vars, "ME")
    
    output$var_select_ui <- shiny::renderUI({
      shiny::selectizeInput(ns("vars_corr"), "Variables numéricas:",
                            choices = num_vars, selected = num_vars, multiple = TRUE,
                            options = list(plugins = list("remove_button")))
    })
    output$model_var_select <- shiny::renderUI({
      shiny::checkboxGroupInput(ns("vars_model"), "Variables explicativas:",
                                choices = num_vars, selected = num_vars)
    })
    
    # Poblado de clusters para simulación
    clusters <- cluster_levels
    if (is.null(clusters)) {
      if ("cluster" %in% names(df)) clusters <- sort(unique(as.character(df$cluster)))
    }
    shiny::updateSelectInput(session, "cluster_sim", choices = clusters)
  }, ignoreInit = TRUE)
  
  # ---------- Correlaciones ----------
  shiny::observeEvent(input$calcular_cor, {
    shiny::req(input$vars_corr)
    df <- datos_con_margen()
    cols <- unique(c(input$vars_corr, "ME"))
    dfc  <- stats::na.omit(df[, cols, drop = FALSE])
    if (nrow(dfc) < 3) {
      output$cor_plot  <- shiny::renderPlot({ plot.new(); title("Datos insuficientes para correlación") })
      output$cor_table <- DT::renderDT({
        data.frame(mensaje = "Datos insuficientes para correlación")
      }, options = list(dom = "t", paging = FALSE))
      return(invisible())
    }
    cm <- stats::cor(dfc, use = "pairwise.complete.obs")
    
    output$cor_plot <- shiny::renderPlot({
      if (requireNamespace("corrplot", quietly = TRUE)) {
        corrplot::corrplot(cm, method = "color", tl.cex = 0.8)
      } else {
        graphics::image(1:ncol(cm), 1:nrow(cm), t(cm[nrow(cm):1,]), axes = FALSE,
                        main = "Mapa de correlaciones", xlab = "", ylab = "")
        axis(3, at = 1:ncol(cm), labels = colnames(cm), las = 2, cex.axis = .8)
        axis(2, at = nrow(cm):1, labels = rownames(cm), las = 2, cex.axis = .8)
      }
    })
    output$cor_table <- DT::renderDT({
      round(cm, 3)
    }, options = list(pageLength = 6))
  })
  
  # ---------- Modelo manual ----------
  modelo_manual <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$ajustar_modelo, {
    shiny::req(input$vars_model)
    df <- datos_con_margen()
    vars <- input$vars_model
    if (!length(vars)) return(NULL)
    form <- stats::as.formula(paste("ME ~", paste(vars, collapse = " + ")))
    fit  <- stats::lm(form, data = df)
    modelo_manual(fit)
    
    output$model_summary <- shiny::renderPrint({ summary(fit) })
    output$resid_plot <- shiny::renderPlot({
      op <- par(mfrow = c(1, 2)); on.exit(par(op), add = TRUE)
      plot(fit, which = 1)  # residuales vs ajustados
      plot(fit, which = 2)  # QQ-plot
    })
  })
  
  # ---------- Modelo automático (Stepwise AIC) ----------
  modelo_auto <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$ajustar_auto, {
    df <- datos_con_margen()
    num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
    num_vars <- setdiff(num_vars, "ME")
    if (!length(num_vars)) return(NULL)
    
    full_form <- stats::as.formula(paste("ME ~", paste(num_vars, collapse = " + ")))
    full_fit  <- stats::lm(full_form, data = df)
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("El paquete MASS es requerido para stepAIC(). Instálalo o cárgalo en la app.")
    }
    auto_fit <- MASS::stepAIC(full_fit, direction = "both", trace = FALSE)
    modelo_auto(auto_fit)
    
    output$auto_summary <- shiny::renderPrint({ summary(auto_fit) })
    
    # Comparación en hold-out 30%
    set.seed(123)
    n   <- nrow(df)
    idx <- if (n > 10) sample(seq_len(n), size = floor(0.3*n)) else seq_len(n)
    df_tr <- df[-idx, , drop = FALSE]
    df_te <- df[ idx, , drop = FALSE]
    
    fit_man <- NULL
    if (!is.null(modelo_manual())) {
      fit_man <- stats::lm(formula(modelo_manual()), data = df_tr)
    }
    fit_aut <- stats::lm(formula(auto_fit), data = df_tr)
    
    rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
    res <- data.frame(Modelo = character(), RMSE = numeric(), R2_adj = numeric())
    if (!is.null(fit_man)) {
      prA <- predict(fit_man, newdata = df_te)
      res <- rbind(res, data.frame(Modelo = "Manual", RMSE = rmse(df_te$ME, prA),
                                   R2_adj = summary(fit_man)$adj.r.squared))
    }
    prB <- predict(fit_aut, newdata = df_te)
    res <- rbind(res, data.frame(Modelo = "Automático", RMSE = rmse(df_te$ME, prB),
                                 R2_adj = summary(fit_aut)$adj.r.squared))
    output$comp_table <- DT::renderDT({
      res
    }, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  # ---------- Simulación y Elasticidad ----------
  shiny::observeEvent(input$simular, {
    # Elegir el modelo seleccionado
    mod <- switch(input$modelo_final,
                  manual = modelo_manual(),
                  automatico = modelo_auto())
    shiny::req(!is.null(mod))
    
    # Grid de tasa
    rr    <- seq(input$rango_tasa[1], input$rango_tasa[2], length.out = 50)
    monto <- input$monto_sim
    plazo <- input$plazo_sim
    score <- input$score_sim
    clus  <- input$cluster_sim
    
    df0 <- datos_con_margen()
    shiny::req(nrow(df0) >= 1)
    base_row <- df0[1, , drop = FALSE]
    
    sims <- lapply(rr, function(r){
      tmp <- base_row
      tmp$rate   <- r
      tmp$amount <- monto
      tmp$term   <- plazo
      if ("score" %in% names(tmp))   tmp$score   <- score
      if ("cluster" %in% names(tmp)) tmp$cluster <- clus
      tmp$ME <- .calc_me(tmp)
      tmp
    })
    df_sim <- do.call(rbind, sims)
    
    # Predicción del ME con el modelo seleccionado
    me_hat <- as.numeric(predict(mod, newdata = df_sim))
    
    # Derivada parcial dME/dr según términos presentes
    cf <- stats::coef(mod)
    deriv_dr <- function(r){
      d <- 0
      if ("rate" %in% names(cf))         d <- d + cf["rate"]
      if ("I(rate^2)" %in% names(cf))    d <- d + 2*cf["I(rate^2)"]*r
      if ("rate:score" %in% names(cf) && "score" %in% names(df_sim))
        d <- d + cf["rate:score"]*score
      d
    }
    dME_dr <- vapply(rr, deriv_dr, numeric(1))
    
    # Elasticidad puntual: E = (dME/dr) * (rate / ME_hat)
    E_r <- dME_dr * (rr / me_hat)
    
    # Graficar
    output$margen_plot <- shiny::renderPlot({
      plot(rr, me_hat, type = "l", lwd = 2,
           xlab = "Tasa (rate)", ylab = "ME predicho",
           main = "Curva de ME predicho vs tasa")
      idx <- which.max(me_hat)
      points(rr[idx], me_hat[idx], pch = 19, col = "red")
      legend("topleft", legend = sprintf("Máximo en r=%.3f", rr[idx]), bty = "n")
    })
    output$elasticidad_plot <- shiny::renderPlot({
      plot(rr, E_r, type = "l", lwd = 2,
           xlab = "Tasa (rate)", ylab = "Elasticidad de ME",
           main = "Elasticidad de ME respecto a tasa")
      abline(h = 0, lty = 2, col = "gray50")
      abline(h = -1, lty = 3, col = "steelblue")
      legend("topright", legend = c("E=0","E=-1"), lty = c(2,3),
             col = c("gray50","steelblue"), bty = "n", cex = 0.9)
    })
  })
}
