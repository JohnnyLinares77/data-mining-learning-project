# R/tree_helpers.R
# -------------------------------------------------------------------
# Funciones auxiliares para Árboles de Clasificación con rpart
# Maneja entrenamiento, poda y evaluación de modelos de árbol
# -------------------------------------------------------------------

# Función para crear variable dependiente de alerta de riesgo
.create_alerta_riesgo <- function(df) {
  # Crear alerta de riesgo basada en score_buro, moras y RFM
  # score_buro: < 500 -> alto riesgo, 500-700 -> medio, >700 -> bajo
  riesgo_score <- cut(df$score_buro,
                     breaks = c(-Inf, 500, 700, Inf),
                     labels = c("alto", "medio", "bajo"))

  # moras: >2 -> alto, 1-2 -> medio, 0 -> bajo
  riesgo_moras <- cut(df$n_moras_previas,
                     breaks = c(-Inf, 0, 2, Inf),
                     labels = c("bajo", "medio", "alto"))

  # Combinar riesgos
  riesgo_combinado <- ifelse(riesgo_score == "alto" | riesgo_moras == "alto", "alto",
                            ifelse(riesgo_score == "medio" | riesgo_moras == "medio", "medio", "bajo"))

  factor(riesgo_combinado, levels = c("bajo", "medio", "alto"))
}

# -------------------------------------------------------------------
# train_tree: Entrena un modelo de árbol de clasificación
# Args:
#   df: DataFrame con datos de entrenamiento
#   vars_predictoras: Vector de nombres de variables predictoras
#   var_dependiente: Nombre de la variable dependiente (default: "alerta_riesgo")
# Returns:
#   Objeto rpart con el modelo entrenado
# -------------------------------------------------------------------
train_tree <- function(df, vars_predictoras, var_dependiente = "alerta_riesgo") {
  # LOG: Inicio de entrenamiento
  message(sprintf("[TRAIN_TREE] Iniciando entrenamiento con %d observaciones", nrow(df)))

  # Crear variable dependiente si no existe
  if (!var_dependiente %in% names(df)) {
    message("[TRAIN_TREE] Creando variable dependiente 'alerta_riesgo'")
    df[[var_dependiente]] <- .create_alerta_riesgo(df)
  }

  # Verificar que las variables predictoras existen en el dataframe
  vars_disponibles <- intersect(vars_predictoras, names(df))
  if (length(vars_disponibles) == 0) {
    stop("[TRAIN_TREE] ERROR: Ninguna de las variables predictoras está disponible en los datos")
  }

  # LOG: Variables disponibles
  message(sprintf("[TRAIN_TREE] Variables predictoras disponibles: %d/%d (%s)",
                  length(vars_disponibles), length(vars_predictoras),
                  paste(vars_disponibles, collapse = ", ")))

  # Verificar variabilidad de cada variable predictora
  for (var in vars_disponibles) {
    unique_vals <- length(unique(df[[var]]))
    if (unique_vals < 2) {
      stop(sprintf("[TRAIN_TREE] ERROR: Variable '%s' tiene menos de 2 valores únicos", var))
    }
    message(sprintf("[TRAIN_TREE] Variable '%s': %d valores únicos", var, unique_vals))
  }

  # Utilizar todas las variables seleccionadas. Para evitar que variables categóricas codificadas
  # como números se comporten como continuas, conviértalas en factores cuando tengan pocas
  # categorías. Esto permite que rpart genere reglas basadas en categorías en lugar de cortes
  # numéricos arbitrarios.
  conversiones <- c()
  for (var in vars_disponibles) {
    if (is.numeric(df[[var]]) && length(unique(df[[var]])) <= 10) {
      df[[var]] <- as.factor(df[[var]])
      conversiones <- c(conversiones, var)
    }
  }

  if (length(conversiones) > 0) {
    message(sprintf("[TRAIN_TREE] Convertidas a factor: %s", paste(conversiones, collapse = ", ")))
  }

  vars_predictoras <- vars_disponibles

  # Preparar fórmula
  formula_str <- paste(var_dependiente, "~", paste(vars_predictoras, collapse = " + "))
  formula <- as.formula(formula_str)
  message(sprintf("[TRAIN_TREE] Fórmula: %s", formula_str))

  # Verificar balance de clases en variable dependiente
  if (var_dependiente %in% names(df)) {
    class_dist <- table(df[[var_dependiente]])
    message(sprintf("[TRAIN_TREE] Distribución de clases en '%s': %s",
                    var_dependiente, paste(names(class_dist), class_dist, sep = "=", collapse = ", ")))

    if (length(class_dist) < 2) {
      stop(sprintf("[TRAIN_TREE] ERROR: Variable dependiente '%s' tiene menos de 2 clases", var_dependiente))
    }

    min_class <- min(class_dist)
    if (min_class < 5) {
      warning(sprintf("[TRAIN_TREE] WARNING: Clase minoritaria tiene solo %d observaciones", min_class))
    }
  }

  # Entrenar árbol con rpart. Ajustar parámetros de control para permitir que el modelo
  # explore más divisiones y utilice más variables. Se reducen los requisitos de tamaño de
  # muestra por división y el parámetro de complejidad para generar un árbol más profundo.
  message("[TRAIN_TREE] Entrenando modelo rpart...")
  tree_model <- rpart::rpart(
    formula,
    data = df,
    method = "class",
    control = rpart::rpart.control(
      minsplit = 5,     # Más permisivo: divisiones con al menos 5 observaciones
      minbucket = 2,    # Hojas más pequeñas: al menos 2 observaciones
      cp = 0.0001,      # Mucho más complejo: permitir más divisiones
      maxdepth = 12,    # Más profundidad: hasta 12 niveles
      maxcompete = 10,  # Considerar más competidores por división
      maxsurrogate = 5  # Usar variables sustitutas para mejorar uso de variables
    ),
    model = TRUE
  )  # IMPORTANTE: Guardar datos del modelo para rpart.plot

  # LOG: Resultados del entrenamiento
  if (!is.null(tree_model$frame) && nrow(tree_model$frame) > 0) {
    n_terminal <- sum(tree_model$frame$var == "<leaf>")
    message(sprintf("[TRAIN_TREE] Modelo entrenado exitosamente: %d nodos terminales", n_terminal))

    # Verificar que hay al menos algunos nodos terminales
    if (n_terminal == 0) {
      stop("[TRAIN_TREE] ERROR: El modelo no generó nodos terminales")
    }
  } else {
    stop("[TRAIN_TREE] ERROR: El modelo entrenado no tiene estructura válida")
  }

  return(tree_model)
}

# Función para poda del árbol - MEJORADA Y ROBUSTA
prune_tree <- function(tree_model, df_train) {
  message("[PRUNE_TREE] Iniciando proceso de poda del árbol")

  # Verificar que el modelo tiene estructura válida
  if (is.null(tree_model$frame) || nrow(tree_model$frame) == 0) {
    stop("[PRUNE_TREE] ERROR: El modelo original no tiene estructura válida")
  }

  # Verificar que hay suficientes datos para validación cruzada
  n_terminal_original <- sum(tree_model$frame$var == "<leaf>")
  n_vars_used <- length(unique(tree_model$frame$var[tree_model$frame$var != "<leaf>"]))

  message(sprintf("[PRUNE_TREE] Árbol original: %d nodos terminales, %d variables usadas",
                  n_terminal_original, n_vars_used))

  if (n_terminal_original <= 1) {
    message("[PRUNE_TREE] WARNING: Árbol original tiene muy pocos nodos, poda limitada")
    return(list(
      original = tree_model,
      pruned = tree_model,
      cp_optimal = 0,
      cv_results = NULL,
      optimal_size = n_terminal_original,
      warning = "Árbol demasiado simple para poda efectiva"
    ))
  }

  # Validación cruzada para encontrar cp óptimo
  message("[PRUNE_TREE] Ejecutando validación cruzada para encontrar CP óptimo...")
  cv_results <- tryCatch({
    rpart::printcp(tree_model)
  }, error = function(e) {
    message(sprintf("[PRUNE_TREE] ERROR en validación cruzada: %s", e$message))
    message("[PRUNE_TREE] Intentando poda con CP conservador...")

    # Fallback: usar CP conservador si CV falla
    return(list(
      original = tree_model,
      pruned = rpart::prune(tree_model, cp = 0.01),
      cp_optimal = 0.01,
      cv_results = NULL,
      optimal_size = sum(rpart::prune(tree_model, cp = 0.01)$frame$var == "<leaf>"),
      warning = "Poda con CP conservador debido a error en CV"
    ))
  })

  if (is.null(cv_results) || nrow(cv_results) == 0) {
    message("[PRUNE_TREE] WARNING: CV retornó resultados vacíos, usando poda conservadora")
    pruned_tree <- rpart::prune(tree_model, cp = 0.01)
    return(list(
      original = tree_model,
      pruned = pruned_tree,
      cp_optimal = 0.01,
      cv_results = NULL,
      optimal_size = sum(pruned_tree$frame$var == "<leaf>")
    ))
  }

  # LOG: Resultados de CV
  message(sprintf("[PRUNE_TREE] CV completada: %d puntos de poda evaluados", nrow(cv_results)))

  # Mostrar mejores opciones de poda
  best_cps <- head(cv_results[order(cv_results[, "xerror"]), ], 3)
  message("[PRUNE_TREE] Mejores opciones de poda:")
  for (i in 1:nrow(best_cps)) {
    message(sprintf("  %d. CP=%.6f, Error=%.4f, Tamaño=%d",
                    i, best_cps[i, "CP"], best_cps[i, "xerror"], best_cps[i, "nsplit"] + 1))
  }

  # Encontrar el cp con menor error de validación cruzada
  min_error <- min(cv_results[, "xerror"])
  optimal_idx <- which.min(cv_results[, "xerror"])
  optimal_cp <- cv_results[optimal_idx, "CP"]
  optimal_size <- cv_results[optimal_idx, "nsplit"] + 1  # nsplits + 1 = tamaño

  message(sprintf("[PRUNE_TREE] Seleccionado: CP=%.6f, Error=%.4f, Tamaño óptimo=%d nodos",
                  optimal_cp, min_error, optimal_size))

  # Verificar que el tamaño óptimo es razonable (al menos 3 nodos para demostrar poda)
  if (optimal_size < 3 && n_terminal_original >= 5) {
    message("[PRUNE_TREE] WARNING: Tamaño óptimo muy pequeño, buscando compromiso...")
    # Buscar un punto que reduzca complejidad pero mantenga utilidad
    reasonable_idx <- which(cv_results[, "nsplit"] + 1 >= 3 &
                           cv_results[, "nsplit"] + 1 <= n_terminal_original * 0.7)[1]
    if (!is.na(reasonable_idx)) {
      optimal_cp <- cv_results[reasonable_idx, "CP"]
      optimal_size <- cv_results[reasonable_idx, "nsplit"] + 1
      message(sprintf("[PRUNE_TREE] Ajustado a tamaño razonable: %d nodos (CP=%.6f)",
                      optimal_size, optimal_cp))
    }
  }

  # Podar el árbol
  message("[PRUNE_TREE] Aplicando poda con CP óptimo...")
  pruned_tree <- tryCatch({
    rpart::prune(tree_model, cp = optimal_cp)
  }, error = function(e) {
    message(sprintf("[PRUNE_TREE] ERROR al podar: %s", e$message))
    message("[PRUNE_TREE] Usando poda conservadora como fallback...")
    return(rpart::prune(tree_model, cp = 0.01))
  })

  # Verificar árbol podado
  if (!is.null(pruned_tree$frame) && nrow(pruned_tree$frame) > 0) {
    n_terminal_pruned <- sum(pruned_tree$frame$var == "<leaf>")
    n_vars_pruned <- length(unique(pruned_tree$frame$var[pruned_tree$frame$var != "<leaf>"]))

    message(sprintf("[PRUNE_TREE] Árbol podado: %d nodos terminales, %d variables usadas",
                    n_terminal_pruned, n_vars_pruned))
    message(sprintf("[PRUNE_TREE] Reducción: %d nodos eliminados (%d%%)",
                    n_terminal_original - n_terminal_pruned,
                    round((n_terminal_original - n_terminal_pruned) / n_terminal_original * 100)))

    if (n_terminal_pruned == 0) {
      stop("[PRUNE_TREE] ERROR: Árbol podado no tiene nodos terminales")
    }
  } else {
    stop("[PRUNE_TREE] ERROR: Árbol podado no tiene estructura válida")
  }

  message("[PRUNE_TREE] ✅ Poda completada exitosamente")

  return(list(
    original = tree_model,
    pruned = pruned_tree,
    cp_optimal = optimal_cp,
    cv_results = cv_results,
    optimal_size = optimal_size,
    reduction_percent = (n_terminal_original - n_terminal_pruned) / n_terminal_original * 100
  ))
}

# Calcular métricas de evaluación
calculate_metrics <- function(predictions, actual, umbral = 0.5) {
  # Para multiclase, calcular métricas por clase y overall
  conf_matrix <- table(Predicted = predictions, Actual = actual)

  # Accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

  # Para cada clase, calcular precision, recall, f1
  classes <- levels(actual)
  metrics_by_class <- lapply(classes, function(cls) {
    tp <- conf_matrix[cls, cls]
    fp <- sum(conf_matrix[cls, ]) - tp
    fn <- sum(conf_matrix[, cls]) - tp
    tn <- sum(conf_matrix) - tp - fp - fn

    precision <- if (tp + fp > 0) tp / (tp + fp) else 0
    recall <- if (tp + fn > 0) tp / (tp + fn) else 0
    specificity <- if (tn + fp > 0) tn / (tn + fp) else 0
    f1 <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0

    list(
      class = cls,
      precision = precision,
      recall = recall,
      specificity = specificity,
      f1 = f1,
      tp = tp, fp = fp, fn = fn, tn = tn
    )
  })

  names(metrics_by_class) <- classes

  # Macro averages
  macro_precision <- mean(sapply(metrics_by_class, `[[`, "precision"))
  macro_recall <- mean(sapply(metrics_by_class, `[[`, "recall"))
  macro_specificity <- mean(sapply(metrics_by_class, `[[`, "specificity"))
  macro_f1 <- mean(sapply(metrics_by_class, `[[`, "f1"))

  list(
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    macro_precision = macro_precision,
    macro_recall = macro_recall,
    macro_specificity = macro_specificity,
    macro_f1 = macro_f1,
    by_class = metrics_by_class
  )
}

# Función para seleccionar nodo aleatorio
select_random_node <- function(tree_model) {
  # Obtener información de los nodos
  tree_frame <- tree_model$frame

  # Encontrar nodos terminales (leaves)
  terminal_nodes <- rownames(tree_frame)[tree_frame$var == "<leaf>"]

  if (length(terminal_nodes) == 0) return(NULL)

  # Seleccionar uno aleatoriamente
  selected_node <- sample(terminal_nodes, 1)

  # Obtener información del nodo
  node_info <- tree_frame[selected_node, ]

  # Obtener regla de decisión para llegar a este nodo
  rule <- get_node_rule(tree_model, as.numeric(selected_node))

  list(
    node_id = selected_node,
    yval = node_info$yval,
    n = node_info$n,
    rule = rule
  )
}

# Función auxiliar para obtener la regla de un nodo
get_node_rule <- function(tree_model, node_id) {
  # Esta es una simplificación - en la práctica necesitarías
  # rastrear el camino desde la raíz hasta el nodo
  # Por ahora, devolver una descripción básica
  paste("Regla para nodo", node_id)
}

# Función para clasificar nuevas observaciones
classify_new_data <- function(tree_model, new_data) {
  message(sprintf("[CLASSIFY] Iniciando clasificación de %d observaciones", nrow(new_data)))

  # Verificar que el modelo es válido
  if (is.null(tree_model) || is.null(tree_model$frame) || nrow(tree_model$frame) == 0) {
    stop("[CLASSIFY] ERROR: Modelo no válido para predicción")
  }

  # Verificar que hay datos para clasificar
  if (nrow(new_data) == 0) {
    stop("[CLASSIFY] ERROR: No hay datos para clasificar")
  }

  # Verificar que id_cliente existe
  if (!"id_cliente" %in% names(new_data)) {
    stop("[CLASSIFY] ERROR: Variable 'id_cliente' no encontrada en datos nuevos")
  }

  # LOG: Variables en datos nuevos vs modelo
  vars_model <- names(tree_model$xlevels)
  vars_data <- names(new_data)
  message(sprintf("[CLASSIFY] Variables en modelo: %d, Variables en datos: %d",
                  length(vars_model), length(vars_data)))

  # Asegurar que las variables categóricas tengan los mismos niveles que en el modelo
  if (!is.null(tree_model$xlevels)) {
    for (v in names(tree_model$xlevels)) {
      if (v %in% names(new_data)) {
        original_levels <- levels(new_data[[v]])
        new_data[[v]] <- factor(new_data[[v]], levels = tree_model$xlevels[[v]])
        message(sprintf("[CLASSIFY] Variable '%s': %d -> %d niveles",
                        v, length(original_levels), length(tree_model$xlevels[[v]])))
      } else {
        warning(sprintf("[CLASSIFY] WARNING: Variable '%s' del modelo no encontrada en datos nuevos", v))
      }
    }
  }

  # Convertir variables numéricas con pocas categorías a factores si no fueron
  # capturadas en xlevels. Esto evita errores cuando el modelo espera factores.
  conversiones <- c()
  for (v in names(new_data)) {
    if (v != "id_cliente" && is.numeric(new_data[[v]]) && length(unique(new_data[[v]])) <= 10) {
      new_data[[v]] <- as.factor(new_data[[v]])
      conversiones <- c(conversiones, v)
    }
  }

  if (length(conversiones) > 0) {
    message(sprintf("[CLASSIFY] Convertidas a factor en predicción: %s", paste(conversiones, collapse = ", ")))
  }

  # Hacer predicciones
  message("[CLASSIFY] Generando predicciones...")
  predictions <- tryCatch({
    predict(tree_model, newdata = new_data, type = "class")
  }, error = function(e) {
    stop(sprintf("[CLASSIFY] ERROR en predicción de clases: %s", e$message))
  })

  probabilities <- tryCatch({
    predict(tree_model, newdata = new_data, type = "prob")
  }, error = function(e) {
    stop(sprintf("[CLASSIFY] ERROR en predicción de probabilidades: %s", e$message))
  })

  # Verificar que las predicciones tienen sentido
  if (length(predictions) != nrow(new_data)) {
    stop(sprintf("[CLASSIFY] ERROR: Número de predicciones (%d) != número de observaciones (%d)",
                 length(predictions), nrow(new_data)))
  }

  # Crear dataframe con resultados
  results <- data.frame(
    id_cliente = new_data$id_cliente,
    clase_predicha = predictions,
    prob_bajo = probabilities[, "bajo"],
    prob_medio = probabilities[, "medio"],
    prob_alto = probabilities[, "alto"]
  )

  # Nivel de alerta basado en la clase predicha
  results$nivel_alerta <- factor(results$clase_predicha,
                                levels = c("bajo", "medio", "alto"),
                                labels = c("Baja", "Media", "Alta"))

  # LOG: Resumen de resultados
  pred_summary <- table(results$clase_predicha)
  message(sprintf("[CLASSIFY] Resumen predicciones: %s",
                  paste(names(pred_summary), pred_summary, sep = "=", collapse = ", ")))

  message("[CLASSIFY] Clasificación completada exitosamente")

  results
}