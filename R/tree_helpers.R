# R/tree_helpers.R
# ---- Árboles de Clasificación con rpart

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

# Entrenar árbol de clasificación
train_tree <- function(df, vars_predictoras, var_dependiente = "alerta_riesgo") {
  # Crear variable dependiente si no existe
  if (!var_dependiente %in% names(df)) {
    df[[var_dependiente]] <- .create_alerta_riesgo(df)
  }

  # Verificar que las variables predictoras existen en el dataframe
  vars_disponibles <- intersect(vars_predictoras, names(df))
  if (length(vars_disponibles) == 0) {
    stop("Ninguna de las variables predictoras está disponible en los datos")
  }

  # MOSTRAR TODAS LAS VARIABLES SELECCIONADAS PERO LIMITAR PARA EVITAR OVERFITTING
  # Si hay más de 15 variables, seleccionar las más importantes, pero mostrar todas en el mensaje
  if (length(vars_disponibles) > 15) {
    # Calcular importancia usando correlación con la variable dependiente
    if (is.factor(df[[var_dependiente]])) {
      # Para categórica, convertir a numérica para correlación
      y_numeric <- as.numeric(df[[var_dependiente]])
      cor_values <- sapply(vars_disponibles, function(var) {
        tryCatch({
          if (is.numeric(df[[var]])) {
            abs(cor(y_numeric, df[[var]], use = "complete.obs"))
          } else {
            0.1  # Valor bajo para variables no numéricas
          }
        }, error = function(e) 0.1)
      })
    } else {
      cor_values <- sapply(vars_disponibles, function(var) {
        tryCatch({
          abs(cor(df[[var_dependiente]], df[[var]], use = "complete.obs"))
        }, error = function(e) 0.1)
      })
    }

    # Seleccionar top 15 variables más importantes
    top_vars <- names(sort(cor_values, decreasing = TRUE))[1:min(15, length(vars_disponibles))]
    vars_predictoras <- top_vars
    message(sprintf("Seleccionadas top 15 variables de %d disponibles para evitar overfitting: %s",
                   length(vars_disponibles), paste(top_vars, collapse = ", ")))
  } else {
    vars_predictoras <- vars_disponibles
    message(sprintf("Usando todas las %d variables seleccionadas: %s",
                   length(vars_disponibles), paste(vars_disponibles, collapse = ", ")))
  }

  # Preparar fórmula
  formula_str <- paste(var_dependiente, "~", paste(vars_predictoras, collapse = " + "))
  formula <- as.formula(formula_str)

  # Entrenar árbol con rpart - parámetros más conservadores para estabilidad
  tree_model <- rpart::rpart(formula, data = df, method = "class",
                            control = rpart::rpart.control(
                              minsplit = 20,      # Mínimo 20 observaciones para dividir
                              minbucket = 10,     # Mínimo 10 observaciones por hoja
                              cp = 0.01,          # Parámetro de complejidad más restrictivo
                              maxdepth = 5,       # Máximo 5 niveles de profundidad
                              maxcompete = 3,     # Máximo 3 competidores por división
                              maxsurrogate = 2   # Máximo 2 surrogates
                            ),
                            model = TRUE)  # IMPORTANTE: Guardar datos del modelo para rpart.plot

  return(tree_model)
}

# Función para poda del árbol
prune_tree <- function(tree_model, df_train) {
  # Validación cruzada para encontrar cp óptimo
  cv_results <- rpart::printcp(tree_model)

  # Encontrar el cp con menor error de validación cruzada
  min_error <- min(cv_results[, "xerror"])
  optimal_cp <- cv_results[which.min(cv_results[, "xerror"]), "CP"]

  # Podar el árbol
  pruned_tree <- rpart::prune(tree_model, cp = optimal_cp)

  return(list(
    original = tree_model,
    pruned = pruned_tree,
    cp_optimal = optimal_cp,
    cv_results = cv_results
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
  predictions <- predict(tree_model, newdata = new_data, type = "class")
  probabilities <- predict(tree_model, newdata = new_data, type = "prob")

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

  results
}