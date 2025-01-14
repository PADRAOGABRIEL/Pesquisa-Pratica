# Carregar pacotes necessários
library(daltoolbox)
library(harbinger)
library(dplyr)
library(ggplot2)

# Carregar o dataset de exemplo
data("examples_anomalies")

# Usar a série temporal 'sequence' do exemplo
dataset <- examples_anomalies$sequence
head(dataset)

# Preparar o dataset, removendo valores ausentes e garantindo que a série seja numérica
dataset <- dataset[!is.na(dataset$serie), ]
dataset$serie <- as.numeric(dataset$serie)

# Estabelecer o modelo han_autoencoder
model_fn <- function() {
  # Inicializa o modelo com os parâmetros desejados
  han_autoencoder(3, 2, cae_encode_decode, num_epochs = 1500)
}

# Variáveis para armazenar as métricas médias
precisao_media <- 0
recall_media <- 0
acuracia_media <- 0
f1_score_media <- 0
especificidade_media <- 0

# Repetir o processo 5 vezes
for (i in 1:5) {
  
  # Treinar o modelo com a série temporal
  model <- model_fn()
  model <- fit(model, dataset$serie)
  
  # Fazer a detecção de anomalias
  detection <- detect(model, dataset$serie)
  
  # Filtrar eventos detectados
  detected_events <- detection |> filter(event == TRUE)
  print(paste("Eventos detectados na iteração", i, ":"))
  print(detected_events)
  
  # Avaliar o desempenho do modelo com base nos eventos reais
  if (!is.null(dataset$event)) {
    # Gerar matriz de confusão
    TP <- sum(detection$event == TRUE & dataset$event == TRUE)
    TN <- sum(detection$event == FALSE & dataset$event == FALSE)
    FP <- sum(detection$event == TRUE & dataset$event == FALSE)
    FN <- sum(detection$event == FALSE & dataset$event == TRUE)
    
    # Exibir matriz de confusão
    confMatrix <- matrix(c(TP, FN, FP, TN), nrow = 2, byrow = TRUE)
    colnames(confMatrix) <- c("Pred. Positivo", "Pred. Negativo")
    rownames(confMatrix) <- c("Verdadeiro Pos.", "Verdadeiro Neg.")
    print("Matriz de Confusão:")
    print(confMatrix)
    
    # Calcular as métricas de desempenho
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    accuracy <- (TP + TN) / (TP + FN + FP + TN)
    f1_score <- 2 * (precision * recall) / (precision + recall)
    specificity <- TN / (TN + FP)
    
    # Atualizar as métricas médias
    precisao_media <- precisao_media + precision
    recall_media <- recall_media + recall
    acuracia_media <- acuracia_media + accuracy
    f1_score_media <- f1_score_media + f1_score
    especificidade_media <- especificidade_media + specificity
  } else {
    print("Eventos reais ausentes. Avaliação não realizada.")
  }
}

# Calcular as médias das métricas após 5 iterações
precisao_media <- precisao_media / 5
recall_media <- recall_media / 5
acuracia_media <- acuracia_media / 5
f1_score_media <- f1_score_media / 5
especificidade_media <- especificidade_media / 5

# Exibir as métricas médias
cat("\nMétricas médias após 5 iterações:\n")
cat("Precisão:", precisao_media, "\n")
cat("Revocação:", recall_media, "\n")
cat("Acurácia:", acuracia_media, "\n")
cat("F1-Score:", f1_score_media, "\n")
cat("Especificidade:", especificidade_media, "\n")

# Plotar os resultados da detecção (opcional)
grf <- har_plot(model, dataset$serie, detection, dataset$event)
plot(grf)

# Plotar resíduos
res <- attr(detection, "res")
plot(res)

# Salvar o modelo treinado
saveRDS(model, "hanct_dtw_model.rds")
