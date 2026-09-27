# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# MODELOS SUAVIZAÇÃO   
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(forecast)
library(ggplot2)

# Carregando os dados
serie_inadp <- read.csv(
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  sep = ",",
  header = TRUE
)

# Criando a série temporal mensal
ts_serie <- ts(
  serie_inadp$y,
  start = c(2011, 3),
  frequency = 12
)

# ============================================================
# 2. MODELAGEM: SUAVIZAÇÃO
# ============================================================

# ------------------------------------------------------------
# 2.1 Separação em treino e teste
# ------------------------------------------------------------

# Últimas 12 observações serão utilizadas para a fase de testes
# n(s) de interesse: 32; 24; 16; 12.

n_teste <- 12

n <- length(ts_serie)

serie_treino <- window(
  ts_serie,
  end = time(ts_serie)[n - n_teste]
)

serie_teste <- window(
  ts_serie,
  start = time(ts_serie)[n - n_teste + 1]
)

length(serie_treino)
length(serie_teste)


# Visualizando a separação

autoplot(ts_serie, series = "Série") +
  autolayer(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  labs(
    title = "2.1 - Separação entre treino e teste",
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 2.2 Construção dos modelos
# ------------------------------------------------------------

# Suavização exponencial simples
modelo_ANN <- ets(
  serie_treino,
  model = "ANN",
  damped = FALSE
)

# Método de Holt
modelo_AAN <- ets(
  serie_treino,
  model = "AAN",
  damped = FALSE
)

# Holt com tendência amortecida
modelo_AAN_damped <- ets(
  serie_treino,
  model = "AAN",
  damped = TRUE
)

# Holt-Winters com sazonalidade aditiva
modelo_AAA <- ets(
  serie_treino,
  model = "AAA",
  damped = FALSE
)

# Holt-Winters com tendência amortecida
modelo_AAA_damped <- ets(
  serie_treino,
  model = "AAA",
  damped = TRUE
)

# Modelo ETS escolhido automaticamente pelo forecast
modelo_auto <- ets(serie_treino)


# ------------------------------------------------------------
# 2.3 Resumo dos modelos
# ------------------------------------------------------------

summary(modelo_ANN)
summary(modelo_AAN)
summary(modelo_AAN_damped)
summary(modelo_AAA)
summary(modelo_AAA_damped)
summary(modelo_auto)


# ------------------------------------------------------------
# 2.4 Previsões para a amostra de teste
# ------------------------------------------------------------

prev_ANN <- forecast(
  modelo_ANN,
  h = n_teste
)

prev_AAN <- forecast(
  modelo_AAN,
  h = n_teste
)

prev_AAN_damped <- forecast(
  modelo_AAN_damped,
  h = n_teste
)

prev_AAA <- forecast(
  modelo_AAA,
  h = n_teste
)

prev_AAA_damped <- forecast(
  modelo_AAA_damped,
  h = n_teste
)

prev_auto <- forecast(
  modelo_auto,
  h = n_teste
)


# ------------------------------------------------------------
# 2.5 Comparação visual das previsões
# ------------------------------------------------------------

grafico <- autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  autolayer(prev_ANN$mean, series = "ETS(ANN)") +
  autolayer(prev_AAN$mean, series = "Holt") +
  autolayer(
    prev_AAN_damped$mean,
    series = "Holt amortizado"
  ) +
  autolayer(
    prev_AAA$mean,
    series = "Holt-Winters"
  ) +
  autolayer(
    prev_AAA_damped$mean,
    series = "Holt-Winters amortizado"
  ) +
  autolayer(
    prev_auto$mean,
    series = "ETS automático"
  ) +
  labs(
    title = "2.5 - Previsões dos modelos na amostra de teste",
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL
  ) +
  scale_colour_discrete(
    limits = c(
      "Treino",
      "Teste",
      "ETS(ANN)",
      "Holt",
      "Holt amortizado",
      "Holt-Winters",
      "Holt-Winters amortizado",
      "ETS automático"
    )
  ) +
  theme_minimal()

grafico


# ------------------------------------------------------------
# 2.6 Avaliação das previsões
# ------------------------------------------------------------

accuracy(prev_ANN, serie_teste)
accuracy(prev_AAN, serie_teste)
accuracy(prev_AAN_damped, serie_teste)
accuracy(prev_AAA, serie_teste)
accuracy(prev_AAA_damped, serie_teste)
accuracy(prev_auto, serie_teste)


# ------------------------------------------------------------
# 2.7 Tabela comparativa dos modelos
# ------------------------------------------------------------

modelos <- list(
  "ETS(ANN)" = modelo_ANN,
  "Holt" = modelo_AAN,
  "Holt amortizado" = modelo_AAN_damped,
  "Holt-Winters" = modelo_AAA,
  "Holt-Winters amortizado" = modelo_AAA_damped,
  "ETS automático" = modelo_auto
)

previsoes <- list(
  "ETS(ANN)" = prev_ANN,
  "Holt" = prev_AAN,
  "Holt amortizado" = prev_AAN_damped,
  "Holt-Winters" = prev_AAA,
  "Holt-Winters amortizado" = prev_AAA_damped,
  "ETS automático" = prev_auto
)

resultados <- data.frame()

for (nome in names(modelos)) {

  acc <- accuracy(
    previsoes[[nome]],
    serie_teste
  )

  resultados <- rbind(
    resultados,
    data.frame(
      Modelo = nome,
      RMSE = acc["Test set", "RMSE"],
      MAE = acc["Test set", "MAE"],
      MAPE = acc["Test set", "MAPE"],
      AICc = modelos[[nome]]$aicc
    )
  )
}

resultados <- resultados %>%
  arrange(RMSE)

resultados


# ============================================================
# 2.8 DIAGNÓSTICO DO MELHOR MODELO
# ============================================================

# Selecionando o modelo com menor RMSE
melhor_modelo_nome <- resultados$Modelo[1]

melhor_modelo_nome


# Recuperando o modelo correspondente
melhor_modelo <- modelos[[melhor_modelo_nome]]

# ------------------------------------------------------------
# Resíduos
# ------------------------------------------------------------

residuos <- residuals(melhor_modelo)

autoplot(residuos) +
  labs(
    title = paste(
      "2.8.1 - Resíduos do modelo",
      melhor_modelo_nome
    ),
    x = "Tempo",
    y = "Resíduo"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# ACF dos resíduos
# ------------------------------------------------------------

ggAcf(residuos) +
  labs(
    title = "2.8.2 - ACF dos resíduos",
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# Diagnóstico completo
# ------------------------------------------------------------

checkresiduals(melhor_modelo)


# ------------------------------------------------------------
# Teste de normalidade dos resíduos
# ------------------------------------------------------------

shapiro_resultado <- shapiro.test(residuos)

shapiro_resultado


# ============================================================
# 3. EXPLORAÇÃO E PREVISÃO FUTURA
# ============================================================

# ------------------------------------------------------------
# 3.1 Reajuste do melhor modelo em toda a série
# ------------------------------------------------------------

if (melhor_modelo_nome == "ETS(ANN)") {

  modelo_final <- ets(
    ts_serie,
    model = "ANN",
    damped = FALSE
  )

} else if (melhor_modelo_nome == "Holt") {

  modelo_final <- ets(
    ts_serie,
    model = "AAN",
    damped = FALSE
  )

} else if (melhor_modelo_nome == "Holt amortizado") {

  modelo_final <- ets(
    ts_serie,
    model = "AAN",
    damped = TRUE
  )

} else if (melhor_modelo_nome == "Holt-Winters") {

  modelo_final <- ets(
    ts_serie,
    model = "AAA",
    damped = FALSE
  )

} else if (melhor_modelo_nome == "Holt-Winters amortizado") {

  modelo_final <- ets(
    ts_serie,
    model = "AAA",
    damped = TRUE
  )

} else if (melhor_modelo_nome == "ETS automático") {

  modelo_final <- ets(ts_serie)
}


# ------------------------------------------------------------
# 3.2 Previsão futura
# ------------------------------------------------------------

h_futuro <- 24

previsao_futura <- forecast(
  modelo_final,
  h = h_futuro
)


# ------------------------------------------------------------
# 3.3 Gráfico da previsão futura
# ------------------------------------------------------------

autoplot(previsao_futura) +
  autolayer(
    ts_serie,
    series = "Observado"
  ) +
  labs(
    title = paste(
      "3.3 Previsão futura -",
      melhor_modelo_nome
    ),
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 3.4 Valores previstos
# ------------------------------------------------------------

previsao_futura$mean


# ------------------------------------------------------------
# 3.5 Intervalos de previsão
# ------------------------------------------------------------

previsao_futura