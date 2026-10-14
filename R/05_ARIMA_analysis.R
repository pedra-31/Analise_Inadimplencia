# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# MODELO ARIMA
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

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
# 2. SEPARAÇÃO ENTRE TREINO E TESTE
# ============================================================

n_teste <- 32
n_total <- length(ts_serie)
n_treino <- n_total - n_teste

if (n_treino <= 0) {
  stop("A série não possui observações suficientes.")
}

# Mantendo a estrutura de série temporal
serie_treino <- window(
  ts_serie,
  end = time(ts_serie)[n_treino]
)

serie_teste <- window(
  ts_serie,
  start = time(ts_serie)[n_treino + 1]
)

# Gráfico da separação
autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  labs(
    title = "2.1 - Separação entre treino e teste",
    x = "Ano",
    y = "Inadimplência"
  ) +
  theme_minimal()


# ============================================================
# 3. MODELAGEM ARIMA COM d = 2
# ============================================================

# ------------------------------------------------------------
# 3.1 Ajuste dos modelos ARIMA
# ------------------------------------------------------------


# ARIMA(1,2,0): AR(1)
modelo_120 <- forecast::Arima(
  serie_treino,
  order = c(1, 2, 0),
  include.drift = FALSE,
  method = "ML"
)

# ARIMA(0,2,1): MA(1)
modelo_021 <- forecast::Arima(
  serie_treino,
  order = c(0, 2, 1),
  include.drift = FALSE,
  method = "ML"
)

# ARIMA(1,2,1): AR(1) + MA(1)
modelo_121 <- forecast::Arima(
  serie_treino,
  order = c(1, 2, 1),
  include.drift = FALSE,
  method = "ML"
)

# ARIMA(2,2,1): AR(2) + MA(1)
modelo_221 <- forecast::Arima(
  serie_treino,
  order = c(2, 2, 1),
  include.drift = FALSE,
  method = "ML"
)

# ARIMA(1,2,2): AR(1) + MA(2)
modelo_122 <- forecast::Arima(
  serie_treino,
  order = c(1, 2, 2),
  include.drift = FALSE,
  method = "ML"
)

# ARIMA(2,2,2): AR(2) + MA(2)
modelo_222 <- forecast::Arima(
  serie_treino,
  order = c(2, 2, 2),
  include.drift = FALSE,
  method = "ML"
)

modelo_123 <- forecast::Arima(
  serie_treino,
  order = c(1, 2, 3),
  include.drift = FALSE,
  method = "ML"
)

# Organizando os modelos em uma lista
modelos <- list(
  "ARIMA(1,2,0)" = modelo_120,
  "ARIMA(0,2,1)" = modelo_021,
  "ARIMA(1,2,1)" = modelo_121,
  "ARIMA(2,2,1)" = modelo_221,
  "ARIMA(1,2,2)" = modelo_122,
  "ARIMA(2,2,2)" = modelo_222,
  "ARIMA(1,2,3)" = modelo_123
)


#Akaike’s Information Criterion (AIC), which was useful in selecting predictors for regression (see Section 7.5), 
#is also useful for determining the order of an ARIMA model. It can be written as:
#AIC=−2log⁡(L)+2(p+q+k+1),
#AIC=−2log(L)+2(p+q+k+1), where LL is the likelihood of the data, k=1k=1 if c≠0c=0 and k=0k=0 if c=0c=0. Note that the last term in parentheses is the number of parameters in the model (including σ2σ2, the variance of the residuals).

#For ARIMA models, the corrected AIC can be written as
#AICc=AIC+2(p+q+k+1)(p+q+k+2)T−p−q−k−2,
#AICc=AIC+T−p−q−k−22(p+q+k+1)(p+q+k+2)​,

# ------------------------------------------------------------
# 3.2 Função para calcular as métricas
# ------------------------------------------------------------

avaliar_modelo <- function(modelo, nome) {
  
  # Previsão para o período de teste
  previsao <- forecast(
    modelo,
    h = n_teste
  )
  
  valores_reais <- as.numeric(serie_teste)
  valores_previstos <- as.numeric(previsao$mean)
  
  erros <- valores_reais - valores_previstos
  
  data.frame(
    modelo = nome,
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    RMSE = sqrt(mean(erros^2))
  )
}


# ------------------------------------------------------------
# 3.3 Avaliação individual dos modelos
# ------------------------------------------------------------

resultado_021 <- avaliar_modelo(
  modelo_021, "ARIMA(0,2,1)"
)

resultado_120 <- avaliar_modelo(
  modelo_120, "ARIMA(1,2,0)"
)

resultado_121 <- avaliar_modelo(
  modelo_121, "ARIMA(1,2,1)"
)

resultado_221 <- avaliar_modelo(
  modelo_221, "ARIMA(2,2,1)"
)

resultado_122 <- avaliar_modelo(
  modelo_122, "ARIMA(1,2,2)"
)

resultado_222 <- avaliar_modelo(
  modelo_222, "ARIMA(2,2,2)"
)

resultado_123 <- avaliar_modelo(
  modelo_123, "ARIMA(1,2,3)"
)


# ------------------------------------------------------------
# 3.4 Comparação dos modelos
# ------------------------------------------------------------

resultados <- bind_rows(
  resultado_021,
  resultado_120,
  resultado_121,
  resultado_221,
  resultado_122,
  resultado_222,
  resultado_123
)

# Ordenação pelo AICc
resultados_aic <- resultados |>
  arrange(AIC)

print(resultados_aic)

resultados_rmse <- resultados |>
  arrange(RMSE)

print(resultados_rmse)


# ============================================================
# 4. SELEÇÃO DO MODELO
# ============================================================

# Seleção pelo menor RMSE na amostra de teste
melhor_nome <- resultados_rmse$modelo[1]

melhor_modelo <- modelos[[melhor_nome]]

cat("Modelo com menor RMSE no teste:", melhor_nome, "\n")

summary(melhor_modelo)


# ------------------------------------------------------------
# 4.1 Previsões do modelo selecionado
# ------------------------------------------------------------

previsao_teste <- forecast(
  melhor_modelo,
  h = n_teste
)

autoplot(previsao_teste) +
  autolayer(
    serie_teste,
    series = "Valores observados"
  ) +
  labs(
    title = paste(
      "4.1 - Previsões no teste:",
      melhor_nome
    ),
    x = "Ano",
    y = "Inadimplência"
  ) +
  theme_minimal()


# ============================================================
# 5. ANÁLISE DOS RESÍDUOS
# ============================================================

# ------------------------------------------------------------
# 5.1 Gráfico e ACF dos resíduos
# ------------------------------------------------------------

residuos <- residuals(melhor_modelo)

autoplot(residuos) +
  labs(
    title = "5.1 - Resíduos do modelo selecionado",
    x = "Ano",
    y = "Resíduo"
  ) +
  theme_minimal()

acf(
  residuos,
  lag.max = 36,
  main = "5.2 - ACF dos resíduos"
)


# ------------------------------------------------------------
# 5.3 Diagnóstico completo dos resíduos
# ------------------------------------------------------------

checkresiduals(melhor_modelo)


# ------------------------------------------------------------
# 5.4 Teste de normalidade
# ------------------------------------------------------------

shapiro.test(
  as.numeric(na.omit(residuos))
)

# ------------------------------------------------------------
# 5.5 AIC e BIC
# ------------------------------------------------------------
print(resultados_rmse[1,"AIC"])
print(resultados_rmse[1,"BIC"])


# ------------------------------------------------------------
# 6. usando a função auto.arima() para análise
# ------------------------------------------------------------

modelo_auto <- auto.arima(serie_treino)
summary(modelo_auto)
checkresiduals(modelo_auto)