# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# BUSCA E ANÁLISE DE MODELOS SARIMA
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

# Série mensal, conforme definido na análise inicial (R/01_series_analysis.R).
ts_serie <- ts(
  serie_inadp$y,
  start = c(2011, 3),
  frequency = 12
)

# ============================================================
# 1. SEPARAÇÃO ENTRE TREINO E TESTE
# ============================================================

# n=40 - SARIMA(1,2,2)(2,2,2)[3] 
# n=32 - SARIMA(1,2,2)(2,2,2)[3] 
# n=24 - SARIMA(1,2,2)(0,3,1)[6]
# n=18 - SARIMA(2,2,1)(1,2,0)[6]


n_teste <- 36
n_total <- length(ts_serie)
n_treino <- n_total - n_teste

serie_treino <- window(
  ts_serie,
  end = time(ts_serie)[n_treino]
)

serie_teste <- window(
  ts_serie,
  start = time(ts_serie)[n_treino + 1]
)

autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  labs(
    title = "1.1 - Separação entre treino e teste",
    x = "Ano",
    y = "Inadimplência"
  ) +
  theme_minimal()

# ============================================================
# 2. BUSCA DE MODELOS SARIMA
# ============================================================

# Modelo: SARIMA(p, 2, q)(P, D, Q)[s]. Os termos não sazonais p e q,
# sazonais P, D e Q variam de 0 a 3; os períodos sazonais são 3, 6 e 12.
# A diferenciação não sazonal d permanece fixa em 2.
ordens_sarima <- expand.grid(
  p = 1:2,
  q = 1:2,
  P = 0:2,
  D = 0:2,
  Q = 0:2,
  s = c(3, 6)
)

ordens_sarima$nome <- with(
  ordens_sarima,
  sprintf("SARIMA(%d,2,%d)(%d,%d,%d)[%d]", p, q, P, D, Q, s)
)

ajustar_sarima <- function(p, q, P, D, Q, s) {
  tryCatch(
    forecast::Arima(
      serie_treino,
      order = c(p, 2, q),
      seasonal = list(order = c(P, D, Q), period = s),
      include.drift = FALSE,
      method = "ML"
    ),
    error = function(e) NULL
  )
}

modelos <- setNames(
  lapply(seq_len(nrow(ordens_sarima)), function(i) {
    ajustar_sarima(
      ordens_sarima$p[i],
      ordens_sarima$q[i],
      ordens_sarima$P[i],
      ordens_sarima$D[i],
      ordens_sarima$Q[i],
      ordens_sarima$s[i]
    )
  }),
  ordens_sarima$nome
)

# Remove candidatos que não puderam ser estimados.
modelos <- modelos[!vapply(modelos, is.null, logical(1))]

# ============================================================
# 3. AVALIAÇÃO E COMPARAÇÃO
# ============================================================

avaliar_modelo <- function(modelo, nome) {
  previsao <- forecast(modelo, h = n_teste)
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

resultados <- bind_rows(lapply(names(modelos), function(nome) {
  avaliar_modelo(modelos[[nome]], nome)
}))

resultados_rmse <- resultados |>
  arrange(RMSE)

print(resultados_rmse)

# ============================================================
# 4. SELEÇÃO E PREVISÃO DO MELHOR MODELO
# ============================================================

melhor_nome <- "SARIMA(1,2,2)(2,2,2)[3]"
melhor_modelo <- Arima(ts_serie, 
  order = c(1, 2, 2), 
  seasonal = list(order = c(2, 2, 2), 
  period = 3)
)

previsao_teste <- forecast(melhor_modelo, h = n_teste)

erro_melhor_modelo <- as.numeric(serie_teste) - as.numeric(previsao_teste$mean)
RMSE_melhor_modelo = sqrt(mean(erro_melhor_modelo^2))

cat("Modelo com menor RMSE no teste:", melhor_nome, "\n")
cat("RMSE no teste:", RMSE_melhor_modelo, "\n")

summary(melhor_modelo)

autoplot(previsao_teste) +
  autolayer(serie_teste, series = "Valores observados") +
  labs(
    title = paste("4.1 - Previsões no teste:", melhor_nome),
    x = "Ano",
    y = "Inadimplência"
  ) +
  theme_minimal()

# ============================================================
# 5. ANÁLISE DOS RESÍDUOS DO MELHOR MODELO
# ============================================================

residuos <- residuals(melhor_modelo)

autoplot(residuos) +
  labs(
    title = paste("5.1 - Resíduos do modelo", melhor_nome),
    x = "Ano",
    y = "Resíduo"
  ) +
  theme_minimal()

acf(
  residuos,
  lag.max = 36,
  main = "5.2 - ACF dos resíduos"
)

# Diagnóstico gráfico e teste de Ljung-Box para autocorrelação residual.
checkresiduals(melhor_modelo)

# Avaliação da normalidade dos resíduos (Shapiro-Wilk aceita até 5.000 valores).
residuos_validos <- as.numeric(na.omit(residuos))
if (length(residuos_validos) >= 3 && length(residuos_validos) <= 5000) {
  print(shapiro.test(residuos_validos))
} else {
  message("O teste de Shapiro-Wilk requer entre 3 e 5.000 resíduos válidos.")
}

qqnorm(residuos_validos, main = "5.3 - Gráfico Q-Q dos resíduos")
qqline(residuos_validos, col = "red")

# Critérios de informação do modelo selecionado.
cat("AIC do melhor modelo:", AIC(melhor_modelo), "\n")
cat("BIC do melhor modelo:", BIC(melhor_modelo), "\n")


# ------------------------------------------------------------
# 6 PREVISÃO FUTURA
# ------------------------------------------------------------


h_futuro <- 24

previsao_futura <- forecast(
  melhor_modelo,
  h = h_futuro
)


# ------------------------------------------------------------
# 6.1 Gráfico da previsão
# ------------------------------------------------------------

autoplot(
  previsao_futura
) +
  autolayer(
    ts_serie,
    series = "Observado"
  ) +
  labs(
    title = paste(
      "6.1 - Previsão futura:",
      melhor_nome
    ),
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal()