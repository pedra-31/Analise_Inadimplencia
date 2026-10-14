# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# ANÁLISE INICIAL DA SÉRIE
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(forecast)
library(ggplot2)
library(forecast)
library(tseries)

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
# 1. ANÁLISE DESCRITIVA DA SÉRIE
# ============================================================

# ------------------------------------------------------------
# 1.1 Visualização da série
# ------------------------------------------------------------

autoplot(ts_serie) +
  labs(
    title = "1.1 - Inadimplência no Brasil",
    x = "Tempo",
    y = "Percentual de inadimplência"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.2 Estatísticas descritivas
# ------------------------------------------------------------

summary(ts_serie)

estatisticas <- data.frame(
  Media = mean(ts_serie),
  Mediana = median(ts_serie),
  Desvio_Padrao = sd(ts_serie),
  Minimo = min(ts_serie),
  Maximo = max(ts_serie),
  Q1 = quantile(ts_serie, 0.25),
  Q3 = quantile(ts_serie, 0.75)
)

estatisticas


# ------------------------------------------------------------
# 1.3 Distribuição dos valores
# ------------------------------------------------------------

ggplot(
  data.frame(valor = as.numeric(ts_serie)),
  aes(x = valor)
) +
  geom_histogram(bins = 25) +
  labs(
    title = "1.3 - Distribuição da inadimplência",
    x = "Inadimplência",
    y = "Frequência"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.4 Comportamento sazonal
# ------------------------------------------------------------

ggseasonplot(
  ts_serie,
  year.labels = TRUE,
  year.labels.left = TRUE
) +
  labs(
    title = "1.4 - Comportamento sazonal da inadimplência",
    x = "Mês",
    y = "Inadimplência"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.5 Decomposição da série
# ------------------------------------------------------------

decomposicao <- stl(
  ts_serie,
  s.window = "periodic"
)

autoplot(decomposicao) +
  labs(
    title = "1.5 - Decomposição da série temporal"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.6 ACF e PACF
# ------------------------------------------------------------

ggAcf(ts_serie) +
  labs(
    title = "1.6.1 - Função de Autocorrelação (ACF)"
  ) +
  theme_minimal()

ggPacf(ts_serie) +
  labs(
    title = "1.6.2 - Função de Autocorrelação Parcial (PACF)"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.7 Testes de estacionalidade
# ------------------------------------------------------------

adf.test(ts_serie)
kpss.test(ts_serie)

# ------------------------------------------------------------
# 1.8 Olhando a série diferenciada
# ------------------------------------------------------------

ts_diff1 <- diff(ts_serie, differences = 1)

autoplot(ts_diff1) +
  labs(
    title = "1.8.1 - Série diferenciada, differences = 1",
    x = "Tempo",
    y = "Percentual de inadimplência"
  ) +
  theme_minimal()


ggAcf(ts_diff1) +
  labs(
    title = "1.8.2 - ACF da série diferenciada, differences = 1",
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

ggPacf(ts_diff1) +
  labs(
    title = "1.8.3 - PACF da série diferenciada, differences = 1",
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

adf.test(ts_diff1)
kpss.test(ts_diff1)

ts_diff2 <- diff(ts_serie, differences = 2)

autoplot(ts_diff2) +
  labs(
    title = "1.8.4 - Série diferenciada, differences = 2",
    x = "Tempo",
    y = "Percentual de inadimplência"
  ) +
  theme_minimal()

ggAcf(ts_diff2) +
  labs(
    title = "1.8.5 - ACF da série diferenciada, differences = 2",
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

ggPacf(ts_diff2) +
  labs(
    title = "1.8.6 - PACF da série diferenciada, differences = 2",
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

adf.test(ts_diff2)
kpss.test(ts_diff2)