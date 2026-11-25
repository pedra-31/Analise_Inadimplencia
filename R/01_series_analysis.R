# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# ANÁLISE INICIAL DA SÉRIE
# ============================================================

# ------------------------------------------------------------
# 1.1. Preparação
# ------------------------------------------------------------

# library(dplyr)
# library(forecast)
# library(ggplot2)
# library(tseries)

# ============================================================
# 1.2. ANÁLISE DESCRITIVA DA SÉRIE
# ============================================================

# ------------------------------------------------------------
# 1.3 Visualização da série
# ------------------------------------------------------------

autoplot(ts_serie) +
  labs(
    title = paste("01 | Série original —", series_name),
    x = "Tempo",
    y = series_value_label
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 1.4 Separação entre treino e teste final
# ------------------------------------------------------------

n_treino <- n_serie - n_teste
serie_treino <- window(ts_serie, end = time(ts_serie)[n_treino])
serie_teste <- window(ts_serie, start = time(ts_serie)[n_treino + 1L])

autoplot(ts_serie, series = series_name) +
  autolayer(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste final") +
  labs(
    title = paste("01 | Separação treino/teste —", series_name),
    subtitle = paste("Teste final:", n_teste, "observações"),
    x = "Tempo",
    y = series_value_label,
    colour = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.5 Estatísticas descritivas
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
# 1.6 Distribuição dos valores
# ------------------------------------------------------------

ggplot(
  data.frame(valor = as.numeric(ts_serie)),
  aes(x = valor)
) +
  geom_histogram(bins = 25) +
  labs(
    title = paste("01 | Distribuição —", series_name),
    x = series_value_label,
    y = "Frequência"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.7 Comportamento sazonal
# ------------------------------------------------------------

ggseasonplot(
  ts_serie,
  year.labels = TRUE,
  year.labels.left = TRUE
) +
  labs(
    title = paste("01 | Sazonalidade —", series_name),
    x = "Mês",
    y = series_value_label
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.8 Decomposição da série
# ------------------------------------------------------------

decomposicao <- stl(
  ts_serie,
  s.window = "periodic"
)

autoplot(decomposicao) +
  labs(
    title = paste("01 | Decomposição —", series_name)
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.9 ACF e PACF
# ------------------------------------------------------------

ggAcf(ts_serie) +
  labs(
    title = paste("01 | ACF da série original —", series_name)
  ) +
  theme_minimal()

ggPacf(ts_serie) +
  labs(
    title = paste("01 | PACF da série original —", series_name)
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 1.10 Testes de estacionalidade
# ------------------------------------------------------------

adf.test(ts_serie)
kpss.test(ts_serie)

# ------------------------------------------------------------
# 1.11 Olhando a série diferenciada
# ------------------------------------------------------------

ts_diff1 <- diff(ts_serie, differences = 1)

autoplot(ts_diff1) +
  labs(
    title = paste("01 | Série diferenciada (d = 1) —", series_name),
    x = "Tempo",
    y = series_value_label
  ) +
  theme_minimal()


ggAcf(ts_diff1) +
  labs(
    title = paste("01 | ACF após uma diferença —", series_name),
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

ggPacf(ts_diff1) +
  labs(
    title = paste("01 | PACF após uma diferença —", series_name),
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

adf.test(ts_diff1)
kpss.test(ts_diff1)

ts_diff2 <- diff(ts_serie, differences = 2)

autoplot(ts_diff2) +
  labs(
    title = paste("01 | Série diferenciada (d = 2) —", series_name),
    x = "Tempo",
    y = series_value_label
  ) +
  theme_minimal()

ggAcf(ts_diff2) +
  labs(
    title = paste("01 | ACF após duas diferenças —", series_name),
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

ggPacf(ts_diff2) +
  labs(
    title = paste("01 | PACF após duas diferenças —", series_name),
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()

adf.test(ts_diff2)
kpss.test(ts_diff2)
