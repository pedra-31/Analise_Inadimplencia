# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# REGRESSÃO LINEAR
# ============================================================

# ------------------------------------------------------------
# 2.1. Preparação
# ------------------------------------------------------------

# library(dplyr)
# library(ggplot2)
# library(forecast)

# Criando variável de tempo
serie_inadp$t <- 1:nrow(serie_inadp)

# ============================================================
# 2.2. MODELAGEM: REGRESSÃO LINEAR
# ============================================================

# ------------------------------------------------------------
# 2.3 Regressão linear simples
# ------------------------------------------------------------

modelo <- lm(
  y ~ t,
  data = serie_inadp
)

summary(modelo)


# ------------------------------------------------------------
# 2.4 Valores ajustados e resíduos
# ------------------------------------------------------------

ajustados <- fitted(modelo)
residuos <- residuals(modelo)


# ------------------------------------------------------------
# 2.5 Gráfico da série e reta ajustada
# ------------------------------------------------------------

ggplot(serie_inadp, aes(x = t, y = y)) +

  geom_line() +

  geom_line(
    aes(y = ajustados),
    linewidth = 1
  ) +

  labs(
    title = paste("02 | Regressão linear —", series_name),
    x = "Tempo",
    y = series_value_label
  ) +

  theme_minimal()


# ============================================================
# 2.6. ANÁLISE DOS RESÍDUOS
# ============================================================

# ------------------------------------------------------------
# 2.7 Gráfico dos resíduos
# ------------------------------------------------------------

ggplot(
  serie_inadp,
  aes(x = t, y = residuos)
) +

  geom_line() +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  labs(
    title = paste("02 | Resíduos da regressão —", series_name),
    x = "Tempo",
    y = "Resíduo"
  ) +

  theme_minimal()


# ------------------------------------------------------------
# 2.8 Autocovariância dos resíduos
# ------------------------------------------------------------

autocovariancia <- function(x, h) {

  x <- as.numeric(x)

  x <- x - mean(x)

  n <- length(x)

  if (h >= n) {
    return(NA)
  }

  soma <- sum(
    x[(h + 1):n] *
    x[1:(n - h)]
  )

  return(soma / n)
}


# Calculando para os primeiros 12 lags
hs <- 1:12

autocovs <- sapply(
  hs,
  function(h) autocovariancia(residuos, h)
)

resultado_autocov <- data.frame(
  h = hs,
  autocovariancia = autocovs
)

print(resultado_autocov)


# ------------------------------------------------------------
# 2.9 Autocovariância usando função do R
# ------------------------------------------------------------

acf(
  residuos,
  type = "covariance",
  main = paste("02 | Autocovariância dos resíduos —", series_name)
)


# ------------------------------------------------------------
# 2.10 Autocorrelação dos resíduos
# ------------------------------------------------------------

acf(
  residuos,
  main = paste("02 | ACF dos resíduos —", series_name)
)


# ============================================================
# 2.11. TESTES E DIAGNÓSTICOS
# ============================================================

# ------------------------------------------------------------
# 2.12 Teste de normalidade dos resíduos
# ------------------------------------------------------------

shapiro.test(residuos)


# ------------------------------------------------------------
# 2.13 Diagnóstico completo dos resíduos
# ------------------------------------------------------------

checkresiduals(modelo)
