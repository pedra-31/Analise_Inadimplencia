# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# REGRESSÃO LINEAR
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(forecast)

serie_inadp <- read.csv(
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  sep = ",",
  header = TRUE
)

# Criando variável de tempo
serie_inadp$t <- 1:nrow(serie_inadp)

# Criando a série temporal mensal
ts_serie <- ts(
  serie_inadp$y,
  start = c(2011, 3),
  frequency = 12
)

# ============================================================
# 2. MODELAGEM: REGRESSÃO LINEAR
# ============================================================

# ------------------------------------------------------------
# 2.1 Regressão linear simples
# ------------------------------------------------------------

modelo <- lm(
  y ~ t,
  data = serie_inadp
)

summary(modelo)


# ------------------------------------------------------------
# 2.2 Valores ajustados e resíduos
# ------------------------------------------------------------

ajustados <- fitted(modelo)
residuos <- residuals(modelo)


# ------------------------------------------------------------
# 2.3 Gráfico da série e reta ajustada
# ------------------------------------------------------------

ggplot(serie_inadp, aes(x = t, y = y)) +

  geom_line() +

  geom_line(
    aes(y = ajustados),
    linewidth = 1
  ) +

  labs(
    title = "2.3 - Inadimplência e tendência linear",
    x = "Tempo",
    y = "Inadimplência"
  ) +

  theme_minimal()


# ============================================================
# 3. ANÁLISE DOS RESÍDUOS
# ============================================================

# ------------------------------------------------------------
# 3.1 Gráfico dos resíduos
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
    title = "3.1 - Resíduos da regressão",
    x = "Tempo",
    y = "Resíduo"
  ) +

  theme_minimal()


# ------------------------------------------------------------
# 3.2 Autocovariância dos resíduos
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
# 3.3 Autocovariância usando função do R
# ------------------------------------------------------------

acf(
  residuos,
  type = "covariance",
  main = "3.3 - Autocovariância dos resíduos"
)


# ------------------------------------------------------------
# 3.4 Autocorrelação dos resíduos
# ------------------------------------------------------------

acf(
  residuos,
  main = "3.4 - ACF dos resíduos"
)


# ============================================================
# 4. TESTES E DIAGNÓSTICOS
# ============================================================

# ------------------------------------------------------------
# 4.1 Teste de normalidade dos resíduos
# ------------------------------------------------------------

shapiro.test(residuos)


# ------------------------------------------------------------
# 4.2 Diagnóstico completo dos resíduos
# ------------------------------------------------------------

checkresiduals(modelo)