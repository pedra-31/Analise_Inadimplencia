# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# BUSCA E ANÁLISE DE MODELOS SARIMA
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

# library(dplyr)
# library(ggplot2)
# library(forecast)
# library(tseries)

# ============================================================
# 1. SEPARAÇÃO ENTRE TREINO E TESTE
# ============================================================

# n_teste é calculado dinamicamente em main.R.
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

# Modelo: SARIMA(p, d, q)(P, D, Q)[s]. O período sazonal vem da frequência
# configurada no main.R e d e estimado pela serie.
sarima_d <- min(forecast::ndiffs(serie_treino), 2L)
ordens_sarima <- expand.grid(
  p = 1:2,
  q = 1:2,
  P = 0:2,
  D = 0:1,
  Q = 0:2,
  s = frequency(ts_serie)
)

ordens_sarima$nome <- with(
  ordens_sarima,
  sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]", p, sarima_d, q, P, D, Q, s)
)

ajustar_sarima <- function(p, q, P, D, Q, s) {
  tryCatch(
    forecast::Arima(
      serie_treino,
      order = c(p, sarima_d, q),
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

# Os melhores modelos da avaliacao inicial seguem para a validacao cruzada.
sarima_shortlist <- ordens_sarima[
  match(head(resultados_rmse$modelo, 3), ordens_sarima$nome),
]
sarima_shortlist$d <- sarima_d
sarima_shortlist$periodo <- sarima_shortlist$s
sarima_shortlist$modelo <- sarima_shortlist$nome

# Reavalia os candidatos em alguns tamanhos de teste antes da validacao cruzada.
sarima_resultados_n <- do.call(rbind, lapply(seq_len(nrow(sarima_shortlist)), function(i) {
  sp <- sarima_shortlist[i, ]
  do.call(rbind, lapply(n_teste_candidatos, function(n_holdout) {
    n_fit <- n_total - n_holdout
    treino_n <- ts_serie[seq_len(n_fit)]
    teste_n <- ts_serie[(n_fit + 1L):n_total]
    ajuste_n <- tryCatch(forecast::Arima(
      treino_n, order = c(sp$p, sp$d, sp$q),
      seasonal = list(order = c(sp$P, sp$D, sp$Q), period = sp$periodo),
      include.drift = FALSE, method = "ML"
    ), error = function(e) NULL)
    if (is.null(ajuste_n)) return(NULL)
    previsao_n <- forecast::forecast(ajuste_n, h = n_holdout)
    data.frame(modelo = sp$modelo, n_teste = n_holdout,
               RMSE = sqrt(mean((as.numeric(teste_n) - as.numeric(previsao_n$mean))^2)))
  }))
}))
sarima_rank_n <- aggregate(RMSE ~ modelo, sarima_resultados_n, mean)
sarima_rank_n <- sarima_rank_n[order(sarima_rank_n$RMSE), ]
sarima_shortlist <- merge(sarima_shortlist, sarima_rank_n, by = "modelo")
sarima_shortlist <- sarima_shortlist[order(sarima_shortlist$RMSE), ]
sarima_shortlist <- head(sarima_shortlist, 3L)
write.csv(sarima_resultados_n, "data/processed/sarima_analise_por_tamanho_teste.csv", row.names = FALSE)
write.csv(sarima_shortlist, "data/processed/sarima_shortlist.csv", row.names = FALSE)

# ============================================================
# 4. SELEÇÃO E PREVISÃO DO MELHOR MODELO
# ============================================================

melhor_nome <- sarima_shortlist$modelo[1]
melhor_modelo <- modelos[[melhor_nome]]

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
