# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# BUSCA E ANÁLISE DE MODELOS SARIMA
# ============================================================

# ------------------------------------------------------------
# 7.1. Preparação
# ------------------------------------------------------------

# library(dplyr)
# library(ggplot2)
# library(forecast)
# library(tseries)

# ============================================================
# 7.2. SEPARAÇÃO ENTRE TREINO E TESTE
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

# ============================================================
# 7.3. BUSCA DE MODELOS SARIMA
# ============================================================

# Modelo: SARIMA(p, d, q)(P, D, Q)[s]. d vem da configuracao no main.R;
# o periodo sazonal vem da frequencia da serie.
sarima_d <- nonseasonal_differencing
ordens_sarima <- expand.grid(
  p = 0:2,
  q = 0:2,
  P = 0:1,
  D = 0:1,
  Q = 0:1,
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
# 7.4. AVALIAÇÃO E COMPARAÇÃO
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

resultados_aic <- resultados |>
  arrange(AIC)

# Os melhores modelos da avaliacao inicial seguem para a validacao cruzada.
sarima_shortlist <- ordens_sarima[
  match(head(resultados_aic$modelo, n_modelos_pre_cv), ordens_sarima$nome),
]
sarima_shortlist$d <- sarima_d
sarima_shortlist$periodo <- sarima_shortlist$s
sarima_shortlist$modelo <- sarima_shortlist$nome

# Reavalia os candidatos em alguns tamanhos de teste antes da validacao cruzada.
sarima_resultados_n <- do.call(rbind, lapply(seq_len(nrow(sarima_shortlist)), function(i) {
  sp <- sarima_shortlist[i, ]
  do.call(rbind, lapply(n_teste_candidatos, function(n_holdout) {
    n_fit <- n_treino - n_holdout
    treino_n <- serie_treino[seq_len(n_fit)]
    teste_n <- serie_treino[(n_fit + 1L):n_treino]
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
sarima_shortlist <- head(sarima_shortlist, n_modelos_cv)
write.csv(sarima_resultados_n, series_output_path("sarima_analise_por_tamanho_teste.csv"), row.names = FALSE)
write.csv(sarima_shortlist, series_output_path("sarima_shortlist.csv"), row.names = FALSE)

# ============================================================
# 7.5. SELEÇÃO E PREVISÃO DO MELHOR MODELO
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
    title = paste("07 | SARIMA — melhor modelo da análise inicial:", melhor_nome),
    subtitle = paste("Série:", series_name, "| Teste final:", n_teste, "observações"),
    x = "Tempo",
    y = series_value_label
  ) +
  theme_minimal()

# ============================================================
# 7.6. ANÁLISE DOS RESÍDUOS DO MELHOR MODELO
# ============================================================

residuos <- residuals(melhor_modelo)

autoplot(residuos) +
  labs(
    title = paste("07 | SARIMA — resíduos do modelo pré-selecionado:", melhor_nome),
    subtitle = series_name,
    x = "Tempo",
    y = "Resíduo"
  ) +
  theme_minimal()

acf(
  residuos,
  lag.max = 36,
  main = paste("07 | SARIMA — ACF dos resíduos:", melhor_nome)
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

qqnorm(residuos_validos, main = paste("07 | SARIMA — Q-Q dos resíduos:", melhor_nome))
qqline(residuos_validos, col = "red")

# Critérios de informação do modelo selecionado.
cat("AIC do melhor modelo:", AIC(melhor_modelo), "\n")
cat("BIC do melhor modelo:", BIC(melhor_modelo), "\n")


# ------------------------------------------------------------
# 7.7 PREVISÃO FUTURA
# ------------------------------------------------------------


h_futuro <- 24

previsao_futura <- forecast(
  melhor_modelo,
  h = h_futuro
)


# ------------------------------------------------------------
# 7.8 Gráfico da previsão
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
      "07 | SARIMA — previsão futura após ajuste na série completa:",
      melhor_nome
    ),
    subtitle = series_name,
    x = "Tempo",
    y = series_value_label,
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal()
