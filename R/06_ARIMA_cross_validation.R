# ============================================================
# VALIDACAO CRUZADA TEMPORAL DE MODELOS ARIMA
# ============================================================

# library(forecast)
# library(ggplot2)
# library(dplyr)
# library(tseries)


# ------------------------------------------------------------
# 1. Separacao entre treino e teste final
# ------------------------------------------------------------

# n_teste, h_cv e initial sao calculados dinamicamente em main.R.
n <- length(ts_serie)

if (n <= n_teste) {
  stop("A serie precisa ter mais observacoes do que o tamanho do teste final.")
}

serie_treino <- window(ts_serie, end = time(ts_serie)[n - n_teste])
serie_teste <- window(ts_serie, start = time(ts_serie)[n - n_teste + 1])

# ------------------------------------------------------------
# 2. Parametros da validacao cruzada
# ------------------------------------------------------------

# As metricas serao calculadas em cada passo de previsao, de 1 ate h_cv.
if (length(serie_treino) < initial + h_cv) {
  stop("Serie de treino curta demais para initial e h_cv escolhidos.")
}

# ------------------------------------------------------------
# 3. Modelos ARIMA candidatos
# ------------------------------------------------------------

# Usa somente os melhores modelos encontrados em R/05_ARIMA_analysis.R.
if (!exists("arima_shortlist") || !nrow(arima_shortlist)) {
  stop("Execute R/05_ARIMA_analysis.R antes da validacao cruzada.")
}
modelos <- arima_shortlist[, c("modelo", "p", "d", "q")]
names(modelos)[names(modelos) == "modelo"] <- "nome"

ajustar_arima <- function(y, especificacao) {
  forecast::Arima(
    y,
    order = c(especificacao$p, especificacao$d, especificacao$q),
    include.drift = FALSE,
    method = "ML"
  )
}

# ------------------------------------------------------------
# 4. Validacao cruzada de origem movel
# ------------------------------------------------------------

# Treino expansivo: em cada origem entram apenas dados anteriores.
# As origens sao espaçadas para limitar o custo dos ajustes.
origens <- unique(round(seq(initial, length(serie_treino) - h_cv,
                            length.out = n_cv_origins)))

erros_cv <- array(
  NA_real_,
  dim = c(nrow(modelos), length(origens), h_cv),
  dimnames = list(
    modelo = modelos$nome,
    origem = paste0("origem_", origens),
    horizonte = paste0("h", seq_len(h_cv))
  )
)

total_ajustes <- nrow(modelos) * length(origens)
contador_ajustes <- 0L

for (m in seq_len(nrow(modelos))) {
  for (i in seq_along(origens)) {
    fim_treino <- origens[i]
    treino_origem <- serie_treino[seq_len(fim_treino)]
    observado <- as.numeric(
      serie_treino[(fim_treino + 1):(fim_treino + h_cv)]
    )

    ajuste <- tryCatch(
      ajustar_arima(treino_origem, modelos[m, ]),
      error = function(e) {
        message(
          "Falha em ", modelos$nome[m], " na origem ", fim_treino,
          ": ", conditionMessage(e)
        )
        NULL
      }
    )

    if (!is.null(ajuste)) {
      previsto <- tryCatch(
        as.numeric(forecast(ajuste, h = h_cv)$mean),
        error = function(e) {
          message(
            "Falha ao prever com ", modelos$nome[m], " na origem ",
            fim_treino, ": ", conditionMessage(e)
          )
          NULL
        }
      )

      if (!is.null(previsto) && length(previsto) == h_cv) {
        erros_cv[m, i, ] <- observado - previsto
      }
    }

    contador_ajustes <- contador_ajustes + 1L
    message(
      "Ajuste ", contador_ajustes, "/", total_ajustes,
      " concluido: ", modelos$nome[m], ", origem ", fim_treino, "."
    )
  }
}

# ------------------------------------------------------------
# 5. Metricas por modelo e horizonte
# ------------------------------------------------------------

# RMSE/MAE agrupados entre origens; desvio padrao/variancia do erro
# medem quanto os erros daquele horizonte variam entre origens.
resultados_horizonte <- do.call(rbind, lapply(seq_len(nrow(modelos)), function(m) {
  do.call(rbind, lapply(seq_len(h_cv), function(h) {
    e <- erros_cv[m, , h]
    e <- e[is.finite(e)]

    data.frame(
      modelo = modelos$nome[m],
      horizonte = h,
      RMSE = if (length(e)) sqrt(mean(e^2)) else NA_real_,
      MAE = if (length(e)) mean(abs(e)) else NA_real_,
      desvio_padrao_erro = if (length(e) > 1) sd(e) else NA_real_,
      variancia_erro = if (length(e) > 1) var(e) else NA_real_,
      origens_validas = length(e)
    )
  }))
}))

# Resumo agregado usado para selecionar o modelo antes do teste final.
resumo_cv <- do.call(rbind, lapply(seq_len(nrow(modelos)), function(m) {
  e <- as.numeric(erros_cv[m, , ])
  e <- e[is.finite(e)]

  data.frame(
    modelo = modelos$nome[m],
    RMSE_CV = if (length(e)) sqrt(mean(e^2)) else NA_real_,
    MAE_CV = if (length(e)) mean(abs(e)) else NA_real_,
    desvio_padrao_erro_CV = if (length(e) > 1) sd(e) else NA_real_,
    variancia_erro_CV = if (length(e) > 1) var(e) else NA_real_,
    previsoes_validas = length(e)
  )
}))

resumo_cv <- resumo_cv[order(resumo_cv$RMSE_CV), ]
rownames(resumo_cv) <- NULL

print(resultados_horizonte, row.names = FALSE)
print(resumo_cv, row.names = FALSE)

# ------------------------------------------------------------
# 6. Salvamento dos resultados da validacao cruzada
# ------------------------------------------------------------

write.csv(
  resultados_horizonte,
  series_output_path("arima_cv_por_horizonte.csv"),
  row.names = FALSE
)
write.csv(
  resumo_cv,
  series_output_path("arima_cv_resumo.csv"),
  row.names = FALSE
)

grafico_cv <- ggplot(
  resultados_horizonte,
  aes(x = horizonte, y = RMSE, color = modelo, group = modelo)
) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq_len(h_cv)) +
  labs(
    title = "ARIMA: RMSE da validacao cruzada por horizonte",
    x = "Horizonte de previsao (passos a frente)",
    y = "RMSE entre origens",
    color = "Modelo"
  ) +
  theme_minimal()

print(grafico_cv)

# ------------------------------------------------------------
# 7. Avaliacao unica no teste final
# ------------------------------------------------------------

if (all(!is.finite(resumo_cv$RMSE_CV))) {
  stop("Nenhum modelo teve previsoes validas na validacao cruzada.")
}

melhor_modelo_nome <- resumo_cv$modelo[which.min(resumo_cv$RMSE_CV)]
melhor_indice <- match(melhor_modelo_nome, modelos$nome)
melhor_especificacao <- modelos[melhor_indice, ]

ajuste_teste <- tryCatch(
  ajustar_arima(serie_treino, melhor_especificacao),
  error = function(e) {
    stop("Falha ao ajustar o modelo selecionado para o teste: ",
         conditionMessage(e))
  }
)

previsao_teste <- forecast(ajuste_teste, h = n_teste)
resultado_teste <- accuracy(previsao_teste, serie_teste)

cat("Modelo selecionado pela validacao cruzada:", melhor_modelo_nome, "\n")
print(resultado_teste)

write.csv(
  data.frame(
    modelo = melhor_modelo_nome,
    horizonte = seq_len(n_teste),
    observado = as.numeric(serie_teste),
    previsto = as.numeric(previsao_teste$mean),
    erro = as.numeric(serie_teste) - as.numeric(previsao_teste$mean)
  ),
  series_output_path("arima_teste_final.csv"),
  row.names = FALSE
)

autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  autolayer(previsao_teste$mean, series = melhor_modelo_nome) +
  labs(
    title = paste("ARIMA selecionado:", melhor_modelo_nome),
    x = "Tempo",
    y = series_value_label,
    colour = NULL
  ) +
  theme_minimal()
