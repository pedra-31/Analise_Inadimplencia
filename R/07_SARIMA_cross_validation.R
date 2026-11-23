# ============================================================
# VALIDACAO CRUZADA TEMPORAL DE MODELOS SARIMA
# ============================================================

rm(list = ls())

library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)


serie_inadp <- read.csv(
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  sep = ",",
  header = TRUE
)

ts_serie <- ts(serie_inadp$y, start = c(2011, 3), frequency = 12)

# ------------------------------------------------------------
# 1. Separacao entre treino e teste final
# ------------------------------------------------------------

n_teste <- 12
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
h_cv <- 6
initial <- 60

if (length(serie_treino) < initial + h_cv) {
  stop("Serie de treino curta demais para initial e h_cv escolhidos.")
}

# ------------------------------------------------------------
# 3. Modelos SARIMA candidatos
# ------------------------------------------------------------

# A primeira especificacao enviada apareceu duplicada; e incluida uma vez.
modelos <- data.frame(
  nome = c(
    "SARIMA(1,2,2)(2,2,2)[3]",
    "SARIMA(1,2,2)(0,3,1)[6]",
    "SARIMA(2,2,1)(1,2,0)[6]",
    "SARIMA(2,2,0)(0,2,2)[3]",
    "SARIMA(2,2,3)(0,2,1)[3]",
    "SARIMA(3,2,1)(1,1,2)[3]",
    "SARIMA(2,2,2)(0,2,1)[3]"
  ),
  p = c(1, 1, 2, 2, 2, 3, 2),
  d = c(2, 2, 2, 2, 2, 2, 2),
  q = c(2, 2, 1, 0, 3, 1, 2),
  P = c(2, 0, 1, 0, 0, 1, 0),
  D = c(2, 3, 2, 2, 2, 1, 2),
  Q = c(2, 1, 0, 2, 1, 2, 1),
  periodo = c(3, 6, 6, 3, 3, 3, 3),
  stringsAsFactors = FALSE
)

ajustar_sarima <- function(y, especificacao) {
  forecast::Arima(
    y,
    order = c(especificacao$p, especificacao$d, especificacao$q),
    seasonal = list(
      order = c(especificacao$P, especificacao$D, especificacao$Q),
      period = especificacao$periodo
    ),
    include.drift = FALSE,
    method = "ML"
  )
}

# ------------------------------------------------------------
# 4. Validacao cruzada de origem movel
# ------------------------------------------------------------

# Treino expansivo: em cada origem entram apenas dados anteriores.
# Todas as origens possiveis sao avaliadas dentro da serie de treino.
origens <- seq.int(initial, length(serie_treino) - h_cv)

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
      ajustar_sarima(treino_origem, modelos[m, ]),
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
  "data/processed/sarima_cv_por_horizonte.csv",
  row.names = FALSE
)
write.csv(
  resumo_cv,
  "data/processed/sarima_cv_resumo.csv",
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
    title = "SARIMA: RMSE da validacao cruzada por horizonte",
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
  ajustar_sarima(serie_treino, melhor_especificacao),
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
  "data/processed/sarima_teste_final.csv",
  row.names = FALSE
)

    forecast::Arima(
      serie_treino,
      order = c(3, 2, 1),
      seasonal = list(order = c(1, 1, 2), period = 3),
      include.drift = FALSE,
      method = "ML"
    )

    previsao <- forecast(modelo, h = n_teste)
    
autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  autolayer(previsao_teste$mean, series = melhor_modelo_nome) +
  labs(
    title = paste("SARIMA selecionado:", melhor_modelo_nome),
    x = "Tempo",
    y = "Inadimplencia",
    colour = NULL
  ) +
  theme_minimal()
