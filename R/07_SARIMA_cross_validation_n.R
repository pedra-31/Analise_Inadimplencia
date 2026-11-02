# ============================================================
# VALIDACAO CRUZADA TEMPORAL SARIMA: HORIZONTES E ORDENS SAZONAIS
# ============================================================

rm(list = ls())

library(forecast)
library(ggplot2)

serie_inadp <- read.csv(
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  sep = ",",
  header = TRUE
)

ts_serie <- ts(serie_inadp$y, start = c(2011, 3), frequency = 12)


# Horizontes discretos no intervalo inclusivo 12 <= n <= 36.
n_valores <- seq(12, 36, by = 4)
h_max <- max(n_valores)

# No maximo quatro origens para limitar o tempo: com as grades abaixo,
# serao ate 4 x 7 x 4 = 112 ajustes.
initial <- min(60, length(ts_serie) - h_max)
max_origens <- 4
passo_origens <- max(1, floor((length(ts_serie) - initial - h_max) /
                               max(1, max_origens - 1)))

if (initial < 30) {
  stop("Serie curta demais para initial >= 30 e o horizonte maximo escolhido.")
}

origens <- seq(initial, length(ts_serie) - h_max, by = passo_origens)
if (length(origens) > max_origens) origens <- tail(origens, max_origens)
if (length(origens) == 0) {
  stop("Nao ha observacoes suficientes para os horizontes definidos.")
}

# Grade sazonal reduzida: sete candidatos, em periodos 3 e 6.
# Cada linha representa (P, D, Q)[periodo].
ordens_sazonais <- data.frame(
  P = c(0, 1, 2, 0, 0, 1, 2),
  D = c(1, 1, 2, 2, 1, 1, 2),
  Q = c(1, 1, 2, 1, 1, 1, 2),
  periodo = c(3, 3, 3, 3, 6, 6, 6)
)

# p e q nao sazonais variam independentemente entre 1 e 2.
grade_modelos <- merge(
  expand.grid(p = 1:2, q = 1:2),
  ordens_sazonais,
  by = NULL
)
grade_modelos$modelo <- with(
  grade_modelos,
  sprintf("SARIMA(%d,2,%d)(%d,%d,%d)[%d]", p, q, P, D, Q, periodo)
)

erros <- array(
  NA_real_,
  dim = c(nrow(grade_modelos), length(origens), h_max),
  dimnames = list(
    modelo = grade_modelos$modelo,
    origem = paste0("origem_", origens),
    horizonte = paste0("h", seq_len(h_max))
  )
)

total_ajustes <- nrow(grade_modelos) * length(origens)
contador_ajustes <- 0L

for (m in seq_len(nrow(grade_modelos))) {
  for (i in seq_along(origens)) {
    fim_treino <- origens[i]
    treino <- ts_serie[seq_len(fim_treino)]
    observado <- as.numeric(ts_serie[(fim_treino + 1):(fim_treino + h_max)])

    ajuste <- tryCatch(
      forecast::Arima(
        treino,
        order = c(grade_modelos$p[m], 2, grade_modelos$q[m]),
        seasonal = list(
          order = c(grade_modelos$P[m], grade_modelos$D[m], grade_modelos$Q[m]),
          period = grade_modelos$periodo[m]
        ),
        include.drift = FALSE,
        method = "ML"
      ),
      error = function(e) {
        message("Falha em ", grade_modelos$modelo[m], " na origem ",
                fim_treino, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(ajuste)) {
      previsto <- forecast(ajuste, h = h_max)$mean
      erros[m, i, ] <- observado - as.numeric(previsto)
    }

    contador_ajustes <- contador_ajustes + 1L
    message("Ajuste ", contador_ajustes, "/", total_ajustes,
            " concluido: ", grade_modelos$modelo[m],
            ", origem ", fim_treino, ".")
  }
}

# Para cada modelo e n, agrega os erros dos horizontes 1:n em todas as origens.
resultados <- do.call(rbind, lapply(seq_len(nrow(grade_modelos)), function(m) {
  do.call(rbind, lapply(n_valores, function(n) {
    e <- erros[m, , seq_len(n), drop = FALSE]
    data.frame(
      modelo = grade_modelos$modelo[m],
      n = n,
      RMSE = sqrt(mean(e^2, na.rm = TRUE)),
      MAE = mean(abs(e), na.rm = TRUE),
      previsoes_validas = sum(!is.na(e))
    )
  }))
}))

# Mantem todos os resultados na tabela, mas plota apenas os cinco modelos
# com menor RMSE geral, agregando os horizontes e origens da validacao.
rmse_geral <- data.frame(
  modelo = grade_modelos$modelo,
  RMSE_geral = vapply(seq_len(nrow(grade_modelos)), function(m) {
    sqrt(mean(erros[m, , ]^2, na.rm = TRUE))
  }, numeric(1))
)

resultados <- resultados[order(resultados$n, resultados$RMSE), ]
print(resultados, row.names = FALSE)
write.csv(resultados, "data/processed/sarima_cv_por_n.csv", row.names = FALSE)

top5_modelos <- head(rmse_geral$modelo[order(rmse_geral$RMSE_geral)], 5)
resultados_grafico <- subset(resultados, modelo %in% top5_modelos)

grafico <- ggplot(resultados_grafico, aes(x = n, y = RMSE, color = modelo, group = modelo)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "SARIMA: RMSE de validacao cruzada por horizonte n",
    subtitle = "p e q entre 1 e 2; sete ordens sazonais candidatas",
    x = "Horizonte n (meses)",
    y = "RMSE",
    color = "Modelo"
  ) +
  theme_minimal()

print(grafico)

comparacao_geral <- do.call(rbind, lapply(seq_len(nrow(grade_modelos)), function(m) {
  e <- erros[m, , , drop = FALSE]
  
  data.frame(
    modelo = grade_modelos$modelo[m],
    RMSE = sqrt(mean(e^2, na.rm = TRUE)),
    MAE = mean(abs(e), na.rm = TRUE),
    previsoes_validas = sum(!is.na(e))
  )
}))

comparacao_geral <- comparacao_geral[
  order(comparacao_geral$RMSE),
]

print(comparacao_geral, row.names = FALSE)
