# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# ANÁLISE EXPLORATÓRIA + VALIDAÇÃO CRUZADA + ETS
# ============================================================

# ------------------------------------------------------------
# 4.1. Preparação
# ------------------------------------------------------------

# library(forecast)
# library(ggplot2)


# As últimas n_teste observações ficam completamente fora
# da escolha do modelo.

n <- length(ts_serie)

serie_treino <- window(
  ts_serie,
  end = time(ts_serie)[n - n_teste]
)

serie_teste <- window(
  ts_serie,
  start = time(ts_serie)[n - n_teste + 1]
)


# Visualização da separação

# ============================================================
# 4.2. VALIDAÇÃO CRUZADA TEMPORAL
# ============================================================

# Horizonte de previsão definido dinamicamente em main.R.

# h_cv é definido dinamicamente em main.R.

# Quantidade mínima de observações antes da primeira
# previsão da validação cruzada.

# initial é definido dinamicamente em main.R.


# ------------------------------------------------------------
# 4.3 Definição dos modelos
# ------------------------------------------------------------

modelos <- list(

  "ETS(ANN)" = list(
    model = "ANN",
    damped = FALSE
  ),

  "Holt" = list(
    model = "AAN",
    damped = FALSE
  ),

  "Holt amortizado" = list(
    model = "AAN",
    damped = TRUE
  ),

  "Holt-Winters" = list(
    model = "AAA",
    damped = FALSE
  ),

  "Holt-Winters amortizado" = list(
    model = "AAA",
    damped = TRUE
  )
)


# ------------------------------------------------------------
# 4.4 Função para gerar previsões
# ------------------------------------------------------------

criar_funcao_previsao <- function(modelo, damped) {

  function(y, h) {

    ajuste <- ets(
      y,
      model = modelo,
      damped = damped
    )

    forecast(
      ajuste,
      h = h
    )
  }
}


# Criando uma função de previsão para cada modelo

funcoes_previsao <- lapply(
  modelos,
  function(x) {

    criar_funcao_previsao(
      x$model,
      x$damped
    )
  }
)


# ------------------------------------------------------------
# 4.5 Execução da validação cruzada
# ------------------------------------------------------------

erros_cv <- lapply(
  funcoes_previsao,
  function(funcao) {

    tsCV(
      serie_treino,
      funcao,
      h = h_cv,
      initial = initial
    )
  }
)


# ============================================================
# 4.6. AVALIAÇÃO DA VALIDAÇÃO CRUZADA
# ============================================================

# ------------------------------------------------------------
# 4.7 Funções para cálculo das métricas
# ------------------------------------------------------------

calcular_rmse <- function(x) {

  sqrt(
    mean(
      x^2,
      na.rm = TRUE
    )
  )
}


calcular_mae <- function(x) {

  mean(
    abs(x),
    na.rm = TRUE
  )
}


# ------------------------------------------------------------
# 4.8 Tabela geral dos modelos
# ------------------------------------------------------------

resultados_cv <- data.frame(
  Modelo = names(erros_cv),
  RMSE = sapply(
    erros_cv,
    calcular_rmse
  ),
  MAE = sapply(
    erros_cv,
    calcular_mae
  )
)

resultados_cv <- resultados_cv[
  order(resultados_cv$RMSE),
]

rownames(resultados_cv) <- NULL

resultados_cv


# ------------------------------------------------------------
# 4.9 Variabilidade do erro entre origens da validação
# ------------------------------------------------------------

# O RMSE global mistura erros de várias origens de previsão.
# Aqui calculamos um RMSE por origem (agregando os h_cv meses)
# e medimos quanto esse erro varia ao longo do tempo.
# Isso ajuda a identificar modelos cujo desempenho depende muito
# do período usado como janela de teste.

rmse_por_origem <- sapply(
  erros_cv,
  function(erros) {

    sqrt(
      rowMeans(
        erros^2,
        na.rm = TRUE
      )
    )
  }
)

# Remover origens sem previsão válida em nenhum modelo.
rmse_por_origem <- rmse_por_origem[
  rowSums(is.finite(rmse_por_origem)) > 0,
  ,
  drop = FALSE
]

resumo_ets <- data.frame(
  Modelo = colnames(rmse_por_origem),
  RMSE_medio_origem = sapply(
    as.data.frame(rmse_por_origem),
    mean,
    na.rm = TRUE
  ),
  Desvio_padrao_RMSE = sapply(
    as.data.frame(rmse_por_origem),
    sd,
    na.rm = TRUE
  ),
  Min_RMSE = sapply(
    as.data.frame(rmse_por_origem),
    min,
    na.rm = TRUE
  ),
  Max_RMSE = sapply(
    as.data.frame(rmse_por_origem),
    max,
    na.rm = TRUE
  )
)

resumo_ets$CV_RMSE <- with(
  resumo_ets,
  Desvio_padrao_RMSE / RMSE_medio_origem
)

resumo_ets <- resumo_ets[
  order(resumo_ets$RMSE_medio_origem),
]

resumo_ets

write.csv(
  resumo_ets,
  file = series_output_path("resumo_ets.csv"),
  row.names = FALSE
)

# Em cada origem, identificar qual modelo teve o menor RMSE.
# A frequência de vitórias mostra se há um vencedor consistente.
modelo_vencedor_origem <- apply(
  rmse_por_origem,
  1,
  function(x) {

    if (all(!is.finite(x))) {
      return(NA_character_)
    }

    names(which.min(replace(x, !is.finite(x), Inf)))
  }
)

frequencia_vitorias <- sort(
  prop.table(table(modelo_vencedor_origem)),
  decreasing = TRUE
)

frequencia_vitorias


# ------------------------------------------------------------
# 4.10 Erro por horizonte de previsão
# ------------------------------------------------------------

rmse_horizonte <- lapply(
  erros_cv,
  function(erros) {

    apply(
      erros,
      2,
      calcular_rmse
    )
  }
)

dados_horizonte <- data.frame(
  Horizonte = 1:h_cv
)

for (nome in names(rmse_horizonte)) {

  dados_horizonte[[nome]] <-
    rmse_horizonte[[nome]]
}


# Visualização

dados_horizonte_long <- reshape(
  dados_horizonte,
  varying = names(dados_horizonte)[-1],
  v.names = "RMSE",
  timevar = "Modelo",
  times = names(dados_horizonte)[-1],
  direction = "long"
)

ggplot(
  dados_horizonte_long,
  aes(
    x = Horizonte,
    y = RMSE,
    group = Modelo,
    linetype = Modelo
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  labs(
    title = paste("04 | ETS — CV rolling-origin:", series_name),
    subtitle = paste("Horizonte:", h_cv, "| Origens:", nrow(rmse_por_origem)),
    x = "Horizonte de previsão (passos)",
    y = "RMSE",
    linetype = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 4.11 Escolha do melhor modelo
# ------------------------------------------------------------

melhor_modelo_nome <- resultados_cv$Modelo[1]

melhor_modelo_nome


# ------------------------------------------------------------
# 4.12 Teste do modelo
# ------------------------------------------------------------

# O modelo foi escolhido usando apenas a validação cruzada.
# Agora é avaliado uma única vez no período reservado.

melhor_funcao <- funcoes_previsao[[melhor_modelo_nome]]

previsao_teste <- melhor_funcao(
  serie_treino,
  n_teste
)


# ------------------------------------------------------------
# 4.13 Métricas no teste final
# ------------------------------------------------------------

resultado_teste <- accuracy(
  previsao_teste,
  serie_teste
)

resultado_teste


# ------------------------------------------------------------
# 4.14 Gráfico do teste final
# ------------------------------------------------------------

autoplot(
  serie_treino,
  series = "Treino"
) +
  autolayer(
    serie_teste,
    series = "Teste"
  ) +
  autolayer(
    previsao_teste$mean,
    series = melhor_modelo_nome
  ) +
  labs(
    title = paste(
      "04 | ETS — modelo selecionado por CV:",
      melhor_modelo_nome
    ),
    subtitle = paste("Teste final:", n_teste, "observações"),
    x = "Tempo",
    y = series_value_label,
    colour = NULL
  ) +
  theme_minimal()


# ============================================================
# 4.15. AJUSTE FINAL DO MELHOR MODELO
# ============================================================

# Agora que o modelo foi:
#
# 1. escolhido pela validação cruzada;
# 2. avaliado no teste final;
#
# ele pode ser reajustado utilizando toda a série.

configuracao <- modelos[[melhor_modelo_nome]]


if (is.null(configuracao$model)) {

  modelo_final <- ets(
    ts_serie
  )

} else {

  modelo_final <- ets(
    ts_serie,
    model = configuracao$model,
    damped = configuracao$damped
  )
}


summary(modelo_final)


# ============================================================
# 4.18. DIAGNÓSTICO DO MODELO FINAL
# ============================================================

residuos <- residuals(
  modelo_final
)


# ------------------------------------------------------------
# 4.19 Resíduos ao longo do tempo
# ------------------------------------------------------------

autoplot(residuos) +
  labs(
    title = paste(
      "04 | ETS — resíduos do modelo selecionado por CV:",
      melhor_modelo_nome
    ),
    x = "Tempo",
    y = "Resíduo"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 4.20 ACF dos resíduos
# ------------------------------------------------------------

ggAcf(residuos) +
  labs(
    title = paste("04 | ETS — ACF dos resíduos:", melhor_modelo_nome),
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 4.21 Diagnóstico completo
# ------------------------------------------------------------

checkresiduals(
  modelo_final
)


# ------------------------------------------------------------
# 4.22 Teste de normalidade
# ------------------------------------------------------------

shapiro_resultado <- shapiro.test(
  residuos
)

shapiro_resultado


# ------------------------------------------------------------
# 4.23 PREVISÃO FUTURA
# ------------------------------------------------------------

h_futuro <- 24

previsao_futura <- forecast(
  modelo_final,
  h = h_futuro
)


# ------------------------------------------------------------
# 4.24 Gráfico da previsão
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
      "04 | ETS — previsão futura após ajuste na série completa:",
      melhor_modelo_nome
    ),
    subtitle = series_name,
    x = "Tempo",
    y = series_value_label,
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 4.25 Valores previstos
# ------------------------------------------------------------

previsao_futura$mean


# ------------------------------------------------------------
# 4.26 Intervalos de previsão
# ------------------------------------------------------------

previsao_futura
