# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# ANÁLISE EXPLORATÓRIA + VALIDAÇÃO CRUZADA + ETS
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
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

autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  labs(
    title = "3.1 - Separação entre treino e teste final",
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL
  ) +
  theme_minimal()


# ============================================================
# 4. VALIDAÇÃO CRUZADA TEMPORAL
# ============================================================

# Horizonte de previsão definido dinamicamente em main.R.

# h_cv é definido dinamicamente em main.R.

# Quantidade mínima de observações antes da primeira
# previsão da validação cruzada.

# initial é definido dinamicamente em main.R.


# ------------------------------------------------------------
# 4.1 Definição dos modelos
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
# 4.2 Função para gerar previsões
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
# 4.3 Execução da validação cruzada
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
# 5. AVALIAÇÃO DA VALIDAÇÃO CRUZADA
# ============================================================

# ------------------------------------------------------------
# 5.1 Funções para cálculo das métricas
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
# 5.2 Tabela geral dos modelos
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
# 5.3 Variabilidade do erro entre origens da validação
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
  file = "data/processed/resumo_ets.csv",
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
# 5.3 Erro por horizonte de previsão
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
    title = "5.1 - RMSE por horizonte de previsão",
    x = "Horizonte de previsão (meses)",
    y = "RMSE",
    linetype = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 5.4 Escolha do melhor modelo
# ------------------------------------------------------------

melhor_modelo_nome <- resultados_cv$Modelo[1]

melhor_modelo_nome


# ------------------------------------------------------------
# 5.5 Teste do modelo
# ------------------------------------------------------------

# O modelo foi escolhido usando apenas a validação cruzada.
# Agora é avaliado uma única vez no período reservado.

melhor_funcao <- funcoes_previsao[[melhor_modelo_nome]]

previsao_teste <- melhor_funcao(
  serie_treino,
  n_teste
)


# ------------------------------------------------------------
# 6.1 Métricas no teste final
# ------------------------------------------------------------

resultado_teste <- accuracy(
  previsao_teste,
  serie_teste
)

resultado_teste


# ------------------------------------------------------------
# 6.2 Gráfico do teste final
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
      "6.2 - Previsão do modelo selecionado:",
      melhor_modelo_nome
    ),
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL
  ) +
  theme_minimal()


# ============================================================
# 6. AJUSTE FINAL DO MELHOR MODELO
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
# 7. DIAGNÓSTICO DO MODELO FINAL
# ============================================================

residuos <- residuals(
  modelo_final
)


# ------------------------------------------------------------
# 7.1 Resíduos ao longo do tempo
# ------------------------------------------------------------

autoplot(residuos) +
  labs(
    title = paste(
      "7.1 - Resíduos:",
      melhor_modelo_nome
    ),
    x = "Tempo",
    y = "Resíduo"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 7.2 ACF dos resíduos
# ------------------------------------------------------------

ggAcf(residuos) +
  labs(
    title = "7.2 - ACF dos resíduos",
    x = "Defasagem",
    y = "Autocorrelação"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 7.3 Diagnóstico completo
# ------------------------------------------------------------

checkresiduals(
  modelo_final
)


# ------------------------------------------------------------
# 7.4 Teste de normalidade
# ------------------------------------------------------------

shapiro_resultado <- shapiro.test(
  residuos
)

shapiro_resultado


# ------------------------------------------------------------
# 7.5 PREVISÃO FUTURA
# ------------------------------------------------------------

h_futuro <- 24

previsao_futura <- forecast(
  modelo_final,
  h = h_futuro
)


# ------------------------------------------------------------
# 7.6 Gráfico da previsão
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
      "7.6 - Previsão futura:",
      melhor_modelo_nome
    ),
    x = "Tempo",
    y = "Inadimplência",
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 7.7 Valores previstos
# ------------------------------------------------------------

previsao_futura$mean


# ------------------------------------------------------------
# 7.8 Intervalos de previsão
# ------------------------------------------------------------

previsao_futura
