# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# MODELO ARIMA
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

# library(dplyr)
# library(ggplot2)
# library(forecast)
# library(tseries)

# ============================================================
# 2. SEPARAÇÃO ENTRE TREINO E TESTE
# ============================================================

# n_teste é calculado dinamicamente em main.R.
n_total <- length(ts_serie)
n_treino <- n_total - n_teste

if (n_treino <= 0) {
  stop("A série não possui observações suficientes.")
}

# Mantendo a estrutura de série temporal
serie_treino <- window(
  ts_serie,
  end = time(ts_serie)[n_treino]
)

serie_teste <- window(
  ts_serie,
  start = time(ts_serie)[n_treino + 1]
)

# Gráfico da separação
autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  labs(
    title = "2.1 - Separação entre treino e teste",
    x = "Ano",
    y = "Inadimplência"
  ) +
  theme_minimal()


# ============================================================
# 3. MODELAGEM ARIMA
# ============================================================

# ------------------------------------------------------------
# 3.1 Ajuste dos modelos ARIMA
# ------------------------------------------------------------


# Matriz de ordens: d e estimado pela serie; p/q variam de 0 a 10.
arima_d <- min(forecast::ndiffs(serie_treino), 2L)
ordens_arima <- expand.grid(p = 0:10, q = 0:10)
ordens_arima$nome <- with(
  ordens_arima,
  sprintf("ARIMA(%d,%d,%d)", p, arima_d, q)
)

ajustar_arima <- function(p, q) {
  tryCatch(
    forecast::Arima(
      serie_treino,
      order = c(p, arima_d, q),
      include.drift = FALSE,
      method = "ML"
    ),
    error = function(e) NULL
  )
}

modelos <- setNames(
  lapply(seq_len(nrow(ordens_arima)), function(i) {
    ajustar_arima(ordens_arima$p[i], ordens_arima$q[i])
  }),
  ordens_arima$nome
)

# Remove candidatos que não puderam ser estimados.
modelos <- modelos[!vapply(modelos, is.null, logical(1))]

if (length(modelos) == 0) {
  stop("Nenhum modelo ARIMA da matriz pôde ser ajustado.")
}


#Akaike’s Information Criterion (AIC), which was useful in selecting predictors for regression (see Section 7.5), 
#is also useful for determining the order of an ARIMA model. It can be written as:
#AIC=−2log(L)+2(p+q+k+1),
#AIC=−2log(L)+2(p+q+k+1), where LL is the likelihood of the data, k=1k=1 if c≠0c=0 and k=0k=0 if c=0c=0. Note that the last term in parentheses is the number of parameters in the model (including σ2σ2, the variance of the residuals).

#For ARIMA models, the corrected AIC can be written as
#AICc=AIC+2(p+q+k+1)(p+q+k+2)T−p−q−k−2,
#AICc=AIC+T−p−q−k−22(p+q+k+1)(p+q+k+2),

# ------------------------------------------------------------
# 3.2 Função para calcular as métricas
# ------------------------------------------------------------

avaliar_modelo <- function(modelo, nome) {
  
  # Previsão para o período de teste
  previsao <- forecast(
    modelo,
    h = n_teste
  )
  
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


# ------------------------------------------------------------
# 3.3 Avaliação individual dos modelos
# ------------------------------------------------------------

# Avalia todos os modelos que foram ajustados na matriz.
resultados <- bind_rows(lapply(names(modelos), function(nome) {
  avaliar_modelo(modelos[[nome]], nome)
}))


# ------------------------------------------------------------
# 3.4 Comparação dos modelos
# ------------------------------------------------------------

# Ordenação pelo AICc
resultados_aic <- resultados |>
  arrange(AIC)

print(resultados_aic)

resultados_rmse <- resultados |>
  arrange(RMSE)

print(resultados_rmse)

# Os melhores modelos da avaliacao inicial seguem para a validacao cruzada.
arima_shortlist <- ordens_arima[
  match(head(resultados_rmse$modelo, 5), ordens_arima$nome),
  c("nome", "p", "q")
]
names(arima_shortlist)[names(arima_shortlist) == "nome"] <- "modelo"
arima_shortlist$d <- arima_d

# Reavalia os candidatos em alguns tamanhos de teste antes da validacao cruzada.
arima_resultados_n <- do.call(rbind, lapply(seq_len(nrow(arima_shortlist)), function(i) {
  sp <- arima_shortlist[i, ]
  do.call(rbind, lapply(n_teste_candidatos, function(n_holdout) {
    n_fit <- n_total - n_holdout
    treino_n <- ts_serie[seq_len(n_fit)]
    teste_n <- ts_serie[(n_fit + 1L):n_total]
    ajuste_n <- tryCatch(forecast::Arima(treino_n, order = c(sp$p, sp$d, sp$q),
                                         include.drift = FALSE, method = "ML"),
                         error = function(e) NULL)
    if (is.null(ajuste_n)) return(NULL)
    previsao_n <- forecast::forecast(ajuste_n, h = n_holdout)
    data.frame(modelo = sp$modelo, n_teste = n_holdout,
               RMSE = sqrt(mean((as.numeric(teste_n) - as.numeric(previsao_n$mean))^2)))
  }))
}))
arima_rank_n <- aggregate(RMSE ~ modelo, arima_resultados_n, mean)
arima_rank_n <- arima_rank_n[order(arima_rank_n$RMSE), ]
arima_shortlist <- merge(arima_shortlist, arima_rank_n, by = "modelo")
arima_shortlist <- arima_shortlist[order(arima_shortlist$RMSE), ]
arima_shortlist <- head(arima_shortlist, 3L)
write.csv(arima_resultados_n, "data/processed/arima_analise_por_tamanho_teste.csv", row.names = FALSE)
write.csv(arima_shortlist, "data/processed/arima_shortlist.csv", row.names = FALSE)


# ============================================================
# 4. SELEÇÃO DO MODELO
# ============================================================

# Selecao inicial pelo menor RMSE no periodo de teste.
melhor_nome <- arima_shortlist$modelo[1]

melhor_modelo <- modelos[[melhor_nome]]

cat("Modelo com menor RMSE no teste:", melhor_nome, "\n")

summary(melhor_modelo)
qqnorm(melhor_modelo$residuals)


# ------------------------------------------------------------
# 4.1 Previsões do modelo selecionado
# ------------------------------------------------------------

previsao_teste <- forecast(
  melhor_modelo,
  h = n_teste
)

autoplot(previsao_teste) +
  autolayer(
    serie_teste,
    series = "Valores observados"
  ) +
  labs(
    title = paste(
      "4.1 - Previsões no teste:",
      melhor_nome
    ),
    x = "Ano",
    y = "Inadimplência"
  ) +
  theme_minimal()


# ============================================================
# 5. ANÁLISE DOS RESÍDUOS
# ============================================================

# ------------------------------------------------------------
# 5.1 Gráfico e ACF dos resíduos
# ------------------------------------------------------------

residuos <- residuals(melhor_modelo)

autoplot(residuos) +
  labs(
    title = "5.1 - Resíduos do modelo selecionado",
    x = "Ano",
    y = "Resíduo"
  ) +
  theme_minimal()

acf(
  residuos,
  lag.max = 36,
  main = "5.2 - ACF dos resíduos"
)


# ------------------------------------------------------------
# 5.3 Diagnóstico completo dos resíduos
# ------------------------------------------------------------

checkresiduals(melhor_modelo)


# ------------------------------------------------------------
# 5.4 Teste de normalidade
# ------------------------------------------------------------

#Nenhum arima que eu verifiquei passou no teste de normalidade
shapiro.test(
  as.numeric(na.omit(residuos))
)

# ------------------------------------------------------------
# 5.5 AIC e BIC
# ------------------------------------------------------------
print(resultados_rmse[1,"AIC"])
print(resultados_rmse[1,"BIC"])


# ------------------------------------------------------------
# 6. Utilizando a função auto.arima() do forecast para análise
# ------------------------------------------------------------

modelo_auto <- auto.arima(serie_treino)
summary(modelo_auto)
checkresiduals(modelo_auto)

modelo_auto2 <- auto.arima(serie_treino, d = 2)
summary(modelo_auto2)
checkresiduals(modelo_auto2)
