# ============================================================
# SÉRIE TEMPORAL - INADIMPLÊNCIA NO BRASIL
# MODELO ARIMA
# ============================================================

# ------------------------------------------------------------
# 0. Preparação
# ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

serie_inadp <- read.csv(
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  sep = ",",
  header = TRUE
)

# Criando a série temporal mensal
ts_serie <- ts(
  serie_inadp$y,
  start = c(2011, 3),
  frequency = 12
)

# ============================================================
# 2. SEPARAÇÃO ENTRE TREINO E TESTE
# ============================================================

n_teste <- 32
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
# 3. MODELAGEM ARIMA COM d = 2
# ============================================================

# ------------------------------------------------------------
# 3.1 Ajuste dos modelos ARIMA
# ------------------------------------------------------------


# Matriz de ordens: d permanece fixo em 2 e p/q variam de 0 a 3.
ordens_arima <- expand.grid(p = 0:10, q = 0:10)
ordens_arima$nome <- with(
  ordens_arima,
  sprintf("ARIMA(%d,2,%d)", p, q)
)

ajustar_arima <- function(p, q) {
  tryCatch(
    forecast::Arima(
      serie_treino,
      order = c(p, 2, q),
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


# ============================================================
# 4. SELEÇÃO DO MODELO
# ============================================================

# Considerei que o melhor modelo foi o ARIMA(1,2,2) -> 2º menor RMSE, Simples e AIC/BIC pequenos
melhor_nome <- "ARIMA(1,2,2)"

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
