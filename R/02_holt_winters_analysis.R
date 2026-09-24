#limpar ambiente
rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(forecast)
library(ggplot2)

serie_inadp <- read.csv(
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  sep = ",",
  header = TRUE
)

ts_serie <- ts(
  serie_inadp$y,
  start = c(2011, 3),
  frequency = 12
)

## Separando as ultimas 32 observacoes para avaliar as previsoes
n_teste <- 32
n <- length(ts_serie)

serie_treino <- window(
  ts_serie,
  end = time(ts_serie)[n - n_teste]
)

serie_teste <- window(
  ts_serie,
  start = time(ts_serie)[n - n_teste + 1]
)

#### Modelos de suavização exponencial ####

### Suavização exponencial simples ####

# Nivel local: ETS(A,N,N)
modelo_ANN <- ets(ts_serie, model = "ANN", damped = FALSE)
summary(modelo_ANN)
modelo_ANN$par

### 3.2 Método de Holt ####
# Tendencia aditiva: ETS(A,A,N)
modelo_AAN <- ets(ts_serie, model = "AAN", damped = FALSE)
summary(modelo_AAN)
modelo_AAN$par
modelo_AAN$states[1:3,]

alpha <- modelo_AAN$par[1]
beta <- modelo_AAN$par[2]/alpha
nivel_0 <- modelo_AAN$par[3]
tend_0 <- modelo_AAN$par[4]

nivel_1 <- alpha*ts_serie[1] + (1-alpha)*(nivel_0+tend_0)
tend_1 <- beta*(nivel_1-nivel_0) + (1-beta)*tend_0
nivel_1
tend_1

modelo_AAN$states[1:3,]

# Testando o modelo holt para prever por 1 ano

future_AAN <- forecast(modelo_AAN, n = 24)

autoplot(ts_serie, series = "Treino") +
  autolayer(future_AAN, series = "Holt") +
  labs(
    x = "Tempo",
    y = "Consumo de Energia",
    colour = NULL,
    fill = NULL
  ) +
  theme_minimal()

# Observado e valores ajustados

autoplot(ts_serie, series = "Observado") +
  autolayer(
    fitted(modelo_AAN),
    series = "ETS(A,A,N)"
  ) +
  labs(
    x = "Tempo",
    y = "Consumo Energia",
    colour = ""
  ) +
  theme_minimal()

checkresiduals(modelo_AAN)


### 3.3 Método de Holt-Winters ####

# Tendencia e sazonalidade aditivas: ETS(A,A,A)
modelo_AAA <- ets(ts_serie, model = "AAA", damped = FALSE)
summary(modelo_AAA)

# Parametros de suavizacao
modelo_AAA$par

# Estados estimados: nivel, tendencia e componentes sazonais
modelo_AAA$states

alpha <- modelo_AAA$par[1]
beta <- modelo_AAA$par[2]/alpha
gamma <- modelo_AAA$par[3]/(1-alpha)
nivel_0 <- modelo_AAA$par[4]
tend_0 <- modelo_AAA$par[5]
saz_ini <- modelo_AAA$states[1,14]

nivel_1 <- alpha*(ts_serie[1]-saz_ini) + (1-alpha)*(nivel_0+tend_0)
tend_1 <- beta*(nivel_1-nivel_0) + (1-beta)*tend_0
saz_1 <- gamma*(ts_serie[1]-nivel_1) + (1-gamma)*saz_ini

nivel_1
tend_1
saz_1

modelo_AAA$states[1:3,1:3]

# Ajuste do modelo sazonal

autoplot(ts_serie, series = "Observado") +
  autolayer(
    fitted(modelo_AAA),
    series = "Holt-Winters"
  ) +
  labs(
    x = "Tempo",
    y = "Consumo Energia",
    colour = ""
  ) +
  theme_minimal()

# Diagnostico dos residuos
checkresiduals(modelo_AAA)

# Comparação entre modelos de suavização pelo critério de informação
modelo_ANN$aicc
modelo_AAN$aicc
modelo_AAA$aicc

### 3.4 Análise de previsões com modelos de suavização ####

## Ajustando os modelos somente na amostra de treino
modelo_ANN_treino <- ets(
  serie_treino,
  model = "ANN",
  damped = FALSE
)

modelo_AAN_treino <- ets(
  serie_treino,
  model = "AAN",
  damped = FALSE
)

modelo_AAA_treino <- ets(
  serie_treino,
  model = "AAA",
  damped = FALSE
)

## Previsoes para a base de teste
prev_ANN <- forecast(modelo_ANN_treino, h = n_teste)
prev_AAN <- forecast(modelo_AAN_treino, h = n_teste)
prev_AAA <- forecast(modelo_AAA_treino, h = n_teste)


## Comparacao das previsoes na base de teste
autoplot(serie_treino, series = "Treino") +
  autolayer(serie_teste, series = "Teste") +
  autolayer(prev_ANN$mean, series = "Suavizacao simples") +
  autolayer(prev_AAN$mean, series = "Holt") +
  autolayer(prev_AAA$mean, series = "Holt-Winters") +
  labs(
    x = "Tempo",
    y = "Consumo Energia",
    colour = ""
  ) +
  theme_minimal()

## Medidas na base de teste
accuracy(prev_ANN, serie_teste)
accuracy(prev_AAN, serie_teste)
accuracy(prev_AAA, serie_teste)
