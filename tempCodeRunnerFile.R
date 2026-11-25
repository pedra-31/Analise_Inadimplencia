# Ponto de entrada: informe o CSV tratado e as colunas de data e valor.
# A frequencia e inferida pelos intervalos das datas; os tamanhos variam com a serie.
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)

input_file <- "data/processed/serie_temporal_inadimplencia_filtered.csv"
value_column <- "y"
date_column <- "data"
series_name <- "Inadimplência"
series_value_label <- "Valor"
nonseasonal_differencing <- 2
cv_horizon <- 24
n_modelos_pre_cv <- 12L
n_modelos_cv <- 8L
series_file_prefix <- tolower(gsub(
  "_+$", "",
  gsub("^_+", "", gsub("[^[:alnum:]]+", "_",
                       iconv(series_name, to = "ASCII//TRANSLIT")))
))
series_output_path <- function(file_name) {
  file.path("data/processed", paste0(series_file_prefix, "_", file_name))
}

if (!file.exists(input_file)) {
  stop("Arquivo tratado não encontrado: ", input_file,
       ". Execute scripts/prepare_series.R ou ajuste input_file.")
}

serie_dados <- read.csv(input_file, stringsAsFactors = FALSE)
if (!value_column %in% names(serie_dados)) stop("Coluna não encontrada: ", value_column)
if (anyNA(serie_dados[[value_column]])) stop("A série contém valores ausentes.")

# Infere a frequência a partir do intervalo das datas. Se não houver uma
# coluna de datas utilizável, busca a periodicidade dominante dos valores.
if (date_column %in% names(serie_dados)) {
  datas_serie <- as.Date(serie_dados[[date_column]])
  if (anyNA(datas_serie)) stop("A coluna de datas contém valores inválidos: ", date_column)

  indice_mes <- as.integer(format(datas_serie, "%Y")) * 12L +
    as.integer(format(datas_serie, "%m"))
  diferencas_mes <- diff(indice_mes)
  diferencas_dia <- as.integer(diff(datas_serie))

  gcd <- function(a, b) if (b == 0L) abs(a) else Recall(b, a %% b)
  passo_mes <- if (length(diferencas_mes) && all(diferencas_mes > 0L)) {
    Reduce(gcd, diferencas_mes)
  } else 0L

  if (passo_mes > 0L && 12L %% passo_mes == 0L) {
    series_frequency <- 12L %/% passo_mes
  } else if (length(diferencas_dia) && all(diferencas_dia > 0L) &&
             all(diferencas_dia %% 7L == 0L)) {
    series_frequency <- 52L
  } else if (length(diferencas_dia) && all(diferencas_dia == 1L)) {
    series_frequency <- 7L
  } else {
    series_frequency <- forecast::findfrequency(as.numeric(serie_dados[[value_column]]))
  }

  ano_inicio <- as.integer(format(datas_serie[1], "%Y"))
  mes_inicio <- as.integer(format(datas_serie[1], "%m"))
  if (series_frequency %in% c(1L, 2L, 4L, 12L)) {
    series_start <- c(ano_inicio, ceiling(mes_inicio * series_frequency / 12))
  } else if (series_frequency == 52L) {
    series_start <- c(ano_inicio, as.integer(format(datas_serie[1], "%U")) + 1L)
  } else {
    series_start <- c(ano_inicio, as.integer(format(datas_serie[1], "%u")))
  }
} else {
  series_frequency <- forecast::findfrequency(as.numeric(serie_dados[[value_column]]))
  series_start <- c(1, 1)
}

message("Frequência inferida: ", series_frequency, " observações por ciclo.")
ts_serie <- ts(as.numeric(serie_dados[[value_column]]), start = series_start,
               frequency = series_frequency)
serie_inadp <- serie_dados
serie_inadp$y <- as.numeric(serie_dados[[value_column]])
n_serie <- length(ts_serie)
if (n_serie < max(8, 2 * series_frequency)) {
  stop("Série curta demais para as análises: ", n_serie, " observações.")
}

# Reserva 15% para teste final, com limites compatíveis com séries curtas.
n_teste <- min(max(1L, floor(n_serie * 0.15)), max(1L, n_serie - 2L * series_frequency))
n_treino <- n_serie - n_teste

#teste_candidatos define a porcentagem reservada para validação em cada corte interno. 
#Então, quanto menor a porcentagem de teste, mais dados sobram para treinar:
#- 20% para teste → 80% para treino
#- 50% para teste → 50% para treino
#- 80% para teste → 20% para treino

n_teste_candidatos <- sort(unique(pmax(1L, floor(n_treino * c(0.10, 0.20, 0.30)))))
n_teste_candidatos <- n_teste_candidatos[
  n_treino - n_teste_candidatos >= max(8L, 2L * series_frequency)
]
h_cv <- min(cv_horizon, n_treino)
initial_cv <- min(max(2L * series_frequency, floor(n_treino * 0.60)), n_treino - h_cv)
initial <- initial_cv
n_cv_origins <- min(8L, max(1L, n_treino - initial_cv - h_cv + 1L))
h_futuro <- 2L * series_frequency
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

scripts <- c("01_series_analysis.R", "02_regression_model_analysis.R",
             "03_ETS_analysis.R", "04_ETS_cross_validation_analysis.R",
             "05_ARIMA_analysis.R", "06_ARIMA_cross_validation.R",
             "07_SARIMA_analysis.R", "08_SARIMA_cross_validation.R")
for (script in scripts) {
  message("Executando R/", script)
  source(file.path("R", script), local = .GlobalEnv, print.eval = TRUE)
}
