# Ponto de entrada: informe o CSV tratado e as colunas de data e valor.
# A frequencia e inferida pelos intervalos das datas; os tamanhos variam com a serie.
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)

input_file <- "data/processed/serie_temporal_inadimplencia_filtered.csv"
value_column <- "y"
series_start <- c(2011, 3) # c(ano, período dentro do ano)
series_frequency <- 12
series_name <- "Inadimplência"
series_value_label <- "Valor"
n_modelos_pre_cv <- 12L
n_modelos_cv <- 8L
nonseasonal_differencing <- 2
cv_horizon <- 24
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

ts_serie <- ts(as.numeric(serie_dados[[value_column]]), start = series_start,
               frequency = series_frequency)
serie_inadp <- serie_dados
serie_inadp$y <- as.numeric(serie_dados[[value_column]])
n_serie <- length(ts_serie)
if (n_serie < max(8, 2 * series_frequency)) {
  stop("Série curta demais para as análises: ", n_serie, " observações.")
}

# Reserva 15% para teste final, com limites compatíveis com séries curtas.
n_teste <- min(max(1L, floor(n_serie * 0.19)), max(1L, n_serie - 2L * series_frequency))
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
