# Análise genérica de séries temporais

## Fluxo

- `main.R` recebe uma série tratada, cria o objeto `ts` e calcula os tamanhos de treino, teste e validação cruzada a partir do comprimento disponível.
- `R/01_series_analysis.R` faz a avaliação exploratória.
- `R/02_regression_model_analysis.R` avalia uma tendência linear.
- `R/03_ETS_analysis.R` e `R/04_ETS_cross_validation_analysis.R` avaliam ETS.
- `R/05_ARIMA_analysis.R` e `R/07_SARIMA_analysis.R` comparam modelos em alguns tamanhos de teste; suas shortlists alimentam as validações `R/06_ARIMA_cross_validation.R` e `R/08_SARIMA_cross_validation.R`.
- `scripts/prepare_series.R` prepara o CSV bruto deste projeto. A pasta `scripts/` é destinada a scripts de preparação, não às análises genéricas.

## Usar outra série tratada

Edite no início de `main.R`:

```r
input_file <- "data/processed/minha_serie.csv"
value_column <- "y"
series_start <- c(2011, 3)
series_frequency <- 12
```

O CSV precisa ter cabeçalho e uma coluna numérica com observações ordenadas no tempo. As datas devem estar igualmente espaçadas; a indexação do objeto `ts` vem de `series_start` e `series_frequency`.

Para reproduzir a preparação da série bruta fornecida, execute `Rscript scripts/prepare_series.R` na raiz. Em seguida execute `Rscript main.R`. Os resumos de busca e validação são gravados em `data/processed/`.

## Dependências

Pacotes R: `forecast`, `ggplot2` e `tseries`.
