#limpar ambiente
rm(list = ls())

dados <- read.csv(
  "data/raw/serie_temporal_inadimplencia_total.csv",
  sep = ";",
  header = FALSE,
  skip = 1
)

# Remover última linha 
dados <- dados[-nrow(dados), ]

names(dados) <- c("data", "y")

#converter a coluna de data
dados$data <- as.Date(paste0("01/", dados$data), format = "%d/%m/%Y")


dados$t <- 1:nrow(dados)

head(dados)

#regressao linear simples
modelo <- lm(y ~ t, data = dados)
summary(modelo)

residuos <- residuals(modelo)

autocovariancia <- function(x, h) {
  x <- as.numeric(x)
  x <- x - mean(x)
  
  n <- length(x)
  
  #Se h for maior ou igual ao tamanho da serie, nao faz sentido calcular
  if (h >= n) {
    return(NA)
  }
  
  soma <- sum(x[(h + 1):n] * x[1:(n - h)])
  
  #dividindo por n, que e uma forma simples de estimar autocovariancia
  return(soma / n)
}

#calcular a autocovariancia para alguns valores de h
hs <- 1:12
autocovs <- sapply(hs, function(h) autocovariancia(residuos, h))

#juntar os resultados em uma tabela
resultado_autocov <- data.frame(
  h = hs,
  autocovariancia = autocovs
)

print(resultado_autocov)

# Gráfico da serie original
plot(
  dados$data, dados$y,
  type = "l",
  col = "blue",
  lwd = 2,
  main = "Serie temporal de inadimplencia",
  xlab = "Data",
  ylab = "Percentual"
)

# Gráfico dos residuos
plot(
  dados$data, residuos,
  type = "l",
  col = "darkgreen",
  lwd = 2,
  main = "Residuos da regressao",
  xlab = "Data",
  ylab = "Residuo"
)

# Autocovariancia usando a funcao pronta do R
# type = "covariance" mostra a autocovariancia em vez da autocorrelacao
acf(residuos, type = "covariance", main = "Autocovariancia dos residuos")

# Se quiser ver a autocorrelacao tambem
acf(residuos, main = "ACF dos residuos")

