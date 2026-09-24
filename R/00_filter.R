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

write.csv(
  dados[, c("data", "y")],
  "data/processed/serie_temporal_inadimplencia_filtered.csv",
  row.names = FALSE
)