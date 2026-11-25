# Prepara a série bruta do Banco Central para uso em main.R.
input_file <- "data/raw/btc.csv"
output_file <- "data/processed/btc_filtered.csv"

dados <- read.csv(input_file, sep = ",", header = TRUE)
dados$data <- as.Date(paste0("01/", dados$data), format = "%d/%m/%Y")
dados$y <- as.numeric(gsub(",", ".", dados$y, fixed = TRUE))
dados <- dados[complete.cases(dados), ]
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(dados, output_file, row.names = FALSE)
message("Série tratada salva em ", output_file)
