# Prepara a série bruta do Banco Central para uso em main.R.
input_file <- "data/raw/serie_temporal_inadimplencia_total.csv"
output_file <- "data/processed/serie_temporal_inadimplencia_filtered.csv"

dados <- read.csv(input_file, sep = ";", header = FALSE, skip = 1,
                  stringsAsFactors = FALSE)
if (ncol(dados) < 2) stop("O arquivo bruto precisa conter data e valor.")
dados <- dados[, 1:2]
names(dados) <- c("data", "y")
dados$date <- as.Date(paste0("01/", dados$date), format = "%d/%m/%Y")
dados$y <- as.numeric(gsub(",", ".", dados$y, fixed = TRUE))
dados <- dados[complete.cases(dados), ]
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(dados, output_file, row.names = FALSE)
message("Série tratada salva em ", output_file)
