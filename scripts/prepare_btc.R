input_file <- "data/raw/btc.csv"
output_file <- "data/processed/btc_filtered.csv"

dados <- read.csv(input_file, sep = ",", header = TRUE)

dados <- data.frame(
  date = as.Date(dados$Date),
  y = dados$Close
)

dados <- dados[complete.cases(dados), ]
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(dados, output_file, row.names = FALSE)
message("Série tratada salva em ", output_file)