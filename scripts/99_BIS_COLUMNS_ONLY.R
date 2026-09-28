zipfile <- "data/raw/BIS_WS_CBPOL.zip"

tmp <- tempfile("BIS_")
dir.create(tmp)

unzip(zipfile, exdir=tmp)

f <- file.path(tmp,"WS_CBPOL_csv_col.csv")

cat("\n============================================================\n")
cat(" STRUCTURE REELLE DU FICHIER BIS\n")
cat("============================================================\n\n")

con <- file(f, open="r", encoding="UTF-8")
header <- readLines(con, n=1)
close(con)

cat("EN-TETE BRUT :\n\n")
cat(header, "\n\n")

cat("------------------------------------------------------------\n")
cat("NOMS DES COLONNES\n")
cat("------------------------------------------------------------\n\n")

x <- read.csv(
  f,
  nrows=5,
  stringsAsFactors=FALSE,
  check.names=FALSE
)

print(names(x))

cat("\n------------------------------------------------------------\n")
cat("PREMIERES 5 LIGNES\n")
cat("------------------------------------------------------------\n\n")

print(x)

cat("\n============================================================\n")
cat("FIN\n")
cat("============================================================\n")
