library(readxl)

f <- "data/raw/gpr_official_current.xls"

x <- read_excel(
  f,
  sheet = 1,
  .name_repair = "minimal"
)

cat("\n--- STRUCTURE ---\n")
str(x[, c("month","GPR","GPRT","GPRA")])

cat("\n--- PREMIERES LIGNES ---\n")
print(head(x[, c("month","GPR","GPRT","GPRA")], 20))

cat("\n--- VALEURS GPRT ---\n")
print(unique(x$GPRT)[1:30])

cat("\n--- VALEURS GPRA ---\n")
print(unique(x$GPRA)[1:30])

cat("\n--- CLASSES ---\n")
print(sapply(x[, c("month","GPR","GPRT","GPRA")], class))

cat("\n--- NOMBRE UNIQUE ---\n")
print(sapply(x[, c("GPR","GPRT","GPRA")], function(z)
    length(unique(z[!is.na(z)]))
))
