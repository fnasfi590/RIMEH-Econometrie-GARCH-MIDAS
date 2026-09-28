options(stringsAsFactors=FALSE)

library(data.table)

cat("\n============================================\n")
cat(" RIMEH - DIAGNOSTIC DES DATES\n")
cat("============================================\n\n")

monthly <- fread(
  "data/processed/monthly_model_data_FINAL.csv",
  data.table=FALSE
)

daily <- fread(
  "data/processed/daily_clean.csv",
  data.table=FALSE
)

cat("MONTHLY\n")
cat("-------\n")
cat("Rows :",nrow(monthly),"\n")
cat("Date class :",class(monthly$date),"\n")
cat("Date type  :",typeof(monthly$date),"\n")
cat("Date length:",length(monthly$date),"\n\n")

cat("Premieres valeurs monthly$date:\n")
print(head(monthly$date,10))

cat("\nStructure monthly$date:\n")
str(monthly$date)

cat("\n--------------------------------------------\n")

cat("DAILY\n")
cat("-----\n")
cat("Rows :",nrow(daily),"\n")
cat("Date class :",class(daily$date),"\n")
cat("Date type  :",typeof(daily$date),"\n")
cat("Date length:",length(daily$date),"\n\n")

cat("Premieres valeurs daily$date:\n")
print(head(daily$date,10))

cat("\nStructure daily$date:\n")
str(daily$date)

cat("\n============================================\n")
cat(" LONGUEURS DES VALEURS DATE\n")
cat("============================================\n\n")

ml <- nchar(as.character(monthly$date))
dl <- nchar(as.character(daily$date))

cat("MONTHLY max length :",max(ml,na.rm=TRUE),"\n")
cat("DAILY   max length :",max(dl,na.rm=TRUE),"\n")

cat("\nMONTHLY valeurs anormalement longues (>30 caracteres):\n")
badm <- unique(as.character(monthly$date)[ml>30])
print(head(badm,20))

cat("\nDAILY valeurs anormalement longues (>30 caracteres):\n")
badd <- unique(as.character(daily$date)[dl>30])
print(head(badd,20))

cat("\n============================================\n")
cat(" FIN DIAGNOSTIC\n")
cat("============================================\n")