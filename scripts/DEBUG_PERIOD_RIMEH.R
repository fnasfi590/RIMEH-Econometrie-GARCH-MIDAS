library(rumidas)
library(readr)

ROOT <- normalizePath(".", winslash="/", mustWork=TRUE)

rds <- readRDS(
  file.path(
    ROOT,
    "results",
    "models",
    "garch_midas",
    "DEU_GPRA_SUP.rds"
  )
)

cat("\n============================================================\n")
cat(" DEBUG PERIOD / DATES - DEU_GPRA_SUP\n")
cat("============================================================\n\n")

cat("CLASS model$period:\n")
print(class(rds$period))

cat("\nTYPE model$period:\n")
print(typeof(rds$period))

cat("\nLENGTH model$period:\n")
print(length(rds$period))

cat("\nmodel$period COMPLET:\n")
print(rds$period)

cat("\nSTRUCTURE model$period:\n")
str(rds$period)

cat("\n------------------------------------------------------------\n")
cat("DAILY DATA\n")
cat("------------------------------------------------------------\n")

daily <- read_csv(
  file.path(ROOT,"data","processed","daily_clean.csv"),
  show_col_types=FALSE
)

print(names(daily))
cat("\nCLASS daily$date:\n")
print(class(daily$date))

cat("\nTYPE daily$date:\n")
print(typeof(daily$date))

cat("\nPREMIERES DATES:\n")
print(head(daily$date,10))

cat("\nDERNIERES DATES:\n")
print(tail(daily$date,10))

cat("\n------------------------------------------------------------\n")
cat("MONTHLY DATA\n")
cat("------------------------------------------------------------\n")

monthly <- read_csv(
  file.path(ROOT,"data","processed","monthly_model_data.csv"),
  show_col_types=FALSE
)

cat("CLASS monthly$date:\n")
print(class(monthly$date))

cat("\nTYPE monthly$date:\n")
print(typeof(monthly$date))

cat("\nPREMIERES DATES:\n")
print(head(monthly$date,10))

cat("\nDERNIERES DATES:\n")
print(tail(monthly$date,10))

cat("\n------------------------------------------------------------\n")
cat("CONVERSION NUMERIQUE POSSIBLE\n")
cat("------------------------------------------------------------\n")

if (is.numeric(rds$period)) {
  cat("period numeric -> test as.Date(origin=1970-01-01)\n")
  print(as.Date(rds$period, origin="1970-01-01"))
  
  cat("\nperiod numeric -> test as.Date(origin=1970-01-01) + structure\n")
  print(
    data.frame(
      value=rds$period,
      date=as.Date(rds$period, origin="1970-01-01")
    )
  )
}

cat("\n============================================================\n")
cat("FIN DEBUG\n")
cat("============================================================\n")