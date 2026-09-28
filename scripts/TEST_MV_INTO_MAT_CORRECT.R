library(rumidas)
library(readr)
library(dplyr)
library(lubridate)
library(xts)

ROOT <- normalizePath(".", winslash="/", mustWork=TRUE)

cat("\n============================================================\n")
cat(" TEST mv_into_mat - PERIODE CORRECTE\n")
cat("============================================================\n")

model <- readRDS(
  file.path(
    ROOT,
    "results", "models", "garch_midas",
    "DEU_GPRA_SUP.rds"
  )
)

daily <- read_csv(
  file.path(ROOT, "data", "processed", "daily_clean.csv"),
  show_col_types=FALSE
)

monthly <- read_csv(
  file.path(ROOT, "data", "processed", "monthly_model_data.csv"),
  show_col_types=FALSE
)

K <- 3

period_start <- model$period[1]
period_end   <- model$period[2]

cat("\nMODEL PERIOD:\n")
print(model$period)

# ============================================================
# DAILY : exactement la période du modèle
# ============================================================

daily_deu <- daily %>%
  filter(
    country == "DEU",
    date >= period_start,
    date <= period_end
  ) %>%
  arrange(date)

r <- xts(
  daily_deu$return,
  order.by=daily_deu$date
)

# ============================================================
# MONTHLY :
# K+1 mois AVANT le premier rendement
# Juin 2002 -> Février 2002
# ============================================================

mv_start <- floor_date(
  period_start,
  unit="month"
) %m-% months(K + 1)

cat("\nMV START:\n")
print(mv_start)

monthly_deu <- monthly %>%
  filter(
    country == "DEU",
    date >= mv_start,
    date <= floor_date(period_end, unit="month")
  ) %>%
  arrange(date)

cat("\nMONTHLY RANGE:\n")
cat("START =", as.character(min(monthly_deu$date)), "\n")
cat("END   =", as.character(max(monthly_deu$date)), "\n")
cat("N     =", nrow(monthly_deu), "\n")

z <- xts(
  monthly_deu$GPRA,
  order.by=monthly_deu$date
)

cat("\nR INDEX START:\n")
print(head(index(r)))

cat("\nZ INDEX START:\n")
print(head(index(z)))

cat("\nR N =", nrow(r), "\n")
cat("Z N =", nrow(z), "\n")

# ============================================================
# mv_into_mat
# ============================================================

cat("\n============================================================\n")
cat(" EXECUTION mv_into_mat\n")
cat("============================================================\n")

test_mv <- tryCatch(
  {
    mv_into_mat(
      r,
      z,
      K=K,
      type="monthly"
    )
  },
  error=function(e) {
    cat("\nERREUR:\n")
    cat(conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(test_mv)) {

  cat("\n****************************************************\n")
  cat(" SUCCESS : mv_into_mat FONCTIONNE\n")
  cat("****************************************************\n")

  cat("\nCLASS:\n")
  print(class(test_mv))

  cat("\nDIM:\n")
  print(dim(test_mv))

  cat("\nHEAD:\n")
  print(head(test_mv))

  cat("\nTAIL:\n")
  print(tail(test_mv))
}

cat("\n============================================================\n")
cat(" FIN TEST\n")
cat("============================================================\n")