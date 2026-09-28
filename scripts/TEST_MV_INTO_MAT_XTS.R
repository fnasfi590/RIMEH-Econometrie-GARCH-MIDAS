library(rumidas)
library(readr)
library(dplyr)
library(lubridate)
library(xts)

ROOT <- normalizePath(".", winslash="/", mustWork=TRUE)

cat("\n============================================================\n")
cat(" TEST mv_into_mat AVEC XTS\n")
cat("============================================================\n")

model <- readRDS(
  file.path(
    ROOT,
    "results",
    "models",
    "garch_midas",
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

period_start <- model$period[1]
period_end   <- model$period[2]

# ------------------------------------------------------------
# DAILY
# ------------------------------------------------------------

daily_deu <- daily %>%
  filter(
    country == "DEU",
    date >= period_start,
    date <= period_end
  ) %>%
  arrange(date)

# ------------------------------------------------------------
# MONTHLY
# ------------------------------------------------------------

monthly_deu <- monthly %>%
  filter(
    country == "DEU",
    date >= floor_date(period_start, "month"),
    date <= floor_date(period_end, "month")
  ) %>%
  arrange(date)

# ------------------------------------------------------------
# CREATION XTS
# ------------------------------------------------------------

r <- xts(
  daily_deu$return,
  order.by=daily_deu$date
)

z <- xts(
  monthly_deu$GPRA,
  order.by=monthly_deu$date
)

cat("\nCLASS r:\n")
print(class(r))

cat("\nINDEX r:\n")
print(head(index(r)))
print(tail(index(r)))

cat("\nCLASS z:\n")
print(class(z))

cat("\nINDEX z:\n")
print(head(index(z)))
print(tail(index(z)))

cat("\nDIM r:\n")
print(dim(r))

cat("\nDIM z:\n")
print(dim(z))

# ------------------------------------------------------------
# TEST mv_into_mat
# ------------------------------------------------------------

cat("\n============================================================\n")
cat(" EXECUTION mv_into_mat\n")
cat("============================================================\n")

test_mv <- tryCatch(
  {
    mv_into_mat(
      r,
      z,
      K=3,
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
  cat(" SUCCESS : mv_into_mat FONCTIONNE AVEC XTS\n")
  cat("****************************************************\n")

  cat("\nCLASS RESULT:\n")
  print(class(test_mv))

  cat("\nDIM RESULT:\n")
  print(dim(test_mv))

  cat("\nHEAD RESULT:\n")
  print(head(test_mv))

  cat("\nTAIL RESULT:\n")
  print(tail(test_mv))
}

cat("\n============================================================\n")
cat(" FIN TEST\n")
cat("============================================================\n")