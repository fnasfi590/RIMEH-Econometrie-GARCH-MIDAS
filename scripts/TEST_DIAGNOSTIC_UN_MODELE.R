library(rumidas)
library(readr)
library(dplyr)
library(lubridate)

ROOT <- normalizePath(".", winslash="/", mustWork=TRUE)

cat("\n============================================================\n")
cat(" TEST DIAGNOSTIC UN MODELE - DEU_GPRA_SUP\n")
cat("============================================================\n")

model_path <- file.path(
  ROOT,
  "results", "models", "garch_midas",
  "DEU_GPRA_SUP.rds"
)

model <- readRDS(model_path)

cat("\nMODEL PERIOD:\n")
print(model$period)
cat("CLASS:", class(model$period), "\n")
cat("TYPE :", typeof(model$period), "\n")

daily <- read_csv(
  file.path(ROOT, "data", "processed", "daily_clean.csv"),
  show_col_types=FALSE
)

monthly <- read_csv(
  file.path(ROOT, "data", "processed", "monthly_model_data.csv"),
  show_col_types=FALSE
)

cat("\nDAILY DATE:\n")
cat("CLASS:", class(daily$date), "\n")
cat("TYPE :", typeof(daily$date), "\n")
print(head(daily$date, 5))
print(tail(daily$date, 5))

cat("\nMONTHLY DATE:\n")
cat("CLASS:", class(monthly$date), "\n")
cat("TYPE :", typeof(monthly$date), "\n")
print(head(monthly$date, 5))
print(tail(monthly$date, 5))

period_start <- model$period[1]
period_end   <- model$period[2]

cat("\nPERIOD START:\n")
print(period_start)

cat("\nPERIOD END:\n")
print(period_end)

cat("\n============================================================\n")
cat(" TEST FILTRAGE DAILY\n")
cat("============================================================\n")

daily_deu <- daily %>%
  filter(
    country == "DEU",
    date >= period_start,
    date <= period_end
  ) %>%
  arrange(date)

cat("N DAILY =", nrow(daily_deu), "\n")

if (nrow(daily_deu) > 0) {
  cat("START =", as.character(min(daily_deu$date)), "\n")
  cat("END   =", as.character(max(daily_deu$date)), "\n")
}

cat("\n============================================================\n")
cat(" TEST FILTRAGE MONTHLY\n")
cat("============================================================\n")

monthly_deu <- monthly %>%
  filter(
    country == "DEU",
    date >= floor_date(period_start, unit="month"),
    date <= floor_date(period_end, unit="month")
  ) %>%
  arrange(date)

cat("N MONTHLY =", nrow(monthly_deu), "\n")

if (nrow(monthly_deu) > 0) {
  cat("START =", as.character(min(monthly_deu$date)), "\n")
  cat("END   =", as.character(max(monthly_deu$date)), "\n")
}

cat("\n============================================================\n")
cat(" VARIABLES\n")
cat("============================================================\n")

print(names(monthly_deu))

vars <- c("GPRT","GPRA","SUP","DEM","PRE","VIX")

for (v in vars) {
  if (v %in% names(monthly_deu)) {
    x <- monthly_deu[[v]]
    cat(
      v,
      ": N =", length(x),
      "| finite =", sum(is.finite(x)),
      "| NA =", sum(is.na(x)),
      "\n"
    )
  }
}

cat("\n============================================================\n")
cat(" TEST RETURN\n")
cat("============================================================\n")

r <- daily_deu$return

cat("N =", length(r), "\n")
cat("finite =", sum(is.finite(r)), "\n")
cat("min =", min(r, na.rm=TRUE), "\n")
cat("max =", max(r, na.rm=TRUE), "\n")

cat("\n============================================================\n")
cat(" TEST mv_into_mat\n")
cat("============================================================\n")

z <- monthly_deu$GPRA

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
    cat("ERREUR mv_into_mat:\n")
    cat(conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(test_mv)) {
  cat("mv_into_mat SUCCESS\n")
  cat("CLASS:\n")
  print(class(test_mv))
  cat("DIM:\n")
  print(dim(test_mv))
  cat("HEAD:\n")
  print(head(test_mv))
}

cat("\n============================================================\n")
cat(" TEST seq.Date\n")
cat("============================================================\n")

test_seq <- tryCatch(
  {
    seq.Date(
      period_start,
      period_end,
      by="month"
    )
  },
  error=function(e) {
    cat("ERREUR seq.Date:\n")
    cat(conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(test_seq)) {
  cat("seq.Date SUCCESS\n")
  cat("N =", length(test_seq), "\n")
  print(head(test_seq))
  print(tail(test_seq))
}

cat("\n============================================================\n")
cat(" FIN TEST\n")
cat("============================================================\n")