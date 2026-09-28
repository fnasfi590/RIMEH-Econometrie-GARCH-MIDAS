P <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

library(readr)
library(dplyr)

cat("\n============================================================\n")
cat(" RIMEH - AUDIT DAILY_MODEL_DATA - DATE FIX\n")
cat("============================================================\n")

F <- file.path(P, "data", "processed", "daily_model_data.csv")

if (!file.exists(F)) {
  stop("Fichier daily_model_data.csv introuvable")
}

df <- read_csv(F, show_col_types = FALSE)

cat("Dimensions :", nrow(df), "x", ncol(df), "\n")

# ------------------------------------------------------------
# DATE : conversion robuste
# ------------------------------------------------------------

date_raw <- df[[2]]

cat("\nExemples bruts de date :\n")
print(head(date_raw, 10))

# Plusieurs formats possibles
date_parsed <- suppressWarnings(as.Date(date_raw))

if (all(is.na(date_parsed))) {
  date_parsed <- suppressWarnings(
    as.Date(date_raw, format = "%Y-%m-%d")
  )
}

if (all(is.na(date_parsed))) {
  date_parsed <- suppressWarnings(
    as.Date(date_raw, format = "%Y/%m/%d")
  )
}

if (all(is.na(date_parsed))) {
  date_parsed <- suppressWarnings(
    as.Date(date_raw, format = "%d/%m/%Y")
  )
}

if (all(is.na(date_parsed))) {
  # Tentative avec POSIXct
  tmp <- suppressWarnings(as.POSIXct(
    date_raw,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ))
  date_parsed <- as.Date(tmp)
}

cat("\nDates valides :", sum(!is.na(date_parsed)), "/", length(date_parsed), "\n")
cat("Dates NA      :", sum(is.na(date_parsed)), "\n")

if (sum(!is.na(date_parsed)) > 0) {
  cat("Date min :", as.character(min(date_parsed, na.rm = TRUE)), "\n")
  cat("Date max :", as.character(max(date_parsed, na.rm = TRUE)), "\n")
}

# ------------------------------------------------------------
# DOUBLONS COUNTRY + DATE
# ------------------------------------------------------------

country <- df[[1]]

tmp <- data.frame(
  country = country,
  date = date_parsed,
  stringsAsFactors = FALSE
)

dup <- tmp %>%
  filter(!is.na(date)) %>%
  count(country, date, name = "N") %>%
  filter(N > 1)

cat("\n============================================================\n")
cat(" DOUBLONS COUNTRY + DATE\n")
cat("============================================================\n")

cat("Nombre de couples dupliques :", nrow(dup), "\n")

if (nrow(dup) > 0) {
  print(head(dup, 30))
}

# ------------------------------------------------------------
# COUVERTURE DAILY PAR PAYS
# ------------------------------------------------------------

coverage <- tmp %>%
  filter(!is.na(date)) %>%
  group_by(country) %>%
  summarise(
    Date_min = min(date),
    Date_max = max(date),
    N_days = n(),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat(" COUVERTURE DAILY PAR PAYS\n")
cat("============================================================\n")

print(coverage)

# ------------------------------------------------------------
# RETOURS
# ------------------------------------------------------------

ret <- suppressWarnings(as.numeric(df[[5]]))

cat("\n============================================================\n")
cat(" AUDIT RETURN\n")
cat("============================================================\n")

cat("N total :", length(ret), "\n")
cat("NA      :", sum(is.na(ret)), "\n")
cat("Inf     :", sum(is.infinite(ret)), "\n")
cat("Finite  :", sum(is.finite(ret)), "\n")

if (sum(is.finite(ret)) > 0) {

  r <- ret[is.finite(ret)]

  cat("Min     :", min(r), "\n")
  cat("Median  :", median(r), "\n")
  cat("Mean    :", mean(r), "\n")
  cat("Max     :", max(r), "\n")
  cat("SD      :", sd(r), "\n")

  q <- quantile(
    r,
    probs = c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1),
    na.rm = TRUE
  )

  print(q)
}

# ------------------------------------------------------------
# N_OBS DAILY
# ------------------------------------------------------------

nobs <- suppressWarnings(as.numeric(df[[11]]))

cat("\n============================================================\n")
cat(" AUDIT N_OBS\n")
cat("============================================================\n")

cat("NA     :", sum(is.na(nobs)), "\n")
cat("Inf    :", sum(is.infinite(nobs)), "\n")
cat("Min    :", min(nobs, na.rm = TRUE), "\n")
cat("Median :", median(nobs, na.rm = TRUE), "\n")
cat("Mean   :", mean(nobs, na.rm = TRUE), "\n")
cat("Max    :", max(nobs, na.rm = TRUE), "\n")

# ------------------------------------------------------------
# DAILY PAR PAYS\n# ------------------------------------------------------------

daily_country <- data.frame(
  country = country,
  date = date_parsed,
  return = ret,
  n_obs = nobs,
  stringsAsFactors = FALSE
)

daily_country <- daily_country %>%
  group_by(country) %>%
  summarise(
    N_rows = n(),
    N_dates = n_distinct(date, na.rm = TRUE),
    Return_valid = sum(is.finite(return)),
    Return_mean = mean(return[is.finite(return)]),
    Return_sd = sd(return[is.finite(return)]),
    Nobs_mean = mean(n_obs, na.rm = TRUE),
    Nobs_median = median(n_obs, na.rm = TRUE),
    Nobs_min = min(n_obs, na.rm = TRUE),
    Nobs_max = max(n_obs, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat(" DAILY PAR PAYS\n")
cat("============================================================\n")

print(daily_country)

# ------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------

OUT <- file.path(P, "results", "preliminary_tests")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

write.csv(
  coverage,
  file.path(OUT, "TABLEAU_15_DAILY_COUVERTURE.csv"),
  row.names = FALSE
)

write.csv(
  daily_country,
  file.path(OUT, "TABLEAU_14_NDAILY.csv"),
  row.names = FALSE
)

write.csv(
  dup,
  file.path(OUT, "TABLEAU_12_DAILY_DOUBLONS_COUNTRY_DATE.csv"),
  row.names = FALSE
)

cat("\n============================================================\n")
cat(" AUDIT DAILY TERMINE\n")
cat("============================================================\n")

cat("Fichiers produits dans :", OUT, "\n")
