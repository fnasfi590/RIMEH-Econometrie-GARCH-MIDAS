library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

cat("\n============================================================\n")
cat("RIMEH - AUDIT COMPLET DES VARIABLES MACRO\n")
cat("============================================================\n")

RAW <- "data/raw"
OUT <- "results/tables"

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

countries <- c(
  "DEU","GBR","IND","JPN","KOR",
  "MEX","NOR","PAK","SAU","TUR","USA"
)

files <- list(
  inflation = file.path(RAW, "IMF_INFLATION_MENSUELLE_2000_2025.csv"),
  interest  = file.path(RAW, "IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv"),
  exchange  = file.path(RAW, "IMF_TAUX_CHANGE_MENSUEL_2000_2025_FINAL.csv"),
  growth    = file.path(RAW, "WORLD_BANK_CROISSANCE_REELLE_ANNUELLE_2000_2025.csv")
)

cat("\n--- FICHIERS ---\n")

for (v in names(files)) {
  cat(
    sprintf(
      "%-12s : %s\n",
      v,
      ifelse(file.exists(files[[v]]), "OK", "MANQUANT")
    )
  )
}

if (!all(file.exists(unlist(files)))) {
  stop("Un ou plusieurs fichiers sont manquants.")
}

infl <- read_csv(files$inflation, show_col_types = FALSE)
interest <- read_csv(files$interest, show_col_types = FALSE)
exchange <- read_csv(files$exchange, show_col_types = FALSE)
growth <- read_csv(files$growth, show_col_types = FALSE)

infl <- infl %>%
  mutate(
    date = as.Date(date),
    country = as.character(country)
  )

interest <- interest %>%
  mutate(
    date = as.Date(date),
    country = as.character(country)
  )

exchange <- exchange %>%
  mutate(
    date = as.Date(date),
    country = as.character(country)
  )

growth <- growth %>%
  mutate(
    country = as.character(country),
    year = as.integer(year)
  )

cat("\n============================================================\n")
cat("1. AUDIT PAR PAYS ET VARIABLE\n")
cat("============================================================\n")

audit_general <- bind_rows(

  infl %>%
    transmute(
      variable = "Inflation",
      country = country,
      date = date,
      value = inflation
    ),

  interest %>%
    transmute(
      variable = "Interest_rate",
      country = country,
      date = date,
      value = interest_rate
    ),

  exchange %>%
    transmute(
      variable = "Exchange_rate",
      country = country,
      date = date,
      value = exchange_rate
    ),

  growth %>%
    transmute(
      variable = "GDP_growth",
      country = country,
      date = as.Date(paste0(year, "-01-01")),
      value = GDP_growth
    )

) %>%
  filter(country %in% countries) %>%
  group_by(variable, country) %>%
  summarise(
    observations = n(),
    valid = sum(is.finite(value)),
    missing = sum(!is.finite(value)),
    minimum = ifelse(
      sum(is.finite(value)) > 0,
      min(value, na.rm = TRUE),
      NA_real_
    ),
    moyenne = ifelse(
      sum(is.finite(value)) > 0,
      mean(value, na.rm = TRUE),
      NA_real_
    ),
    maximum = ifelse(
      sum(is.finite(value)) > 0,
      max(value, na.rm = TRUE),
      NA_real_
    ),
    date_debut = ifelse(
      sum(is.finite(value)) > 0,
      as.character(min(date[is.finite(value)])),
      NA_character_
    ),
    date_fin = ifelse(
      sum(is.finite(value)) > 0,
      as.character(max(date[is.finite(value)])),
      NA_character_
    ),
    .groups = "drop"
  ) %>%
  arrange(
    variable,
    match(country, countries)
  )

write_csv(
  audit_general,
  file.path(OUT, "AUDIT_MACRO_GENERAL_11_PAYS.csv")
)

print(audit_general, n = 100)

cat("\n============================================================\n")
cat("2. MATRICE 11 PAYS x 4 VARIABLES\n")
cat("============================================================\n")

audit_matrix <- audit_general %>%
  select(variable, country, valid) %>%
  pivot_wider(
    names_from = variable,
    values_from = valid,
    values_fill = 0
  ) %>%
  arrange(match(country, countries))

write_csv(
  audit_matrix,
  file.path(OUT, "AUDIT_MACRO_MATRICE_11_PAYS.csv")
)

print(audit_matrix, n = 20)

cat("\n============================================================\n")
cat("3. DOUBLONS\n")
cat("============================================================\n")

dup_infl <- infl %>%
  count(country, date) %>%
  filter(n > 1)

dup_interest <- interest %>%
  count(country, date) %>%
  filter(n > 1)

dup_exchange <- exchange %>%
  count(country, date) %>%
  filter(n > 1)

dup_growth <- growth %>%
  count(country, year) %>%
  filter(n > 1)

cat("Inflation     :", nrow(dup_infl), "\n")
cat("Interest rate :", nrow(dup_interest), "\n")
cat("Exchange rate :", nrow(dup_exchange), "\n")
cat("GDP growth    :", nrow(dup_growth), "\n")

cat("\n============================================================\n")
cat("4. COUVERTURE MENSUELLE\n")
cat("============================================================\n")

check_monthly <- function(df, variable) {

  df %>%
    filter(country %in% countries) %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(country) %>%
    summarise(
      variable = variable,
      first_month = min(date, na.rm = TRUE),
      last_month = max(date, na.rm = TRUE),
      n_months = n_distinct(date),
      expected = 312,
      complete_312 = n_months == 312,
      .groups = "drop"
    ) %>%
    arrange(match(country, countries))
}

monthly_checks <- bind_rows(
  check_monthly(infl, "Inflation"),
  check_monthly(interest, "Interest_rate"),
  check_monthly(exchange, "Exchange_rate")
)

write_csv(
  monthly_checks,
  file.path(OUT, "AUDIT_MACRO_FREQUENCE_MENSUELLE.csv")
)

print(monthly_checks, n = 50)

cat("\n============================================================\n")
cat("5. GDP CROISSANCE ANNUELLE\n")
cat("============================================================\n")

growth_check <- growth %>%
  filter(country %in% countries) %>%
  group_by(country) %>%
  summarise(
    n_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    valid = sum(is.finite(GDP_growth)),
    missing = sum(!is.finite(GDP_growth)),
    complete_2000_2025 = n_years == 26,
    .groups = "drop"
  ) %>%
  arrange(match(country, countries))

write_csv(
  growth_check,
  file.path(OUT, "AUDIT_MACRO_GDP_ANNUEL.csv")
)

print(growth_check, n = 20)

cat("\n============================================================\n")
cat("6. CONTROLE PAKISTAN\n")
cat("============================================================\n")

pak <- interest %>%
  filter(country == "PAK") %>%
  arrange(date)

cat("Observations :", nrow(pak), "\n")
cat("Valides      :", sum(is.finite(pak$interest_rate)), "\n")
cat("NA           :", sum(!is.finite(pak$interest_rate)), "\n")

print(
  pak %>%
    filter(date >= as.Date("2024-07-01")) %>%
    select(date, interest_rate)
)

cat("\n============================================================\n")
cat("7. RESUME GLOBAL\n")
cat("============================================================\n")

resume <- audit_general %>%
  group_by(variable) %>%
  summarise(
    pays = n_distinct(country),
    observations = sum(observations),
    valid = sum(valid),
    missing = sum(missing),
    .groups = "drop"
  )

write_csv(
  resume,
  file.path(OUT, "AUDIT_MACRO_RESUME_GLOBAL.csv")
)

print(resume)

cat("\n============================================================\n")
cat("8. FICHIERS CREES\n")
cat("============================================================\n")

cat("AUDIT_MACRO_GENERAL_11_PAYS.csv\n")
cat("AUDIT_MACRO_MATRICE_11_PAYS.csv\n")
cat("AUDIT_MACRO_FREQUENCE_MENSUELLE.csv\n")
cat("AUDIT_MACRO_GDP_ANNUEL.csv\n")
cat("AUDIT_MACRO_RESUME_GLOBAL.csv\n")

cat("\n============================================================\n")
cat("AUDIT TERMINE - AUCUNE DONNEE MODIFIEE\n")
cat("============================================================\n")
