library(readr)
library(dplyr)
library(tidyr)

countries <- c(
  "DEU","GBR","IND","JPN","KOR",
  "MEX","NOR","PAK","SAU","TUR","USA"
)

infl <- read_csv(
  "data/raw/IMF_INFLATION_MENSUELLE_2000_2025.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date))

interest <- read_csv(
  "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025_FINAL.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date))

calendar <- expand_grid(
  country = countries,
  date = seq(
    as.Date("2000-01-01"),
    as.Date("2025-12-01"),
    by = "month"
  )
)

# ============================================================
# INFLATION
# ============================================================

infl_missing <- calendar %>%
  left_join(
    infl %>%
      select(country, date, inflation),
    by = c("country", "date")
  ) %>%
  filter(!is.finite(inflation)) %>%
  group_by(country) %>%
  summarise(
    n_missing = n(),
    first_missing = min(date),
    last_missing = max(date),
    missing_months = paste(
      format(date, "%Y-%m"),
      collapse = ", "
    ),
    .groups = "drop"
  )

# ============================================================
# INTEREST RATE
# ============================================================

interest_missing <- calendar %>%
  left_join(
    interest %>%
      select(country, date, interest_rate),
    by = c("country", "date")
  ) %>%
  filter(!is.finite(interest_rate)) %>%
  group_by(country) %>%
  summarise(
    n_missing = n(),
    first_missing = min(date),
    last_missing = max(date),
    missing_months = paste(
      format(date, "%Y-%m"),
      collapse = ", "
    ),
    .groups = "drop"
  )

# ============================================================
# AFFICHAGE
# ============================================================

cat("\n============================================================\n")
cat("MOIS MANQUANTS - INFLATION\n")
cat("============================================================\n")

print(infl_missing, n = 20, width = Inf)

cat("\n============================================================\n")
cat("MOIS MANQUANTS - TAUX D'INTERET\n")
cat("============================================================\n")

print(interest_missing, n = 20, width = Inf)

# ============================================================
# FICHIERS
# ============================================================

dir.create(
  "results/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  infl_missing,
  "results/tables/AUDIT_MOIS_MANQUANTS_INFLATION.csv"
)

write_csv(
  interest_missing,
  "results/tables/AUDIT_MOIS_MANQUANTS_INTERET.csv"
)

cat("\n============================================================\n")
cat("FICHIERS CREES\n")
cat("============================================================\n")

cat("AUDIT_MOIS_MANQUANTS_INFLATION.csv\n")
cat("AUDIT_MOIS_MANQUANTS_INTERET.csv\n")

cat("\nTERMINE.\n")
