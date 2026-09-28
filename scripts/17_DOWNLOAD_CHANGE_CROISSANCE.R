# ============================================================
# RIMEH - TELECHARGEMENT TAUX DE CHANGE + CROISSANCE
# PERIODE : 2000-01 -> 2025-12
# PAYS : 11 pays
# ============================================================

options(stringsAsFactors = FALSE)

packages <- c(
  "httr2",
  "jsonlite",
  "readr",
  "dplyr",
  "tidyr",
  "lubridate",
  "purrr"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(httr2)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

ROOT <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

dir.create(file.path(ROOT, "data/raw/macro_final"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(ROOT, "results/macro_final"), recursive = TRUE, showWarnings = FALSE)

countries <- c(
  DEU = "DE",
  GBR = "GB",
  IND = "IN",
  JPN = "JP",
  KOR = "KR",
  MEX = "MX",
  NOR = "NO",
  PAK = "PK",
  SAU = "SA",
  TUR = "TR",
  USA = "US"
)

# ============================================================
# 1. TAUX DE CHANGE BIS
# ============================================================

message("============================================================")
message("1. TELECHARGEMENT TAUX DE CHANGE BIS")
message("============================================================")

download_bis <- function(iso3, iso2) {

  # BIS WS_XRU
  # M = mensuel
  # iso2 = pays
  # E = observation de fin de période

  url <- paste0(
    "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_XRU/1.0/",
    "M.", iso2, "..E",
    "?startPeriod=2000-01",
    "&endPeriod=2025-12",
    "&format=csv"
  )

  message("Téléchargement : ", iso3, " -> ", url)

  tmp <- tempfile(fileext = ".csv")

  tryCatch({

    req <- request(url)
    resp <- req_perform(req)

    writeBin(resp_body_raw(resp), tmp)

    x <- read_csv(
      tmp,
      show_col_types = FALSE,
      progress = FALSE
    )

    message("Colonnes BIS : ", paste(names(x), collapse = " | "))

    x

  }, error = function(e) {

    message("ERREUR BIS ", iso3, " : ", e$message)
    NULL

  })
}

bis_list <- map2(
  names(countries),
  countries,
  download_bis
)

names(bis_list) <- names(countries)

# ============================================================
# 2. NORMALISATION BIS
# ============================================================

exchange_all <- map2_dfr(
  bis_list,
  names(bis_list),
  function(x, iso3) {

    if (is.null(x) || nrow(x) == 0) {
      return(tibble())
    }

    names(x) <- tolower(names(x))

    # Recherche automatique de la colonne date
    date_col <- names(x)[grepl("time|period|date", names(x))][1]

    # Recherche automatique de la valeur
    value_col <- names(x)[grepl("obs_value|value", names(x))][1]

    if (is.na(date_col) || is.na(value_col)) {
      message("Structure BIS non reconnue pour ", iso3)
      return(tibble())
    }

    tibble(
      country = iso3,
      date = as.character(x[[date_col]]),
      exchange_rate = suppressWarnings(
        as.numeric(x[[value_col]])
      )
    )
  }
)

exchange_all <- exchange_all %>%
  mutate(
    date = substr(date, 1, 7),
    date = as.Date(paste0(date, "-01"))
  ) %>%
  filter(
    date >= as.Date("2000-01-01"),
    date <= as.Date("2025-12-01")
  ) %>%
  arrange(country, date)

write_csv(
  exchange_all,
  file.path(
    ROOT,
    "data/raw/macro_final/TAUX_CHANGE_BIS_2000_2025.csv"
  )
)

message("")
message("TAUX DE CHANGE")
message("Observations : ", nrow(exchange_all))
print(
  exchange_all %>%
    group_by(country) %>%
    summarise(
      n = sum(is.finite(exchange_rate)),
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE),
      .groups = "drop"
    )
)

# ============================================================
# 3. CROISSANCE PIB REEL - IMF DATAMAPPER
# ============================================================

message("")
message("============================================================")
message("2. CROISSANCE PIB REEL - IMF")
message("============================================================")

imf_url <- paste0(
  "https://www.imf.org/external/datamapper/api/v1/",
  "NGDP_RPCH/",
  paste(names(countries), collapse = ",")
)

message("URL IMF : ", imf_url)

imf_json <- request(imf_url) %>%
  req_perform() %>%
  resp_body_string()

imf_data <- fromJSON(
  imf_json,
  simplifyVector = FALSE
)

# ============================================================
# Extraction de NGDP_RPCH
# ============================================================

growth_raw <- imf_data$values$NGDP_RPCH

growth_all <- map_dfr(
  names(growth_raw),
  function(cc) {

    vals <- growth_raw[[cc]]

    if (length(vals) == 0) {
      return(tibble())
    }

    tibble(
      country = cc,
      year = as.integer(names(vals)),
      growth = as.numeric(unlist(vals))
    )
  }
)

growth_all <- growth_all %>%
  filter(
    year >= 2000,
    year <= 2025
  ) %>%
  arrange(country, year)

write_csv(
  growth_all,
  file.path(
    ROOT,
    "data/raw/macro_final/CROISSANCE_PIB_REEL_IMF_2000_2025_ANNUEL.csv"
  )
)

message("")
message("CROISSANCE ANNUELLE IMF")
print(
  growth_all %>%
    group_by(country) %>%
    summarise(
      n = sum(is.finite(growth)),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      .groups = "drop"
    )
)

# ============================================================
# 4. PASSAGE ANNUEL -> BASE MENSUELLE
# ============================================================

message("")
message("============================================================")
message("3. CREATION CROISSANCE MENSUELLE POUR RIMEH")
message("============================================================")

monthly_dates <- tibble(
  date = seq(
    as.Date("2000-01-01"),
    as.Date("2025-12-01"),
    by = "month"
  )
)

monthly_country <- tidyr::expand_grid(
  country = names(countries),
  date = monthly_dates$date
)

growth_monthly <- monthly_country %>%
  mutate(
    year = year(date)
  ) %>%
  left_join(
    growth_all,
    by = c("country", "year")
  ) %>%
  select(
    country,
    date,
    growth
  ) %>%
  arrange(country, date)

write_csv(
  growth_monthly,
  file.path(
    ROOT,
    "data/raw/macro_final/CROISSANCE_PIB_REEL_IMF_2000_2025_MENSUEL.csv"
  )
)

message("")
message("CROISSANCE MENSUELLE")
print(
  growth_monthly %>%
    group_by(country) %>%
    summarise(
      n = sum(is.finite(growth)),
      missing = sum(!is.finite(growth)),
      .groups = "drop"
    )
)

# ============================================================
# 5. FUSION TAUX DE CHANGE + CROISSANCE
# ============================================================

macro_new <- monthly_country %>%
  left_join(
    exchange_all %>%
      select(country, date, exchange_rate),
    by = c("country", "date")
  ) %>%
  left_join(
    growth_monthly,
    by = c("country", "date")
  ) %>%
  arrange(country, date)

write_csv(
  macro_new,
  file.path(
    ROOT,
    "results/macro_final/TAUX_CHANGE_CROISSANCE_FINAL_11_PAYS.csv"
  )
)

# ============================================================
# 6. AUDIT
# ============================================================

audit <- macro_new %>%
  group_by(country) %>%
  summarise(
    observations = n(),
    exchange_valid = sum(is.finite(exchange_rate)),
    exchange_missing = sum(!is.finite(exchange_rate)),
    growth_valid = sum(is.finite(growth)),
    growth_missing = sum(!is.finite(growth)),
    .groups = "drop"
  )

write_csv(
  audit,
  file.path(
    ROOT,
    "results/macro_final/AUDIT_CHANGE_CROISSANCE_FINAL.csv"
  )
)

message("")
message("============================================================")
message("AUDIT FINAL")
message("============================================================")

print(audit)

message("")
message("FICHIERS CREES :")
message("1. TAUX_CHANGE_BIS_2000_2025.csv")
message("2. CROISSANCE_PIB_REEL_IMF_2000_2025_ANNUEL.csv")
message("3. CROISSANCE_PIB_REEL_IMF_2000_2025_MENSUEL.csv")
message("4. TAUX_CHANGE_CROISSANCE_FINAL_11_PAYS.csv")
message("5. AUDIT_CHANGE_CROISSANCE_FINAL.csv")

message("")
message("TERMINE.")