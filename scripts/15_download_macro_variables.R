# ============================================================
# RIMEH - RECUPERATION MACROECONOMIQUE ROBUSTE
# Inflation / taux interet / taux change / croissance
# 11 pays - 2000-2025
# ============================================================

options(stringsAsFactors = FALSE)

pkgs <- c("httr", "jsonlite", "dplyr", "readr", "lubridate")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)

ROOT <- "C:/Users/clien/Downloads/RIMEH_ECONOMETRIE_PART3_GARCH_MIDAS/RIMEH_ECONOMETRIE"

RAW <- file.path(ROOT, "data", "raw")
PROCESSED <- file.path(ROOT, "data", "processed")
RESULTS <- file.path(ROOT, "results", "tables")

dir.create(RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS, recursive = TRUE, showWarnings = FALSE)

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

START <- as.Date("2000-01-01")
END   <- as.Date("2025-12-01")

# ============================================================
# FONCTION GENERIQUE DBNOMICS
# ============================================================

download_dbnomics <- function(dataset, code) {

  url <- paste0(
    "https://api.db.nomics.world/v22/series/IMF/",
    dataset,
    "/",
    code
  )

  cat("\nURL:", url, "\n")

  ans <- tryCatch(
    GET(
      url,
      query = list(observations = 1),
      timeout(60)
    ),
    error = function(e) {
      cat("ERREUR connexion:", conditionMessage(e), "\n")
      return(NULL)
    }
  )

  if (is.null(ans)) {
    return(NULL)
  }

  status <- status_code(ans)

  cat("HTTP:", status, "\n")

  if (status != 200) {
    return(NULL)
  }

  txt <- content(
    ans,
    as = "text",
    encoding = "UTF-8"
  )

  obj <- tryCatch(
    fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(obj)) {
    cat("JSON invalide\n")
    return(NULL)
  }

  docs <- tryCatch(
    obj$series$docs,
    error = function(e) NULL
  )

  if (is.null(docs) || length(docs) == 0) {
    cat("Aucune série trouvée\n")
    return(NULL)
  }

  d <- docs[[1]]

  periods <- unlist(
    d$period,
    use.names = FALSE
  )

  values <- unlist(
    d$value,
    use.names = FALSE
  )

  if (length(periods) == 0 ||
      length(values) == 0) {
    cat("Aucune observation\n")
    return(NULL)
  }

  n <- min(
    length(periods),
    length(values)
  )

  data.frame(
    period = periods[1:n],
    value = suppressWarnings(
      as.numeric(values[1:n])
    )
  )
}

# ============================================================
# 1. INFLATION
# ============================================================

cat("\n============================================================\n")
cat("1/4 INFLATION\n")
cat("============================================================\n")

inflation_list <- list()

for (ctry in names(countries)) {

  cc <- countries[[ctry]]

  cat("\n---", ctry, "---\n")

  # IMF IFS CPI
  # Dataset CPI, fréquence M
  code <- paste0(
    "M.",
    cc,
    ".PCPI_IX"
  )

  x <- download_dbnomics(
    "CPI",
    code
  )

  if (is.null(x)) {
    cat("CPI NON DISPONIBLE:", ctry, "\n")
    next
  }

  x$date <- as.Date(
    paste0(x$period, "-01")
  )

  x <- x %>%
    filter(
      date >= START,
      date <= END
    ) %>%
    arrange(date) %>%
    mutate(
      country = ctry,
      CPI = value,
      inflation = 100 *
        (CPI / lag(CPI) - 1)
    ) %>%
    select(
      date,
      country,
      CPI,
      inflation
    )

  inflation_list[[ctry]] <- x

  cat(
    "N =", nrow(x),
    "| inflation valide =",
    sum(is.finite(x$inflation)),
    "\n"
  )
}

if (length(inflation_list) > 0) {

  inflation_df <- bind_rows(
    inflation_list
  )

} else {

  inflation_df <- data.frame(
    date = as.Date(character()),
    country = character(),
    CPI = numeric(),
    inflation = numeric()
  )
}

write_csv(
  inflation_df,
  file.path(
    RAW,
    "IMF_INFLATION_MENSUELLE_2000_2025.csv"
  )
)

# ============================================================
# 2. TAUX D'INTERET
# ============================================================

cat("\n============================================================\n")
cat("2/4 TAUX D'INTERET\n")
cat("============================================================\n")

interest_list <- list()

for (ctry in names(countries)) {

  cc <- countries[[ctry]]

  cat("\n---", ctry, "---\n")

  code <- paste0(
    "M.",
    cc,
    ".FPOLM_PA"
  )

  x <- download_dbnomics(
    "IFS",
    code
  )

  if (is.null(x)) {
    cat(
      "Taux directeur indisponible:",
      ctry,
      "\n"
    )
    next
  }

  x$date <- as.Date(
    paste0(x$period, "-01")
  )

  x <- x %>%
    filter(
      date >= START,
      date <= END
    ) %>%
    mutate(
      country = ctry,
      interest_rate = value
    ) %>%
    select(
      date,
      country,
      interest_rate
    )

  interest_list[[ctry]] <- x

  cat(
    "N =",
    nrow(x),
    "\n"
  )
}

if (length(interest_list) > 0) {

  interest_df <- bind_rows(
    interest_list
  )

} else {

  interest_df <- data.frame(
    date = as.Date(character()),
    country = character(),
    interest_rate = numeric()
  )
}

write_csv(
  interest_df,
  file.path(
    RAW,
    "IMF_TAUX_INTERET_MENSUEL_2000_2025.csv"
  )
)

# ============================================================
# 3. TAUX DE CHANGE
# ============================================================

cat("\n============================================================\n")
cat("3/4 TAUX DE CHANGE\n")
cat("============================================================\n")

fx_list <- list()

for (ctry in names(countries)) {

  cc <- countries[[ctry]]

  cat("\n---", ctry, "---\n")

  code <- paste0(
    "M.",
    cc,
    ".ENDA_XDC_USD_RATE"
  )

  x <- download_dbnomics(
    "IFS",
    code
  )

  if (is.null(x)) {
    cat(
      "Taux de change indisponible:",
      ctry,
      "\n"
    )
    next
  }

  x$date <- as.Date(
    paste0(x$period, "-01")
  )

  x <- x %>%
    filter(
      date >= START,
      date <= END
    ) %>%
    mutate(
      country = ctry,
      exchange_rate = value
    ) %>%
    select(
      date,
      country,
      exchange_rate
    )

  fx_list[[ctry]] <- x

  cat(
    "N =",
    nrow(x),
    "\n"
  )
}

if (length(fx_list) > 0) {

  fx_df <- bind_rows(
    fx_list
  )

} else {

  fx_df <- data.frame(
    date = as.Date(character()),
    country = character(),
    exchange_rate = numeric()
  )
}

write_csv(
  fx_df,
  file.path(
    RAW,
    "IMF_TAUX_CHANGE_MENSUEL_2000_2025.csv"
  )
)

# ============================================================
# 4. CROISSANCE DU PIB
# ============================================================

cat("\n============================================================\n")
cat("4/4 CROISSANCE PIB\n")
cat("============================================================\n")

growth_list <- list()

for (ctry in names(countries)) {

  cc <- countries[[ctry]]

  cat("\n---", ctry, "---\n")

  # IMF WEO octobre 2025
  # NGDP_RPCH = Real GDP growth

  url <- paste0(
    "https://api.db.nomics.world/v22/series/IMF/WEO:2025-10/",
    cc,
    ".NGDP_RPCH?observations=1"
  )

  ans <- tryCatch(
    GET(
      url,
      timeout(60)
    ),
    error = function(e) NULL
  )

  if (is.null(ans) ||
      status_code(ans) != 200) {

    cat(
      "WEO indisponible:",
      ctry,
      "\n"
    )

    next
  }

  txt <- content(
    ans,
    as = "text",
    encoding = "UTF-8"
  )

  obj <- tryCatch(
    fromJSON(
      txt,
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(obj) ||
      is.null(obj$series$docs)) {

    cat(
      "Aucune donnée WEO:",
      ctry,
      "\n"
    )

    next
  }

  d <- obj$series$docs[[1]]

  periods <- unlist(
    d$period,
    use.names = FALSE
  )

  values <- unlist(
    d$value,
    use.names = FALSE
  )

  n <- min(
    length(periods),
    length(values)
  )

  x <- data.frame(
    country = ctry,
    year = as.integer(
      substr(
        periods[1:n],
        1,
        4
      )
    ),
    GDP_growth = suppressWarnings(
      as.numeric(
        values[1:n]
      )
    )
  ) %>%
    filter(
      year >= 2000,
      year <= 2025
    )

  growth_list[[ctry]] <- x

  cat(
    "N =", nrow(x),
    "\n"
  )
}

if (length(growth_list) > 0) {

  growth_df <- bind_rows(
    growth_list
  )

} else {

  growth_df <- data.frame(
    country = character(),
    year = integer(),
    GDP_growth = numeric()
  )
}

write_csv(
  growth_df,
  file.path(
    RAW,
    "IMF_WEO_CROISSANCE_REELLE_ANNUELLE_2000_2025.csv"
  )
)

# ============================================================
# 5. RESUME
# ============================================================

cat("\n============================================================\n")
cat("RESUME RECUPERATION\n")
cat("============================================================\n")

resume <- data.frame(
  variable = c(
    "Inflation",
    "Taux_interet",
    "Taux_change",
    "Croissance_PIB"
  ),
  observations = c(
    nrow(inflation_df),
    nrow(interest_df),
    nrow(fx_df),
    nrow(growth_df)
  ),
  valeurs_valides = c(
    sum(is.finite(inflation_df$inflation)),
    sum(is.finite(interest_df$interest_rate)),
    sum(is.finite(fx_df$exchange_rate)),
    sum(is.finite(growth_df$GDP_growth))
  )
)

print(resume)

write_csv(
  resume,
  file.path(
    RESULTS,
    "MACRO_RECUPERATION_RESUME.csv"
  )
)

# ============================================================
# 6. RESUME PAR PAYS
# ============================================================

country_base <- data.frame(
  country = names(countries)
)

r1 <- inflation_df %>%
  group_by(country) %>%
  summarise(
    inflation_N =
      sum(is.finite(inflation)),
    .groups = "drop"
  )

r2 <- interest_df %>%
  group_by(country) %>%
  summarise(
    interest_N =
      sum(is.finite(interest_rate)),
    .groups = "drop"
  )

r3 <- fx_df %>%
  group_by(country) %>%
  summarise(
    exchange_N =
      sum(is.finite(exchange_rate)),
    .groups = "drop"
  )

r4 <- growth_df %>%
  group_by(country) %>%
  summarise(
    growth_N =
      sum(is.finite(GDP_growth)),
    .groups = "drop"
  )

resume_country <- country_base %>%
  left_join(r1, by = "country") %>%
  left_join(r2, by = "country") %>%
  left_join(r3, by = "country") %>%
  left_join(r4, by = "country") %>%
  mutate(
    across(
      everything(),
      ~ ifelse(
        is.na(.),
        0,
        .
      )
    )
  )

print(resume_country)

write_csv(
  resume_country,
  file.path(
    RESULTS,
    "MACRO_RECUPERATION_PAR_PAYS.csv"
  )
)

# ============================================================
# 7. FUSION MENSUELLE
# ============================================================

macro_monthly <- country_base

dates <- seq(
  START,
  END,
  by = "month"
)

calendar <- expand.grid(
  date = dates,
  country = names(countries),
  stringsAsFactors = FALSE
)

macro_monthly <- calendar %>%
  left_join(
    inflation_df,
    by = c(
      "date",
      "country"
    )
  ) %>%
  left_join(
    interest_df,
    by = c(
      "date",
      "country"
    )
  ) %>%
  left_join(
    fx_df,
    by = c(
      "date",
      "country"
    )
  ) %>%
  arrange(
    country,
    date
  )

write_csv(
  macro_monthly,
  file.path(
    PROCESSED,
    "macro_monthly_IMF_2000_2025.csv"
  )
)

# ============================================================
# 8. VERIFICATION
# ============================================================

cat("\n============================================================\n")
cat("VERIFICATION PAR PAYS\n")
cat("============================================================\n")

verification <- macro_monthly %>%
  group_by(country) %>%
  summarise(
    N_total = n(),
    inflation = sum(
      is.finite(inflation)
    ),
    interest_rate = sum(
      is.finite(interest_rate)
    ),
    exchange_rate = sum(
      is.finite(exchange_rate)
    ),
    .groups = "drop"
  )

print(verification)

cat("\n============================================================\n")
cat("FICHIERS CREES\n")
cat("============================================================\n")

cat(
  "data/raw/IMF_INFLATION_MENSUELLE_2000_2025.csv\n"
)

cat(
  "data/raw/IMF_TAUX_INTERET_MENSUEL_2000_2025.csv\n"
)

cat(
  "data/raw/IMF_TAUX_CHANGE_MENSUEL_2000_2025.csv\n"
)

cat(
  "data/raw/IMF_WEO_CROISSANCE_REELLE_ANNUELLE_2000_2025.csv\n"
)

cat(
  "data/processed/macro_monthly_IMF_2000_2025.csv\n"
)

cat(
  "results/tables/MACRO_RECUPERATION_RESUME.csv\n"
)

cat(
  "results/tables/MACRO_RECUPERATION_PAR_PAYS.csv\n"
)

cat("\nTERMINE.\n")