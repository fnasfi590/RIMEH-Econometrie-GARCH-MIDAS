# ============================================================
# RIMEH - PARTIE 2
# Download and prepare the monthly oil-market SVAR inputs
# 2000-01 to 2025-12
# ============================================================
# Methodological basis:
# Kilian (2009): z_t = (Delta oil production, global real activity,
# real oil price). The structural impact matrix is recursive/lower
# triangular. See AER 99(3), 1053-1069.
#
# Primary global crude production series:
# EIA INTL.57-1-WORL-TBPD.M
# Primary global real activity series:
# FRED IGREA (updated Kilian index, Dallas Fed)
# Primary real oil price: EIA refiners' acquisition cost if available;
# fallback: FRED WTI deflated by US CPI.
# ============================================================

rm(list = ls())
source("scripts/00_packages.R")

START <- "2000-01"
END   <- "2025-12"

for (d in c("data/oil", "results/models", "results/tables", "results/figures")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

get_fred <- function(symbol) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", symbol)
  x <- readr::read_csv(url, na = c("", ".", "NA"), show_col_types = FALSE)
  names(x) <- c("date", "value")
  x$date <- as.Date(x$date)
  x$value <- suppressWarnings(as.numeric(x$value))
  x
}

# ------------------------------------------------------------
# 1. Kilian global real economic activity index
# ------------------------------------------------------------
rea <- get_fred("IGREA") %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31")) %>%
  transmute(date, global_activity = value)

# ------------------------------------------------------------
# 2. Global crude oil production - EIA
# ------------------------------------------------------------
# EIA International Energy Statistics series used in the literature:
# INTL.57-1-WORL-TBPD.M = World crude oil including lease condensate,
# thousand barrels/day, monthly.
# EIA API v2 requires a free API key.
key <- Sys.getenv("EIA_API_KEY", unset = "")
production <- NULL

if (nzchar(key)) {
  eia_url <- paste0(
    "https://api.eia.gov/v2/international/data/?api_key=",
    URLencode(key, reserved = TRUE),
    "&frequency=monthly",
    "&data[0]=value",
    "&facets[seriesId][]=INTL.57-1-WORL-TBPD.M",
    "&start=", START,
    "&end=", END,
    "&sort[0][column]=period",
    "&sort[0][direction]=asc",
    "&offset=0&length=5000"
  )

  ans <- tryCatch(
    jsonlite::fromJSON(eia_url, flatten = TRUE),
    error = function(e) {
      warning("EIA API request failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(ans) && !is.null(ans$response$data)) {
    dd <- as.data.frame(ans$response$data)
    if (nrow(dd) > 0 && all(c("period", "value") %in% names(dd))) {
      production <- as_tibble(dd) %>%
        transmute(
          date = as.Date(paste0(period, "-01")),
          global_oil_production_kbd = suppressWarnings(as.numeric(value))
        )
    }
  }
}

# Manual fallback. Download the EIA series from its Data Browser and save it as:
# data/oil/world_crude_production.csv
# Required columns: date, global_oil_production_kbd
if (is.null(production) || nrow(production) == 0) {
  fallback <- "data/oil/world_crude_production.csv"
  if (!file.exists(fallback)) {
    stop(
      "Global crude production unavailable. Set EIA_API_KEY or create ",
      fallback, " with columns date, global_oil_production_kbd."
    )
  }
  production <- readr::read_csv(fallback, show_col_types = FALSE) %>%
    transmute(
      date = as.Date(date),
      global_oil_production_kbd = as.numeric(global_oil_production_kbd)
    )
}

production <- production %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31")) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE)

# ------------------------------------------------------------
# 3. Real oil price
# ------------------------------------------------------------
# Preferred Kilian-style price: monthly refiners' acquisition cost for
# imported crude oil from EIA. Because the EIA international/petroleum
# API route can vary, this script accepts a local file if supplied.
# Required local fallback columns: date, oil_price_nominal
#
# If not supplied, use FRED WTI as a transparent reproducible fallback.
refiner_file <- "data/oil/refiners_acquisition_cost_monthly.csv"
price_source <- "FRED WTI fallback"

if (file.exists(refiner_file)) {
  refiner <- readr::read_csv(refiner_file, show_col_types = FALSE) %>%
    transmute(date = as.Date(date), oil_price_nominal = as.numeric(oil_price_nominal)) %>%
    filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31"))
  price_source <- "EIA refiners acquisition cost (local EIA export)"
} else {
  wti <- get_fred("DCOILWTICO") %>%
    mutate(month = lubridate::floor_date(date, "month")) %>%
    group_by(month) %>%
    summarise(oil_price_nominal = mean(value, na.rm = TRUE), .groups = "drop") %>%
    transmute(date = month, oil_price_nominal) %>%
    filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31"))
  refiner <- wti
}

cpi <- get_fred("CPIAUCSL") %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(cpi = mean(value, na.rm = TRUE), .groups = "drop") %>%
  transmute(date = month, cpi) %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31"))

oil_price <- refiner %>%
  left_join(cpi, by = "date") %>%
  mutate(real_oil_price = oil_price_nominal / cpi) %>%
  dplyr::select(date, oil_price_nominal, cpi, real_oil_price) %>%
  filter(is.finite(real_oil_price), real_oil_price > 0)

# ------------------------------------------------------------
# 4. Merge raw inputs
# ------------------------------------------------------------
oil_raw <- full_join(production, rea, by = "date") %>%
  full_join(oil_price, by = "date") %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31")) %>%
  arrange(date)

# ------------------------------------------------------------
# 5. Transform exactly for the SVAR
# ------------------------------------------------------------
# Kilian vector:
# z_t = (Delta prod_t, rea_t, Delta real oil price_t)'
# Production and oil price are log-differenced; IGREA is used in levels.
oil_svar <- oil_raw %>%
  mutate(
    dlog_oil_prod = 100 * (log(global_oil_production_kbd) - lag(log(global_oil_production_kbd))),
    rea = global_activity,
    dlog_real_oil_price = 100 * (log(real_oil_price) - lag(log(real_oil_price)))
  ) %>%
  dplyr::select(date, global_oil_production_kbd, global_activity,
         oil_price_nominal, cpi, real_oil_price,
         dlog_oil_prod, rea, dlog_real_oil_price)

write_csv(oil_raw, "data/oil/oil_svar_raw_inputs.csv")
write_csv(oil_svar, "data/oil/oil_svar_input.csv")

# ------------------------------------------------------------
# 6. Audit
# ------------------------------------------------------------
audit <- tibble(
  variable = names(oil_svar),
  n_non_missing = sapply(oil_svar, function(x) sum(!is.na(x))),
  n_missing = sapply(oil_svar, function(x) sum(is.na(x))),
  first_non_missing = sapply(oil_svar, function(x) {
    i <- which(!is.na(x)); if(length(i)) as.character(oil_svar$date[min(i)]) else NA_character_
  }),
  last_non_missing = sapply(oil_svar, function(x) {
    i <- which(!is.na(x)); if(length(i)) as.character(oil_svar$date[max(i)]) else NA_character_
  })
)
write_csv(audit, "results/tables/SVAR_data_audit.csv")

meta <- tibble(
  item = c("Sample", "Production source", "Activity source", "Oil price source", "Oil price deflator", "Transformation"),
  value = c(
    "2000-01 to 2025-12",
    "EIA INTL.57-1-WORL-TBPD.M",
    "FRED IGREA / Dallas Fed updated Kilian index",
    price_source,
    "FRED CPIAUCSL",
    "100*log differences for production and real oil price; IGREA in level"
  )
)
write_csv(meta, "results/tables/SVAR_data_metadata.csv")

message("PART 2 input preparation completed.")
message("Created: data/oil/oil_svar_input.csv")
message("Production source: EIA INTL.57-1-WORL-TBPD.M")
message("Oil price source: ", price_source)
message("Next: source('scripts/03_svar_oil_FINAL.R')")
