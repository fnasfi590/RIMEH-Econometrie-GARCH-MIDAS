# ============================================================
# 00_download_oil_svar.R
# Download / prepare the monthly oil-market SVAR inputs.
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

# Primary source for current monthly world crude production:
# EIA International / STEO. EIA API access requires an API key.
# Register at EIA and set the key before running:
# Sys.setenv(EIA_API_KEY = "YOUR_KEY")
# The script also supports a local CSV fallback.

START <- "2000-01"
END   <- "2025-12"

key <- Sys.getenv("EIA_API_KEY", unset = "")

# ---- 1. Kilian global real economic activity index ----
# FRED series IGREA is the updated Kilian index published by Dallas Fed.
# quantmod::getSymbols downloads the FRED CSV endpoint.
get_fred <- function(symbol) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", symbol)
  x <- read_csv(url, na = c("", "."), show_col_types = FALSE)
  names(x) <- c("date", symbol)
  x$date <- as.Date(x$date)
  x[[symbol]] <- as.numeric(x[[symbol]])
  x
}

rea <- get_fred("IGREA") %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31"))

# ---- 2. Real oil price ----
# FRED DCOILWTICO is daily WTI. We average daily observations by month,
# then deflate with US CPI. This is a transparent reproducible fallback.
wti <- get_fred("DCOILWTICO") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(wti_nominal = mean(DCOILWTICO, na.rm = TRUE), .groups = "drop")

cpi <- get_fred("CPIAUCSL") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(cpi = mean(CPIAUCSL, na.rm = TRUE), .groups = "drop")

oil_price <- wti %>%
  left_join(cpi, by = "month") %>%
  mutate(real_oil_price = wti_nominal / cpi) %>%
  transmute(date = month, real_oil_price) %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31"))

# ---- 3. Global crude oil production ----
# EIA API v2 path. The exact series ID can evolve; this is deliberately
# parameterised so the source can be audited rather than silently guessed.
# Current STEO documentation identifies COPR_WORLD as world crude-oil production.
# If your EIA account/API returns a different route, change only this block.

production <- NULL

if (nzchar(key)) {
  eia_url <- paste0(
    "https://api.eia.gov/v2/steo/",
    "data/?api_key=", URLencode(key, reserved = TRUE),
    "&frequency=monthly",
    "&data[0]=value",
    "&facets[seriesId][]=COPR_WORLD",
    "&start=", START,
    "&end=", END,
    "&sort[0][column]=period&sort[0][direction]=asc",
    "&offset=0&length=5000"
  )

  ans <- tryCatch(jsonlite::fromJSON(eia_url, flatten = TRUE), error = function(e) NULL)
  if (!is.null(ans) && !is.null(ans$response$data) && nrow(ans$response$data) > 0) {
    production <- as_tibble(ans$response$data) %>%
      transmute(
        date = as.Date(paste0(period, "-01")),
        global_oil_production = as.numeric(value)
      )
  }
}

# Local fallback: data/oil/world_crude_production.csv
# Required columns: date, global_oil_production
if (is.null(production) || nrow(production) == 0) {
  fallback <- "data/oil/world_crude_production.csv"
  if (file.exists(fallback)) {
    production <- read_csv(fallback, show_col_types = FALSE) %>%
      mutate(date = as.Date(date), global_oil_production = as.numeric(global_oil_production))
  } else {
    stop(
      "No global crude production data were obtained. Set EIA_API_KEY or provide ",
      fallback, ". Do not fabricate production values."
    )
  }
}

production <- production %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31")) %>%
  distinct(date, .keep_all = TRUE)

# ---- 4. Build final SVAR input ----
oil <- rea %>%
  transmute(date, global_activity = IGREA) %>%
  left_join(production, by = "date") %>%
  left_join(oil_price, by = "date") %>%
  arrange(date)

write_csv(oil, "data/oil/oil_svar_input.csv")

# Audit report.
audit <- tibble(
  variable = names(oil),
  n = sapply(oil, function(x) sum(!is.na(x))),
  missing = sapply(oil, function(x) sum(is.na(x))),
  first = sapply(oil, function(x) { z <- which(!is.na(x)); if(length(z)) as.character(oil$date[min(z)]) else NA_character_ }),
  last = sapply(oil, function(x) { z <- which(!is.na(x)); if(length(z)) as.character(oil$date[max(z)]) else NA_character_ })
)
write_csv(audit, "results/tables/oil_svar_data_audit.csv")

message("Created data/oil/oil_svar_input.csv")
message("Next: run scripts/03_svar_oil.R")
