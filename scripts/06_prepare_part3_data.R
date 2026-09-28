# ============================================================
# RIMEH - PARTIE 3 / DATA PREPARATION
# Merge daily returns, monthly GPR, controls and Kilian shocks.
# ============================================================
rm(list = ls())
source("scripts/00_packages.R")

req <- c("data/processed/daily_clean.csv", "data/processed/monthly_clean.csv", "data/oil/oil_structural_shocks.csv", "data/raw/country_classification.csv")
miss <- req[!file.exists(req)]
if (length(miss)) stop("Missing files. Run Part 1 and Part 2 first: ", paste(miss, collapse=", "))

dir.create("data/processed", recursive=TRUE, showWarnings=FALSE)
dir.create("results/tables", recursive=TRUE, showWarnings=FALSE)

daily <- readr::read_csv("data/processed/daily_clean.csv", show_col_types=FALSE) %>%
  mutate(date=as.Date(date), month=lubridate::floor_date(date, "month")) %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-31"))
monthly <- readr::read_csv("data/processed/monthly_clean.csv", show_col_types=FALSE) %>%
  mutate(date=as.Date(date)) %>%
  filter(date >= as.Date("2000-01-01"), date <= as.Date("2025-12-01"))
shocks <- readr::read_csv("data/oil/oil_structural_shocks.csv", show_col_types=FALSE) %>%
  mutate(date=lubridate::floor_date(as.Date(date), "month"))
classif <- readr::read_csv("data/raw/country_classification.csv", show_col_types=FALSE)

# Global monthly variables are common to all countries; country-level controls may be present.
global_monthly <- monthly %>%
  dplyr::select(date, any_of(c("GPRT","GPRA","VIX","dlog_real_oil_price","IGREA","real_wti"))) %>%
  distinct(date, .keep_all=TRUE) %>%
  left_join(shocks %>% distinct(date, .keep_all=TRUE), by="date")

country_monthly <- monthly %>%
  dplyr::select(country, date, any_of(c("interest_rate","exchange_rate","GDP","inflation","RV","n_obs"))) %>%
  left_join(classif %>% dplyr::select(country, exporter), by="country") %>%
  left_join(global_monthly, by="date") %>%
  mutate(
    exporter=as.integer(exporter),
    GPR_SUP = as.numeric(as.character(GPRT)) * as.numeric(as.character(SUP)),
    GPRT_SUP = as.numeric(as.character(GPRT)) * as.numeric(as.character(SUP)),
    GPRA_SUP = as.numeric(as.character(GPRA)) * as.numeric(as.character(SUP)),
    SUP_EXPORTER=SUP*exporter,
    SUP_positive=pmax(SUP,0),
    SUP_negative=pmin(SUP,0),
    DEM_EXPORTER=DEM*exporter,
    PRE_EXPORTER=PRE*exporter
  ) %>% arrange(country,date)

# If monthly GPR/controls were not already present, preserve explicit NA rather than fabricate.
write_csv(country_monthly, "data/processed/monthly_model_data.csv")

# Daily returns with monthly key; this is the input for GARCH/GARCH-MIDAS.
daily_model <- daily %>%
  dplyr::select(country,date,month,any_of(c("stock_price","return","exporter"))) %>%
  left_join(country_monthly, by=c("country","month"="date"))
write_csv(daily_model, "data/processed/daily_model_data.csv")

# Coverage audit
coverage <- country_monthly %>%
  group_by(country) %>%
  summarise(
    first_date=min(date,na.rm=TRUE), last_date=max(date,na.rm=TRUE), n_months=n(),
    n_GPRT=sum(!is.na(GPRT)), n_GPRA=sum(!is.na(GPRA)),
    n_SUP=sum(!is.na(SUP)), n_DEM=sum(!is.na(DEM)), n_PRE=sum(!is.na(PRE)),
    n_VIX=sum(!is.na(VIX)), n_interest=sum(!is.na(interest_rate)),
    n_exchange=sum(!is.na(exchange_rate)), n_GDP=sum(!is.na(GDP)), n_inflation=sum(!is.na(inflation)),
    .groups="drop"
  )
write_csv(coverage, "results/tables/part3_data_coverage.csv")
message("Part 3 data preparation completed.")
